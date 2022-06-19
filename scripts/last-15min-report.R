library(fgu.avoidance)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
source("functions/visualisations.R")

knitr::opts_chunk$set(echo = TRUE)

data_pth <- "data/OTTER_raw_runs 43_44_final_normative_data/"
folders <- list.dirs(data_pth, full.names = FALSE, recursive = FALSE)

## Table prep ------
df_request <- as.data.frame(str_split(folders, "-", simplify = TRUE))
df_request <- df_request %>%
  mutate(across(everything(), str_trim)) %>%
  separate(V1, into=c("condition", "group"), sep = "\\s+") %>%
  separate(V3, into=c("analysis_1", "analysis_2"), sep="\\s\\+\\s",
           fill="left") %>%
  mutate(folder = list.dirs(data_pth, recursive = FALSE),
         average_heatmap = grepl("average heatmap", analysis_2),
         individual_analysis = !is.na(analysis_1))%>%
  unite(code, condition, group, remove=FALSE)

df_request <- df_request %>%
  filter(condition == "trial1",
         group == "handled")

files <- list.files(data_pth, recursive = TRUE, full.names = TRUE)
files <- files[grepl("//", files)]
files <- gsub(data_pth, "", files)

df_files <- as.data.frame(str_split(files, "/", simplify = TRUE)) %>%
  select(-V1, folder = V2, file = V3) %>%
  mutate(filename = gsub(".CSV", "", file)) %>%
  separate(filename, into = c("run", "trial", "animal", "context"),
           sep = "_", remove = TRUE) %>%
  mutate(animal = gsub("rat[0]*", "", animal),
         context = gsub("context", "", context),
         run = gsub("run", "", run))

codingtables <- list()
for(foldercode in names(all_data)){
  if("codingtable" %in% names(all_data[[foldercode]])){
    all_data[[foldercode]]$codingtable <- NULL
  }
  dat <- all_data[[foldercode]]
  out <- sapply(names(dat), function(x){return(list(animalcode = x,
                                             source = dat[[x]]$filepath))},
              simplify = FALSE, USE.NAMES = TRUE)
  codingtable <- data.frame(matrix(unlist(out),
                                    nrow = length(out),
                                    byrow = TRUE),
                             stringsAsFactors = FALSE)
  colnames(codingtable) <- c("animal", "source")
  codingtable <- codingtable %>%
    mutate(source = gsub(data_pth, "", source)) %>%
    separate(source, into = c("trail", "folder", "file"), sep = "/", remove = FALSE) %>%
    select(-trail)
  codingtables[[foldercode]]$codingtable <- codingtable
}

all_data <- list()
for(i in 1:nrow(df_request)){
  line <- df_request[i,]
  dat <- load_folder(line$folder)
  all_data[[line$code]] <- dat
}

## Filtering last 15 minutes -------
anim <- all_data$trial1_handled$animal_1
View(anim$position$data)

res <- data.frame()
for(anim in names(all_data$trial1_handled)){
  anim_data <- all_data$trial1_handled[[anim]]
  dat <- anim_data$position$data
  dat <- filter(dat, timestamp > tail(timestamp,1) - 900)
  anim_data$position$data <- dat
  out <- session_results(anim_data)
  out$duration <- tail(dat$timestamp, 1)
  out$animal <- anim
  res <- rbind(res, as.data.frame(out))
}

out <- res %>%
  left_join(codingtables[[line$code]]$codingtable, by="animal") %>%
  left_join(df_files, by=c("folder", "file")) %>%
  select(-c(source, folder, file))

write.csv(out, "trial1-last15min-run43-44.csv", row.names = FALSE)
