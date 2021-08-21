library(fgu.avoidance)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
source("functions/visualisations.R")

anim_arena <- load_data("data/OTTER_raw_runs 43_44_final_normative_data/arena size_contextB.CSV")
anim_dead <- load_data("data/OTTER_raw_runs 43_44_final_normative_data/blue_box_freezing_imtation of dead animal.CSV")
anim_round <- load_data("data/OTTER_raw_runs 43_44_final_normative_data/round arena size_contextA.CSV")

plot_path(anim_round$animal_8888)

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

## 
anim_folder1 <- load_folder(df_request[1, "folder"])
names(anim_folder1)

session_results(anim_folder1$animal_1)
collect_crosses(anim_folder1$animal_1)

collect_crosses(anim_folder1)
create_heatmap(anim_folder1)

## Loading all -----
all_data <- list()
for(i in 1:nrow(df_request)){
  line <- df_request[i,]
  dat <- load_folder(line$folder)
  all_data[[line$code]] <- dat
}

session_results(all_data$hab1_all)

## Coding tables ----
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

## INDIVIDUAL results ----
df_individual_results <- data.frame()
for(i in 1:nrow(df_request)){
  line <- df_request[i,]
  if(line$individual_analysis) next
  out <- session_results(all_data[[line$code]]) %>%
    left_join(codingtables[[line$code]]$codingtable, by="animal") %>%
    left_join(df_files, by=c("folder", "file")) %>%
    select(-c(source, folder, file))
  df_individual_results <- rbind(df_individual_results, out)

}

df_individual_results

# Plot ALL individual graphs



for(i in 1:nrow(df_request)){
  line <- df_request[i, ]
  temp <- codingtables$hab1_all$codingtable %>%
    left_join(df_files, by=c("folder", "file")) %>%
    select(-c(source, folder, file))
  
  contextA <- filter_animals(all_data[[line$code]], temp$animal.x[temp$context == "A"])
  contextB <- filter_animals(all_data[[line$code]], temp$animal.x[temp$context == "B"])

  print(paper_heatmap(contextA, "A") + ggtitle(line$code, subtitle = "Context A"))
  print(paper_heatmap(contextB, "B") + ggtitle(line$code, subtitle = "Context B"))
}

plot_area_presence(contextA, scale = 100, colors = c("blue", "yellow")) +
  theme(legend.position = c(0.9, 1.25))
plot_area_presence(contextB, scale = 100) + theme(legend.position = c(0.9, 1.25))
