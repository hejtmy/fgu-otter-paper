library(tidyverse)
library(here)
source(here("functions/loading.R"))
source(here("functions/analysis.R"))

data_pth <- here("data/OTTER_raw_runs 43_44_final_normative_data/")
folders <- list.dirs(data_pth, full.names = FALSE, recursive = FALSE)

## Table prep ------
df_request_run4344 <- as.data.frame(str_split(folders, "-", simplify = TRUE))
df_request_run4344 <- df_request_run4344 %>%
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

df_files_run4344 <- as.data.frame(str_split(files, "/", simplify = TRUE)) %>%
  select(-V1, folder = V2, file = V3) %>%
  mutate(filename = gsub(".CSV", "", file)) %>%
  separate(filename, into = c("run", "trial", "animal", "context"),
           sep = "_", remove = TRUE) %>%
  mutate(animal = gsub("rat[0]*", "", animal),
         context = gsub("context", "", context),
         run = gsub("run", "", run)) %>%
  filter(!is.na(animal))

data_run4344 <- list()
for(i in 1:nrow(df_request_run4344)){
  line <- df_request_run4344[i,]
  dat <- load_folder(line$folder)
  data_run4344[[line$code]] <- dat
}
codingtables_run4344 <- create_codingtables(data_run4344, data_pth)
dir.create(here("temp"), showWarnings = FALSE)
save(data_run4344, codingtables_run4344, df_request_run4344, df_files_run4344, 
     file = "temp/data_run4344.RData")

## INDIVIDUAL results ----
df_results_run4344 <- data.frame()
for(i in 1:nrow(df_request_run4344)){
  line <- df_request_run4344[i,]
  if(!line$individual_analysis) next
  out <- session_results(data_run4344[[line$code]]) %>%
    left_join(codingtables_run4344[[line$code]]$codingtable, by="animal") %>%
    left_join(df_files_run4344, by=c("folder", "file")) %>%
    select(-c(source, folder, file))
  df_results_run4344 <- rbind(df_results_run4344, out)
}

dir.create(here("processed-results"), showWarnings = FALSE)
write.csv(df_results_run4344, "processed-results/individual-results-run4344.csv",
          row.names = FALSE)

rm(df_files_run4344, folders, files, data_pth, df_request_run4344)
