library(tidyverse)
library(here)
library(fgu.avoidance)
source(here("functions/loading.R"))
source(here("functions/analysis.R"))
data_pth <- here("data/OTTER 49export raw/")
folders <- list.dirs(data_pth, full.names = FALSE, recursive = FALSE)

files <- list.files(data_pth, recursive = TRUE, full.names = TRUE)
files <- gsub(data_pth, "", files)

# df files has  columns "folder file trial animal context run"
df_files_run49 <- as.data.frame(str_split(files, "/", simplify = TRUE)) %>%
  select(-V1, folder = V2, file = V3) %>%
  mutate(filename = gsub(".CSV", "", file)) %>%
  separate(filename, into = c("trial", "animal", "context"),
           sep = "_", remove = TRUE) %>%
  mutate(context = gsub("context", "", context),
         run = "49")

data_run49 <- load_run_data(data_pth, df_files_run49)
codingtables_run49 <- create_codingtables(data_run49, data_pth)
dir.create(here("temp"), showWarnings = FALSE)
save(data_run49, codingtables_run49, df_files_run49,
     file = "temp/data_run49.RData")

## EXPORT INDIVIDUAL results ----
df_results_run49 <- calculate_individual_results(data_run49, codingtables_run49,
                                                 df_files_run49)
dir.create(here("processed-results"), showWarnings = FALSE)
write.csv(df_results_run49, "processed-results/individual-results-run49.csv",
          row.names = FALSE)

rm(files, folders, data_pth, df_files_run49)
