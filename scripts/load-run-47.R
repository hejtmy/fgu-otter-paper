library(tidyverse)
library(here)
library(fgu.avoidance)
source(here("functions/loading.R"))
source(here("functions/analysis.R"))
data_pth <- here("data/reveiw_run47/")
folders <- list.dirs(data_pth, full.names = FALSE, recursive = FALSE)

files <- list.files(data_pth, recursive = TRUE, full.names = TRUE)
files <- files[grepl("//", files)]
files <- gsub(data_pth, "", files)

# df files has  columns "folder file trial animal context run"
df_files_run47 <- as.data.frame(str_split(files, "/", simplify = TRUE)) %>%
  select(-V1, folder = V2, file = V3) %>%
  mutate(filename = gsub(".CSV", "", file)) %>%
  separate(filename, into = c("trial", "animal", "context"),
           sep = "_", remove = TRUE) %>%
  mutate(animal = gsub("rat[0]*", "", animal),
         context = gsub("context", "", context),
         run = "47")

data_run47 <- load_run_data(data_pth, df_files_run47)
codingtables_run47 <- create_codingtables(data_run47, data_pth)
dir.create(here("temp"), showWarnings = FALSE)
save(data_run47, codingtables_run47, df_files_run47,
     file = "temp/data_run47.RData")

## EXPORT INDIVIDUAL results ----
df_results_run47 <- calculate_individual_results(data_run47, codingtables_run47,
                                                   df_files_run47)
dir.create(here("processed-results"), showWarnings = FALSE)
write.csv(df_results_run47, "processed-results/individual-results-run47.csv",
          row.names = FALSE)

rm(files, folders, data_pth, df_files_run47)
