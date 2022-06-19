library(fgu.avoidance)

load_run_data <- function(datafolder, df_files){
  all_data <- list()
  for(i in 1:nrow(df_files)){
    line <- df_files[i, ]
    dat <- load_folder(file.path(datafolder, line$folder))
    all_data[[line$trial]] <- dat
  }
  return(all_data)
}

create_codingtables <- function(all_data, datafolder){
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
    mutate(source = gsub(datafolder, "", source)) %>%
    separate(source, into = c("trial", "folder", "file"), sep = "/", remove = FALSE) %>%
    select(-trial)
  codingtables[[foldercode]]$codingtable <- codingtable
  }
  return(codingtables)
}
