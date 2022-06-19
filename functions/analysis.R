calculate_individual_results <- function(all_data, codingtables, df_files){
  df_individual_results <- data.frame()
  for(code in names(all_data)){
    out <- session_results(all_data[[code]]) %>%
      left_join(codingtables[[code]]$codingtable, by="animal") %>%
      left_join(df_files, by=c("folder", "file")) %>%
      select(-c(source, folder, file))
    df_individual_results <- rbind(df_individual_results, out)
  }
  return(df_individual_results)
}
