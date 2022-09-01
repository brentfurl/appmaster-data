means_sd_cpi <- function(df){
  
  
  
  df$CPI_missing <- apply(df %>% select(contains("cpi_")), MARGIN = 1, function(x) sum(is.na(x)))
  df$CPI_index <- rowMeans(df %>% select(contains("cpi_")), na.rm = TRUE)
  df$Direction <- rowMeans(df %>% select(contains("_direction_")),, na.rm = TRUE)
  df$Operations <- rowMeans(df %>% select(contains("_operations_")), na.rm = TRUE)
  df$People <- rowMeans(df %>% select(contains("_people_")), na.rm = TRUE)
  df$Engagement <- rowMeans(df %>% select(contains("_engagement_")), na.rm = TRUE)
  df$Vision <- rowMeans(df %>% select(contains("_vision_")), na.rm = TRUE)
  df$Strategy <- rowMeans(df %>% select(contains("_strategy_")), na.rm = TRUE)
  df$Leadership <- rowMeans(df %>% select(contains("_leadership_")), na.rm = TRUE)
  df$Adaptability <- rowMeans(df %>% select(contains("_adaptability_")), na.rm = TRUE)
  df$Performance <- rowMeans(df %>% select(contains("_performance_")), na.rm = TRUE)
  df$Systems <- rowMeans(df %>% select(contains("_systems_")), na.rm = TRUE)
  df$Teamwork <- rowMeans(df %>% select(contains("_teamwork_")), na.rm = TRUE)
  df$Talent <- rowMeans(df %>% select(contains("_talent_")), na.rm = TRUE)
  df$Development <- rowMeans(df %>% select(contains("_development_")), na.rm = TRUE)
  df$Fit <- rowMeans(df %>% select(contains("_fit_")), na.rm = TRUE)
  df$Customer <- rowMeans(df %>% select(contains("_customer_")), na.rm = TRUE)
  df$Climate <- rowMeans(df %>% select(contains("_climate_")), na.rm = TRUE)
  df$Positivity <- rowMeans(df %>% select(contains("_positivity_")), na.rm = TRUE)
  #df_prep6_recode_to_integer$tot_missing <- pmap(df_prep6_recode_to_integer %>% select(contains("cpi_")), ~sum(is.na(.)))
  
  
  df <- df %>% #select(contains("cpi")) %>%
    mutate(CPI_tot_sd = apply(df %>% select(contains("cpi_")), 1, sd, na.rm = TRUE),
           Direction_sd = apply(df %>% select(contains("_direction_")), 1, sd, na.rm = TRUE),
           Operations_sd = apply(df %>% select(contains("_operations_")), 1, sd, na.rm = TRUE),
           People_sd = apply(df %>% select(contains("_people_")), 1, sd, na.rm = TRUE),
           Engagement_sd = apply(df %>% select(contains("_engagement_")), 1, sd, na.rm = TRUE))
  
  return(df)
  
}




