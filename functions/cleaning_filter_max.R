

cleaning_filter_cpi <- function(df) {
  
  df <- df %>%
  filter(
          df$Direction_sd > 0 &
           df$Direction_sd < 40 &
          df$Operations_sd > 0 &
           df$Operations_sd < 40 &
           df$People_sd > 0 &
           df$People_sd < 40 &
           df$Engagement_sd > 0 &
           df$Engagement_sd < 40 
      )
      #   df$tot_missing < 4)
# filter(#df$Positivity != "NaN"
#   df$Direction_sd > 0
#   & df$Direction_sd < 30
#   & df$Operations_sd > 0
#   & df$Operations_sd < 30
#   & df$People_sd > 0
#   & df$People_sd < 30
#   & df$Engagement_sd > 0
#   & df$Engagement_sd < 30)

  return(df)
}


