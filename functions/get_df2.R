get_df2 <- function(df) {

#df <- fetch_survey(surveyID = qual_surv_id, 
#                    start_date = "2019-01-14",
#                    label = TRUE, 
#                    force_request = TRUE) 
                          
df <- raw
## THIS IS THE KEY JUNCTURE...DIVIDE INTO 3 SETS.  WITH 1-67, WITH DEMOS, WITH OTHER.
## 1. DEMOS
df_demos <- df %>%
   select_if(is.factor) 

demo_vars <- toupper(colnames(df_demos))
colnames(df_demos) <- demo_vars
df_demos <- df_demos %>%
   add_column(ResponseId = df$ResponseId, .before=1)
saveRDS(demo_vars, paste0(file.data, "demo_vars.rds"))


   
##CHANGE COLUMN NAMES ON 1-67.
df_cpi <- df %>% 
   select(Direction_1:Direction_15, Operations_1:Operations_15, People_1:People_15, Engagement_1:Engagement_15,Positivity_1:Extra_2) 
colnames_cpi_loop <- c()
for (l in 1:length(df_cpi)){
   
   colName <- trimws(attributes(df_cpi[[l]])[[1]][[1]])
   colnames_cpi_loop[[l]] <- colName
}

df_cpi <- df_cpi %>%
   add_column(ResponseId = df$ResponseId, .before=1)
   
## OTHER vars
df_others <- df %>%
   select(ResponseId, StartDate, EndDate, `Duration (in seconds)`, Net_NPS_GROUP, Net, Change)

## REJOIN
df_full <- full_join(df_others, df_cpi) %>%
   full_join(., df_demos) %>% 
   mutate(Company = company)


df_prep4_reverse_coded <- reverse_code_cpi(df_full)  ## to view number of missing for each item run num_missing(df_recoded)...from num_missing.R.
#write_csv(df_prep4_reverse_coded, paste0(file.i.prep, "df_prep4_reverse_coded.csv"))                                        # y <- num_missing(df_recoded)

df_prep5_recode_to_integer <- recode_to_integer_cpi(df_prep4_reverse_coded) 
#write_csv(df_prep5_recode_to_integer, paste0(file.i.prep, "df_prep5_recode_to_integer.csv"))  

df_prep6 <- means_sd_cpi(df_prep5_recode_to_integer) %>%
   add_column(ALL = "ALL") %>%
   mutate(ALL = factor(ALL)) %>%
   rename(nps = "Net", nps_group = "Net_NPS_GROUP")

df <- cleaning_filter_min(df_prep6) 

return(df)

}

