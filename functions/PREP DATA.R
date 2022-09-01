library(tidyverse)
library(forcats)
library(lubridate)
library(qualtRics)
library(googlesheets4)
library(googledrive)
library(tidytext)


surveys <- all_surveys() %>%
   filter(grepl("CPI", name)) %>%
   arrange(desc(lastModified))
#    
 qualtrics_id <- "SV_3egUI8xxHrTZSwC"

 company <- "Noble"
 computer <- "~/Dropbox/"
 side <- "Clients/"
# #computer <- "C:/Users/Brent/Dropbox/Clients/"
# #vertical_demo_vars <- c("DEPARTMENT", "RACE")
# 
 source(paste0(computer, side, company,  "/scripts/file names.R"))
# 
source(paste0(file.scripts.prep, "reverse_code_cpi.R"))
source(paste0(file.scripts.prep, "recode_to_integer_cpi.R"))
source(paste0(file.scripts.prep, "means_sd_cpi.R"))
source(paste0(file.scripts.prep, "cleaning_filter_min.R"))
source(paste0(file.scripts.prep, "cleaning_filter_max.R"))
source(paste0(file.scripts.prep, "num_missing.R"))
# source(paste0("~/Dropbox/Clients/UT - FAS/scripts/categorizing/", "cat_oeqs.R"))
#  source(paste0("~/Dropbox/Clients/UT - FAS/scripts/categorizing/", "single change cat.R"))
#source(paste0(file.scripts.prep, "vars.R")) ## could require changes
#source(paste0(file.scripts.prep, "get_df2.R")) ## could require changes
#source(paste0(file.scripts.prep, "app_data.R"))
# 
# #----------------------------------------------------------------------------------------
# 
 raw1 <- fetch_survey(surveyID = qualtrics_id, start_date = 
                        "2022-03-14",label = TRUE, force_request = TRUE) 
                           
 
 colnames(raw1)
 demo_vars <- colnames(raw1 %>% select_if(is.factor))
 #demo_vars <- c("TENURE"  ,   "GENERATION", "GENDER"    , "DEPARTMENT"   ,    "LEVEL"    ,  "LOCATION"    )
 # raw <- raw %>% 
 #   rename(DEPARTMENT = "DEPT", LOCATION = "LOC")
 demo_options <- read_csv("data/demo options.csv")
 colnames(demo_options) <- toupper(colnames(demo_options))
 raw <- raw1 %>% 
   mutate(LEVEL = factor(LEVEL, levels = demo_options %>% select(LEVEL) %>% filter(!is.na(LEVEL)) %>% pull(), ordered = TRUE),
          LOCATION = factor(LOCATION, levels = demo_options %>% select(LOCATION) %>% filter(!is.na(LOCATION)) %>% pull(), ordered = TRUE),
          REGION = factor(REGION, levels = demo_options %>% select(REGION) %>% filter(!is.na(REGION)) %>% pull(), ordered = TRUE),
          TENURE = factor(TENURE, levels = demo_options %>% select(TENURE) %>% filter(!is.na(TENURE)) %>% pull(), ordered = TRUE))#,
         # GENERATION = factor(GENERATION, levels = demo_options %>% select(GENERATION) %>% filter(!is.na(GENERATION)) %>% pull(), ordered = TRUE))
          
 
 
 saveRDS(demo_vars, paste0(file.data, "demo_vars.rds"))
 df_demos <- raw %>%
    select(all_of(demo_vars)) %>% 
    # select_if(is.factor) %>%
    add_column(ResponseId = raw$ResponseId, .before=1)
 
 df_cpi <- raw %>% 
    select(Direction_1:Direction_15, Operations_1:Operations_15, People_1:People_15, Engagement_1:Engagement_15,Positivity_1:Safety_10) 
 cpi_var_length <- length(df_cpi)
 df_nps_vars <- raw %>%
    select(Net_NPS_GROUP, Net) %>%
    rename(nps = "Net", nps_group = "Net_NPS_GROUP") %>%
    add_column(ResponseId = raw$ResponseId, .before=1)
 df_oeq <- raw %>%
    select(Change:PURPOSE_10) %>%
    add_column(ResponseId = raw$ResponseId, .before=1)
 saveRDS(df_oeq, paste0(file.data, "df_oeq.rds"))
 
 colnames_cpi_loop <- c()
 for (l in 1:length(df_cpi)){
    
    colName <- trimws(sub(".+? - ", "", attributes(df_cpi[[l]])[[1]][[1]]))
    colnames_cpi_loop[[l]] <- colName
 }
 cpi_prefs <- c(paste0("cpi_Direction_vision_", 1:5),	paste0("cpi_Direction_strategy_", 1:5),	paste0("cpi_Direction_leadership_", 1:5),		
                paste0("cpi_Operations_adaptability_", 1:5),	paste0("cpi_Operations_performance_", 1:5),	paste0("cpi_Operations_systems_", 1:5),
                paste0("cpi_People_teamwork_", 1:5),	paste0("cpi_People_talent_", 1:5),	paste0("cpi_People_development_", 1:5),		
                paste0("cpi_Engagement_fit_", 1:5),	paste0("cpi_Engagement_customer_", 1:5),	paste0("cpi_Engagement_climate_", 1:5),
                
                paste0("extra_pos_positivity_", 1:5), #paste0("extra_belonging_belonging_", 1:5), 
                "extra_sat_satisfaction_1",	"extra_eff_effort_2",
                paste0("extra_safety_safety_", 1:10)
 )
 
 
 #astericks <- c(rep("", 15), "*", "", "*", rep("", 7), rep("*", 3), rep("", 10), "*", rep("", 7), "*", rep("", 12), "*", rep("", 7)) # standard
 astericks <- c(rep("", 15), "*", "", "*", rep("", 7), rep("*", 3), rep("", 10), "", rep("", 7), "*", rep("", 12), "*", rep("", 12), "*", rep("", 4))
 colnames(df_cpi)  <- paste0(cpi_prefs, "_", colnames_cpi_loop, astericks)
 
# colnames_app <- paste0(colnames_cpi_loop, astericks)
# colnames(df_cpi) <- colnames_app
 
 
 all_cpi_vars <- colnames(df_cpi)
 saveRDS(all_cpi_vars, paste0(file.data, "all_cpi_vars.rds"))
 saveRDS(length(all_cpi_vars), paste0(file.data, "cpi_var_length.rds"))
 all_items <- paste0(colnames_cpi_loop, astericks)
 saveRDS(all_items, paste0(file.data, "all_items.rds"))



df_cpi_ri <- df_cpi %>% 
  add_column(ResponseId = raw$ResponseId, .before=1)
df_unprocessed <- full_join(df_cpi_ri, df_nps_vars) %>%
   #full_join(., df_oeq) %>%
   full_join(., df_demos) %>% 
  full_join(., df_oeq)
write_csv(df_unprocessed, paste0(file.data, "formatted_not_reversed.csv"))

missing <- num_missing(df_unprocessed)
write_csv(missing, paste0(file.data, "missing by item.csv"))
df_reverse <- reverse_code_cpi(df_unprocessed)
write_csv(df_reverse, paste0(file.data, "reversed.csv"))
df_recode <- recode_to_integer_cpi(df_reverse)
df_means <- means_sd_cpi(df_recode)
df_cleaned <- cleaning_filter_min(df_means, 3) 
write_csv(as_tibble(df_means$CPI_missing), paste0(file.data, "missing cpi by person.csv"))

df_oeq <- df_cleaned %>% 
  select(Change:PURPOSE_10)
saveRDS(df_oeq, paste0(file.data, "df_oeq.rds"))

df <- df_cleaned %>%
   add_column(ALL = "ALL") 

write_csv(df, paste0(file.data, "df.csv"))
saveRDS(df, paste0(file.data, "df.rds"))





df_long <- df %>%
   #select(-(Change:Traditions_10)) %>%  
   select(-(Change:Engagement_sd)) %>%
   pivot_longer(`cpi_Direction_vision_1_The organization's vision is very clear.`:`extra_safety_safety_10_Employees rarely seem overwhelmed in their daily work.`,
                names_to = "quantitative") %>%
   separate(quantitative, into = c("cpi_or_not", "quadrant", "dimension", "question_number", "item"), sep = "_") %>%
   mutate(quadrant = factor(quadrant, levels = c("Direction", "Operations", "People", "Engagement", "pos", "sat", "eff", "safety")),
          dimension = factor(dimension, levels = c("vision", "strategy", "leadership", "adaptability", "performance", "systems",
                                                   "teamwork", "talent", "development", "fit", "customer", "climate", "positivity", "satisfaction", "effort", "safety", "nps")),
          question_number = as.integer(question_number)) 


write_csv(df_long, paste0(file.data, "df_long.csv"))
saveRDS(df_long, paste0(file.data, "df_long.rds"))

#demo_vars <- c("ALL", "ROLE", "WORK LOCATION", "SCHOOL AREA", "TENURE", "GENERATION")
saveRDS(list(demo_vars, all_cpi_vars), paste0(file.data, "cpi_vars_n_demo_vars.rds"))


# GO TO FOLDER AND TAKE THE PREVIOUS CURRENT ONE AND MOVE IT TO PAST

####################################RUN PLOTS##################################################################
library(openxlsx)
cat_oeqs(df_oeq)  # this creates a dataset for each oeq and categorizes each of them except for Change... paste0(file.data, "df_oeq.rds")
single_change_cat(readRDS(paste0(file.data,"oeqs/datasets/df_Change.rds"))) # this categorizes Change
