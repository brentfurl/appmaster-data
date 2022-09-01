
reverse_code_cpi <- function(df) {
  #df <- df_unprocessed
            df <- df %>% mutate_at(vars(contains("cpi_operations_adaptability_1")), 
                                   ~dplyr::recode(.,"Strongly Disagree" = "Strongly Agree",
                                               "Disagree" = "Agree",
                                               "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                               "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                               "Agree" = "Disagree",
                                               "Strongly Agree" = "Strongly Disagree")) %>%
              mutate_at(vars(contains("cpi_operations_adaptability_3")), 
                        ~dplyr::recode(.,"Strongly Disagree" = "Strongly Agree",
                                    "Disagree" = "Agree",
                                    "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                    "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                    "Agree" = "Disagree",
                                    "Strongly Agree" = "Strongly Disagree")) %>%
              mutate_at(vars(contains("cpi_operations_systems_1")), 
                        ~dplyr::recode(.,"Strongly Disagree" = "Strongly Agree",
                                    "Disagree" = "Agree",
                                    "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                    "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                    "Agree" = "Disagree",
                                    "Strongly Agree" = "Strongly Disagree")) %>%
              mutate_at(vars(contains("cpi_operations_systems_2")), 
                        ~dplyr::recode(.,"Strongly Disagree" = "Strongly Agree",
                                    "Disagree" = "Agree",
                                    "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                    "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                    "Agree" = "Disagree",
                                    "Strongly Agree" = "Strongly Disagree")) %>%
              mutate_at(vars(contains("cpi_operations_systems_3")), 
                        ~dplyr::recode(.,"Strongly Disagree" = "Strongly Agree",
                                    "Disagree" = "Agree",
                                    "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                    "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                    "Agree" = "Disagree",
                                    "Strongly Agree" = "Strongly Disagree")) %>%
              mutate_at(vars(contains("cpi_people_talent_4")),
                        ~dplyr::recode(.,"Strongly Disagree" = "Strongly Agree",
                                    "Disagree" = "Agree",
                                    "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                    "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                    "Agree" = "Disagree",
                                    "Strongly Agree" = "Strongly Disagree")) %>%
              mutate_at(vars(contains("cpi_engagement_fit_2")),
                        ~dplyr::recode(.,"Strongly Disagree" = "Strongly Agree",
                                    "Disagree" = "Agree",
                                    "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                    "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                    "Agree" = "Disagree",
                                    "Strongly Agree" = "Strongly Disagree")) %>%
              mutate_at(vars(contains("cpi_engagement_climate_5")),
                        ~dplyr::recode(.,"Strongly Disagree" = "Strongly Agree",
                                    "Disagree" = "Agree",
                                    "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                    "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                    "Agree" = "Disagree",
                                    "Strongly Agree" = "Strongly Disagree")) %>% 
              mutate_at(vars(contains("extra_safety_safety_6")),
                        ~dplyr::recode(.,"Strongly Disagree" = "Strongly Agree",
                                       "Disagree" = "Agree",
                                       "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                       "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                       "Agree" = "Disagree",
                                       "Strongly Agree" = "Strongly Disagree"))

return(df)
}



