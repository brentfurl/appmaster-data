
get_means_nps <- function(df, df_long, demo_vars) {
  
  # df<- df
  # df_long <- df_long
  demo_varsf <- c("ALL", demo_vars)
  
  dimensions_basic <- c("vision", "strategy" ,"leadership" ,"adaptability" ,"performance" ,"systems" ,"teamwork", "talent" ,"development", "fit", "customer", "climate" ,"positivity", "sat", "nps")
  
  # this finds any extra dimension prefixes for any extra questions
  extra_dims <- setdiff(unique(df_long$dimension)[!is.na(unique(df_long$dimension))], dimensions_basic)
  
  means_nps_loop <- list()
  means_nps_loop_app <- list()
  demo_sample_loop <- list()
  for (d in 1:length(demo_varsf))  {
    
    #d<-1
    demo_var <- demo_varsf[[d]]
    nps <- df %>% count(get(demo_var), nps_group) %>%
      filter(nps_group != "NA") %>% 
      #filter(`get(demo_var)` != "NA") %>%
      pivot_wider(names_from = nps_group, values_from = n) %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      rowwise() %>%
      mutate(nps_score = ((Promoter/sum(c(Detractor, Passive, Promoter), na.rm = TRUE))-(Detractor/sum(c(Detractor, Passive, Promoter), na.rm = TRUE))) *100) %>%
      rename(level = colnames(.)[1]) %>%
      #rename(level_loc = colnames(.)[2]) %>%
      mutate(level = as.character(level)) %>%
      filter(!is.na(level)) %>%
      mutate(level = factor(level)) %>%
      add_column(demo = eval(demo_var), .before = 1) %>%
      select(1:2, nps_score, everything()) 
    
    demo_sample_pre <- df %>%  count(get(demo_var), .drop=FALSE) %>% 
      rename(level = colnames(.)[1]) %>% 
      add_column(demo = eval(demo_var), .before= 1)
    
    demo_sample_loop[[d]] <- demo_sample_pre %>% 
      mutate(percentage = round(n/sum(demo_sample_pre$n)*100,1), .after = 3)
      
### get means from df_long for all of the previously calculated rowwise scores.
    
    means_dim_quads <- df_long %>% select(ResponseId, dimension, quadrant:value, cpi_or_not, everything()) %>% 
      group_by(get(demo_var)) %>%
      summarise(
                Direction = round(mean(value[quadrant == "Direction"], na.rm = TRUE),0), Operations = round(mean(value[quadrant == "Operations"], na.rm=TRUE),0),
                People = round(mean(value[quadrant == "People"], na.rm = TRUE),0), Engagement = round(mean(value[quadrant == "Engagement"], na.rm=TRUE),0),
                Vision = round(mean(value[dimension == "vision"], na.rm = TRUE),0), Strategy = round(mean(value[dimension == "strategy"], na.rm=TRUE),0),
                Leadership = round(mean(value[dimension == "leadership"], na.rm = TRUE),0), Adaptability = round(mean(value[dimension == "adaptability"], na.rm=TRUE),0),
                Performance = round(mean(value[dimension == "performance"], na.rm = TRUE),0), Systems = round(mean(value[dimension == "systems"], na.rm=TRUE),0),
                Teamwork = round(mean(value[dimension == "teamwork"], na.rm = TRUE),0), Talent = round(mean(value[dimension == "talent"], na.rm=TRUE),0),
                Development = round(mean(value[dimension == "development"], na.rm = TRUE),0), Fit = round(mean(value[dimension == "fit"], na.rm=TRUE),0),
                Customer = round(mean(value[dimension == "customer"], na.rm = TRUE),0), Climate = round(mean(value[dimension == "climate"], na.rm=TRUE),0),
                Positivity = round(mean(value[dimension == "positivity"], na.rm = TRUE),0)) %>% 
      ungroup() %>%
      mutate(`get(demo_var)` = as.character(`get(demo_var)`)) %>%
      filter(!is.na(`get(demo_var)`)) %>%
      mutate(`get(demo_var)` = factor(`get(demo_var)`)) %>%
      rename(level = colnames(.)[1]) %>% 
      add_column(demo = eval(demo_var), .before= 1)
    
    means_items <- df %>% 
      group_by(get(demo_var)) %>%
      summarise(n = n(),
                across(starts_with("cpi_", ignore.case = FALSE), ~ round(mean(.x, na.rm = TRUE),0)),
                across(starts_with("pos_"), ~ round(mean(.x, na.rm = TRUE),0)),
                across(starts_with("sat_"), ~ round(mean(.x, na.rm = TRUE),0)),
                across(starts_with("extra_"), ~ round(mean(.x, na.rm = TRUE),0))) %>%
      ungroup() %>%
      mutate(`get(demo_var)` = as.character(`get(demo_var)`)) %>%
      filter(!is.na(`get(demo_var)`)) %>%
      mutate(`get(demo_var)` = factor(`get(demo_var)`)) %>%
      rename(level = colnames(.)[1]) 
  
    means_items_app <- means_items
    colnames(means_items_app)[3:69] <- str_after_nth(colnames(means_items_app)[3:69], "_", 4)

#### I'll probably need to work on the code below the first time I try this with extra questions, but I think it's close for now.  
    ### Also remember to do the extra join when getting the means object.  
        
    # means_extra <- df %>%  
    #   group_by(get(demo_var)) %>%
    #   summarise(
    #             across(starts_with("extra_"), ~ round(mean(.x, na.rm = TRUE),0))) %>%
    #   ungroup() %>%
    #   mutate(`get(demo_var)` = as.character(`get(demo_var)`)) %>%
    #   filter(!is.na(`get(demo_var)`)) %>%
    #   mutate(`get(demo_var)` = factor(`get(demo_var)`)) %>%
    #   rename(level = colnames(.)[1]) 
    # 
    # colnames(means_extra)[2:ncol(means_extra)] <- str_after_nth(colnames(means_extra)[2:ncol(means_extra)], "_", 4)
    
      means <- inner_join(means_items, means_dim_quads, by = "level") %>% 
       # inner_join(., means_extra) %>% 
        select(demo, level, n, everything()) 
      
      means_app <- inner_join(means_items_app, means_dim_quads, by = "level") %>% 
        # inner_join(., means_extra) %>% 
        select(demo, level, n, everything()) 
      
    
    nps_means <- full_join(means, nps)
    nps_means_app <- full_join(means_app, nps)
    means_nps_loop[[d]] <- nps_means
    means_nps_loop_app[[d]] <- nps_means_app
  }
  nps_means_demos <- do.call(rbind, means_nps_loop) %>% 
    add_column(`graph number` = "", title = "", `number of items` = "", .before=1)
  
   saveRDS(nps_means_demos, paste0(file.analyses, "means_nps.rds")) 
   write_csv(nps_means_demos, paste0(file.analyses, "means_nps.csv")) 
  
nps_means_demos_app <- do.call(rbind, means_nps_loop_app) %>% filter(n > 4) %>% select(-(n)) %>% 
  add_column(`graph number` = "", title = "", `number of items` = "", .before=1)
   saveRDS(nps_means_demos_app, paste0(file.analyses, "means_nps_app.rds")) 
   write_csv(nps_means_demos_app, paste0(file.analyses, "means_nps_app.csv")) 
   
demo_samples <- do.call(rbind, demo_sample_loop) %>% add_column(maxScaleValue = "", scale = "") %>% 
  mutate_all(~replace(., is.na(.), "No response")) %>% 
  filter(demo != "ALL")
  saveRDS(demo_samples, paste0(file.analyses, "demo_samples.rds")) 
  write_csv(demo_samples, paste0(file.analyses, "demo_samples.csv")) 
  
  return(nps_means_demos)
  
}

