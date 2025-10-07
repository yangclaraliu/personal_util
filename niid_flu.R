library(pacman)
p_load(tidyverse, lubridate)

path_dropbox <- "/Users/yangliu/Library/CloudStorage/Dropbox/Github_Data/"
df_list <- read_rds(paste0(path_dropbox, "NIID/niid_3.rds"))
pathogen_target <- "Influenza(excld. avian influenza and pandemic influenza)"
flu_index <- sapply(1:length(df_list), function(x) which(df_list[[x]][3,] == pathogen_target))
flu_list <- df_list %>% 
  map(~.[,1:3]) %>% 
  map(set_names, c("prefecture", "cumulative", "per_sen"))
flu_list[[1]]

colnames(flu_list[[1]])

flu <- flu_list %>% 
  bind_rows(.id = "fn") %>% 
  mutate(cumulative = as.numeric(cumulative),
         per_sen = as.numeric(per_sen),
         year = as.numeric(substr(fn, 1, 4)),
         week = as.numeric(substr(fn, 5, 6)),
         date = ymd(paste0(year, "-01-01")) + week*7) %>% 
  dplyr::filter(!grepl("week, ", prefecture),
                !prefecture %in% c("Prefecture", "Total No."),
                !is.na(prefecture),
                year > 2012, year < 2020) %>% 
  split(~year + prefecture) %>% 
  map(arrange, date) %>% 
  map(mutate, 
      per_sen_weekly = per_sen - lag(per_sen),
      per_sen_weekly = if_else(week == 1, per_sen, per_sen_weekly),
      per_sen_weekly = if_else(per_sen_weekly <0, NA, per_sen_weekly),
      cumulative_weekly = cumulative - lag(cumulative),
      cumulative_weekly = if_else(week == 1, cumulative, cumulative_weekly),
      cumulative_weekly = if_else(cumulative_weekly < 0, NA, cumulative_weekly)) %>% 
  bind_rows() 

flu %>% 
  ggplot(., aes(x = date, y = cumulative_weekly)) +
  geom_line() +
  geom_point() +
  facet_wrap(~prefecture)

write_rds(flu, paste0(path_dropbox, "NIID/flu.rds"))

