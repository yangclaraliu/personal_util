library(tidyverse)
library(data.table)
library(forecast)

data_path <- "/Users/yangliu/Library/CloudStorage/Dropbox/Github_Data/NIID/"
data_niid <- read_rds(paste0(data_path, "niid_3.rds"))
lapply(1:637, function(x) c(data_niid[[x]][3,])) %>% unlist %>% table -> disease_names
lapply(1:637, function(x) which(data_niid[[x]][3,] == "Respiratory syncytial virus infection")) %>% unlist() %>% table()

# it's all flu in columns 2 and 3 and all rsv in columns 4 and 5
data_niid %>% 
  map(~.[3,2]) %>% 
  unlist %>% 
  table

# extract flu
find_flu_rsv <- list()
for(i in 1:637){
  find_flu_rsv[[i]] <- data_niid[[i]][-c(1:5),1:5] %>% 
    mutate(file_name = names(data_niid)[i])
}

find_flu_rsv %>% 
  bind_rows() %>% 
  setnames(c("pref",
             "cumulative_flu",
             "per_sen_flu",
             "cumulative_rsv",
             "per_sen_rsv",
             "file_name")) %>% 
  mutate(year = substr(file_name,1,4),
         week = substr(file_name,5,6),
         per_sen_flu = as.numeric(per_sen_flu),
         per_sen_rsv = as.numeric(per_sen_rsv),
         cumulative_flu = as.numeric(cumulative_flu),
         cumulative_rsv = as.numeric(cumulative_rsv),
         date = lubridate::ymd(paste0(year,"01-01")) + as.numeric(week)*7) -> data_flu_rsv







data_flu %>% 
  group_by(pref, year) %>% 
  group_split %>% 
  map(arrange, date) %>% 
  map(mutate, 
      per_sen_diff = c(NA, diff(per_sen)),
      cumulative_diff = c(NA, diff(cumulative)),
      per_sen_diff = if_else(week == "01", per_sen, per_sen_diff),
      cumulative_diff = if_else(week == "01", cumulative, cumulative_diff)) %>% 
  bind_rows() %>% 
  mutate(per_sen_diff = if_else(per_sen_diff < 0, 0, per_sen_diff),
         cumulative_diff = if_else(cumulative_diff < 0, 0, cumulative_diff)) %>% 
  mutate(week_numeric = as.numeric(week)) %>% 
  mutate(epidemic_week = week_numeric - 36,
         epidemic_week = if_else(epidemic_week <= 0,
                                 epidemic_week + 52,
                                 epidemic_week)) %>% 
  mutate(season_LL = if_else(week_numeric < 37, as.numeric(year) - 1, as.numeric(year)),
         season_UL = if_else(week_numeric < 37, as.numeric(year), as.numeric(year) + 1),
         epidemic_season = paste0(season_LL, "-", season_UL)) -> data_flu

data_flu %>% 
  dplyr::filter(pref == "Aichi",
                date <= ymd("2020-01-01")) %>% 
  mutate(per_sen_diff = if_else(is.na(per_sen_diff), 0, per_sen_diff)) -> tmp

cycle1 <- seq(1*4, 14*4, 1)
cycle2 <- seq(18*4, 36*3, 1)

cycle1 %>% 
  map(~ts((tmp$per_sen_diff), frequency = .)) %>% 
  map(stl, s.window = "periodic") %>% 
  map(~.$time.series) %>% 
  map(data.table) %>% 
  map(pull, remainder) %>% 
  map(auto.arima, stationary = T) -> arima_fit

arima_fit %>% 
  map(AIC) %>% 
  unlist() %>% plot()

cycle1 %>% 
  map(~ts(tmp$per_sen_diff, frequency = .)) %>% 
  map(stl, s.window = "periodic") %>% 
  map(~.$time.series) %>% 
  map(data.table) %>% 
  bind_rows(.id = "cycle1_index") %>% 
  left_join(CJ(cycle1) %>% rownames_to_column(var = "cycle1_index"), by = "cycle1_index") %>% 
  mutate(cycle1_index = as.numeric(cycle1_index)) %>% 
  group_by(cycle1_index) %>% mutate(MAE = mean(abs(remainder))) %>% 
  ggplot(., aes(x = cycle1_index, y = 1, fill = MAE)) +
  geom_tile()

map2(CJ(cycle1, cycle2) %>% pull(cycle1), 
     CJ(cycle1, cycle2) %>% pull(cycle2),
     ~msts(data = (tmp$per_sen_diff), 
           seasonal.periods = c(.x, .y))) %>% 
  map(mstl, iterate = 3) %>% 
  map(data.table) %>% 
  map(setNames,
      c("Data", "Trend", "component_cycle1", "component_cycle2", "remainder")) %>% 
  map(pull, remainder) %>% 
  map(auto.arima, stationary = T) -> arima_fit_mstl


arima_fit_mstl %>% 
  map(AIC) %>% 
  unlist() %>% enframe %>% bind_cols(CJ(cycle1, cycle2)) %>%
  mutate(value_min = min(value)) %>% 
  dplyr::filter(value == value_min)
  


msts(tmp$per_sen, seasonal.periods = c(52,87)) %>% mstl(., iterate = 5) %>% plot()
  
  
map2(CJ(cycle1, cycle2) %>% pull(cycle1), 
     CJ(cycle1, cycle2) %>% pull(cycle2),
     ~msts(data = (tmp$per_sen), 
           seasonal.periods = c(.x, .y))) %>% 
  map(mstl, iterate = 3) %>% 
  map(data.table) %>% 
  map(setNames,
      c("Data", "Trend", "component_cycle1", "component_cycle2", "remainder")) %>% 
  bind_rows(.id = "combo_index") -> output_mstl

output_mstl %>% 
  group_by(combo_index) %>% 
  summarise(MAE = mean(abs(remainder))) %>% 
  left_join(CJ(cycle1, cycle2) %>% rownames_to_column(var = "combo_index"), by = "combo_index") %>% 
  mutate(combo_index = as.numeric(combo_index)) %>% 
  ggplot(., aes(x = cycle1, y = cycle2, fill = MAE)) +
  geom_tile()

data_flu %>% 
  group_by(pref, epidemic_season) %>% 
  group_split() %>% 
  map(mutate, 
      per_sen_daily_max = max(per_sen_diff),
      cumulative_daily_max = max(cumulative_diff)) %>% 
  bind_rows() -> tmp

tmp %>% 
  # dplyr::filter(per_sen_daily_max == per_sen_diff) %>% 
  dplyr::filter(cumulative_daily_max == cumulative_diff) %>%
  mutate(pre_pandemic = date <= "2020-01-01") %>% 
  ggplot(., aes(x = epidemic_week, y = pref, fill = pre_pandemic)) +
  ggridges::geom_density_ridges()

data_flu  %>% 
  ggplot(., aes(x = epidemic_week, y = cumulative_diff, group = pref, color = year)) +
  geom_line() +
  facet_wrap(~epidemic_season)


data_flu %>% 
  mutate(week_numeric = as.numeric(week)) %>% 
  ggplot(., aes(x = date, y = cumulative_diff, group = pref, color = year)) +
  geom_line() +
  facet_wrap(~pref, scales = "free")
