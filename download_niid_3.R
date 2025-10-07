library(tidyverse)
library(data.table)

data_path <- "/Users/yangliu/Library/CloudStorage/Dropbox/Github_Data/NIID/"

data.frame(week_number = as.character(1:53)) %>% 
  mutate(n_character = nchar(week_number),
         week_number = if_else(n_character == 1, paste0("0",week_number), week_number)) %>% 
  pull(week_number) -> week_index
year_index <- (2012:2024) %>% as.character()
download_index <- CJ(year = year_index,
                     week = week_index) %>% 
  mutate(week_numeric = parse_number(week)) %>%
  dplyr::filter(!(year == "2024" & week_numeric > 41)) %>% 
  dplyr::filter(!(year == "2012" & week_numeric < 31)) 

week_index

# 2015 to 2023 is the same formatting
for(j in as.character(2015:2016)){
  for(i in 1:53){
    par <- c(j, week_index[i])
    
    file_url <- paste0("https://www.niid.go.jp/niid/images/idwr/data-e/idwr-e", 
                       par[1],
                       "/",
                       par[1],
                       par[2],
                       "/teitenrui",
                       par[2],
                       ".csv")
    
    download.file(file_url,
                  destfile = paste0(data_path,paste0(par[1],
                                                     par[2],
                                                     ".csv")))
  }
}

# 2015 to 2023 is the same formatting
for(j in as.character(2012)){
  for(i in 31:53){
    par <- c(j, week_index[i])
    
    file_url <- paste0("https://www.niid.go.jp/niid/images/idwr/data-e/idwr-e", 
                       par[1],
                       "/",
                       substr(par[1],3,4),
                       par[2],
                       "/teitenrui",
                       par[2],
                       ".csv")
    
    download.file(file_url,
                  destfile = paste0(data_path,paste0(par[1],
                                                     par[2],
                                                     ".csv")))
  }
}

# the very beginning
for(j in as.character(2012)){
  for(i in 32:38){
    par <- c(j, week_index[i])
    
    file_url <- paste0("https://www.niid.go.jp/niid/images/idwr/data-e", 
                       # par[1],
                       "/",
                       substr(par[1],3,4),
                       par[2],
                       "/teitenrui",
                       par[2],
                       ".csv")
    
    download.file(file_url,
                  destfile = paste0(data_path,paste0(par[1],
                                                     par[2],
                                                     ".csv")))
  }
}

file_names <- list.files(paste0(data_path,"raw"))
data_niid <- list()

for(i in seq_along(file_names)){
  read_csv(paste0(paste0(data_path, "raw/",file_names[i])),
           comment = "") -> data_niid[[file_names[i]]]
  print(i)
}


write_rds(data_niid, paste0(data_path, "niid_3.rds"))

              