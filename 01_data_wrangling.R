library(tidyverse)
library(lubridate)


# loading custom function
source("R-functions/merge_files.R")


list_of_files <- list.files(path = "data/env", full.names = T)

temp <- merge_files(list_of_files)

region_names <- str_match(temp$file_name, "env/\\s*(.*?)\\s*.csv") 

temp$region <- region_names[,2]


env <- temp %>% 
        separate(region, c("region", "variable")) %>% 
        select(-file_name) %>% 
        pivot_wider(c(region, date), names_from=variable, values_from = var) %>%
        filter(date>=as.POSIXct(as.Date("2003-01-01")) & date<=as.POSIXct(as.Date("2019-12-01")))


rm(temp, region_names, list_of_files)
