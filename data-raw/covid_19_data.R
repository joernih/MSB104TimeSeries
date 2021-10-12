## code to prepare `DATASET` dataset goes here
library(COVID19)
library(dplyr)
library(lubridate)
library(zoo)

all_data <- COVID19::covid19(verbose = F)
unique(all_data$id)

# Countries to be included in the dataset
sel_cou <- c('NOR','ITA','SWE','GBR','ISR','FIN','CZE','ESP','USA','CAN','SVK','IND')

COVID19 <- all_data %>% dplyr::filter(id%in%sel_cou) %>%
# Datering
dplyr::mutate(year=as.factor(lubridate::year(date))) %>%
dplyr::mutate(dayofyear=lubridate::yday(date)) %>%
dplyr::select(id,date,confirmed,deaths,hosp,dayofyear,year,population) %>%
# Deaths per day
dplyr::mutate(c_deaths=deaths-dplyr::lag(deaths)) %>%
# Deaths moving average (7-days)
dplyr::mutate(ma_deaths=round(rollmean(c_deaths,k=7, fill=NA)),digits=4) %>%
#  Deaths moving average (7-days) per 100k
dplyr::mutate(ma_deaths_perc=(ma_deaths/population)*100000) 
usethis::use_data(COVID19, overwrite = TRUE)
