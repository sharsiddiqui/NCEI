setwd("~/FIRO AR/Risk Communication/Compounding Hazards")

# install libraries
library(lubridate)
library(tidyverse)
library(readxl)

## download data
## Consumer Price Index CPI data (accessed from GIT repository)
cpi <- read_excel("CPI_1996_2022_BLS.xlsx", skip = 10)

## NCEI data (accessed from HydroShare repository)
files <- list.files(pattern='NCEI_Details')

file1 <- read.csv(files[1])
file2 <- read.csv(files[2])
file3 <- read.csv(files[3])
file4 <- read.csv(files[4])
file5 <- read.csv(files[5])

data <- rbind(file1,file2,file3,file4,file5)
rm(file1,file2,file3,file4,file5)

## remove tornado columns to increase processing speed
data <- data[,-c(33:39)]

## format to date, year, month columns (45 columns -> x columns)
data$start_year <- substr(data$BEGIN_YEARMONTH,1,4)
data$start_month <- substr(data$BEGIN_YEARMONTH,5,6)
data$start_date <- as.Date(paste0(data$start_month,'-',data$BEGIN_DAY,'-',data$start_year),format='%m-%d-%Y')
data$end_year <- substr(data$END_YEARMONTH,1,4)
data$end_month <- substr(data$END_YEARMONTH,5,6)
data$end_date <- as.Date(paste0(data$end_month,'-',data$END_DAY,'-',data$end_year),format='%m-%d-%Y')
data$Year <- as.numeric(data$start_year) ## for left join later on

## remove extra date columns
data <- data[,-c(1:7)]
## remove other unneccesary columns
data <- data[,-c(5,27,30)]

## convert damage to numeric
factor_property <- ifelse(str_detect(data$DAMAGE_PROPERTY,'K'),1e3,ifelse(str_detect(data$DAMAGE_PROPERTY,'M'),1e6,1))
data$damage_property_numeric <- factor_property * as.numeric(str_remove(data$DAMAGE_PROPERTY,'K|M'))
factor_crop <- ifelse(str_detect(data$DAMAGE_PROPERTY,'K'),1e3,ifelse(str_detect(data$DAMAGE_PROPERTY,'M'),1e6,1))
data$damage_crop_numeric <- factor_crop * as.numeric(str_remove(data$DAMAGE_CROPS,'K|M'))

## NOTE: There are some columns that only contain the event narrative, i.e. the episode ID only has the event narrative..removing these for now
## but I wonder if it was like that in the raw data file
data <- data[-which(substr(data$EPISODE_ID,1,4)=='well'),] ## the narrative starts with 'well built' so I'm removing those rows 

## adjust to 2022 inflation, could go monthly but starting with annual average
## inflation formula: val_22 = val_y(CPI_22/CPI_y)
data <- left_join(data,cpi %>% select('Year','Annual'),by='Year')
names(data)[45] <- 'CPI_y'

data$damage_property_adj2022 <- data$damage_property_numeric * (292.655/data$CPI_y) ## these values seem to check out given the CPI went up 87% over this time period
data$damage_crop_adj2022 <- data$damage_crop_numeric * (292.655/data$CPI_y)

## calculate sum of losses - still working on this
## by year, episode, and event (original dataset has virtually all unique event ids
## some lost uniqueness because the same event was listed more than once a day)
data_year_summary <- data %>%
  group_by(start_year) %>%
  summarise(sum_property = sum(damage_property_adj2022),
            sum_crop = sum(damage_crop_adj2022)) %>%
  ungroup()

data_episode_summary <- data %>%
  group_by(EPISODE_ID) %>%
  summarise(sum_property = sum(damage_property_adj2022,na.rm=T),
            sum_crop = sum(damage_crop_adj2022,na.rm=T)) %>%
  ungroup()
