## NCEI data processing
## EMW Dec 22 2023

## install libraries #####
library(lubridate)
library(tidyverse)
library(readxl)
library(tidyverse)
library(tidyr)
library(data.table)
library(readr)
library(cdlTools) # for converting geographic names, abbreviations, FIPS
library(dplyr)

################## DATA DOWNLOAD ######################
## Consumer Price Index CPI data (accessed from GIT repository)
cpi <- read_excel(path = "C:/Users/emily/OneDrive/Desktop/Compound Threats/Historical Analysis/Data/NCEI/CPI_1996_2022_BLS.xlsx", skip = 10)

## NCEI data (accessed from HydroShare repository) ########
files <- list.files(path = "C:/Users/emily/OneDrive/Desktop/Compound Threats/Historical Analysis/Data/NCEI/Details/", pattern='NCEI_Details')
setwd("C:/Users/emily/OneDrive/Desktop/Compound Threats/Historical Analysis/Data/NCEI/Details")


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
## remove other unneccesary columns (year, begin_azimuth, end azimuth)
data <- data[,-c(5,27,30)]


# add leading zeros to CZ_FIPS so that each has 3 digits
data$CZ_FIPS = str_pad(data$CZ_FIPS, 3, pad = "0")
head(data$CZ_FIPS)

# convert data df state names to abbreviations to align with NWS data
data$State = fips(data$STATE_FIPS, to='Abbreviation')
head(data$State)

## create binary indicators for CZ_TYPE
## may not be necessary
table(data$CZ_TYPE)
data$Geo_type_county = ifelse(data$CZ_TYPE =="C", 1, 0)
data$Geo_type_zone = ifelse(data$CZ_TYPE =="Z", 1, 0)
data$Geo_type_marine = ifelse(data$CZ_TYPE =="M", 1, 0)




############# PROCESSING OF DAMAGE ESTIMATES ######################
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

################################################################################
### join NWS Zone to County correlation data ####
##################################################################################

# EMW uploaded csv on GitHub
# nws = read.csv("NWS_Zone_County.csv")

nws = read.csv("C:/Users/emily/OneDrive/Desktop/Compound Threats/Historical Analysis/Data/NWS_Zone_County.csv")
names(nws)[1] <- 'State'

# remove unnecessary columns from NWS data
nws <- nws[,-c(8:9)]

# add leading zeros so that each zone  has 3 digits and each county has 5
nws$Zone = str_pad(nws$Zone, 3, pad = "0")
head(nws$Zone)
nws$County_FIPS = str_pad(nws$County_FIPS, 5, pad = "0")
head(nws$County_FIPS)


# Data from both dfs to consider joining by...
# data: State (abbrev), STATE_FIPS, CZ_TYPE, CZ_FIPS
# nws: State (abbrev), Zone (FIPS), State_zone, County_FIPS
# complication: in data df, CZ_TYPE FIPS codes can be for counties or zones. 
# Subset data into separate dfs by CZ_TYPE (C, Z, M)
# Join CZ_FIPS to County_FIPS for county hazards
# Join CZ_FIPS to State_zone for zone hazards
#### first have to convert CZ_FIPS to State_zone format

county = data %>% filter(CZ_TYPE == "C") # 780k obs
zone = data %>% filter(CZ_TYPE == "Z" | CZ_TYPE == "M") #502k obs


# start with zone level NCEI data
# create a state_zone variable for zone data
zone$State_zone = paste(zone$State, zone$CZ_FIPS, sep = '')
head(zone$State_zone)


# right join to get all relevant counties per zone?
zone = full_join(zone, nws, by = c('State_zone'))
#zone = zone %>% filter(State.x == "CA") # looking at 24.5k obs in CA; 595k obs in US
zone$EVENT_ID = as.numeric(zone$EVENT_ID)
length(unique(zone$EVENT_ID)) # 14609 unique event IDs in CA, 502k unique in US
# which may imply that multiple counties per zone

# rename as State_geo_FIPS so that can be letter merged with county df
colnames(zone)[which(names(zone) == 'State_zone')] = 'State_geo_FIPS'
head(zone$State_geo_FIPS)



# shift to county-level NCEI data
# create a County_FIPS variable from NCEI to join to County_FIPS nws data
county$County_FIPS = paste(county$STATE_FIPS, county$CZ_FIPS, sep = '')
head(county$County_FIPS)

# left or full join here... hmmm

county = left_join(county, nws, by = c('County_FIPS'))
# 1.1M obs of 60 variables


# rename as State_geo_FIPS so that can be letter merged with county df
colnames(county)[which(names(county) == 'County_FIPS')] = 'State_geo_FIPS'
head(county$State_geo_FIPS)


### Merge zone and county data
# zone data will now contain relevant counties
# for file size purposes, filter to just CA for now
rm(cpi, data, nws)
zone = zone %>% filter(State.x == "CA") # looking at 24.5k obs in CA; 595k obs in US
county = county %>% filter(State == "CA")

county$EVENT_ID = as.character(county$EVENT_ID)
zone$EVENT_ID = as.character(zone$EVENT_ID)


data = bind_rows(county, zone)

# export
write.csv(data, 'C:\\Users\\emily\\OneDrive\\Desktop\\Compound Threats\\Historical Analysis\\Data\\NCEI_CA_22DEC23.csv' )


################ CALCULATE SUM OF LOSSES #####################
## by year, episode, and event (original dataset has virtually all unique event ids)
data <- data %>%
  group_by(start_year) %>%
  mutate(sum_damage_property_annual = sum(damage_property_adj2022,na.rm=T),
         sum_damage_crop_annual = sum(damage_crop_adj2022,na.rm=T)) %>%
  ungroup() %>%
  group_by(EPISODE_ID) %>%
  mutate(sum_damage_property_episode = sum(damage_property_adj2022,na.rm=T),
         sum_damage_crop_episode = sum(damage_crop_adj2022,na.rm=T)) %>%
  ungroup()

################ SHAR SAN BERNARDINO #########################
## county level analysis
## NOTE: for a specific county, it is important to rerun the total sums of losses again
## otherwise those numbers would reflect all counties across the US, this is done right after
## filtering for the county
sanbern <- data[data$CZ_NAME == 'SAN BERNARDINO',]

sanbern <- sanbern %>%
  group_by(start_year) %>%
  mutate(sum_damage_property_annual = sum(damage_property_adj2022,na.rm=T),
         sum_damage_crop_annual = sum(damage_crop_adj2022,na.rm=T)) %>%
  ungroup() %>%
  group_by(EPISODE_ID) %>%
  mutate(sum_damage_property_episode = sum(damage_property_adj2022,na.rm=T),
         sum_damage_crop_episode = sum(damage_crop_adj2022,na.rm=T)) %>%
  ungroup()


sanbern %>%
  mutate(t_events = nrow(.), ## total events
  t_episodes = length(unique(EPISODE_ID))) ## total episodes
  
## MEAN NUMBER OF EVENTS PER YEAR
sanbern %>% group_by(start_year) %>%
  summarise(x = length(unique(EVENT_ID))) %>%
  ungroup() %>%
  summarise(mean(x),sd(x))

## MEAN NUMBER OF EPISODES PER YEAR
sanbern %>% group_by(start_year) %>%
  summarise(x = length(unique(EPISODE_ID))) %>%
  ungroup() %>%
  summarise(mean(x),sd(x))

## MEAN NUMBER OF EVENTS PER EPISODE 
sanbern %>% group_by(EPISODE_ID) %>%
  summarise(x = length(EVENT_ID)) %>%
  ungroup() %>%
  summarise(mean(x),sd(x))

hazard_table <- as.data.frame(table(sanbern$start_year,sanbern$EVENT_TYPE))
names(hazard_table) <- c('Year','Hazard','Count')
hazard_table <- hazard_table %>%
  group_by(Year) %>%
  mutate(unique_ct = length(which(Count!=0))) %>%
  ungroup() %>% ## these are supposed to be the same for every row, 
  mutate(mean = mean(unique_ct),sd = sd(unique_ct))  ## calculates an overall average

hazard_table_episode <- as.data.frame(table(sanbern$EPISODE_ID,sanbern$EVENT_TYPE))
names(hazard_table_episode) <- c('EPISODE_ID','Hazard','Count')
hazard_table_episode <- hazard_table_episode %>%
  group_by(EPISODE_ID) %>%
  mutate(unique_ct = length(which(Count!=0))) %>%
  ungroup() %>% ## these are supposed to be the same for every row, 
  mutate(mean = mean(unique_ct),sd = sd(unique_ct))  ## calculates an overall average

## visualization of hazard data
## Event Frequency vs Year
ggplot(sanbern) +
  geom_histogram(aes(Year),bins=26,color='black') +
  theme_bw() + ylab('Count') 

##  Event Frequency vs Year by hazard type
ggplot(sanbern[!is.na(sanbern$EVENT_TYPE),]) +
  geom_histogram(aes(Year),bins=26,color='black') +
  #geom_point(aes(Year,sum_damage_property_annual/1e7),color='black') +
  facet_wrap(~EVENT_TYPE)+
  theme_bw() + ylab('Count') 

## Event-scale Losses by Year by Hazard type
ggplot(sanbern[!is.na(sanbern$EVENT_TYPE),]) +
  geom_point(aes(Year,damage_property_adj2022),color='black') +
  facet_wrap(~EVENT_TYPE)+
  theme_bw() + ylab('Count') 

## Episode Frequency vs Year
sanbern_episode <- sanbern %>% 
  group_by(Year, EPISODE_ID) %>%
  summarise(sum_damage_property_annual_episode = sum(sum_damage_property_annual)) %>%
  ungroup()

ggplot(sanbern_episode) +
  geom_histogram(aes(Year),bins=26,color='black') +
  theme_bw() + ylab('Count') 



########                        #######
########  VENTURA COUNTY  06111 #######
########                        #######


#Dataset with just California Data
California <- data %>%
  filter(STATE == 'CALIFORNIA')

#Creating new dataset with just Ventura County Data 
Ventura <- California[grep('VENTURA',California$CZ_NAME), ] ##Tried to find the zoning codes...kept the CZ_NAME column but made an additional column (COUNTY) labeling all of the names just Ventura. 
Ventura <- Ventura%>%
  mutate("COUNTY" = substr( CZ_NAME, start=1,stop=7))

#Filter out NAs
Ventura <- Ventura%>%
  filter(!is.na(EPISODE_ID),
         !is.na(EVENT_ID))

##Episode and ID Count
n_distinct(Ventura$EPISODE_ID)#352
n_distinct(Ventura$EVENT_ID)#557 

#######                   ########
####### Descriptive Stats ########
#######                   ########


##Number of (non)distinct episodes/events per year
Summary <- Ventura%>%
  group_by(Year)%>%
  summarise(numEpisodeUnique = sum(n_distinct(EPISODE_ID)),
          numEventUnique = sum(n_distinct(EVENT_ID)))

mean(Summary$numEpisode)
sd(Summary$numEpisode)
#~13.04 episodes happened on avergage per year with a std of 5.33

mean(Summary$numEvent)
sd(Summary$numEvent)
#~20.63 events happened on average per year with a std of 11.68

##Number of unique Events per Episode 
Summary2 <- Ventura%>%
  group_by(EPISODE_ID)%>%
  summarise(numEvent = sum(n_distinct(EVENT_ID)))

mean(Summary2$numEvent)
sd(Summary2$numEvent)
#~1.6 events on average per episode with a std of 1.12

###Number of event types per year
Summary3 <- Ventura%>%
  group_by(Year)%>%
  summarise(numEventType = sum(n_distinct(EVENT_TYPE)))

mean(Summary3$numEventType)
sd(Summary3$numEventType)
##~5.19 event types on average per year with a std of 2.06

###Number of event types per episode 
Summary4 <- Ventura%>%
  group_by(EPISODE_ID)%>%
  summarise(numEventType = n_distinct(EVENT_TYPE))

mean(Summary4$numEventType)
sd(Summary4$numEventType)
##~1.13 event types on average per episode with a std of .48



###############################
### LOS ANGELES COUNTY ###
##############################

################ SHAR SAN BERNARDINO #########################
## county level analysis

#Dataset with just California Data
California <- data %>%
  filter(STATE == 'CALIFORNIA') # 32959 obs in CA

#Creating new dataset with just Ventura County Data 
LosAngeles <- California[grep('LOS ANGELES',California$CZ_NAME), ] # 792 obs for LA County
##Tried to find the zoning codes...kept the CZ_NAME column but made an additional column (COUNTY) labeling all of the names just Ventura. 
##Compared CZ_Name and CZ_FIPS... found that the Los Angeles County FIPS ('37) accounted for about half of total Los Angeles observations
table(LosAngeles$CZ_FIPS) # 8 different CZ_FIPS for Los Angeles
#LosAngeles <- California[grep('37',California$CZ_FIPS), ] # 350 obs for LA County

LosAngeles<- LosAngeles%>%
  mutate("COUNTY" = substr( CZ_NAME, start=1,stop=11))

#Filter out NAs
LosAngeles <- LosAngeles %>%
  filter(!is.na(EPISODE_ID),
         !is.na(EVENT_ID))

##Episode and ID Count
n_distinct(LosAngeles$EPISODE_ID)#492
n_distinct(LosAngeles$EVENT_ID)#792

##################################################
####### Los Angeles Descriptive Stats ########
##################################################


##Number of (non)distinct episodes/events per year
Summary <- LosAngeles %>% 
  group_by(Year) %>%
  summarise(numEpisodeUnique = sum(n_distinct(EPISODE_ID)),
            numEventUnique = sum(n_distinct(EVENT_ID)))

mean(Summary$numEpisodeUnique)
sd(Summary$numEpisodeUnique)
# 18.22 episodes happened on avergage per year with a std of 7.19

mean(Summary$numEventUnique)
sd(Summary$numEventUnique)
# 29.33 events happened on average per year with a std of 14.05

##Number of unique Events per Episode 
Summary2 <- LosAngeles%>%
  group_by(EPISODE_ID)%>%
  summarise(numEvent = sum(n_distinct(EVENT_ID)))

mean(Summary2$numEvent)
sd(Summary2$numEvent)
# 1.61 events on average per episode with a std of 1.22

###Number of event types per year
Summary3 <- LosAngeles%>%
  group_by(Year)%>%
  summarise(numEventType = sum(n_distinct(EVENT_TYPE)))

mean(Summary3$numEventType)
sd(Summary3$numEventType)
## 6.85 event types on average per year with a std of 3.1

###Number of event types per episode 
Summary4 <- LosAngeles%>%
  group_by(EPISODE_ID)%>%
  summarise(numEventType = n_distinct(EVENT_TYPE))

mean(Summary4$numEventType)
sd(Summary4$numEventType)
##~1.18 event types on average per episode with a std of .53



########################################################################
### SHAR'S CODE ON LOS ANGELES DATA #####
# replace sanbern with losangeles
# different approach than that beth took
# double check consistency of results
# annual damages
#####################################################################
losangeles <- LosAngeles %>%
  group_by(start_year) %>%
  mutate(sum_damage_property_annual = sum(damage_property_adj2022,na.rm=T),
         sum_damage_crop_annual = sum(damage_crop_adj2022,na.rm=T)) %>%
  ungroup() %>%
  group_by(EPISODE_ID) %>%
  mutate(sum_damage_property_episode = sum(damage_property_adj2022,na.rm=T),
         sum_damage_crop_episode = sum(damage_crop_adj2022,na.rm=T)) %>%
  ungroup()


losangeles %>%
  mutate(t_events = nrow(.), ## total events
         t_episodes = length(unique(EPISODE_ID))) ## total episodes

## MEAN NUMBER OF EVENTS PER YEAR
losangeles %>% group_by(start_year) %>%
  summarise(x = length(unique(EVENT_ID))) %>%
  ungroup() %>%
  summarise(mean(x),sd(x))
# 29.3 mean events per year with sd of 14.1

## MEAN NUMBER OF EPISODES PER YEAR
losangeles %>% group_by(start_year) %>%
  summarise(x = length(unique(EPISODE_ID))) %>%
  ungroup() %>%
  summarise(mean(x),sd(x))
# 18.2 mean episodes per year with sd of 7.19

## MEAN NUMBER OF EVENTS PER EPISODE 
losangeles %>% group_by(EPISODE_ID) %>%
  summarise(x = length(EVENT_ID)) %>%
  ungroup() %>%
  summarise(mean(x),sd(x))
# 1.61 mean events per episode with sd of 1.22

fips_table <- as.data.frame(table(losangeles$CZ_FIPS,losangeles$CZ_NAME, losangeles$CZ_TYPE))

                            
hazard_table <- as.data.frame(table(losangeles$start_year,losangeles$EVENT_TYPE))
names(hazard_table) <- c('Year','Hazard','Count')
hazard_table <- hazard_table %>%
  group_by(Year) %>%
  mutate(unique_ct = length(which(Count!=0))) %>%
  ungroup() %>% ## these are supposed to be the same for every row, 
  mutate(mean = mean(unique_ct),sd = sd(unique_ct))  ## calculates an overall average

hazard_table_episode <- as.data.frame(table(losangeles$EPISODE_ID,losangeles$EVENT_TYPE))
names(hazard_table_episode) <- c('EPISODE_ID','Hazard','Count')
hazard_table_episode <- hazard_table_episode %>%
  group_by(EPISODE_ID) %>%
  mutate(unique_ct = length(which(Count!=0))) %>%
  ungroup() %>% ## these are supposed to be the same for every row, 
  mutate(mean = mean(unique_ct),sd = sd(unique_ct))  ## calculates an overall average

## visualization of hazard data
## Event Frequency vs Year
ggplot(losangeles) +
  geom_histogram(aes(Year),bins=26,color='black') +
  theme_bw() + ylab('Count') 

##  Event Frequency vs Year by hazard type
ggplot(losangeles[!is.na(losangeles$EVENT_TYPE),]) +
  geom_histogram(aes(Year),bins=26,color='black') +
  #geom_point(aes(Year,sum_damage_property_annual/1e7),color='black') +
  facet_wrap(~EVENT_TYPE)+
  theme_bw() + ylab('Count') 

## Event-scale Losses by Year by Hazard type
ggplot(losangeles[!is.na(losangeles$EVENT_TYPE),]) +
  geom_point(aes(Year,damage_property_adj2022),color='black') +
  facet_wrap(~EVENT_TYPE)+
  theme_bw() + ylab('Count') 

## Episode Frequency vs Year
losangeles_episode <- losangeles %>% 
  group_by(Year, EPISODE_ID) %>%
  summarise(sum_damage_property_annual_episode = sum(sum_damage_property_annual)) %>%
  ungroup()

ggplot(losangeles_episode) +
  geom_histogram(aes(Year),bins=26,color='black') +
  theme_bw() + ylab('Count') 



