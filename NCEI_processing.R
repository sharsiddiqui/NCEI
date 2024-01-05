## NCEI data processing
## EMW SS BE 1/5/24

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
setwd("C:/Users/emily/OneDrive/Desktop/Compound Threats/Historical Analysis/Data/NCEI/Details")
files <- list.files(path = "C:/Users/emily/OneDrive/Desktop/Compound Threats/Historical Analysis/Data/NCEI/Details/", pattern='NCEI_Details')
print(files)

file1 <- read.csv(files[1])
file2 <- read.csv(files[2])
file3 <- read.csv(files[3])
file4 <- read.csv(files[4])
file5 <- read.csv(files[5]) # added 2019 - 2022 data noticed missing 1/4/24

data <- rbind(file1,file2,file3,file4,file5) # 1.5M obs of 52 variables
rm(file1,file2,file3,file4,file5)

# make sure all years included
table(data$YEAR)

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


# convert data df state names to abbreviations to align with NWS data
data$State = fips(data$STATE_FIPS, to='Abbreviation')
head(data$State) # "IL" "OK" "OK" "CA" "MN" "TX"


# delete the obs containing CZ_TYPE -- "farm equipment" (data recording error)
table(data$CZ_TYPE)
data=data[!grepl("farm", data$CZ_TYPE),]

# add leading zeros to CZ_FIPS so that each has 3 digits
data$CZ_FIPS = str_pad(data$CZ_FIPS, 3, pad = "0")
head(data$CZ_FIPS) # "034" "013" "137" "009" "064" "309"
###################################################################
### PROCESSING OF DAMAGE ESTIMATES ###
###################################################################

## convert damage to numeric
factor_property <- ifelse(str_detect(data$DAMAGE_PROPERTY,'K'),1e3,ifelse(str_detect(data$DAMAGE_PROPERTY,'M'),1e6,1))
data$damage_property_numeric <- factor_property * as.numeric(str_remove(data$DAMAGE_PROPERTY,'K|M'))
factor_crop <- ifelse(str_detect(data$DAMAGE_PROPERTY,'K'),1e3,ifelse(str_detect(data$DAMAGE_PROPERTY,'M'),1e6,1))
data$damage_crop_numeric <- factor_crop * as.numeric(str_remove(data$DAMAGE_CROPS,'K|M'))

## NOTE: There are some columns that only contain the event narrative, i.e. the episode ID only has the event narrative..removing these for now
## but I wonder if it was like that in the raw data file
## EMW: this is now resulting in 0 obs 
#data <- data[-which(substr(data$EPISODE_ID,1,4)=='well'),] ## the narrative starts with 'well built' so I'm removing those rows 


## adjust to 2022 inflation, could go monthly but starting with annual average
## inflation formula: val_22 = val_y(CPI_22/CPI_y)
data <- left_join(data,cpi %>% select('Year','Annual'),by='Year')
colnames(data)[which(names(data) == "Annual")] <- "CPI_y"

data$damage_property_adj2022 <- data$damage_property_numeric * (292.655/data$CPI_y) ## these values seem to check out given the CPI went up 87% over this time period
data$damage_crop_adj2022 <- data$damage_crop_numeric * (292.655/data$CPI_y)
summary(data$damage_property_adj2022)


################################################################################
### Export the NCEI data ###
### With adjusted loss values ###
################################################################################


# US
# rerun 1/5/2024
#write.csv(data, 'C:\\Users\\emily\\OneDrive\\Desktop\\Compound Threats\\Historical Analysis\\Data\\NCEI_US_04JAN24.csv' )


# State of: California (or state of interest)
#data= data %>% filter(State == "CA") #  obs in CA
#write.csv(data, 'C:\\Users\\emily\\OneDrive\\Desktop\\Compound Threats\\Historical Analysis\\Data\\NCEI_CA_04JAN24.csv' )
# 32959 obs


################################################################################

# import merged NCEI data for CA 
data = read.csv('C:\\Users\\emily\\OneDrive\\Desktop\\Compound Threats\\Historical Analysis\\Data\\NCEI_CA_04JAN24.csv')
# 32959 obs

# create a County_FIPS variable from NCEI to join to County_FIPS nws data
# leading zeros for state code
data$STATE_FIPS = str_pad(data$STATE_FIPS, 2, pad = "0")
head(data$STATE_FIPS) # "06" "06" "06" "06" "06" "06"
# leading zeros for CZ_FIPS code
data$CZ_FIPS = str_pad(data$CZ_FIPS, 3, pad = "0")
head(data$CZ_FIPS) # "009" "006" "015" "017" "016" "013"
# create general STATE_CZ_FIPS code which includes BOTH county and zone FIPS
data$STATE_CZ_FIPS = paste(data$STATE_FIPS, data$CZ_FIPS, sep = '')
head(data$STATE_CZ_FIPS) # "06009" "06006" "06015" "06017" "06016" "06013"
# may sure that County_FIPS has leading zeros

table(data$CZ_TYPE)

################################################################################
### join NWS Zone to County correlation data ####
################################################################################

# EMW uploaded csv on GitHub
# nws = read.csv("NWS_Zone_County.csv")
nws = read.csv("C:/Users/emily/OneDrive/Desktop/Compound Threats/Historical Analysis/Data/NWS_Zone_County.csv")
names(nws)[1] <- 'State'

# remove unnecessary columns from NWS data
nws <- nws[,-c(8:9)]

# add leading zeros so that each zone  has 3 digits and each county has 5
nws$Zone = str_pad(nws$Zone, 3, pad = "0")
head(nws$Zone) # "324" "181" "181" "102" "332" "817"
nws$County_FIPS = str_pad(nws$County_FIPS, 5, pad = "0")
head(nws$County_FIPS) # "02105" "02013" "02164" "02020" "02198" "02188"


# filter by state data
nws= nws %>% filter(State == "CA")
# 276 obs for CA


# Data from both dfs to consider joining by...
# data: State (abbrev), STATE_FIPS, CZ_TYPE, CZ_FIPS
# nws: State (abbrev), Zone (FIPS), State_zone, County_FIPS
# complication: in data df, CZ_TYPE FIPS codes can be for counties or zones. 
# Subset data into separate dfs by CZ_TYPE (C, Z, M)
# Join CZ_FIPS to County_FIPS for county hazards
# Join CZ_FIPS to State_zone for zone hazards
# first have to convert CZ_FIPS to State_zone format


# create separate dataframes for county and zone hazards to join to NWS zone-county data
county = data %>% filter(CZ_TYPE == "C") # 10,058 obs CA
zone = data %>% filter(CZ_TYPE == "Z") # 22,902 obs CA

# start with zone level NCEI data
# create a state_zone variable for zone data
zone$State_zone = paste(zone$State, zone$CZ_FIPS, sep = '')
head(zone$State_zone) # "CA009" "CA006" "CA015" "CA017" "CA016" "CA013"
head(nws$State_zone)


# join to get all relevant counties per zone
# zone = 14609 obs of 54 variables
# nws = 276 obs of 9 variables
zone = full_join(zone, nws, by = c('State_zone')) # 24690 obs of 62 variables
zone$EVENT_ID = as.numeric(zone$EVENT_ID)
length(unique(zone$EVENT_ID)) # 22902 unique event IDs in CA, 502k unique in US
# which may imply that multiple counties per zone

table(is.na(zone$County_FIPS), zone$Year) # 9037 zones not joined to counties
# may need to return to this
# "coast" zones seem to be missing...
# 001 is redwood coast; 002 is mendocino coast; 003 is north coast interior
# "county" named zones also seem to be missing -- possible conflation between zones/counties for these
# example: Siskiyou County; various Siskiyou zones


# can likely delete this chunk: 
# Create version of STATE_CZ_FIPS with state abbreviation rather than state FIPS in case later useful
#colnames(zone)[which(names(zone) == 'State_zone')] = 'STATE_CZ_FIPS2'
#head(zone$STATE_CZ_FIPS)

# Create a zone dataframe with cleaned up columns 
# keep key NCEI data and processed data columns
zone_simple = zone %>% 
  select(EPISODE_ID, EVENT_ID, STATE, County, EVENT_TYPE, CZ_TYPE, CZ_FIPS, CZ_NAME, STATE_CZ_FIPS,
         County_FIPS, start_year, start_month, start_date, end_date, CPI_y, damage_property_adj2022, damage_crop_adj2022,
         DAMAGE_PROPERTY, DAMAGE_CROPS, INJURIES_DIRECT, INJURIES_INDIRECT,  
         WFO,BEGIN_LAT, BEGIN_LON, SOURCE, MAGNITUDE,EVENT_NARRATIVE) %>% 
  distinct() 


# write csv file
#write.csv(zone_simple, 'C:\\Users\\emily\\OneDrive\\Desktop\\Compound Threats\\Historical Analysis\\Data\\NCEI_CA_05JAN24_zones.csv')

##################################################################################
## Goal: Distribute losses for each county
##       Based on proportion of that zone in the county
##
## For instance, if there was a zone level event that caused $1000 in damages across 5 counties
## The value of damages would show up as $1000 for all of those counties
## Instead, we calculate the area that each zone intersects for each of those counties
## and divide accordingly 

## Bring in data
counties <- sf::read_sf('NWS_Zones/c_05mr24.shp')
zones <- sf::read_sf('NWS_Zones/z_05mr24.shp')
zones <- zones[zones$STATE == 'CA',]

## turn off spherical geometry
sf_use_s2(FALSE) ## if I don't do this it gives me a few errors

## Identify Episodes with zonal events that distributed damages to all counties

data_zonal <- data[data$CZ_TYPE=='Z',]
data_zonal <- data_zonal[as.numeric(data_zonal$damage_property_adj2022) > 0,]
data_zonal <- data_zonal[!is.na(data_zonal$damage_property_adj2022),]
data$proportion_property <- NA ## these aren't separate conceptually, but just because we use to different columns to
data$proportion_crop <- NA
## determine if this a case where losses for either property or crops need to be distributed

### PROPERTY FOR LOOP
## for every episode ID
for (i in 1:length(unique(data_zonal$EPISODE_ID))) {
  ## create a subset of the data for just this episode
  ep_indx <- data_zonal[data_zonal$EPISODE_ID %in% unique(data_zonal$EPISODE_ID)[64],]
  ## and just this zone (in the case of more than one zone)
  for (j in 1:length(unique(ep_indx$CZ_FIPS))) {
    indx <- ep_indx[ep_indx$CZ_FIPS %in% unique(ep_indx$CZ_FIPS)[j],]
    ## if that episode has more than one record
    if (length(indx$damage_property_adj2022) > 1) {
      #print(paste0('Distributing losses for EPISODE ID: ', as.character(unique(data_zonal$EPISODE_ID)[i])))
      ## and if all damages are the same
      sum_damage <- as.character(round(sum(indx$damage_property_adj2022)/length(indx$damage_property_adj2022),2))
      single_damage <- as.character(round(indx$damage_property_adj2022[1],2))
      if (sum_damage == single_damage) {
        ## if counties exist (for i = 425 they were blank)
        ## ie if the length of records is not equal to the length of NAs in these records
        if (length(indx$County)  == length(which(!is.na(indx$County)))) {
          ## Standardize Data
          # add leading zeros to County FIPS so that each has 5 digits
          indx$County_FIPS = str_pad(indx$County_FIPS, 5, pad = "0")
          # add leading zeros to CZ_FIPS so that each has 3 digits
          indx$CZ_FIPS = str_pad(indx$CZ_FIPS, 3, pad = "0")
          ## Get the right counties and zones
          county_of_interest <- counties[counties$FIPS %in% indx$County_FIPS[1],]
          counties_of_interest <- counties[counties$FIPS %in% indx$County_FIPS[1:7],]
          zone_of_interest <- zones[zones$ZONE %in% indx$CZ_FIPS[1],]
          ## make sure coordinate reference systems match before doing the intersection
          st_crs(zone_of_interest) <- st_crs(counties_of_interest)
          # Intersect
          pi <- st_intersection(counties_of_interest, zone_of_interest)
          # for each field, get area per county intersection
          ## we also need to wrap an error handler for degenerate edges in a few shapefiles
          result <- pi %>% 
            mutate(area = st_area(.) %>% as.numeric()) %>% 
            as_tibble() %>% 
            group_by(COUNTYNAME,FIPS) %>% 
            summarize(area = sum(area),
                      proportion = area/(st_area(zone_of_interest))) %>%
            mutate(EPISODE_ID = unique(data_zonal$EPISODE_ID)[i]) %>% ungroup()
          ## last step, assign proportions back to the original data frame by episode ID, county FIPS
          data$proportion_property[data$EPISODE_ID == unique(data_zonal$EPISODE_ID)[i] & ## to the proper EPISODE ID
                                     data$CZ_FIPS == unique(ep_indx$CZ_FIPS)[j] &!is.na(data$EPISODE_ID)] <- result$proportion ## to the proper ZONE FIPS code
          ## last thing!! just need to make sure that the counties are always in order for both joins
          ## this seems to be the case, but would be good to add something to make sure..
        }
      }
    }
  }
}

## CROP DAMAGE LOOP
for (i in 1:length(unique(data_zonal$EPISODE_ID))) {
  ## create a subset of the data for just this episode
  ep_indx <- data_zonal[data_zonal$EPISODE_ID %in% unique(data_zonal$EPISODE_ID)[i],]
  ## and just this zone (in the case of more than one zone)
  for (j in 1:length(unique(ep_indx$CZ_FIPS))) {
    indx <- ep_indx[ep_indx$CZ_FIPS %in% unique(ep_indx$CZ_FIPS)[j],]
    ## if that episode has more than one record
    if (length(which(!is.na(indx$damage_crop_adj2022))) > 1) {
      if(sum(indx$damage_crop_adj2022,na.rm=T) != 0) {
        #print(paste0('Distributing losses for EPISODE ID: ', as.character(unique(data_zonal$EPISODE_ID)[i])))
        ## and if all damages are the same
        if (sum(indx$damage_crop_adj2022)/length(indx$damage_crop_adj2022) == indx$damage_crop_adj2022[1] ) {
          ## if counties exist (for i = 425 they were blank)
          ## ie if the length of records is not equal to the length of NAs in these records
          if (length(indx$County)  == length(which(!is.na(indx$County)))) {
            ## Standardize Data
            # add leading zeros to County FIPS so that each has 5 digits
            indx$County_FIPS = str_pad(indx$County_FIPS, 5, pad = "0")
            # add leading zeros to CZ_FIPS so that each has 3 digits
            indx$CZ_FIPS = str_pad(indx$CZ_FIPS, 3, pad = "0")
            ## Get the right counties and zones
            county_of_interest <- counties[counties$FIPS %in% indx$County_FIPS[1],]
            counties_of_interest <- counties[counties$FIPS %in% indx$County_FIPS[1:7],]
            zone_of_interest <- zones[zones$ZONE %in% indx$CZ_FIPS[1],]
            ## make sure coordinate reference systems match before doing the intersection
            st_crs(zone_of_interest) <- st_crs(counties_of_interest)
            # Intersect
            pi <- st_intersection(counties_of_interest, zone_of_interest)
            # for each field, get area per county intersection
            ## we also need to wrap an error handler for degenerate edges in a few shapefiles
            result <- pi %>% 
              mutate(area = st_area(.) %>% as.numeric()) %>% 
              as_tibble() %>% 
              group_by(COUNTYNAME,FIPS) %>% 
              summarize(area = sum(area),
                        proportion = area/(st_area(zone_of_interest))) %>%
              mutate(EPISODE_ID = unique(data_zonal$EPISODE_ID)[i]) %>% ungroup()
            ## last step, assign proportions back to the original data frame by episode ID, county FIPS
            data$proportion_crop[data$EPISODE_ID == unique(data_zonal$EPISODE_ID)[i] & ## to the proper EPISODE ID
                                   data$CZ_FIPS == unique(ep_indx$CZ_FIPS)[j] &!is.na(data$EPISODE_ID)] <- result$proportion ## to the proper ZONE FIPS code
            ## last thing!! just need to make sure that the counties are always in order for both joins
            ## this seems to be the case, but would be good to add something to make sure..
          }
        }
      }
    }
  }
}

data$damage_dist_property <- data$damage_property_adj2022 * data$proportion_property
data$damage_dist_crop <- data$damage_crop_adj2022 * data$proportion_crop


#### TESTING ######
## FOR JUST ONE CASE
test1 <- data[data$EPISODE_ID=='2150868',] 
# add leading zeros to County FIPS so that each has 5 digits
test1$County_FIPS = str_pad(test1$County_FIPS, 5, pad = "0")
# add leading zeros to CZ_FIPS so that each has 3 digits
test1$CZ_FIPS = str_pad(test1$CZ_FIPS, 3, pad = "0")

county_of_interest <- counties[counties$FIPS %in% test1$County_FIPS[1],]
counties_of_interest <- counties[counties$FIPS %in% test1$County_FIPS[1:7],]
zone_of_interest <- zones[zones$ZONE %in% test1$CZ_FIPS[1],]

## This zone is 7,736,053,024 m2 or 7,736 km2
st_area(zone_of_interest)/1e6

## make sure coordinate reference systems match before doing the intersection
st_crs(zone_of_interest) <- st_crs(counties_of_interest) ## thse are both epsg: 4269

# intersect and visualize
pi <- st_intersection(counties_of_interest, zone_of_interest)
plot(counties_of_interest$geometry, axes = TRUE)
plot(zone_of_interest$geometry, add = TRUE)
plot(pi$geometry, add = TRUE, col = 'red')

mapview(counties_of_interest,col.regions='red') + 
  mapview(zone_of_interest,col.regions='yellow')

# add in areas in m2
propArea <- pi %>% 
  mutate(area = st_area(.) %>% as.numeric())

# for each field, get area per county intersection
propArea %>% 
  as_tibble() %>% 
  group_by(COUNTYNAME) %>% 
  summarize(area = sum(area),
            proportion = area/(st_area(zone_of_interest)))
## Yolo and Sacramento counties make up the largest proportions so this makes sense



## County-level NCEI data processing

# create county_FIPS variable
county$County_FIPS = county$STATE_CZ_FIPS

# join nws data to county data so that both zone and county data have same columns
county = left_join(county, nws, by = c('County_FIPS'))
# 77,194 obs CA -- with duplicate rows

# Create a county dataframe with cleaned up columns 
# keep key NCEI data and processed data columns
county_simple = county %>% 
  select(EPISODE_ID, EVENT_ID, STATE, County, EVENT_TYPE, CZ_TYPE, CZ_FIPS, CZ_NAME, STATE_CZ_FIPS,
         County_FIPS, start_year, start_month, start_date, end_date, CPI_y, damage_property_adj2022, damage_crop_adj2022,
         DAMAGE_PROPERTY, DAMAGE_CROPS, INJURIES_DIRECT, INJURIES_INDIRECT,  
         WFO,BEGIN_LAT, BEGIN_LON, SOURCE, MAGNITUDE,EVENT_NARRATIVE) %>%
  distinct() # delete duplicate rows
# 10,058 obs in CA


### Merge zone and county data into one dataframe
# zone data will now contain unique rows per hazard for each county it spans
data2 = rbind(county_simple, zone_simple)
# 50,073 obs of 26 variables
# 10,058 obs county + 40,011 obs zone


# export the joined and clean zone and county data 
# where each CZ_TYPE = Z can have multiple rows per county, if zone crosses county boundaries
#write.csv(data2, 'C:\\Users\\emily\\OneDrive\\Desktop\\Compound Threats\\Historical Analysis\\Data\\NCEI_CA_5JAN24_clean.csv' )


###################################################################################
### import updated NCEI data for with adj damages and zone-county information
###################################################################################

data = read.csv('C:/Users/emily/OneDrive/Desktop/Compound Threats/Historical Analysis/Data/NCEI_CA_5JAN24_clean.csv')
# 50,073 obs of 28 vars

# delete first column -- unnecessary
data <- data[ -c(1) ]

# create a County_FIPS variable from NCEI to join to County_FIPS nws data
# leading zeros for CZ_FIPS code and County_FIPS code
data$CZ_FIPS = str_pad(data$CZ_FIPS, 3, pad = "0")
head(data$CZ_FIPS) # "009" "006" "015" "017" "016" "013"
data$County_FIPS = str_pad(data$County_FIPS, 5, pad = "0")
# leading zeros for STATE_CZ_FIPS
data$STATE_CZ_FIPS = str_pad(data$STATE_CZ_FIPS, 5, pad = "0")
head(data$STATE_CZ_FIPS) # "06009" "06006" "06015" "06017" "06016" "06013"


# from here, we should be looking at the County_FIPS level.
data$EVENT_ID = as.numeric(data$EVENT_ID)
length(unique(data$EVENT_ID)) # 32,960 unique event IDs in CA
# implies that for the CZ_TYPE = Z obs, there are multiple counties (distinct rows) per zone

# how many obs have NA County_FIPS?
table(is.na(data$County_FIPS)) # 9037 obs have NA County_FIPS
table(is.na(data$County_FIPS), data$CZ_TYPE) # all obs with NA County_FIPS are zone events

# OUTSTANDING ISSUE
table(is.na(data$County_FIPS), data$start_year) # 9037 zones not joined to counties
# may need to return to this
# "coast" zones seem to be missing...
# 001 is redwood coast; 002 is mendocino coast; 003 is north coast interior
# "county" named zones also seem to be missing -- possible conflation between zones/counties for these
# example: Siskiyou County; various Siskiyou zones


# damage estimates per county
# NCEI records at EITHER the county or zone level depending on CZ_TYPE
# keep CZ_TYPE = C -- county hazards -- as is
# need to scale CZ_TYPE == Z -- zone hazards -- by each zone's land area per county


#############################################################################
### County-level hazard occurrence visualizations ###
############################################################################


##each zone align with county or counties --> column for each zone create # zones in each county and county represents each zone 
##For each county - area dominance per county 

data <- read_csv("NCEI_CA_5JAN24_clean.csv")


###Choose 3-5 hazards for Ventura County
###Individual event occurrence trends in Ventura County

##sub-setting the data for Ventura County 
California <- data %>%
  filter(STATE == 'CALIFORNIA') # may already be filtered

##looking at loss distribution within X County (Zone and County)
##note: i replaced "ventura" with "county" so that we can plug in whatever county we want here
county <- California[grep('Los Angeles',California$County), ] 
# note: renamed county
# so that this is the only place where we could need to change county name

###Occurance by event type 


###facet by event type -- all hazards
# all hazards
table(county$EVENT_TYPE)
ggplot(county, aes(x = start_year, fill = EVENT_TYPE)) +
  geom_bar()
# specific hazards
county %>% 
  filter(EVENT_TYPE == "Wildfire" | EVENT_TYPE =="Flood"|EVENT_TYPE == "Heavy Rain"|EVENT_TYPE =="Flash Flood"|EVENT_TYPE =="Debris Flow" | EVENT_TYPE == "High Wind") %>%
  ggplot(aes(x = start_year, fill = EVENT_TYPE)) +
  geom_bar() + 
  ylab('County Hazard Event Count') + xlab('Year')






###Wildfires (30)
County_With_Zones %>%
  filter(EVENT_TYPE=='Wildfire') %>%
  group_by(start_month) %>%
  summarise(occurance = n()) %>%
  ggplot(aes(x = (start_month), y = occurance)) +
  geom_col() +
  labs(title = "Occurances per Month",
       x = "Month Name",
       y = "Number of Occurances") 

###High Wind (207)
County_With_Zones %>%
  filter(EVENT_TYPE=='High Wind') %>%
  group_by(MONTH_NAME) %>%
  summarise(occurance = n()) %>%
  ggplot(aes(x = MONTH_NAME, y = occurance)) +
  geom_col() +
  labs(title = "Occurances per Month",
       x = "Month Name",
       y = "Number of Occurances") 

###Flash Flood(49)
County_With_Zones %>%
  filter(EVENT_TYPE=='Flash Flood') %>%
  group_by(MONTH_NAME) %>%
  summarise(occurance = n()) %>%
  ggplot(aes(x = MONTH_NAME, y = occurance)) +
  geom_col() +
  labs(title = "Occurances per Month",
       x = "Month Name",
       y = "Number of Occurances") 

###Winter Storm(56)
County_With_Zones %>%
  filter(EVENT_TYPE=='Winter Storm') %>%
  group_by(MONTH_NAME) %>%
  summarise(occurance = n()) %>%
  ggplot(aes(x = MONTH_NAME, y = occurance)) +
  geom_col() +
  labs(title = "Occurances per Month",
       x = "Month Name",
       y = "Number of Occurances") 




####VISUALS with top  4 event types

selected_hazards <- c('Wildfire', 'High Wind', 'Flash Flood', 'Winter Storm')


filtered_data <- County_With_Zones %>%
  filter(EVENT_TYPE %in% selected_hazards)


##Density plot (probability based plot - relative measure of the likelihood of an observation occurring at a specific month)
ggplot(filtered_data, aes(x = MONTH_NAME, fill = EVENT_TYPE)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Hazard Occurrences by Month",
       x = "Month Name",
       y = "Density") +
  scale_fill_manual(values = c('Wildfire' = 'red', 'High Wind' = 'blue', 'Flash Flood' = 'green', 'Winter Storm' = 'purple')) +
  theme_minimal()

##Stacked Bar Chart 
ggplot(filtered_data, aes(x = MONTH_NAME, fill = EVENT_TYPE)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Chart of Hazard Occurrences by Month",
       x = "Month Name",
       y = "Number of Occurrences") +
  scale_fill_manual(values = c('Wildfire' = 'red', 'High Wind' = 'blue', 'Flash Flood' = 'green', 'Winter Storm' = 'purple')) +
  theme_minimal()

##Faceted Plot 
ggplot(filtered_data, aes(x = MONTH_NAME, fill = EVENT_TYPE)) +
  geom_bar(position = "stack") +
  labs(title = "Faceted Bar Chart of Hazard Occurrences by Month",
       x = "Month Name",
       y = "Number of Occurrences") +
  facet_wrap(~EVENT_TYPE, scales = "free_y") +
  scale_fill_manual(values = c('Wildfire' = 'red', 'High Wind' = 'blue', 'Flash Flood' = 'green', 'Winter Storm' = 'purple')) +
  theme_minimal()

###Bar Plot
ggplot(filtered_data, aes(x = MONTH_NAME, fill = EVENT_TYPE)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Seasonality of Hazard Occurrences",
       x = "Month Name",
       y = "Number of Occurrences",
       fill = "Hazard Type") +
  scale_fill_manual(values = c('Wildfire' = 'red', 'High Wind' = 'blue', 'Flash Flood' = 'green', 'Winter Storm' = 'purple')) +
  theme_minimal()

###Looking at Seasonality weekly

filtered_data_weekly <- filtered_data%>%
  mutate(Week = week(start_date))

##Density plot (probability based plot - relative measure of the likelihood of an observation occurring at a specific month)
ggplot(filtered_data_weekly, aes(x = Week, fill = EVENT_TYPE)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Hazard Occurrences by Week",
       x = "Week",
       y = "Density") +
  scale_fill_manual(values = c('Wildfire' = 'red', 'High Wind' = 'blue', 'Flash Flood' = 'green', 'Winter Storm' = 'purple')) +
  theme_minimal()

##Stacked Bar Chart 
ggplot(filtered_data_weekly, aes(x = Week, fill = EVENT_TYPE)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Chart of Hazard Occurrences by Week",
       x = "Week",
       y = "Number of Occurrences") +
  scale_fill_manual(values = c('Wildfire' = 'red', 'High Wind' = 'blue', 'Flash Flood' = 'green', 'Winter Storm' = 'purple')) +
  theme_minimal()

##Faceted Plot 
ggplot(filtered_data_weekly, aes(x = Week, fill = EVENT_TYPE)) +
  geom_bar(position = "stack") +
  labs(title = "Faceted Bar Chart of Hazard Occurrences by Week",
       x = "Week",
       y = "Number of Occurrences") +
  facet_wrap(~EVENT_TYPE, scales = "free_y") +
  scale_fill_manual(values = c('Wildfire' = 'red', 'High Wind' = 'blue', 'Flash Flood' = 'green', 'Winter Storm' = 'purple')) +
  theme_minimal()

###Bar Plot
ggplot(filtered_data_weekly, aes(x = Week, fill = EVENT_TYPE)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Seasonality of Hazard Occurrences",
       x = "Week",
       y = "Number of Occurrences",
       fill = "Hazard Type") +
  scale_fill_manual(values = c('Wildfire' = 'red', 'High Wind' = 'blue', 'Flash Flood' = 'green', 'Winter Storm' = 'purple')) +
  theme_minimal()


###HEATMAPS

aggregated_data <- filtered_data_weekly %>%
  group_by(Year, start_month, EVENT_TYPE) %>%
  summarise(Count = n()) #getting the frequency of event type of every month in every year 

# Create the heatmap
ggplot(aggregated_data, aes(x = start_month, y = Year, fill = Count)) +
  geom_tile() +
  labs(title = "Hazard Occurrences Count Over Years and Months",
       x = "Month",
       y = "Year",
       fill = "Hazard Count") +
  theme_minimal()


ggplot(aggregated_data, aes(x = start_month, y = Year, fill = Count)) +
  geom_tile() +
  labs(title = "Hazard Occurrences Count Over Years and Months",
       x = "Month",
       y = "Year",
       fill = "Hazard Count") +
  facet_wrap(~ EVENT_TYPE, scales = "free_y") +  # Facet by EVENT_TYPE
  theme_minimal()




##Lookiing into the damages 
view(County_With_Zones %>%
       filter(EVENT_TYPE %in% c('Flash Flood', 'Flood', 'Wildfire', 'Heavy Rain')) %>%
       group_by(EPISODE_ID) %>%
       summarise(total_damage = sum(damage_property_adj2022)))

County_With_Zones %>%
  filter(EVENT_TYPE %in% c('Flash Flood', 'Flood', 'Wildfire', 'Heavy Rain')) %>%
  group_by(EPISODE_ID) %>%
  summarise(total_damage = sum(damage_property_adj2022)) %>%
  ggplot(aes(x = as.factor(EPISODE_ID), y = total_damage)) +
  geom_col() +
  coord_flip() +
  labs(title = "Damages per Episode",
       x = "Episode ID",
       y = "Total Damage") +
  ylim(0, NA) 

##how many of each event type are there?
Num_event_type_County <- County%>%
  count(EVENT_TYPE) ##Major difference between flood and flash flood?





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

summary(data$sum_damage_crop_annual)

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
## county level analysis

#Dataset with just California Data
California <- data %>%
  filter(STATE == 'CALIFORNIA') # 32959 obs in CA

#Creating new dataset with just Ventura County Data 
LosAngeles <- California[grep('LOS ANGELES',California$CZ_NAME), ] # 698 obs for LA County
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
n_distinct(LosAngeles$EPISODE_ID)#421
n_distinct(LosAngeles$EVENT_ID)#698

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
# 18.3 episodes happened on avergage per year with a std of 7.8

mean(Summary$numEventUnique)
sd(Summary$numEventUnique)
# 30.3 events happened on average per year with a std of 14.9

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
## 7.3 event types on average per year with a std of 3.1

###Number of event types per episode 
Summary4 <- LosAngeles%>%
  group_by(EPISODE_ID)%>%
  summarise(numEventType = n_distinct(EVENT_TYPE))

mean(Summary4$numEventType)
sd(Summary4$numEventType)
##~1.2 event types on average per episode with a std of .56



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
  theme_bw() + ylab('Hazard Event Count') 

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
  theme_bw() + ylab('Episode Count') 



## Event types by year
# all hazards
table(losangeles$EVENT_TYPE)
ggplot(losangeles, aes(x = Year, fill = EVENT_TYPE)) +
  geom_bar()

# specific hazards
losangeles2 = losangeles %>%
  filter(EVENT_TYPE == "Wildfire" | EVENT_TYPE =="Flood"|EVENT_TYPE == "Heavy Rain"|EVENT_TYPE =="Flash Flood"|EVENT_TYPE =="Debris Flow")
ggplot(losangeles2, aes(x = Year, fill = EVENT_TYPE)) +
  geom_bar()

library(scales)
losangeles2$start_date = as.Date(losangeles2$start_date, format="%m/%d/%Y")   
ggplot(losangeles2, aes(x = start_date, fill = EVENT_TYPE)) +
  geom_bar() + 
  scale_x_date(date_breaks = "4 months", date_labels = "%m-%Y")


# trend lines
ggplot(losangeles2, aes(x=start_date)) + 
  geom_line(aes(group=EVENT_TYPE, color=EVENT_TYPE), size=2, alpha=0.5)


# select hazard bubble plot

# Most basic bubble plot
losangeles2 %>%
  arrange(desc(start_date)) %>%
  ggplot(aes(x=start_year, y=start_month,color=EVENT_TYPE)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Hazardous Events")



