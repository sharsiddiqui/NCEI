## NCEI data processing
## 01-08-2024
## includes zone-county conversion and losses distributed by zone area/county

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
library(mapview)

#################################################################################
### uploading and merging raw NCEI Storm Data ###
#################################################################################

## NCEI data (accessed from HydroShare repository) ########
## link to data: https://www.hydroshare.org/resource/a4ff2f63a9e44462b7ed7893a085f7ea/ 
setwd("C:/Users/emily/OneDrive/Desktop/Compound Threats/Historical Analysis/Data/NCEI/Details")
files <- list.files(path = "C:/Users/emily/OneDrive/Desktop/Compound Threats/Historical Analysis/Data/NCEI/Details/", pattern='NCEI_Details')
print(files)

# upload the NCEI raw data files
file1 <- read.csv(files[1])
file2 <- read.csv(files[2])
file3 <- read.csv(files[3])
file4 <- read.csv(files[4])
file5 <- read.csv(files[5]) # added 2019 - 2022 data noticed missing 1/4/24

data <- rbind(file1,file2,file3,file4,file5) # 1.5M obs of 52 variables
rm(file1,file2,file3,file4,file5) # remove 

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

## Consumer Price Index CPI data (accessed from GIT repository)
cpi <- read_excel(path = "C:/Users/emily/OneDrive/Desktop/Compound Threats/Historical Analysis/Data/NCEI/CPI_1996_2022_BLS.xlsx", skip = 10)

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


### Export the NCEI data ###
### With adjusted loss values ###

# US
#write.csv(data, 'C:\\Users\\emily\\OneDrive\\Desktop\\Compound Threats\\Historical Analysis\\Data\\NCEI_US_1_8_24.csv' )



# State of: California (or state of interest)
#data= data %>% filter(State == "CA") #  obs in CA
#write.csv(data, 'C:\\Users\\emily\\OneDrive\\Desktop\\Compound Threats\\Historical Analysis\\Data\\NCEI_CA_1_8_24.csv' )
# 32959 obs for California


################################################################################

# checkpoint: able to import merged NCEI data for CA here or continue from code above
#data = read.csv('C:\\Users\\emily\\OneDrive\\Desktop\\Compound Threats\\Historical Analysis\\Data\\NCEI_CA_1_8_24.csv')
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
# return to this
# "coast" zones seem to be missing...
# 001 is redwood coast; 002 is mendocino coast; 003 is north coast interior
# "county" named zones also seem to be missing -- possible conflation between zones/counties for these
# example: Siskiyou County; various Siskiyou zones


# Create a zone dataframe with cleaned up columns 
# keep key NCEI data and processed data columns
zone_simple = zone %>% 
  select(EPISODE_ID, EVENT_ID, STATE, County, EVENT_TYPE, CZ_TYPE, CZ_FIPS, CZ_NAME, STATE_CZ_FIPS,
         County_FIPS, start_year, start_month, start_date, end_date, CPI_y, damage_property_adj2022, damage_crop_adj2022,
         DAMAGE_PROPERTY, DAMAGE_CROPS, INJURIES_DIRECT, INJURIES_INDIRECT,  
         WFO,BEGIN_LAT, BEGIN_LON, SOURCE, MAGNITUDE,EVENT_NARRATIVE) %>% 
  distinct() 


# write csv file
#write.csv(zone_simple, 'C:\\Users\\emily\\OneDrive\\Desktop\\Compound Threats\\Historical Analysis\\Data\\NCEI_CA_1_8_24_zones.csv')


##### County-level NCEI data processing
## CZ_TYPE = "C" 

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
#write.csv(data2, 'C:\\Users\\emily\\OneDrive\\Desktop\\Compound Threats\\Historical Analysis\\Data\\NCEI_CA_1_8_24_clean.csv' )

# save data2 as data
data = data2
rm(data2)

##################################################################################
## Goal: Distribute zone losses to each county a zone falls in
## Based on proportion of that zone in the county
## For instance, if there was a zone level event that caused $1000 in damages across 5 counties
## The value of damages would show up as $1000 for all of those counties
## Instead, we calculate the area that each zone intersects for each of those counties
## and divide accordingly 
#################################################################################

## Bring in data
#setwd('C:\\Users\\emily\\OneDrive\\Desktop\\Compound Threats\\Historical Analysis\\Data\\')
library(sf)
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

summary(data$damage_dist_property)
# EMW CA
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0     863    4086   95299   31860 3445348   49771 
summary(data$damage_dist_crop)
# EMW CA
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#     260     1327     4757  1195585   412544 19460309    50027 

#### TESTING ######
## FOR JUST ONE CASE
test1 <- data[data$EPISODE_ID=='2150868',] 
# add leading zeros to County FIPS so that each has 5 digits
test1$County_FIPS = str_pad(test1$County_FIPS, 5, pad = "0")
#test1$STATE_CZ_FIPS = str_pad(test1$STATE_CZ_FIPS, 5, pad = "0")
# add leading zeros to CZ_FIPS so that each has 3 digits
test1$CZ_FIPS = str_pad(test1$CZ_FIPS, 3, pad = "0")

county_of_interest <- counties[counties$FIPS %in% test1$County_FIPS[1],]
counties_of_interest <- counties[counties$FIPS %in% test1$County_FIPS[1:7],]
zone_of_interest <- zones[zones$ZONE %in% test1$CZ_FIPS[1],]

## This zone is 7,736,053,024 m2 or 7,736 km2
st_area(zone_of_interest)/1e6

## make sure coordinate reference systems match before doing the intersection
st_crs(zone_of_interest) <- st_crs(counties_of_interest) ## these are both epsg: 4269

# intersect and visualize
pi <- st_intersection(counties_of_interest, zone_of_interest) 
plot(counties_of_interest$geometry, axes = TRUE)
plot(zone_of_interest$geometry, add = TRUE)
plot(pi$geometry, add = TRUE, col = 'red') # overlap

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


## checkpoint
# write.csv file with zone-county conversion and proportionate losses
#write.csv(data, 'C:\\Users\\emily\\OneDrive\\Desktop\\Compound Threats\\Historical Analysis\\Data\\NCEI_CA_1_8_24_clean_loss.csv' )


###################################################################################
### HAZARD FREQUENCY VISUALIZATIONS ###
### import updated NCEI data for with adj damages and zone-county information
###################################################################################

# without proportionate loss data
#data = read.csv('C:/Users/emily/OneDrive/Desktop/Compound Threats/Historical Analysis/Data/NCEI_CA_1_8_24_clean.csv')
# 50,073 obs of 28 vars

# with proportionate loss data (by land area)
data = read.csv('C:/Users/emily/OneDrive/Desktop/Compound Threats/Historical Analysis/Data/NCEI_CA_1_8_24_clean_loss.csv')
# 50,073 obs of 32 vars

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



#### OUTSTANDING ISSUE ####
# how many obs have NA County_FIPS?
table(is.na(data$County_FIPS)) # 9037 obs have NA County_FIPS
table(is.na(data$County_FIPS), data$CZ_TYPE) # all obs with NA County_FIPS are zone events
table(is.na(data$County_FIPS), data$start_year) # 9037 zones not joined to counties
# may need to return to this
# "coast" zones seem to be missing...
# 001 is redwood coast; 002 is mendocino coast; 003 is north coast interior
# "county" named zones also seem to be missing -- possible conflation between zones/counties for these
# example: Siskiyou County; various Siskiyou zones

# delete the few rows with mostly NA
data = data[!is.na(data$EPISODE_ID),] # 50,069 obs in CA



# delete spaces in EVENT_TYPE names
data$EVENT_TYPE = gsub(" ", "", data$EVENT_TYPE)

# format dates
data$start_date = as.Date(data$start_date, tryFormats = "%Y-%m-%d")
data$end_date = as.Date(data$end_date, tryFormats = "%Y-%m-%d")

#### OUTSTANDING ####
# group EVENT_TYPES for those that are very similar
# example: extreme cold/wind chill + cold/wind chill = cold/wind chill
table(data$EVENT_TYPE)

################################################################################
### ORGANIZING BY HAZARD DAY ###
################################################################################

# Create rows for each event duration, one row per day of hazard
# so there will be multiple rows associated with the same event
# not to be used for losses - just for event occurrences
df_dates = data %>% 
  group_by(EVENT_ID, County) %>% 
  tidyr::nest() %>% 
  mutate(
    Date = purrr::map(
      data, ~ seq(.x$start_date, .x$end_date, by = "1 day")
    )) %>% 
  tidyr::unnest(c(data, Date))
# 212,182 obs of 32 variables 
# delete duplicate rows
df_dates = unique(df_dates)
is.grouped_df(df_dates) # true
#df_dates = as_tibble(df_dates)
str(df_dates)

# Pivot wider so that binary indicators for each hazard by date and county
# one row per date per county
df_wider = df_dates %>% 
  pivot_wider(id_cols = c(County, Date, start_month, start_year), 
              names_from = EVENT_TYPE, values_from = c(EVENT_TYPE), 
              values_fill = 0,
              values_fn = function(x) 1)

# count of hazard sum for date per county
df_wider$event_sum <- rowSums(df_wider[ , c(5:44)], na.rm=TRUE) # column indices subject to change
table(df_wider$event_sum)
# 74,948 obs
#     1     2     3     4     5     6     7     8 
# 60819 11445  2029   492   130    29     3     1 

# create variable for multi-hazard event -- same day
df_wider$multi_hazard_day = ifelse(df_wider$event_sum > 1, 1, 0)
table(df_wider$multi_hazard_day) # 14,129 multihazards

# count of multi hazard days per year
multi_hazard_days_per_year = df_wider %>%
  group_by(County, start_year) %>%
  summarise(haz_days_per_year = n(), 
            multi_haz_days_per_year = sum(multi_hazard_day)) %>%
  mutate(perc_multi_haz_days_per_year = (multi_haz_days_per_year/haz_days_per_year)) %>%
  na.omit(County) 
# 1382 obs of 5 variables

# multi hazard days per year per county
multi_hazard_days_per_year %>%
  group_by(County, start_year) %>%
  filter(County == "Lassen" | County == "Orange" | County == "San Bernardino" | County == "Monterey") %>%
  ggplot(aes(x = start_year, y = multi_haz_days_per_year)) + 
  geom_bar(stat="identity") + 
  labs(x = "Year", y = "Multi-Hazard Days Per Calendar Year") + 
  theme_minimal() + 
  facet_wrap(~County)
# total hazard days per year per county
multi_hazard_days_per_year %>%
  group_by(County, start_year) %>%
  filter(County == "Lassen" | County == "Orange" | County == "San Bernardino" | County == "Monterey") %>%
  ggplot(aes(x = start_year, y = multi_haz_days_per_year)) + 
  geom_bar(stat="identity") + 
  labs(x = "Year", y = "Multi-Hazard Days Per Calendar Year") + 
  theme_minimal() + 
  facet_wrap(~County)

# barplot of county percentage multi hazard days out of total haz days per year
multi_hazard_days_per_year %>%
  group_by(County, start_year) %>%
  filter(County == "Lassen" | County == "Orange" | County == "San Bernardino" | County == "Monterey") %>%
  ggplot(aes(x = start_year, y = perc_multi_haz_days_per_year)) + 
  geom_bar(stat="identity") + 
  labs(x = "Year", y = "Percent of Multi- Out of Total Hazard Days Per Calendar Year") + 
  theme_minimal() + 
  facet_wrap(~County)  
  

#################################################################################
### MAPS OF HAZARD FREQUENCY / COUNTY ### 
#################################################################################

### Map 1A: total hazard events per county from 1996  - 2022
#   create a df for total events/county from 1996 - 2022
map1a = data %>%
  group_by(County) %>% # could be grouped by County_FIPS if needed by mapping package(s)
  summarise(event_count = n()) # event count
#   create a df for total episodes/county from 1996 - 2022
map1a.2 = data %>%
  group_by(County) %>% # could be grouped by County_FIPS if needed by mapping package(s)
  summarise(episode_count = length(unique(EPISODE_ID))) # included this version but assume going with map1a for now


### INSERT SHAR MAPPING CODE FOR MAP 1a ###
mirta <- read_sf(paste0(getwd(),'/MIRTA_shapefiles/DoD_Sites___Boundary.shp'))
ca_mirta <- mirta[mirta$STATENAMEC=='ca',]
usa <- map_data('state')
ca_counties <- counties[counties$STATE=='CA',]
names(map1a)[1] <- 'COUNTYNAME'
names(map1a.2)[1] <- 'COUNTYNAME'
ca_counties <- left_join(ca_counties,map1a,by='COUNTYNAME')
ca_counties <- left_join(ca_counties,map1a.2,by='COUNTYNAME')

library(RColorBrewer)
cols <- rev(brewer.pal(11, 'RdYlBu'))
test <- st_as_sf(usa, coords=c("lon", "lat"), crs = 4326)

ggplot() + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  geom_sf(data=ca_counties,mapping=aes(group=event_count,fill=event_count))+
  scale_fill_gradientn(colours = cols)+
  coord_fixed(1.3) +
  geom_sf(ca_mirta,mapping=aes(),fill='black') +
  #geom_polygon(data=usa, aes(x=long, y=lat, group=group), fill='transparent',color = "black")+
  xlim(-125,-114)+ylim(32,42)+
  theme_bw() +
  theme(legend.position = 'bottom')

ggplot() + 
  #geom_polygon(data=usa,mapping=aes(x=long, y=lat, group=group),fill='transparent',color='black') + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  geom_sf(data=ca_counties,mapping=aes(group=event_count,fill=episode_count))+
  scale_fill_gradientn(colours = cols)+
  #coord_fixed(1.3) +
  geom_sf(ca_mirta,mapping=aes(),fill='black') +
  xlim(-125,-114)+ylim(32,42)+
  theme_bw() +
  theme(legend.position = 'bottom')

### Map 1B: total [specific hazard] events per county from 1996 - 2022
map1b = data %>%
  filter(EVENT_TYPE=='Wildfire') %>% # went with wildfires for now, but could be changed
  group_by(County) %>% # could be grouped by County_FIPS if needed by mapping package(s)
  summarise(wildfire_count = n())

### INSERT SHAR MAPPING CODE FOR MAP 1b ###
names(map1b)[1] <- 'COUNTYNAME'
ca_counties <- left_join(ca_counties,map1b,by='COUNTYNAME')

cols <- brewer.pal(9, 'YlOrRd')
ggplot() + 
  #geom_polygon(data=usa,mapping=aes(x=long, y=lat, group=group),fill='transparent',color='black') + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  geom_sf(data=ca_counties,mapping=aes(group=event_count,fill=wildfire_count))+
  scale_fill_gradientn(colours = cols)+
  #coord_fixed(1.3) +
  geom_sf(ca_mirta,mapping=aes(),fill='black') +
  xlim(-124,-114)+ylim(32,42)+
  theme_bw() +
  theme(legend.position = 'bottom')


### INSERT BETH CODE ON HAZARD DAYS ###

### Map 2A: total compounding hazard events per county from 1996 - 2022 (occur within same day/24 hr period)

### Map 2B: total compounding(/cascading) hazard events per county from 1996 - 2022 (within same week)





