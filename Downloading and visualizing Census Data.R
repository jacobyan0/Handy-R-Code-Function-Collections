library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(mapview)
library(viridis)
library(viridisLite)

options(tigris_use_cache = TRUE)
tigris_cache_dir("C:\\Users\\jacob\\Dropbox (UFL)\\Jacob Box\\TeachingAndWorkshops\\UFTI_DataAnalytics") 

### We need an API key to access the Census Data from census.gov 
#To obtain your Census API key: http://api.census.gov/data/key_signup.html 
#Insert your key to the current environment

census_api_key<-"94a6c385b3259b65826e0213b375e8d779332b52"

### Two main functions:
# get_decennial(): access to 1990, 2000, and 2010 decennial US Census
# get_acs(): Amercian Communicty Survey

####################################################################################################
#######      The load_variable function can help you search the data code              #############
####################################################################################################
variablelist <- load_variables(year=2018,dataset = "acs1",cache = TRUE)
View(variablelist)

### Get 2014-2018 median household income for Florida counties
## Key parameters: geography, variable, year, state, county, survey, key
# B19013_001 is the variable for median housheold income
FL_County_MedHHInc <- get_acs(geography = "county", year=2018, state = "FL", 
                              variables = "B19013_001", 
                              survey="acs5",key = census_api_key)
View(FL_County_MedHHInc)

### Getting 2013-2017 commute mode choice for Florida tracts
FL_Tract_CommuteMode <- get_acs(geography = "tract", variables = NULL, table="B08301", year=2017,
                                state = "FL", county = NULL, geometry = FALSE,
                                key = census_api_key, survey="acs5",cache_table = TRUE)
View(FL_Tract_CommuteMode)

## Convert from wide form to long form
# Need to remove MOE first
FL_Tract_CommuteMode <- FL_Tract_CommuteMode[,-which(names(FL_Tract_CommuteMode) %in% c("moe","NAME"))]
library(tidyr)
FL_Tract_CommuteMode <- spread(FL_Tract_CommuteMode,variable,estimate)
View(FL_Tract_CommuteMode)

## Calculate proportion of works who take public transit to work
FL_Tract_CommuteMode$TransitCommuteModeShare<-FL_Tract_CommuteMode$B08301_010/FL_Tract_CommuteMode$B08301_001
View(FL_Tract_CommuteMode)

#### Export Tables

#write.csv(FL_County_MedHHInc,file="D:\\Dropbox (UFL)\\Jacob Box\\Seattle RLC\\Data\\RawVar.csv",
#          row.names = FALSE)
#write.csv(FL_Tract_CommuteMode,file="D:\\Dropbox (UFL)\\Jacob Box\\Seattle RLC\\Data\\Housing.csv",
#          row.names = FALSE)

####################################################################################################
#######                             Making graphs and maps                             #############
####################################################################################################

### Let us make a graph that ranks the median household income across counties
FL_County_MedHHInc %>%                               
  # keep top 10 wealthiest counties
  #mutate(rank = min_rank(desc(estimate))) %>%  filter(rank<=10) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Household income by tract in Fulton County, Georgia",
       subtitle = "2013-2017 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")

#### Let us make some maps

# 1. Map the median household income in Florida counties
FL_County_MedHHInc <- get_acs(geography = "county", year=2018, state = "FL", 
                              variables = "B19013_001", geometry = TRUE,
                              survey="acs5",key = census_api_key)
View(FL_County_MedHHInc)
### 'geometry' is a list column that contains geographic information (e.g., geometry type and points) for individual observations, or census tracts here. It comes from the US Census Cartographic Boundary Shapefiles, which is a low-resolution version of the TIGER Shapefiles and helps faster processing (By default, 'cb = TRUE').

FL_County_MedHHInc %>%
  ggplot(aes(fill = estimate, color = estimate)) + 
  geom_sf() +                                        # geom_sf helps plot sf objects on a 2D plane.  
  scale_fill_viridis(option = "D", direction = -1) + 
  scale_color_viridis(option = "D", direction = -1)

# 2. Map the 2010 racial compositions in tracts within Orange County
#load_variables(year=2010,dataset = "sf1",cache = TRUE)
racevars <- c(White = "P005003", 
              Black = "P005004", 
              Asian = "P005006", 
              Hispanic = "P004003")

Orange_PopulationByRace <-get_decennial(geography = "tract", variables = racevars, year=2010,  
                                        state = "FL", county = "Orange County", geometry = TRUE,   
                                        summary_var = "P001001",key=census_api_key)  
Orange_PopulationByRace %>%
  mutate(pct = 100 * (value / summary_value)) %>%         # compute the percent of individual racial groups 
  ggplot(aes(fill = pct)) +
  facet_wrap(~variable) +                                 # create faceted plots by race 
  geom_sf(color = "white", size = 0.1) +
  #coord_sf(crs = 26915) + 
  scale_fill_viridis(direction = -1) #+

#3. Interactive mapping
mapview(FL_County_MedHHInc, zcol = "estimate", legend = TRUE)

# 4. Making many maps with one line of code
WA_HousingVar <- read.csv(file="C:\\Users\\jacob\\Dropbox (UFL)\\Jacob Box\\TeachingAndWorkshops\\HousingExample.csv",header = TRUE)
View(WA_HousingVar)

WA_Tract <- get_acs(geography = "tract", variables = "B19013_001", table=NULL, year=2017,
                    state = "WA", county = NULL, geometry = TRUE,
                    key = census_api_key, survey="acs5")
WA_Tract <- WA_Tract[,c("GEOID","geometry")]
WA_HousingVar <- merge(WA_Tract,WA_HousingVar,by="GEOID")  # WA_Tract is the spatial object

mapview(WA_HousingVar,burst=TRUE, hide=TRUE)
