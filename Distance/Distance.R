#5/12, initiated by BS
#Goal:  To isolate tracts based on distance to central city

#Inner/outer suburbs (Hanlon, 2008), distance by tract (Zhang and Pryce, 2019; Bailey et al 2023)

#Works calculates distance between each tract and nearby central city.  Combines this
#distance with distance to major central city, then quartiles the distances and labels
#as urban core, inner urban, inner suburban, outer suburban

#NOTE: Work still needs more generalizing and using objects rather than city names

#Libraries
library(tigris)
library(tidycensus)
library(tidyverse)
library(sf)

options(tigris_use_cache = TRUE)

#Select Years
ST = "VA"
YR1 = 1990
YR2 = 2000
YR3 = 2010
YR4 = 2020

#Select the state, scale/geography, year, CBSA, and central city
#VA
ST = c("VA")
ST_FIP = c("51")
CBSA = c("Richmond")
CENTRAL_CITY = c("Richmond city")

#GEOG
GEOG = "tract"

#NHGIS Values
NHGIS_CITYCODE =c("6760")

#To view tidycensus files (and codes)
#TidycensusFiles_YR2 <- load_variables(YR2, "sf3", cache = TRUE)

#Create a filepath to OneDrive
onedrivepath="~/OneDrive - The Pennsylvania State University/"

#-------------------------------------------------------------------------------
#1990
#Redefine the year and Counties
YR1 = 1990

#County code selection (The counties/cities we're focusing on)
#This must be done separately because older tract data does not include County names
COUNTIES<-c("041", "145", "075", "053",
            "149", "036", "127",
            "085", "087")

#PLACES (cities) selected separately from the counties
PLACES <- c("760", "670", "730", "570")

#Download geographies of interest (in this case, the Richmond CBSA boundary
CBSA_1990 <- read_sf('~/Desktop/US_1990_boundaries/US_MSA_1990/US_msacmsa_1990.shp') %>%
  filter(str_detect(MSACMSA, NHGIS_CITYCODE)) 

#Acquiring the City of Richmond boundary
#Spatial filter the cities to within the CBSA
#Then filter by central city code
CentralCities_1990 <- read_sf('~/Desktop/US_1990_boundaries/US_Place_1990/US_place_1990.shp') %>%
  filter(lengths(st_within(., CBSA_1990)) > 0) %>%
  filter(str_detect(FIPSCC, "C5")) 

#Load in census tract boundaries (downloaded from NHGIS), 
#then filter to predetermined State and County values above
Tracts_1990 <- read_sf('~/Desktop/US_1990_boundaries/US_Tract_1990/US_tract_1990.shp') %>%
  mutate(NHGISCTY = str_replace(NHGISCTY, "\\d$", ""),
         NHGISST = str_replace(NHGISST, "\\d$", "")) %>%
  filter(str_detect(NHGISST, ST_FIP)) %>%
  filter(NHGISCTY %in% COUNTIES | NHGISCTY %in% PLACES) 

#Centroid of the tracts/central cities prior to measurements
CitiesCentroids <- st_centroid(CentralCities_1990)
TractsCentroids <- st_centroid(Tracts_1990) 

#Match the coordinate system from above centroids
TractsCentroids <- st_transform(TractsCentroids, crs = st_crs(CitiesCentroids))

#Calculate distances between each tract and each central city
# Then rename measurement values by City names
CityTractDistances <- st_distance(TractsCentroids, CitiesCentroids) %>%
  as.data.frame() %>%
  rename_all(~ CentralCities_1990$NAME)

#Combine Tracts and distances
Tracts_1990 <- bind_cols(Tracts_1990, CityTractDistances)

#Distance measurements as urban/suburban indicators, then general tidying
Distance_1990 <- Tracts_1990 %>%
  rename(STATEFP = NHGISST,
         COUNTYFP = NHGISCTY) %>%
  mutate(GEOID = paste0(str_sub(GISJOIN, 2, 3), 
                        str_sub(GISJOIN, 5, 7), 
                        str_sub(GISJOIN, 9))) %>%
  mutate_at("GEOID", str_pad, width=11, side="right", pad="0") %>%
  mutate(TRACTFP = str_sub(GEOID, 6, 11)) %>%
  mutate(RichmondDistance = ntile(Richmond, n())) %>%
  rowwise() %>%
  mutate(ClosestCity = min(Richmond, Hopewell, `Colonial Heights`, Petersburg)) %>%
  ungroup() %>%
  mutate(TotalDistance = (`Richmond` + `ClosestCity`)) %>%
  mutate(CityDistanceQuartile = ntile(TotalDistance, 4)) %>%
  mutate(Landscape = case_when(
    CityDistanceQuartile == 1 ~ "Urban Core",
    CityDistanceQuartile == 2 ~ "Inner-Urban",
    CityDistanceQuartile == 3 ~ "Inner-Suburban",
    CityDistanceQuartile == 4 ~ "Outer-Suburban")) %>%
  mutate(State_Name = ST,
         City = CBSA,         
         Definition = "Distance",
         YEAR = YR1) %>%
  select(GEOID, State_Name, STATEFP, City, COUNTYFP, TRACTFP,
         SHAPE_AREA, SHAPE_LEN,
         Petersburg, `Colonial Heights`, Hopewell, Richmond, Landscape, Definition, YEAR)

#For initial visualizing of Richmond (CBSA and Central City boundaries optional)
ggplot() + 
  geom_sf(data = Distance_1990, aes(fill = Landscape), col = "white") +
  # geom_sf(data = CentralCities_1990, fill = NA, aes(color = "blue")) + 
  # geom_sf(data = CBSA_1990, fill = NA, aes(color = "darkred"), linewidth = 0.5) + 
  theme_void() +
  scale_fill_manual(
    values = c("#bd0026", "#f03b20", "#fd8d3c", "#fecc5c"),
    breaks = c("Urban Core", "Inner-Urban", "Inner-Suburban", "Outer-Suburban"),
    labels = c("Urban Core", "Inner-Urban", "Inner-Suburban", "Outer-Suburban"),
    name = NULL,
    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) + 
  scale_color_manual(values=c("blue", "darkred", "white"), 
                     labels=c("Independent City", "MSA"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid", "solid"), 
                                                              fill  = c("white", "white")))) 

#To export map
# ggsave("Distance_1990.png",
#        path = "~/desktop",
#        width = 11,
#        height = 9,
#        units = "in",
#        dpi = 500)

# #Export object to shapefile
# st_write(Distance_1990, paste0(onedrivepath, "Mapping Richmond/Distance/Distance_1990/Distance_1990.shp"))

#---------------------------------------------------------
#2000
#Redefine the year and Counties
YR2 = 2000

#County code selection (The counties/cities we're focusing on)
#This must be done separately because older tract data does not include County names
COUNTIES<-c("041", "145", "075", "053",
            "149", "036", "127",
            "085", "087")

#PLACES (cities) selected separately from the counties
PLACES <- c("760", "670", "730", "570")

#Download geographies of interest (in this case, the Richmond CBSA boundary from NHGIS)
CBSA_2000 <- read_sf('~/Desktop/US_2000_boundaries/US_MSA_2000/US_msacmsa_2000.shp') %>%
  filter(str_detect(MSACMSA, NHGIS_CITYCODE)) 

#Acquiring the City of Richmond boundary
#Spatial filter the cities to within the CBSA
#Then filter by central city code
CentralCities_2000 <- read_sf('~/Desktop/US_2000_boundaries/US_Place_2000/US_place_2000.shp') %>%
  filter(lengths(st_within(., CBSA_2000)) > 0) %>%
  filter(str_detect(FIPSCC, "C7")) 

#Load in census tract boundaries (downloaded from NHGIS), 
#then filter to predetermined State and County values above
Tracts_2000 <- read_sf('~/Desktop/US_2000_boundaries/US_Tract_2000/US_tract_2000_conflated.shp') %>%
  filter(str_detect(STATE, ST_FIP)) %>%
  filter(COUNTY %in% COUNTIES | COUNTY %in% PLACES) 
  
#Centroid of the tracts/central cities prior to measurements
CitiesCentroids <- st_centroid(CentralCities_2000)
TractsCentroids <- st_centroid(Tracts_2000) 
  
#Calculate distances between each tract and each central city
# Then rename measurement values by City names
CityTractDistances <- st_distance(TractsCentroids, CitiesCentroids) %>%
  as.data.frame() %>%
  rename_all(~ CentralCities_2000$NAME)

#Combine Tracts and distances
Tracts_2000 <- bind_cols(Tracts_2000, CityTractDistances)

#Distance measurements as urban/suburban indicators
Distance_2000 <- Tracts_2000 %>%
  mutate(GEOID = paste(STATE, COUNTY, AGGTRACT, sep = "")) %>%
  filter(COUNTY %in% COUNTIES | COUNTY %in% PLACES) %>%
  mutate(RichmondDistance = ntile(Richmond, n())) %>%
  rowwise() %>%
  mutate(ClosestCity = min(Richmond, Hopewell, `Colonial Heights`, Petersburg)) %>%
  ungroup() %>%
  mutate(TotalDistance = (`Richmond` + `ClosestCity`)) %>%
  mutate(CityDistanceQuartile = ntile(TotalDistance, 4)) %>%
  mutate(Landscape = case_when(
    CityDistanceQuartile == 1 ~ "Urban Core",
    CityDistanceQuartile == 2 ~ "Inner-Urban",
    CityDistanceQuartile == 3 ~ "Inner-Suburban",
    CityDistanceQuartile == 4 ~ "Outer-Suburban")) %>%
  mutate(State_Name = ST,
         City = CBSA,
         "Definition" = "Distance",
         YEAR = YR2) %>%
  select(GEOID, State_Name, STATE, City, COUNTY, AGGTRACT, NHGISST, NHGISCTY,
         ORIG_AREA, Shape_Area, Shape_Leng,
         Petersburg, `Colonial Heights`, Hopewell, Richmond, Landscape, Definition, YEAR)
  
#For initial visualizing of Richmond (CBSA and Central City boundaries optional)
ggplot() + 
  geom_sf(data = Distance_2000, aes(fill = Landscape), col = "white") +
  # geom_sf(data = CentralCities_2000, fill = NA, aes(color = "blue")) + 
  # geom_sf(data = CBSA_2000, fill = NA, aes(color = "darkred"), linewidth = 0.5) + 
  theme_void() +
  scale_fill_manual(
    values = c("#bd0026", "#f03b20", "#fd8d3c", "#fecc5c"),
    breaks = c("Urban Core", "Inner-Urban", "Inner-Suburban", "Outer-Suburban"),
    labels = c("Urban Core", "Inner-Urban", "Inner-Suburban", "Outer-Suburban"),
    name = NULL,
    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) + 
  scale_color_manual(values=c("blue", "darkred", "white"), 
                     labels=c("Independent City", "MSA"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid", "solid"), 
                                                              fill  = c("white", "white")))) 

#To export map
# ggsave("Distance_2000.png",
#        path = "~/desktop",
#        width = 11,
#        height = 9,
#        units = "in",
#        dpi = 500)

#Export object to shapefile
# st_write(Distance_2000, paste0(onedrivepath, "Mapping Richmond/Distance/Distance_2000/Distance_2000.shp"))

#---------------------------------------------------------
#2010
#Define the year and the counties for that year
YR3 = 2010

#County code selection (The counties/cities we're focusing on)
#This must be done separately because older tract data does not include County names
COUNTIES<-c("075", "085", "087", "041", 
            "097", "101", "127", "145", 
            "183", "053", "007", "149", 
            "036", "109", "049", "033")

#PLACES (cities) selected separately from the counties
PLACES <- c("760", "670", "730", "570")

#Load in MSA boundaries (downloaded from NHGIS)
CBSA_2010 <- read_sf('~/Desktop/US_2010_boundaries/US_CBSA_2010/US_cbsa_2010.shp') %>%
  filter(str_detect(NAME10, CBSA)) %>%
  filter(str_detect(NAME10, ST)) 

#Load in the independent cities boundaries (downloaded from NHGIS), then filter the independant cities
CentralCities_2010 <- read_sf('~/Desktop/US_2010_boundaries/US_Place_2010/US_place_2010.shp') %>%
  filter(str_detect(NAMELSAD10, "city")) 

#Spatial filter the cities within the CBSA/MSA of interest
CentralCities_2010 <- CentralCities_2010[lengths(st_within(CentralCities_2010,CBSA_2010)) > 0,] 

#Load in census tract boundaries (downloaded from NHGIS), 
#then filter to predetermined State and County values above
Tracts_2010 <- read_sf('~/Desktop/US_2010_boundaries/US_Tract_2010/US_tract_2010.shp') %>%
  filter(str_detect(STATEFP10, ST_FIP)) %>%
  filter(COUNTYFP10 %in% COUNTIES | COUNTYFP10 %in% PLACES) 

#Centroid of the tracts/central cities prior to measurements
CitiesCentroids <- st_centroid(CentralCities_2010)
TractsCentroids <- st_centroid(Tracts_2010)

#Calculate distances between each tract and each central city
# Then rename measurement values by City names
CityTractDistances <- st_distance(TractsCentroids, CitiesCentroids) %>%
  as.data.frame() %>%
  rename_all(~ CentralCities_2010$NAME10)

#Combine Tracts and distances
Tracts_2010 <- bind_cols(Tracts_2010, CityTractDistances)

#Distance measurements as urban/suburban indicators
Distance_2010 <- Tracts_2010 %>%
  rename(GEOID = GEOID10) %>%
  filter(COUNTYFP10 %in% COUNTIES | COUNTYFP10 %in% PLACES) %>%
  mutate(RichmondDistance = ntile(Richmond, n())) %>%
  rowwise() %>%
  mutate(ClosestCity = min(Richmond, Hopewell, `Colonial Heights`, Petersburg)) %>%
  ungroup() %>%
  mutate(TotalDistance = (`Richmond` + `ClosestCity`)) %>%
  mutate(CityDistanceQuartile = ntile(TotalDistance, 4)) %>%
  mutate(Landscape = case_when(
    CityDistanceQuartile == 1 ~ "Urban Core",
    CityDistanceQuartile == 2 ~ "Inner-Urban",
    CityDistanceQuartile == 3 ~ "Inner-Suburban",
    CityDistanceQuartile == 4 ~ "Outer-Suburban")) %>%
  mutate(State_Name = ST,
         City = CBSA,
         "Definition" = "Distance",
         YEAR = YR3) %>%
  select(GEOID, State_Name, STATEFP10, City, COUNTYFP10, TRACTCE10, NAME10, NAMELSAD10,
         MTFCC10, FUNCSTAT10, ALAND10, AWATER10, INTPTLAT10, INTPTLON10, Shape_area, Shape_len,
         Petersburg, `Colonial Heights`, Hopewell, Richmond, Landscape, Definition, YEAR)

#For initial visualizing of Richmond (CBSA and Central City boundaries optional)
ggplot() + 
  geom_sf(data = Distance_2010, aes(fill = Landscape), col = "white") +
  # geom_sf(data = CentralCities_2010, fill = NA, aes(color = "blue")) + 
  # geom_sf(data = CBSA_2010, fill = NA, aes(color = "darkred"), linewidth = 0.5) + 
  theme_void() +
  scale_fill_manual(
    values = c("#bd0026", "#f03b20", "#fd8d3c", "#fecc5c"),
    breaks = c("Urban Core", "Inner-Urban", "Inner-Suburban", "Outer-Suburban"),
    labels = c("Urban Core", "Inner-Urban", "Inner-Suburban", "Outer-Suburban"),
    name = NULL,
    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) + 
  scale_color_manual(values=c("blue", "darkred", "white"), 
                     labels=c("Independent City", "MSA"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid", "solid"), 
                                                              fill  = c("white", "white")))) 

#Export object to shapefile
#st_write(Distance_2010, paste0(onedrivepath, "Mapping Richmond/Distance/Distance_2010/Distance_2010.shp"))

#To export map
# ggsave("Distance_2010.png",
#        path = "~/desktop",
#        width = 11,
#        height = 9,
#        units = "in",
#        dpi = 500)

#---------------------------------------------------------
#2020
YR4 = 2020  

#Download geographies of interest (in this case, the Richmond CBSA boundary
CBSA_2020 <- core_based_statistical_areas(resolution = "500k", year = YR4) %>%
  filter(str_detect(NAME, ST)) %>%
  filter(str_detect(NAME, CBSA))

#Download tracts of for VA, prior to a clip
Tracts_2020 <- map_dfr(c(ST), ~{
  tracts(.x, year = YR4)})

#Acquiring the cities within VA
CentralCities_2020 <- places(state = ST, year = YR4) %>%
  filter(str_detect(NAMELSAD, "city")) %>%
  mutate(City = if_else(NAME == CBSA, "Central", "Independant"))

#Spatial filter the cities within the Richmond MSA
CentralCities_2020 <- CentralCities_2020[lengths(st_within(CentralCities_2020,CBSA_2020)) > 0,] 

#Spatial filter of tracts for the the Richmond MSA
#Here we include all tracts that are within Richmond MSA, ending at the boundary
Tracts_2020 <- Tracts_2020[lengths(st_within(Tracts_2020,CBSA_2020)) > 0,]

#Centroid of the tracts/central cities prior to measurements
CitiesCentroids <- st_centroid(CentralCities_2020)
TractsCentroids <- st_centroid(Tracts_2020)

#Calculate distances between each tract and each central city
# Then rename measurement values by City names
CityTractDistances <- st_distance(TractsCentroids, CitiesCentroids) %>%
  as.data.frame() %>%
  rename_all(~ CentralCities_2020$NAME)

#Combine Tracts and distances
Tracts_2020 <- bind_cols(Tracts_2020, CityTractDistances)

#Distance measurements as urban/suburban indicators
Distance_2020 <- Tracts_2020 %>%
  mutate(RichmondDistance = ntile(Richmond, n())) %>%
  rowwise() %>%
  mutate(ClosestCity = min(Richmond, Hopewell, `Colonial Heights`, Petersburg)) %>%
  ungroup() %>%
  mutate(TotalDistance = (`Richmond` + `ClosestCity`)) %>%
  mutate(CityDistanceQuartile = ntile(TotalDistance, 4)) %>%
  mutate(Landscape = case_when(
    CityDistanceQuartile == 1 ~ "Urban Core",
    CityDistanceQuartile == 2 ~ "Inner-Urban",
    CityDistanceQuartile == 3 ~ "Inner-Suburban",
    CityDistanceQuartile == 4 ~ "Outer-Suburban")) %>%
  select(GEOID, STATEFP, COUNTYFP, TRACTCE, NAMELSAD, ALAND, AWATER, INTPTLAT, 
         INTPTLON, Petersburg, `Colonial Heights`, Hopewell, Richmond, 
         Landscape) %>%
  mutate(State_Name = ST,
         City = CBSA) %>%
  relocate(State_Name, .before = STATEFP) %>%
  relocate(City, .before = COUNTYFP) %>%
  mutate("Definition" = "Distance",
         Year = YR4) %>%
  st_transform(Distance_2020, crs = st_crs(Distance_1990))

#For initial visualizing of Richmond (CBSA and Central City boundaries optional)
ggplot() + 
  geom_sf(data = Distance_2020, aes(fill = Landscape), col = "white") +
  # geom_sf(data = CentralCities_2020, fill = NA, aes(color = "blue")) + 
  # geom_sf(data = CBSA_2020, fill = NA, aes(color = "darkred"), linewidth = 0.5) + 
  theme_void() +
  scale_fill_manual(
    values = c("#bd0026", "#f03b20", "#fd8d3c", "#fecc5c"),
    breaks = c("Urban Core", "Inner-Urban", "Inner-Suburban", "Outer-Suburban"),
    labels = c("Urban Core", "Inner-Urban", "Inner-Suburban", "Outer-Suburban"),
    name = NULL,
    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) + 
  scale_color_manual(values=c("blue", "darkred", "white"), 
                     labels=c("Independent City", "MSA"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid", "solid"), 
                                                              fill  = c("white", "white")))) 

#Export object to shapefile
# st_write(Distance_2020, paste0(onedrivepath, "Mapping Richmond/Distance/Distance_2020/Distance_2020.shp"))

#To export map
# ggsave("Distance_2020.png",
#        path = "~/desktop",
#        width = 11,
#        height = 9,
#        units = "in",
#        dpi = 500)





