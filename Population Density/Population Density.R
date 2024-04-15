#12/1, initiated by BS
#Goal:  To isolate urban/suburban tracts based on their population density

# Urban above 1900 people per sq-km, suburb between 550 and 1900, exurb below this
# This definition is inspired by Cooke and Marchant (2006), Hanberry (2022), and Landscan

#NOTE: Initial calculation of area is square mile but definition is based on square KM

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
#Redefine the year
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

#Load and tidy population data
PopulationDensity_1990 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Population in 1989/NP10_1990.csv")) %>%
  filter(STUSAB == ST) %>%
  mutate(GEOID = paste0(str_sub(GISJOIN, 2, 3), 
                        str_sub(GISJOIN, 5, 7), 
                        str_sub(GISJOIN, 9))) %>%
  mutate_at("GEOID", str_pad, width=11, side="right", pad="0") %>%
  mutate(STATE = str_sub(GEOID, 1, 2),
         COUNTY = str_sub(GEOID, 3, 5),
         TRACT = str_sub(GEOID, 6, 11)) %>%
  rename(White = ET2001,
         Black = ET2002,
         Native = ET2003,
         Asian = ET2004,
         OtherRace = ET2005,
         HispWhite = ET2006,
         HispBlack = ET2007,
         HispNative = ET2008,
         HispAsian = ET2009,
         HispOth = ET2010) %>%
  rowwise() %>%
  mutate(Hispanic = sum(across(starts_with("Hisp")))) %>%
  mutate(Pop_Total = sum(c(White, Black, Native, Asian, OtherRace, Hispanic))) %>%
  select(GEOID, Pop_Total)

#Filtering tracts to relevant counties and places, then labeling urban/suburban
#Population density is calculated in km2 following literature.  New column needs converting to mi2
PopulationDensity_1990 <- Tracts_1990 %>%
  rename(STATEFP = NHGISST,
         COUNTYFP = NHGISCTY) %>%
  mutate(GEOID = paste0(str_sub(GISJOIN, 2, 3), 
                        str_sub(GISJOIN, 5, 7), 
                        str_sub(GISJOIN, 9))) %>%
  mutate_at("GEOID", str_pad, width=11, side="right", pad="0") %>%
  mutate(TRACTFP = str_sub(GEOID, 6, 11)) %>%
  left_join(PopulationDensity_1990, by="GEOID") %>%
  mutate(Landscape_area_sqme = st_area(.)) %>%
  mutate(Area_Miles2 = Landscape_area_sqme / 2589988.11) %>%
  mutate(Area_Miles2 = str_remove(Area_Miles2, "\\[m\\^2\\]")) %>%
  mutate(Area_Miles2 = as.numeric(Area_Miles2),
         Area_KM2 = (Area_Miles2 * 2.589988),
         Population_Density = `Pop_Total` / Area_KM2) %>%
  mutate(Landscape = case_when(
    Population_Density >= 4500.1 ~ "High-Density Urban",
    between(Population_Density, 1900.1, 4500) ~ "Low-Density Urban",
    between(Population_Density, 1000.1, 1900) ~ "High-Density Suburban",
    between(Population_Density, 800.1, 1000) ~ "Mid-Density Suburban",
    between(Population_Density, 550, 800) ~ "Low-Density Suburban",
    between(Population_Density, 0.1, 550.1) ~ "Exurban",
    Population_Density == 0 ~ "No Population")) %>%
  mutate(State_Name = ST,
         City = CBSA,         
         Definition = "Population Density",
         YEAR = YR1) %>%
  select(GEOID, State_Name, STATEFP, City, COUNTYFP, TRACTFP,
         SHAPE_AREA, SHAPE_LEN, Area_Miles2, Area_KM2,
         Landscape, Population_Density, Definition, YEAR)

#For initial visualizing of Richmond (CBSA and Central City boundaries optional)
ggplot() + 
  geom_sf(data = PopulationDensity_1990, aes(fill = Landscape), col = "white") + 
  # geom_sf(data = CentralCities_1990, fill = NA, aes(color = "blue")) + 
  # geom_sf(data = CBSA_1990, fill = NA, aes(color = "darkred"), linewidth = 0.5) + 
  theme_void() +
  scale_fill_manual(
    values = c("#b2182b", "#d6604d", "#f4a582", "#fddbc7",  "#F2F2F2", "black", "#999999"),
    breaks = c("High-Density Suburban", "Mid-Density Suburban", "Low-Density Suburban",
               "Exurban", "No Population", "High-Density Urban", "Low-Density Urban"),
    labels = c("High-Density Suburban", "Mid-Density Suburban", "Low-Density Suburban", 
               "Exurban", "No Population", "High-Density Urban", "Low-Density Urban"),
    name = NULL,
    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) +
  scale_color_manual(values=c("blue", "darkred", "white"), 
                     labels=c("Independent City", "MSA"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid", "solid"), 
                                                              fill  = c("white", "white")))) 


#To export map
# ggsave("PopulationDensity_1990.png",
#        path = "~/desktop",
#        width = 11,
#        height = 9,
#        units = "in",
#        dpi = 500)

# #Export object to shapefile
#st_write(PopulationDensity_1990, paste0(onedrivepath, "Mapping Richmond/Population Density/PopulationDensity_1990/PopulationDensity_1990.shp"))


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

#Download housing data for census tracts prior to join
#Table P1 - Population total
PopulationDensity_2000 <- get_decennial(year = YR2,
                                        geography = GEOG,
                                        state = ST,
                                        variables = c("Pop_Total" = "P001001"),
                                        sumfile = "pl",
                                        output = "wide") 

#Join tracts with the housing data
#Calculate area, then divide by total pop
#Population density is calculated in km2 following literature.  New column needs converting to mi2
PopulationDensity_2000 <- Tracts_2000 %>%
  mutate(GEOID = paste(STATE, COUNTY, AGGTRACT, sep = "")) %>%
  left_join(PopulationDensity_2000, by="GEOID") %>%
  filter(COUNTY %in% COUNTIES | COUNTY %in% PLACES) %>%
  mutate(Landscape_area_sqme = st_area(.)) %>%
  mutate(Area_Miles2 = Landscape_area_sqme / 2589988.11) %>%
  mutate(Area_Miles2 = str_remove(Area_Miles2, "\\[m\\^2\\]")) %>%
  mutate(Area_Miles2 = as.numeric(Area_Miles2),
         Area_KM2 = (Area_Miles2 * 2.589988),
         Population_Density = `Pop_Total` / Area_KM2) %>%
  mutate(Landscape = case_when(
    Population_Density >= 4500.1 ~ "High-Density Urban",
    between(Population_Density, 1900.1, 4500) ~ "Low-Density Urban",
    between(Population_Density, 1000.1, 1900) ~ "High-Density Suburban",
    between(Population_Density, 800.1, 1000) ~ "Mid-Density Suburban",
    between(Population_Density, 550, 800) ~ "Low-Density Suburban",
    between(Population_Density, 0.1, 550.1) ~ "Exurban",
    Population_Density == 0 ~ "No Population")) %>%
  mutate(State_Name = ST,
         City = CBSA,
         "Definition" = "Building Age",
         YEAR = YR2) %>%
  select(GEOID, State_Name, STATE, City, COUNTY, AGGTRACT, NHGISST, NHGISCTY,
         ORIG_AREA, Shape_Area, Shape_Leng, Area_Miles2, Area_KM2,
         Population_Density, Landscape, Definition, YEAR)

#For initial visualizing of Richmond (CBSA and Central City boundaries optional)
ggplot() + 
  geom_sf(data = PopulationDensity_2000, aes(fill = Landscape), col = "white") + 
  # geom_sf(data = CentralCities_2000, fill = NA, aes(color = "blue")) + 
  # geom_sf(data = CBSA_2000, fill = NA, aes(color = "darkred"), linewidth = 0.5) + 
  theme_void() +
  scale_fill_manual(
    values = c("#b2182b", "#d6604d", "#f4a582", "#fddbc7",  "#F2F2F2", "black", "#999999"),
    breaks = c("High-Density Suburban", "Mid-Density Suburban", "Low-Density Suburban",
               "Exurban", "No Population", "High-Density Urban", "Low-Density Urban"),
    labels = c("High-Density Suburban", "Mid-Density Suburban", "Low-Density Suburban", 
               "Exurban", "No Population", "High-Density Urban", "Low-Density Urban"),
    name = NULL,
    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) +
  scale_color_manual(values=c("blue", "darkred", "white"), 
                     labels=c("Independent City", "MSA"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid", "solid"), 
                                                              fill  = c("white", "white")))) 

#To export map
# ggsave("PopulationDensity_2000.png",
#        path = "~/desktop",
#        width = 11,
#        height = 9,
#        units = "in",
#        dpi = 500)

# # #Export object to shapefile
# st_write(PopulationDensity_2000, paste0(onedrivepath, "Mapping Richmond/Population Density/PopulationDensity_2000/PopulationDensity_2000.shp"))


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

#Load in census tract boundaries (downloaded from NHGIS), 
#then filter to predetermined State and County values above
Tracts_2010 <- read_sf('~/Desktop/US_2010_boundaries/US_Tract_2010/US_tract_2010.shp') %>%
  filter(str_detect(STATEFP10, ST_FIP)) %>%
  filter(COUNTYFP10 %in% COUNTIES | COUNTYFP10 %in% PLACES) 

#Load in the independent cities boundaries (downloaded from NHGIS), then filter the independant cities
CentralCities_2010 <- read_sf('~/Desktop/US_2010_boundaries/US_Place_2010/US_place_2010.shp') %>%
  filter(str_detect(NAMELSAD10, "city")) 

#Spatial filter the cities within the CBSA/MSA of interest
CentralCities_2010 <- CentralCities_2010[lengths(st_within(CentralCities_2010,CBSA_2010)) > 0,] 

#Download housing data for census tracts prior to join
#Table P1 - Population total
PopulationDensity_2010 <- get_decennial(year = YR3,
                                        geography = GEOG,
                                        state = ST,
                                        variables = c("Pop_Total" = "P001001"),
                                        sumfile = "pl",
                                        output = "wide") 

#Join tracts with the housing data
#Calculate area, then divide by total pop
#Population density is calculated in km2 following literature.  New column needs converting to mi2
PopulationDensity_2010 <- Tracts_2010 %>%
  rename(GEOID = GEOID10) %>%
  left_join(PopulationDensity_2010, by="GEOID") %>%
  filter(COUNTYFP10 %in% COUNTIES | COUNTYFP10 %in% PLACES) %>%
  mutate(Landscape_area_sqme = st_area(.)) %>%
  mutate(Area_Miles2 = Landscape_area_sqme / 2589988.11) %>%
  mutate(Area_Miles2 = str_remove(Area_Miles2, "\\[m\\^2\\]")) %>%
  mutate(Area_Miles2 = as.numeric(Area_Miles2),
         Area_KM2 = (Area_Miles2 * 2.589988),
         Population_Density = `Pop_Total` / Area_KM2) %>%
  mutate(Landscape = case_when(
    Population_Density >= 4500.1 ~ "High-Density Urban",
    between(Population_Density, 1900.1, 4500) ~ "Low-Density Urban",
    between(Population_Density, 1000.1, 1900) ~ "High-Density Suburban",
    between(Population_Density, 800.1, 1000) ~ "Mid-Density Suburban",
    between(Population_Density, 550, 800) ~ "Low-Density Suburban",
    between(Population_Density, 0.1, 550.1) ~ "Exurban",
    Population_Density == 0 ~ "No Population")) %>%
  mutate(State_Name = ST,
         City = CBSA,
         Definition = "Population Density",
         YEAR = YR3) %>%
  select(GEOID, State_Name, STATEFP10, City, COUNTYFP10, TRACTCE10, NAME10, NAMELSAD10,
         MTFCC10, FUNCSTAT10, ALAND10, AWATER10, Area_Miles2, Area_KM2, INTPTLAT10, INTPTLON10, Shape_area, Shape_len,
         Population_Density, Landscape, Definition, YEAR)

#Erase water
#PopulationDensity_2010 <- erase_water(PopulationDensity_2010)

#For initial visualizing of Richmond (CBSA and Central City boundaries optional)
ggplot() + 
  geom_sf(data = PopulationDensity_2010, aes(fill = Landscape), col = "white") + 
  # geom_sf(data = CentralCities_2010, fill = NA, aes(color = "blue")) + 
  # geom_sf(data = CBSA_2010, fill = NA, aes(color = "darkred"), linewidth = 0.5) + 
  theme_void() +
  scale_fill_manual(
    values = c("#b2182b", "#d6604d", "#f4a582", "#fddbc7",  "#F2F2F2", "black", "#999999"),
    breaks = c("High-Density Suburban", "Mid-Density Suburban", "Low-Density Suburban",
               "Exurban", "No Population", "High-Density Urban", "Low-Density Urban"),
    labels = c("High-Density Suburban", "Mid-Density Suburban", "Low-Density Suburban", 
               "Exurban", "No Population", "High-Density Urban", "Low-Density Urban"),
    name = NULL,
    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) +
  scale_color_manual(values=c("blue", "darkred", "white"), 
                     labels=c("Independent City", "MSA"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid", "solid"), 
                                                              fill  = c("white", "white")))) 


#Export object to shapefile
#st_write(PopulationDensity_2010, paste0(onedrivepath, "Mapping Richmond/Population Density/PopulationDensity_2010/PopulationDensity_2010.shp"))

#To export map
# ggsave("PopulationDensity_2010.png",
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
  filter(str_detect(NAMELSAD, "city"))

#Spatial filter the cities within the Richmond MSA
CentralCities_2020 <- CentralCities_2020[lengths(st_within(CentralCities_2020,CBSA_2020)) > 0,] 

#Spatial filter of tracts for the the Richmond MSA
#Here we include all tracts that are within Richmond MSA, ending at the boundary
Tracts_2020 <- Tracts_2020[lengths(st_within(Tracts_2020,CBSA_2020)) > 0,]

#Download housing data for census tracts prior to join
#Table P1 - Population total
PopulationDensity_2020 <- get_decennial(year = YR4,
                                geography = GEOG,
                                state = ST,
                                variables = c("Pop_Total" = "P1_001N"),
                                output = "wide") 

#Join tracts with the housing data
#Calculate area, then divide by total population
#Population density is calculated in km2 following literature.  New column needs converting to mi2
PopulationDensity_2020 <- Tracts_2020 %>%
  left_join(PopulationDensity_2020, by="GEOID") %>%
  mutate(Landscape_area_sqme = st_area(.)) %>%
  mutate(Area_Miles2 = Landscape_area_sqme / 2589988.11) %>%
  mutate(Area_Miles2 = str_remove(Area_Miles2, "\\[m\\^2\\]")) %>%
  mutate(Area_Miles2 = as.numeric(Area_Miles2),
         Area_KM2 = (Area_Miles2 * 2.589988),
         Population_Density = `Pop_Total` / Area_KM2) %>%
  mutate(Landscape = case_when(
    Population_Density >= 4500.1 ~ "High-Density Urban",
    between(Population_Density, 1900.1, 4500) ~ "Low-Density Urban",
    between(Population_Density, 1000.1, 1900) ~ "High-Density Suburban",
    between(Population_Density, 800.1, 1000) ~ "Mid-Density Suburban",
    between(Population_Density, 550, 800) ~ "Low-Density Suburban",
    between(Population_Density, 0.1, 550.1) ~ "Exurban",
    Population_Density == 0 ~ "No Population")) %>%
  select(GEOID, STATEFP, COUNTYFP, TRACTCE, NAME.y, ALAND, AWATER, INTPTLAT, INTPTLON, Area_Miles2, Area_KM2,
         Population_Density, Landscape) %>%
  mutate(State_Name = ST,
         City = CBSA) %>%
  relocate(State_Name, .before = STATEFP) %>%
  relocate(City, .before = COUNTYFP) %>%
  mutate("Definition" = "Population density",
         Year = YR4) %>%
  st_transform(PopulationDensity_2020, crs = st_crs(PopulationDensity_1990))

#Erase water
#PopulationDensity_2020 <- erase_water(PopulationDensity_2020)

#For initial visualizing of Richmond (CBSA and Central City boundaries optional)
ggplot() + 
  geom_sf(data = PopulationDensity_2020, aes(fill = Landscape), col = "white") + 
  # geom_sf(data = CentralCities_2020, fill = NA, aes(color = "blue")) + 
  # geom_sf(data = CBSA_2020, fill = NA, aes(color = "darkred"), linewidth = 0.5) + 
  theme_void() +
  scale_fill_manual(
    values = c("#b2182b", "#d6604d", "#f4a582", "#fddbc7",  "#F2F2F2", "black", "#999999"),
    breaks = c("High-Density Suburban", "Mid-Density Suburban", "Low-Density Suburban",
               "Exurban", "No Population", "High-Density Urban", "Low-Density Urban"),
    labels = c("High-Density Suburban", "Mid-Density Suburban", "Low-Density Suburban", 
               "Exurban", "No Population", "High-Density Urban", "Low-Density Urban"),
    name = NULL,
    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) +
  scale_color_manual(values=c("blue", "darkred", "white"), 
                     labels=c("Independent City", "MSA"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid", "solid"), 
                                                              fill  = c("white", "white")))) 


#Export object to shapefile
#st_write(PopulationDensity_2020, paste0(onedrivepath, "Mapping Richmond/Population Density/PopulationDensity_2020/PopulationDensity_2020.shp"))

#To export map
# ggsave("PopulationDensity_2020.png",
#        path = "~/desktop",
#        width = 11,
#        height = 9,
#        units = "in",
#        dpi = 500)
