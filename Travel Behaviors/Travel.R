#5/13, initiated by BS
#Goal:  To isolate tracts based on travel behaviors

#Active core, auto suburbs, transit suburbs, exurbs (Gordon and Janzen, 2013; Gordon, 2022)

# Works indicate urban/suburban based on the commuting to work and population density of each tract
# Focus is car, walking/cycling, public transit and population density for exurbs

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
#TidycensusFiles <- load_variables(YR2, "pl", cache = TRUE)

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

#Table P03 - Method of travel to work
Travel_1990 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Travel to work 1990/nhgis0056_ds123_1990_tract.csv")) %>%
  filter(STUSAB == ST) %>%
  mutate(GEOID = paste0(str_sub(GISJOIN, 2, 3), 
                        str_sub(GISJOIN, 5, 7), 
                        str_sub(GISJOIN, 9))) %>%
  mutate_at("GEOID", str_pad, width=11, side="right", pad="0") %>%
  mutate(STATE = str_sub(GEOID, 1, 2),
         COUNTY = str_sub(GEOID, 3, 5),
         TRACT = str_sub(GEOID, 6, 11)) %>%
  rename(Car_Alone = E3U001,
         Car_Carpool = E3U002,
         Bus_Trolley = E3U003,
         Streetcar = E3U004,
         Subway = E3U005,
         Rail = E3U006,
         Ferry = E3U007,
         Taxi = E3U008,
         Motorcycle = E3U009,
         Cycle = E3U010,
         Walking = E3U011,
         Other = E3U012,
         Work_Home = E3U013) %>%
  mutate(Total_Car = (`Car_Alone` + `Car_Carpool` + `Motorcycle`),
         Total_Active = (`Walking` + `Cycle`),
         Total_PublicTransport = (`Bus_Trolley` + `Streetcar` + `Subway` + `Rail` +
                                    `Ferry` + `Taxi`)) %>%
  mutate(Travel_Total = `Total_Car` + `Total_Active` + `Total_PublicTransport` +
           `Other` + `Work_Home`) %>%
  mutate(Car_Percent = 100 * (Total_Car / Travel_Total),
         Active_Percent = 100 * (Total_Active / Travel_Total),
         PubTransp_Percent = 100 * (Total_PublicTransport / Travel_Total)) %>%
  mutate_all(~replace_na(., 0)) %>%
  mutate(Median_Active = median(Active_Percent),
         Median_PubTrans = median(PubTransp_Percent)) %>%
  left_join(PopulationDensity_1990, by="GEOID") %>%
  select(GEOID, Car_Percent, Active_Percent, Median_Active, 
         PubTransp_Percent, Median_PubTrans, Pop_Total)

#Join tracts with the housing data
#Label the tracts within the central city as urban, and those outside it as suburban\
#Label the suburban tracts by building age
#We use 58 as the pop dens threshold as we're converting from KM2 to Mi2
Travel_1990 <- Tracts_1990 %>%
  rename(STATEFP = NHGISST,
         COUNTYFP = NHGISCTY) %>%
  mutate(GEOID = paste0(str_sub(GISJOIN, 2, 3), 
                        str_sub(GISJOIN, 5, 7), 
                        str_sub(GISJOIN, 9))) %>%
  mutate_at("GEOID", str_pad, width=11, side="right", pad="0") %>%
  mutate(TRACTFP = str_sub(GEOID, 6, 11)) %>%
  left_join(Travel_1990, by="GEOID") %>%
  mutate(Landscape_area_sqme = st_area(.)) %>%
  mutate(Area_Miles2 = Landscape_area_sqme / 2589988.11) %>%
  mutate(Area_Miles2 = str_remove(Area_Miles2, "\\[m\\^2\\]")) %>%
  mutate(Area_Miles2 = as.numeric(Area_Miles2),
         Area_KM2 = (Area_Miles2 * 2.589988),
         Population_Density = `Pop_Total` / Area_KM2) %>%
  mutate(Landscape = case_when(
    Population_Density <= 58 ~ "Exurb",
    PubTransp_Percent >= 1.5 * Median_PubTrans & Active_Percent <= 1.5 * Median_Active & Population_Density > 58 ~ "Transit Suburb",
    Active_Percent >= 1.5 * Median_Active & Population_Density > 58 ~ "Active Core",
    Active_Percent < 1.5 * Median_Active & PubTransp_Percent < 1.5 * Median_PubTrans & Population_Density > 58 ~ "Auto Suburb")) %>%
  mutate(State_Name = ST,
         City = CBSA,
         "Definition" = "Travel",
         YEAR = YR1) %>%
  select(GEOID, State_Name, STATEFP, City, COUNTYFP, TRACTFP,
         SHAPE_AREA, SHAPE_LEN,
         Car_Percent, Active_Percent, PubTransp_Percent, Population_Density, Landscape, Definition, YEAR)

#For initial visualizing of Richmond (CBSA and Central City boundaries optional)
ggplot() + 
  geom_sf(data = Travel_1990, aes(fill = Landscape), col = "white") +
  # geom_sf(data = CentralCities_1990, fill = NA, aes(color = "blue")) + 
  # geom_sf(data = CBSA_1990, fill = NA, aes(color = "darkred"), linewidth = 0.5) + 
  theme_void() +
  scale_fill_manual(
    values = c("#1f78b4", "#fb9a99", "#33a02c", "#a6cee3"),
    breaks = c("Active Core", "Auto Suburb", "Transit Suburb", "Exurb"),
    labels = c("Active Core", "Auto Suburb", "Transit Suburb", "Exurb"),
    name = NULL,
    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) + 
  scale_color_manual(values=c("blue", "darkred", "white"), 
                     labels=c("Independent City", "MSA"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid", "solid"), 
                                                              fill  = c("white", "white")))) 

#Export object to shapefile
# st_write(Travel_1990, paste0(onedrivepath, "Mapping Richmond/Travel/Travel_1990/Travel_1990.shp"))

#To export map
# ggsave("Travel_1990.png",
#        path = "~/desktop",
#        width = 11,
#        height = 9,
#        units = "in",
#        dpi = 500)


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
                                        variables = c("Pop_Total" = "PL001001"),
                                        sumfile = "pl",
                                        output = "wide",
                                        geometry = T) %>%
  mutate(Landscape_area_sqme = st_area(.)) %>%
  mutate(Area_Miles2 = Landscape_area_sqme / 2589988.11) %>%
  mutate(Area_Miles2 = str_remove(Area_Miles2, "\\[m\\^2\\]")) %>%
  mutate(Area_Miles2 = as.numeric(Area_Miles2),
         Area_KM2 = (Area_Miles2 * 2.589988),
         Population_Density = `Pop_Total` / Area_KM2) %>%
  st_drop_geometry() 

#Table P03 - Method of travel to work
Travel_2000 <- get_decennial(year = YR2,
                       geography = GEOG,
                       state = ST,
                       sumfile = "sf3",
                       variables = c("Travel_Total" = "P030001",
                                     "All_Car" = "P030002",
                                     "Motorcycle" = "P030012",
                                     "Walking" = "P030014",
                                     "Cycle" = "P030013",
                                     "PublicTransport" = "P030005"),
                       output = "wide") %>%
  mutate(Total_Car = (`All_Car` + `Motorcycle`),
         Total_Active = (`Walking` + `Cycle`),
         Total_PublicTransport = (`PublicTransport`)) %>%
  mutate(Car_Percent = 100 * (Total_Car / Travel_Total),
         Active_Percent = 100 * (Total_Active / Travel_Total),
         PubTransp_Percent = 100 * (Total_PublicTransport / Travel_Total)) %>%
  mutate_all(~replace_na(., 0)) %>%
  mutate(Median_Active = median(Active_Percent),
         Median_PubTrans = median(PubTransp_Percent)) %>%
  left_join(PopulationDensity_2000, by="GEOID") %>%
  select(GEOID, Car_Percent, Active_Percent, Median_Active, 
         PubTransp_Percent, Median_PubTrans, Population_Density)

#Join tracts with the housing data
#Label the tracts within the central city as urban, and those outside it as suburban\
#Label the suburban tracts by building age
Travel_2000 <- Tracts_2000 %>%
  mutate(GEOID = paste(STATE, COUNTY, AGGTRACT, sep = "")) %>%
  left_join(Travel_2000, by="GEOID") %>%
  filter(COUNTY %in% COUNTIES | COUNTY %in% PLACES) %>%
  mutate(Landscape = case_when(
    Population_Density <= 58 ~ "Exurb",
    PubTransp_Percent >= 1.5 * Median_PubTrans & Active_Percent <= 1.5 * Median_Active & Population_Density > 58 ~ "Transit Suburb",
    Active_Percent >= 1.5 * Median_Active & Population_Density > 58 ~ "Active Core",
    Active_Percent < 1.5 * Median_Active & PubTransp_Percent < 1.5 * Median_PubTrans & Population_Density > 58 ~ "Auto Suburb")) %>%
  mutate(State_Name = ST,
         City = CBSA,
         "Definition" = "Travel",
         YEAR = YR2) %>%
  select(GEOID, State_Name, STATE, City, COUNTY, AGGTRACT, NHGISST, NHGISCTY,
         ORIG_AREA, Shape_Area, Shape_Leng,
         Car_Percent, Active_Percent, PubTransp_Percent, Population_Density, Landscape, Definition, YEAR)

#For initial visualizing of Richmond (CBSA and Central City boundaries optional)
ggplot() + 
  geom_sf(data = Travel_2000, aes(fill = Landscape), col = "white") +
  # geom_sf(data = CentralCities_2000, fill = NA, aes(color = "blue")) + 
  # geom_sf(data = CBSA_2000, fill = NA, aes(color = "darkred"), linewidth = 0.5) + 
  theme_void() +
  scale_fill_manual(
    values = c("#1f78b4", "#fb9a99", "#33a02c", "#a6cee3"),
    breaks = c("Active Core", "Auto Suburb", "Transit Suburb", "Exurb"),
    labels = c("Active Core", "Auto Suburb", "Transit Suburb", "Exurb"),
    name = NULL,
    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) + 
  scale_color_manual(values=c("blue", "darkred", "white"), 
                     labels=c("Independent City", "MSA"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid", "solid"), 
                                                              fill  = c("white", "white")))) 

#Export object to shapefile
# st_write(Travel_2000, paste0(onedrivepath, "Mapping Richmond/Travel/Travel_2000/Travel_2000.shp"))

#To export map
# ggsave("Travel_2000.png",
#        path = "~/desktop",
#        width = 11,
#        height = 9,
#        units = "in",
#        dpi = 500)

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

#Download travel and population data for census tracts prior to join
#Table P1 - Population total
PopulationDensity_2010 <- get_decennial(year = YR3,
                                        geography = GEOG,
                                        state = ST,
                                        variables = c("Pop_Total" = "P001001"),
                                        sumfile = "pl",
                                        output = "wide",
                                        geometry = T) %>%
  mutate(Landscape_area_sqme = st_area(.)) %>%
  mutate(Area_Miles2 = Landscape_area_sqme / 2589988.11) %>%
  mutate(Area_Miles2 = str_remove(Area_Miles2, "\\[m\\^2\\]")) %>%
  mutate(Area_Miles2 = as.numeric(Area_Miles2),
         Area_KM2 = (Area_Miles2 * 2.589988),
         Population_Density = `Pop_Total` / Area_KM2) %>%
  st_drop_geometry()

#Table PB08 - Method of travel to work
Travel_2010 <- get_acs(year = YR3,
                       geography = GEOG,
                       state = ST,
                       variables = c("Travel_Total_E" = "B08301_001E",
                                     "Travel_Total_MoE" = "B08301_001M",
                                     "All_Car_E" = "B08301_002E",
                                     "All_Car_MoE" = "B08301_002M",
                                     "Motorcycle_E" = "B08301_017E",
                                     "Motorcycle_MoE" = "B08301_017M",
                                     "Walking_E" = "B08301_019E",
                                     "Walking_MoE" = "B08301_019M",
                                     "Cycle_E" = "B08301_018E",
                                     "Cycle_MoE" = "B08301_018M",
                                     "PublicTransport_E" = "B08301_010E",
                                     "PublicTransport_MoE" = "B08301_010M",
                                     "Taxi_E" = "B08301_016E",
                                     "Taxi_MoE" = "B08301_016M"),
                       output = "wide") %>%
  mutate(Total_Car = (`All_Car_E` + `Motorcycle_E`),
         Total_Active = (`Walking_E` + `Cycle_E`),
         Total_PublicTransport = (`PublicTransport_E` + `Taxi_E`)) %>%
  mutate(Car_Percent = 100 * (Total_Car / Travel_Total_E),
         Active_Percent = 100 * (Total_Active / Travel_Total_E),
         PubTransp_Percent = 100 * (Total_PublicTransport / Travel_Total_E)) %>%
  mutate_all(~replace_na(., 0)) %>%
  mutate(Median_Active = median(Active_Percent),
         Median_PubTrans = median(PubTransp_Percent)) %>%
  left_join(PopulationDensity_2010, by="GEOID") %>%
  select(GEOID, Car_Percent, Active_Percent, Median_Active, 
         PubTransp_Percent, Median_PubTrans, Population_Density)

#Join tracts with the housing data
#Label the tracts within the central city as urban, and those outside it as suburban\
#Label the suburban tracts by building age
Travel_2010 <- Tracts_2010 %>%
  rename(GEOID = GEOID10) %>%
  left_join(Travel_2010, by="GEOID") %>%
  filter(COUNTYFP10 %in% COUNTIES | COUNTYFP10 %in% PLACES) %>%
  mutate(Landscape = case_when(
    Population_Density <= 58 ~ "Exurb",
    PubTransp_Percent >= 1.5 * Median_PubTrans & Active_Percent <= 1.5 * Median_Active & Population_Density > 58 ~ "Transit Suburb",
    Active_Percent >= 1.5 * Median_Active & Population_Density > 58 ~ "Active Core",
    Active_Percent < 1.5 * Median_Active & PubTransp_Percent < 1.5 * Median_PubTrans & Population_Density > 58 ~ "Auto Suburb")) %>%
  mutate(State_Name = ST,
         City = CBSA,
         Definition = "Travel",
         YEAR = YR3) %>%
  select(GEOID, State_Name, STATEFP10, City, COUNTYFP10, TRACTCE10, NAME10, NAMELSAD10,
         MTFCC10, FUNCSTAT10, ALAND10, AWATER10, INTPTLAT10, INTPTLON10, Shape_area, Shape_len,
         Car_Percent, Active_Percent, PubTransp_Percent, Population_Density, Landscape, Definition, YEAR)

#For initial visualizing of Richmond (CBSA and Central City boundaries optional)
ggplot() + 
  geom_sf(data = Travel_2010, aes(fill = Landscape), col = "white") +
  # geom_sf(data = CentralCities_2010, fill = NA, aes(color = "blue")) + 
  # geom_sf(data = CBSA_2010, fill = NA, aes(color = "darkred"), linewidth = 0.5) + 
  theme_void() +
  scale_fill_manual(
    values = c("#1f78b4", "#fb9a99", "#33a02c", "#a6cee3"),
    breaks = c("Active Core", "Auto Suburb", "Transit Suburb", "Exurb"),
    labels = c("Active Core", "Auto Suburb", "Transit Suburb", "Exurb"),
    name = NULL,
    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) + 
  scale_color_manual(values=c("blue", "darkred", "white"), 
                     labels=c("Independent City", "MSA"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid", "solid"), 
                                                              fill  = c("white", "white")))) 

#Export object to shapefile
# st_write(Travel_2010, paste0(onedrivepath, "Mapping Richmond/Travel/Travel_2010/Travel_2010.shp"))

#To export map
# ggsave("Travel_2010.png",
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

#Download travel and population data for census tracts prior to join
#Table P1 - Population total
PopulationDensity_2020 <- get_decennial(year = YR4,
                                        geography = GEOG,
                                        state = ST,
                                        variables = c("Pop_Total" = "P1_001N"),
                                        output = "wide",
                                        geometry = T) %>%
  mutate(Landscape_area_sqme = st_area(.)) %>%
  mutate(Area_Miles2 = Landscape_area_sqme / 2589988.11) %>%
  mutate(Area_Miles2 = str_remove(Area_Miles2, "\\[m\\^2\\]")) %>%
  mutate(Area_Miles2 = as.numeric(Area_Miles2),
         Area_KM2 = (Area_Miles2 * 2.589988),
         Population_Density = `Pop_Total` / Area_KM2) %>%
  st_drop_geometry()

#Table PB08 - Method of travel to work
Travel_2020 <- get_acs(year = YR4,
                       geography = GEOG,
                       state = ST,
                       variables = c("Travel_Total_E" = "B08301_001E",
                                     "Travel_Total_MoE" = "B08301_001M",
                                     "All_Car_E" = "B08301_002E",
                                     "All_Car_MoE" = "B08301_002M",
                                     "Motorcycle_E" = "B08301_017E",
                                     "Motorcycle_MoE" = "B08301_017M",
                                     "Walking_E" = "B08301_019E",
                                     "Walking_MoE" = "B08301_019M",
                                     "Cycle_E" = "B08301_018E",
                                     "Cycle_MoE" = "B08301_018M",
                                     "PublicTransport_E" = "B08301_010E",
                                     "PublicTransport_MoE" = "B08301_010M",
                                     "Taxi_E" = "B08301_016E",
                                     "Taxi_MoE" = "B08301_016M"),
                       output = "wide") %>%
  mutate(Total_Car = (`All_Car_E` + `Motorcycle_E`),
         Total_Active = (`Walking_E` + `Cycle_E`),
         Total_PublicTransport = (`PublicTransport_E` + `Taxi_E`)) %>%
  mutate(Car_Percent = 100 * (Total_Car / Travel_Total_E),
         Active_Percent = 100 * (Total_Active / Travel_Total_E),
         PubTransp_Percent = 100 * (Total_PublicTransport / Travel_Total_E)) %>%
  mutate_all(~replace_na(., 0)) %>%
  mutate(Median_Active = median(Active_Percent),
         Median_PubTrans = median(PubTransp_Percent),
         Median_Auto = median(Car_Percent)) %>%
  left_join(PopulationDensity_2020, by="GEOID") %>%
  select(GEOID, Car_Percent, Median_Auto, Active_Percent, Median_Active, 
         PubTransp_Percent, Median_PubTrans, Population_Density)

#Join tracts with the housing data
#Label the tracts within the central city as urban, and those outside it as suburban
#Label the suburban tracts by building age
Travel_2020 <- Tracts_2020 %>%
  left_join(Travel_2020, by="GEOID") %>%
  mutate(Landscape = case_when(
    Population_Density <= 58 ~ "Exurb",
    PubTransp_Percent >= 1.5 * Median_PubTrans & Active_Percent <= 1.5 * Median_Active & Population_Density > 58 ~ "Transit Suburb",
    Active_Percent >= 1.5 * Median_Active & Population_Density > 58 ~ "Active Core",
    Active_Percent < 1.5 * Median_Active & PubTransp_Percent < 1.5 * Median_PubTrans & Population_Density > 58 ~ "Auto Suburb")) %>%
  mutate(State_Name = ST,
         City = CBSA,
         "Definition" = "Travel",
         YEAR = YR4) %>%
  select(GEOID, State_Name, STATEFP, City, COUNTYFP, TRACTCE, NAMELSAD, ALAND, AWATER, INTPTLAT, 
         INTPTLON, Car_Percent, Active_Percent, PubTransp_Percent, Population_Density, 
         Landscape, Definition, YEAR) %>%
  st_transform(Travel_2020, crs = st_crs(Travel_1990))

#For initial visualizing of Richmond (CBSA and Central City boundaries optional)
ggplot() + 
  geom_sf(data = Travel_2020, aes(fill = Landscape), col = "white") +
  # geom_sf(data = CentralCities_2020, fill = NA, aes(color = "blue")) + 
  # geom_sf(data = CBSA_2020, fill = NA, aes(color = "darkred"), linewidth = 0.5) + 
  theme_void() +
  scale_fill_manual(
    values = c("#1f78b4", "#fb9a99", "#33a02c", "#a6cee3"),
    breaks = c("Active Core", "Auto Suburb", "Transit Suburb", "Exurb"),
    labels = c("Active Core", "Auto Suburb", "Transit Suburb", "Exurb"),
    name = NULL,
    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) + 
  scale_color_manual(values=c("blue", "darkred", "white"), 
                     labels=c("Independent City", "MSA"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid", "solid"), 
                                                              fill  = c("white", "white")))) 
#Export object to shapefile
# st_write(Travel_2020, paste0(onedrivepath, "Mapping Richmond/Travel/Travel_2020/Travel_2020.shp"))

#To export map
# ggsave("Travel_2020.png",
#        path = "~/desktop",
#        width = 11,
#        height = 9,
#        units = "in",
#        dpi = 500)






