#Urban/suburban following political definitions
#Choices focuses on the MSA and Independant city (political jurisdictions)

#This definition is inspired by Allard (2018)

#Libraries
library(tigris)
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


#------------------------------------
#1990
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

#Download Counties and filter to within CBSA, and filter accordingly
#Delete NHGIS added values on the ST and County FIPS
Counties_1990 <- read_sf('~/Desktop/US_1990_boundaries/US_County_1990/US_county_1990.shp') %>%
  mutate(NHGISCTY = str_replace(NHGISCTY, "\\d$", ""),
         NHGISST = str_replace(NHGISST, "\\d$", "")) %>%
  filter(str_detect(NHGISST, ST_FIP)) %>%
  filter(NHGISCTY %in% COUNTIES | NHGISCTY %in% PLACES) 

#Load in census tract boundaries (downloaded from NHGIS), 
#then filter to predetermined State and County values above
Tracts_1990 <- read_sf('~/Desktop/US_1990_boundaries/US_Tract_1990/US_tract_1990.shp') %>%
  mutate(NHGISCTY = str_replace(NHGISCTY, "\\d$", ""),
         NHGISST = str_replace(NHGISST, "\\d$", "")) %>%
  filter(str_detect(NHGISST, ST_FIP)) %>%
  filter(NHGISCTY %in% COUNTIES | NHGISCTY %in% PLACES) 

#Filtering tracts to relevant counties and places, then labelling urban/suburban
Political_1990 <- Tracts_1990 %>%
  rename(STATEFP = NHGISST,
         COUNTYFP = NHGISCTY) %>%
  mutate(GEOID = paste0(str_sub(GISJOIN, 2, 3), 
                        str_sub(GISJOIN, 5, 7), 
                        str_sub(GISJOIN, 9))) %>%
  mutate_at("GEOID", str_pad, width=11, side="right", pad="0") %>%
  mutate(TRACTFP = str_sub(GEOID, 6, 11)) %>%
  mutate(Geography = case_when(COUNTYFP %in% COUNTIES ~ 'County',
                               COUNTYFP %in% PLACES ~ 'Independent City')) %>%
  mutate(Landscape = case_when(Geography %in% 'County' ~ 'Suburban',
                               Geography %in% 'Independent City' ~ 'Urban')) %>%
  mutate(State_Name = ST,
         City = CBSA,         
         Definition = "Political",
         YEAR = YR1) %>%
  select(GEOID, State_Name, STATEFP, City, COUNTYFP, TRACTFP,
         SHAPE_AREA, SHAPE_LEN,
         Landscape, Geography, Definition, YEAR)

#For initial visualizing of Richmond
ggplot() + 
  geom_sf(data = Political_1990, aes(fill = Landscape), col = "white") + 
  geom_sf(data = Counties_1990, fill = NA, aes(color = "black"), linewidth = 0.20) + 
  geom_sf(data = CentralCities_1990, fill = NA, aes(color = "blue"), linewidth = 0.65) + 
  geom_sf(data = CBSA_1990, fill = NA, aes(color = "darkred"), linewidth = 0.6) + 
  theme_void() +
  theme(legend.spacing.y = unit(.1, "lines")) +
  scale_fill_manual(values=c("#d9d9d9", "#636363"), 
                    labels=c("Suburban", "Urban"), name = NULL,
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) +
  scale_color_manual(values=c("black", "blue", "darkred"), 
                     labels=c("County", "Independent City", "MSA"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid"), 
                                                              fill  = c("white", "white", "white")),
                                          byrow = TRUE)) 

#To export map
# ggsave("Political_1990.png",
#        path = "~/desktop",
#        width = 11,
#        height = 9,
#        units = "in",
#        dpi = 500)


#Export object to shapefile
#st_write(Political_1990, "~/Desktop/Political_1990/Political_1990.shp")


#------------------------------------
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

#Download Counties and filter to within CBSA, and filter accordingly
  #Delete NHGIS added values on the ST and County FIPS
Counties_2000 <- read_sf('~/Desktop/US_2000_boundaries/US_County_2000/US_County_2000_conflated.shp') %>%
  mutate(NHGISCTY = str_replace(NHGISCTY, "\\d$", ""),
         NHGISST = str_replace(NHGISST, "\\d$", "")) %>%
  filter(str_detect(NHGISST, ST_FIP)) %>%
  filter(NHGISCTY %in% COUNTIES | NHGISCTY %in% PLACES) 

#Load in census tract boundaries (downloaded from NHGIS), 
  #then filter to predetermined State and County values above
Tracts_2000 <- read_sf('~/Desktop/US_2000_boundaries/US_Tract_2000/US_tract_2000_conflated.shp') %>%
  filter(str_detect(STATE, ST_FIP)) %>%
  filter(COUNTY %in% COUNTIES | COUNTY %in% PLACES) 

#Filtering tracts to relevant counties and places, then labelling urban/suburban
#Then tidying name, state, county, etc
Political_2000 <- Tracts_2000 %>%
  mutate(GEOID = paste(STATE, COUNTY, AGGTRACT, sep = "")) %>%
  mutate(Geography = case_when(COUNTY %in% COUNTIES ~ 'County',
                               COUNTY %in% PLACES ~ 'Independent City')) %>%
  mutate(Landscape = case_when(Geography %in% 'County' ~ 'Suburban',
                                             Geography %in% 'Independent City' ~ 'Urban')) %>%
  mutate(State_Name = ST,
         City = CBSA,         
         Definition = "Political",
         YEAR = YR2) %>%
  select(GEOID, State_Name, STATE, City, COUNTY, AGGTRACT, NHGISST, NHGISCTY,
         ORIG_AREA, Shape_Area, Shape_Leng,
         Landscape, Geography, Definition, YEAR)
  
#Erase water
#Political_2000 <- erase_water(Political_2000)

#For initial visualizing of Richmond
ggplot() + 
  geom_sf(data = Political_2000, aes(fill = Landscape), col = "white") + 
  geom_sf(data = Counties_2000, fill = NA, aes(color = "black"), linewidth = 0.20) + 
  geom_sf(data = CentralCities_2000, fill = NA, aes(color = "blue"), linewidth = 0.65) + 
  geom_sf(data = CBSA_2000, fill = NA, aes(color = "darkred"), linewidth = 0.6) + 
  theme_void() +
  theme(legend.spacing.y = unit(.1, "lines")) +
  scale_fill_manual(values=c("#d9d9d9", "#636363"), 
                    labels=c("Suburban", "Urban"), name = NULL,
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) +
  scale_color_manual(values=c("black", "blue", "darkred"), 
                     labels=c("County", "Independent City", "MSA"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid"), 
                                                              fill  = c("white", "white", "white")),
                                          byrow = TRUE)) 

#To export map
# ggsave("Political_2000.png",
#        path = "~/desktop",
#        width = 11,
#        height = 9,
#        units = "in",
#        dpi = 500)

#Export object to shapefile
#st_write(Political_2000, "~/Desktop/Political_2000/Political_2000.shp")


#------------------------------------
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

#Download Counties and filter to within CBSA, and filter accordingly
Counties_2010 <- read_sf('~/Desktop/US_2010_boundaries/US_County_2010/US_County_2010.shp') %>%
  filter(str_detect(STATEFP10, ST_FIP)) %>%
  filter(COUNTYFP10 %in% COUNTIES | COUNTYFP10 %in% PLACES) 

#Label the tracts within the central cities as urban, and those outside it as suburban
#Filter the tracts within our county of interest too
Political_2010 <- Tracts_2010 %>%
  mutate(Geography = case_when(COUNTYFP10 %in% COUNTIES ~ 'County',
                               COUNTYFP10 %in% PLACES ~ 'Independent City')) %>%
  mutate(Landscape = case_when(Geography %in% 'County' ~ 'Suburban',
                               Geography %in% 'Independent City' ~ 'Urban')) %>%
  mutate(State_Name = ST,
         City = CBSA,
         Definition = "Political",
         YEAR = YR3) %>%
  rename(GEOID = GEOID10) %>%
  select(GEOID, State_Name, STATEFP10, City, COUNTYFP10, TRACTCE10, NAME10, NAMELSAD10,
         MTFCC10, FUNCSTAT10, ALAND10, AWATER10, INTPTLAT10, INTPTLON10, Shape_area, Shape_len,
         Landscape, Geography, Definition, YEAR)
  
#Erase water
#Tracts_2010 <- erase_water(Tracts_2010)

#For initial visualizing of Richmond's non-city suburbs
ggplot() + 
  geom_sf(data = Political_2010, aes(fill = Landscape), col = "white") + 
  geom_sf(data = Counties_2010, fill = NA, aes(color = "black"), linewidth = 0.20) + 
  geom_sf(data = CentralCities_2010, fill = NA, aes(color = "blue"), linewidth = 0.65) + 
  geom_sf(data = CBSA_2010, fill = NA, aes(color = "darkred"), linewidth = 0.6) + 
  theme_void() +
  theme(legend.spacing.y = unit(.1, "lines")) +
  scale_fill_manual(values=c("#d9d9d9", "#636363"), 
                    labels=c("Suburban", "Urban"), name = NULL,
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) +
  scale_color_manual(values=c("black", "blue", "darkred"), 
                     labels=c("County", "Independent City", "MSA"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid"), 
                                                              fill  = c("white", "white", "white")),
                                          byrow = TRUE)) 

#Export object to shapefile
#st_write(Political_2010, "~/Desktop/Political_2010/Political_2010.shp")

# #To export map
# ggsave("Political_2010.png",
#        path = "~/desktop",
#        width = 11,
#        height = 9,
#        units = "in",
#        dpi = 500)


#------------------------------------
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

#Download Counties and filter to within CBSA
Counties_2020 <- counties(ST,
                        year = YR4,
                        cb = F) %>%
  filter(lengths(st_within(., CBSA_2020)) > 0)

#Spatial filter of tracts for the the Richmond MSA
#Here we include all tracts that are within Richmond MSA, ending at the boundary
Tracts_2020 <- Tracts_2020[lengths(st_within(Tracts_2020,CBSA_2020)) > 0,]

#Label the tracts within the central city as urban, and those outside it as suburban
Tracts_2020 <- Tracts_2020 %>%
  mutate(Landscape = ifelse(st_within(., CentralCities_2020), "Urban")) %>%
  mutate(Landscape = coalesce(Landscape, "Suburban"))

#Erase water
#Political_2020 <- erase_water(Tracts_2020)

#General tidying
#Final line matches the coordinate system with the NHGIS code to enable comparison
Political_2020  <- Tracts_2020 %>%
  mutate(State_Name = ST,
         City = CBSA) %>%
  relocate(State_Name, .before = STATEFP) %>%
  relocate(City, .before = COUNTYFP) %>%
  relocate(GEOID, .before = State_Name) %>%
  mutate("Definition" = "Political",
         YEAR = YR4) %>%
  st_transform(Political_2020, crs = st_crs(Political_1990))

#For initial visualizing of Richmond
ggplot() + 
  geom_sf(data = Political_2020, aes(fill = Landscape), col = "white") + 
  geom_sf(data = Counties_2020, fill = NA, aes(color = "black"), linewidth = 0.20) + 
  geom_sf(data = CentralCities_2020, fill = NA, aes(color = "blue"), linewidth = 0.65) + 
  geom_sf(data = CBSA_2020, fill = NA, aes(color = "darkred"), linewidth = 0.6) + 
  theme_void() +
  theme(legend.spacing.y = unit(.1, "lines")) +
  scale_fill_manual(values=c("#d9d9d9", "#636363"), 
                    labels=c("Suburban", "Urban"), name = NULL,
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) +
  scale_color_manual(values=c("black", "blue", "darkred"), 
                     labels=c("County", "Independent City", "MSA"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid"), 
                                                              fill  = c("white", "white", "white")),
                                          byrow = TRUE)) 

#Export object to shapefile
# st_write(Political_2020, "~/Desktop/Political_2020/Political_2020.shp")

# #To export map
# ggsave("Political_2020.png",
#        path = "~/desktop",
#        width = 11,
#        height = 9,
#        units = "in",
#        dpi = 500)

