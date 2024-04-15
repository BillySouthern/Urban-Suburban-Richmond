#11/29, initiated by BS
#Goal:  To isolate suburban tracts based on their proportion of homeownership

#Homeownership as suburban
#Choices focuses on the MSA, central city, and proportion of residents owning their home
#This definition is inspired by Moos and Mendez (2015) and Airgood-Obrycki et al. (2021)

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
#TidycensusFiles_YR <- load_variables(YR2, "sf1", cache = TRUE)

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

#Load and tidy housing data
Homeownership_1990 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Tenure in 1990/nhgis0049_ds120_1990_tract.csv")) %>%
  filter(STUSAB == ST) %>%
  mutate(GEOID = paste0(str_sub(GISJOIN, 2, 3), 
                        str_sub(GISJOIN, 5, 7), 
                        str_sub(GISJOIN, 9))) %>%
  mutate_at("GEOID", str_pad, width=11, side="right", pad="0") %>%
  mutate(STATE = str_sub(GEOID, 1, 2),
         COUNTY = str_sub(GEOID, 3, 5),
         TRACT = str_sub(GEOID, 6, 11)) %>%
  rename("Owner_Occupied" = "ES1001",
         "Total_Rented" = "ES1002") %>%
  mutate(Total_Units = Owner_Occupied + Total_Rented,
         OwnerOccupied_Percent = 100 * (Owner_Occupied / Total_Units),
         Rented_Percent = 100 * (Total_Rented / Total_Units)) %>%
  mutate_all(~replace_na(., 0)) %>%
  select(GEOID, Total_Units, Owner_Occupied, 
         Total_Rented, OwnerOccupied_Percent, Rented_Percent) 

#Join tracts with the housing data
#Label the tracts within the central city as urban, and those outside it as suburban\
#Label the suburban tracts by building age
Homeownership_1990 <- Tracts_1990 %>%
  rename(STATEFP = NHGISST,
         COUNTYFP = NHGISCTY) %>%
  mutate(GEOID = paste0(str_sub(GISJOIN, 2, 3), 
                        str_sub(GISJOIN, 5, 7), 
                        str_sub(GISJOIN, 9))) %>%
  mutate_at("GEOID", str_pad, width=11, side="right", pad="0") %>%
  mutate(TRACTFP = str_sub(GEOID, 6, 11)) %>%
  left_join(Homeownership_1990, by="GEOID") %>%
  mutate(Median_Ownership = median(OwnerOccupied_Percent),
         Landscape = if_else(OwnerOccupied_Percent > Median_Ownership,
                               "More suburban", "More urban")) %>%
  mutate(State_Name = ST,
         City = CBSA,         
         Definition = "Homeownership",
         YEAR = YR1) %>%
  select(GEOID, State_Name, STATEFP, City, COUNTYFP, TRACTFP,
         SHAPE_AREA, SHAPE_LEN,
         Landscape, OwnerOccupied_Percent, Rented_Percent, Definition, YEAR)

#For initial visualizing of Richmond
ggplot() + 
  geom_sf(data = Homeownership_1990, aes(fill = Landscape), col = "white") + 
  #geom_sf(data = CentralCities_1990, fill = NA, aes(color = "blue"), linewidth = 0.35) + 
  #geom_sf(data = CBSA_1990, fill = NA, aes(color = "darkred"), linewidth = 0.5) + 
  theme_void() +
  scale_fill_manual(values=c("#66c2a5", "#fc8d62"), 
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) +
  scale_color_manual(values=c("black"), 
                     labels=c("Independent City"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid"), 
                                                              fill  = c("white")))) 

#To export map
# ggsave("Homeownership_1990.png",
#        path = "~/desktop",
#        width = 11,
#        height = 9,
#        units = "in",
#        dpi = 500)


#Export object to shapefile
st_write(Homeownership_1990, "~/Desktop/Homeownership_1990/Homeownership_1990.shp")


#------------------------------------
#2000
#County code selection (The counties/cities we're focusing on)
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

#Download homeownership data for census tracts prior to join
#Table H1 - Owner occupancy
#Filter Pre- and Post-CR housing, rename raw variables with codebook labels
Homeownership_2000 <- get_decennial(year = YR2,
                                    geography = GEOG,
                                    state = ST,
                                    variables = c("Total_Units" = "H004001",
                                                  "Owner_Occupied" = "H004002",
                                                  "Total_Rented" = "H004003"),
                                    sumfile = "sf1",
                                    output = "wide") %>%
  mutate(OwnerOccupied_Percent = 100 * (Owner_Occupied / Total_Units),
         Rented_Percent = 100 * (Total_Rented / Total_Units)) %>%
  mutate_all(~replace_na(., 0)) %>%
  select(!NAME)

#Join tracts with the housing data
#Label the tracts within the central city as urban, and those outside it as suburban\
#Label the suburban tracts by building age
Homeownership_2000 <- Tracts_2000 %>%
  mutate(GEOID = paste(STATE, COUNTY, AGGTRACT, sep = "")) %>%
  left_join(Homeownership_2000, by="GEOID") %>%
  filter(COUNTY %in% COUNTIES | COUNTY %in% PLACES) %>%
  mutate(Median_Ownership = median(OwnerOccupied_Percent),
         Landscape = if_else(OwnerOccupied_Percent > Median_Ownership,
                               "More suburban", "More urban")) %>%
  mutate(State_Name = ST,
         City = CBSA) %>%
  mutate("Definition" = "Homeownership",
         YEAR = YR2) %>%
  select(GEOID, State_Name, STATE, City, COUNTY, AGGTRACT, NHGISST, NHGISCTY,
         ORIG_AREA, Shape_Area, Shape_Leng,
         Landscape, OwnerOccupied_Percent, Rented_Percent, Definition, YEAR)


#For initial visualizing of Richmond
ggplot() + 
  geom_sf(data = Homeownership_2000, aes(fill = Landscape), col = "white") + 
  #geom_sf(data = CentralCities_2000, fill = NA, aes(color = "blue"), linewidth = 0.35) + 
  #geom_sf(data = CBSA_2000, fill = NA, aes(color = "darkred"), linewidth = 0.5) + 
  theme_void() +
  scale_fill_manual(values=c("#66c2a5", "#fc8d62"), name = NULL, 
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) +
  scale_color_manual(values=c("black"), 
                     labels=c("Independent City"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid"), 
                                                              fill  = c("white")))) 

#To export map
# ggsave("Homeownership_2000.png",
#        path = "~/desktop",
#        width = 11,
#        height = 9,
#        units = "in",
#        dpi = 500)
# 
# #Export object to shapefile
# st_write(Homeownership_2000, "~/Desktop/Homeownership_2000/Homeownership_2000.shp")
# 





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

#Download homeownership data for census tracts prior to join
#Table H1 - Owner occupancy
#Filter Pre- and Post-CR housing, rename raw variables with codebook labels
Homeownership_2010 <- get_decennial(year = YR3,
                                    geography = GEOG,
                                    state = ST,
                                    variables = c("Total_Units" = "H014001",
                                                  "Owner_Occupied" = "H014002",
                                                  "Total_Rented" = "H014010"),
                                    sumfile = "sf1",
                                    output = "wide") %>%
  mutate(OwnerOccupied_Percent = 100 * (Owner_Occupied / Total_Units),
         Rented_Percent = 100 * (Total_Rented / Total_Units)) %>%
  mutate_all(~replace_na(., 0)) %>%
  select(!NAME)

#Erase water
#Building_Age_2010 <- erase_water(Tracts_2010)

#Join tract prior to median function (want the median of the city, not the ST)
#General tidying
Homeownership_2010 <- Tracts_2010 %>%
  rename(GEOID = GEOID10) %>%
  left_join(Homeownership_2010, by="GEOID") %>%
  filter(COUNTYFP10 %in% COUNTIES | COUNTYFP10 %in% PLACES) %>%
  mutate(Median_Ownership = median(OwnerOccupied_Percent),
         Landscape = if_else(OwnerOccupied_Percent > Median_Ownership,
                               "More suburban", "More urban")) %>%
  mutate(State_Name = ST,
         City = CBSA) %>%
  mutate("Definition" = "Homeownership",
         YEAR = YR3) %>%
  select(GEOID, State_Name, STATEFP10, City, COUNTYFP10, TRACTCE10, NAME10, NAMELSAD10,
         MTFCC10, FUNCSTAT10, ALAND10, AWATER10, INTPTLAT10, INTPTLON10, Shape_area, Shape_len,
         Landscape, OwnerOccupied_Percent, Rented_Percent, Definition, YEAR)

#For initial visualizing of Richmond (CBSA and Central City boundaries optional)
ggplot() + 
  geom_sf(data = Homeownership_2010, aes(fill = Landscape), col = "white") + 
  #geom_sf(data = CentralCities_2010, fill = NA, aes(color = "blue"), linewidth = 0.35) + 
  #geom_sf(data = CBSA_2010, fill = NA, aes(color = "darkred"), linewidth = 0.5) + 
  theme_void() +
  scale_fill_manual(values=c("#66c2a5", "#fc8d62"), name = NULL,
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) +
  scale_color_manual(values=c("black"), 
                     labels=c("Independent City"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid"), 
                                                              fill  = c("white")))) 

#To export map
# ggsave("Homeownership_2010.png",
#        path = "~/desktop",
#        width = 11,
#        height = 9,
#        units = "in",
#        dpi = 500)

#Export object to shapefile
#st_write(Homeownership_2010, "~/Desktop/Homeownership_2010/Homeownership_2010.shp")

#------------------------------------
#2020
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

#Download homeownership data for census tracts prior to join
#Table H1 - Owner occupancy
#Filter Pre- and Post-CR housing, rename raw variables with codebook labels
Homeownership_2020 <- get_decennial(year = YR4,
                            geography = GEOG,
                            state = ST,
                            variables = c("Total_Units" = "H10_001N",
                                          "Owner_Occupied" = "H10_002N",
                                          "Total_Rented" = "H10_010N"),
                            sumfile = "dhc",
                            output = "wide") %>%
  mutate(OwnerOccupied_Percent = 100 * (Owner_Occupied / Total_Units),
         Rented_Percent = 100 * (Total_Rented / Total_Units)) %>%
  mutate_all(~replace_na(., 0)) %>%
  select(!NAME)

#Erase water
#Building_Age_2020 <- erase_water(Tracts_2020)

#Join tract prior to median function (want the median of the city, not the ST)
#General tidying
Homeownership_2020 <- Tracts_2020 %>%
  left_join(Homeownership_2020, by="GEOID") %>%
    mutate(Median_Ownership = median(OwnerOccupied_Percent),
           Landscape = if_else(OwnerOccupied_Percent > Median_Ownership,
                               "More suburban", "More urban")) %>%
  mutate(State_Name = ST,
         City = CBSA) %>%
  relocate(State_Name, .before = STATEFP) %>%
  relocate(City, .before = COUNTYFP) %>%
  relocate(GEOID, .before = State_Name) %>%
  mutate("Definition" = "Homeownership",
         YEAR = YR4) %>%
  st_transform(Homeownership_2020, crs = st_crs(Homeownership_1990))

#For initial visualizing of Richmond (CBSA and Central City boundaries optional)
ggplot() + 
  geom_sf(data = Homeownership_2020, aes(fill = Landscape), col = "white") + 
  #geom_sf(data = CentralCities_2020, fill = NA, aes(color = "blue"), linewidth = 0.35) + 
  #geom_sf(data = CBSA_2020, fill = NA, aes(color = "darkred"), linewidth = 0.5) + 
  theme_void() +
  scale_fill_manual(values=c("#66c2a5", "#fc8d62"), name = NULL,
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) +
  scale_color_manual(values=c("black"), 
                     labels=c("Independent City"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid"), 
                                                              fill  = c("white")))) 

#Export object to shapefile
#st_write(Homeownership_2020, "~/Desktop/Homeownership_2020/Homeownership_2020.shp")

#To export map
# ggsave("Homeownership_2020.png",
#        path = "~/desktop",
#        width = 11,
#        height = 9,
#        units = "in",
#        dpi = 500)
