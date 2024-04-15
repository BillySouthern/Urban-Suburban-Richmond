#11/26, initiated by BS
#Goal:  To isolate suburban tracts and highlight whether they were developed in the pre/post-CR era

#Post-Civil Rights suburbs (building age)
#Choices focuses on the MSA, central city, and building age between
#This definition is inspired by age of buildings in Post-Civil Rights suburbs (Pfeiffer, 2016)

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
#TidycensusFiles <- load_variables(YR3, "pl", cache = TRUE)

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
HousingData_1990 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Year Structure Built in 1990/nhgis0048_ds123_1990_tract.csv")) %>%
  filter(STUSAB == ST) %>%
  mutate(GEOID = paste0(str_sub(GISJOIN, 2, 3), 
                        str_sub(GISJOIN, 5, 7), 
                        str_sub(GISJOIN, 9))) %>%
  mutate_at("GEOID", str_pad, width=11, side="right", pad="0") %>%
  mutate(STATE = str_sub(GEOID, 1, 2),
         COUNTY = str_sub(GEOID, 3, 5),
         TRACT = str_sub(GEOID, 6, 11)) %>%
  rename("1989to1990" = EX7001,
         "1985to1988" = EX7002,
         "1980to1984" = EX7003,
         "1970to1979" = EX7004,
         "1960to1969" = EX7005,
         "1950to1959" = EX7006,
         "1940to1949" = EX7007,
         "1939orearlier" = EX7008) %>% 
  mutate(Total = `1989to1990` + `1985to1988` + `1980to1984` + `1970to1979` +
                 `1960to1969` + `1950to1959` + `1940to1949` + `1939orearlier`) %>%
  mutate(PostCR_Units = `1989to1990` + `1985to1988` + 
           `1980to1984` + `1970to1979`,
         PreCR_Units = `1960to1969` + `1950to1959` + `1940to1949` +
           `1939orearlier`) %>%
  mutate(PostCR_Percent = 100 * (PostCR_Units / Total),
         PreCR_Percent = 100 * (PreCR_Units / Total)) %>%
  mutate_all(~replace_na(., 0)) %>%
  mutate(Tract_Age = case_when(
    PostCR_Percent >= 75 ~ "Post-Civil Rights",
    between(PostCR_Percent, 0.1, 75) ~ "Pre-Civil Rights",
    between(PreCR_Percent, 75, 100) ~ "Pre-Civil Rights",
    PostCR_Percent == 0 & PreCR_Percent == 0 ~ "No housing")) %>%
  select(GEOID, PostCR_Units, PostCR_Percent, PreCR_Units, PreCR_Percent, Tract_Age) 


#Filtering tracts to relevant counties and places, then labeling urban/suburban
Building_Age_1990 <- Tracts_1990 %>%
  rename(STATEFP = NHGISST,
         COUNTYFP = NHGISCTY) %>%
  mutate(GEOID = paste0(str_sub(GISJOIN, 2, 3), 
                        str_sub(GISJOIN, 5, 7), 
                        str_sub(GISJOIN, 9))) %>%
  mutate_at("GEOID", str_pad, width=11, side="right", pad="0") %>%
  mutate(TRACTFP = str_sub(GEOID, 6, 11)) %>%
  left_join(HousingData_1990, by="GEOID") %>%
  mutate(Geography = case_when(COUNTYFP %in% COUNTIES ~ 'County',
                               COUNTYFP %in% PLACES ~ 'Independent City')) %>%
  mutate(Environment = case_when(Geography %in% 'County' ~ 'Suburban',
                                 Geography %in% 'Independent City' ~ 'Urban')) %>%
  mutate(Environment = coalesce(Environment, "Suburban")) %>%
  mutate(Landscape = if_else(Environment == "Suburban", Tract_Age, Environment)) %>%
  mutate(State_Name = ST,
         City = CBSA,         
         Definition = "Building Age",
         YEAR = YR1) %>%
  select(GEOID, State_Name, STATEFP, City, COUNTYFP, TRACTFP,
         SHAPE_AREA, SHAPE_LEN,
         Landscape, Geography, Definition, YEAR)

#For initial visualizing of Richmond
ggplot() + 
  geom_sf(data = Building_Age_1990, aes(fill = Landscape), col = "white") + 
  # geom_sf(data = CentralCities_1990, fill = NA, aes(color = "blue")) + 
  # geom_sf(data = CBSA_1990, fill = NA, aes(color = "darkred"), linewidth = 0.5) + 
  theme_void() +
  scale_fill_manual(values=c("lightgrey", "#8da0cb", "#fc8d62", "black"), 
                    labels=c("No housing", "Post-Civil Rights", "Pre-Civil Rights", "Urban"), name = NULL) +
  scale_color_manual(values=c("blue", "darkred", "white"), 
                     labels=c("Independent City", "MSA"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid", "solid"), 
                                                              fill  = c("white", "white")))) 


#To export map
# ggsave("Richmond_BuildingAge_1990.png",
#        path = "~/desktop",
#        width = 11,
#        height = 9,
#        units = "in",
#        dpi = 500)


#Export object to shapefile
#st_write(Building_Age_1990, "~/Desktop/Building_Age_1990/Building_Age_1990.shp")

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

#Load in census tract boundaries (downloaded from NHGIS), 
#then filter to predetermined State and County values above
Tracts_2000 <- read_sf('~/Desktop/US_2000_boundaries/US_Tract_2000/US_tract_2000_conflated.shp') %>%
  filter(str_detect(STATE, ST_FIP)) %>%
  filter(COUNTY %in% COUNTIES | COUNTY %in% PLACES) 

#Download housing data for census tracts prior to join
#Table Ho3 - Year Structure Built
#Filter Pre- and Post-CR housing, rename raw variables with codebook labels
HousingData_2000 <- get_decennial(year = YR2,
                            geography = GEOG,
                            state = ST,
                            variables = c("Total" = "H034001",
                                          "1999to2000" = "H034002",
                                          "1995to1998" = "H034003",
                                          "1990to1994" = "H034004",
                                          "1980to1989" = "H034005",
                                          "1970to1979" = "H034006",
                                          "1960to1969" = "H034007",
                                          "1950to1959" = "H034008",
                                          "1940to1949" = "H034009",
                                          "1939orearlier" = "H034010"),
                            sumfile = "sf3",
                            output = "wide") %>%
  mutate(PostCR_Units = `1999to2000` + `1995to1998` + 
           `1990to1994` + `1980to1989` + `1970to1979`,
         PreCR_Units = `1960to1969` + `1950to1959` + `1940to1949` +
           `1939orearlier`) %>%
  mutate(PostCR_Percent = 100 * (PostCR_Units / Total),
         PreCR_Percent = 100 * (PreCR_Units / Total)) %>%
  mutate_all(~replace_na(., 0)) %>%
  mutate(Tract_Age = case_when(
    PostCR_Percent >= 75 ~ "Post-Civil Rights",
    between(PostCR_Percent, 0.1, 75) ~ "Pre-Civil Rights",
    between(PreCR_Percent, 75, 100) ~ "Pre-Civil Rights",
    PostCR_Percent == 0 & PreCR_Percent == 0 ~ "No housing")) %>%
  select(GEOID, PostCR_Units, PostCR_Percent, PreCR_Units, PreCR_Percent, Tract_Age) 

#Join tracts with the housing data
#Label the tracts within the central city as urban, and those outside it as suburban\
#Label the suburban tracts by building age
Building_Age_2000 <- Tracts_2000 %>%
  mutate(GEOID = paste(STATE, COUNTY, AGGTRACT, sep = "")) %>%
  left_join(HousingData_2000, by="GEOID") %>%
  filter(COUNTY %in% COUNTIES | COUNTY %in% PLACES) %>%
  mutate(Geography = case_when(COUNTY %in% COUNTIES ~ 'County',
                               COUNTY %in% PLACES ~ 'Independent City')) %>%
  mutate(Environment = case_when(Geography %in% 'County' ~ 'Suburban',
                                             Geography %in% 'Independent City' ~ 'Urban')) %>%
  mutate(Environment = coalesce(Environment, "Suburban")) %>%
  mutate(Landscape = if_else(Environment == "Suburban", Tract_Age, Environment)) %>%
  mutate(State_Name = ST,
         City = CBSA) %>%
  relocate(State_Name, .before = STATE) %>%
  relocate(City, .before = COUNTY) %>%
  mutate("Definition" = "Building Age",
         YEAR = YR2) %>%
  select(GEOID, State_Name, STATE, City, COUNTY, AGGTRACT, NHGISST, NHGISCTY,
         ORIG_AREA, Shape_Area, Shape_Leng,
         Landscape, Geography, Definition, YEAR)


#For initial visualizing of Richmond
ggplot() + 
  geom_sf(data = Building_Age_2000, aes(fill = Landscape), col = "white") + 
  # geom_sf(data = CentralCities_2000, fill = NA, aes(color = "blue")) + 
  # geom_sf(data = CBSA_2000, fill = NA, aes(color = "darkred"), linewidth = 0.5) + 
  theme_void() +
  scale_fill_manual(values=c("#8da0cb", "#fc8d62", "black"), 
                    labels=c("Post-Civil Rights", "Pre-Civil Rights", "Urban"), name = NULL,
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) +
  scale_color_manual(values=c("blue", "darkred", "white"), 
                     labels=c("Independent City", "MSA"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid", "solid"), 
                                                              fill  = c("white", "white")))) 

#To export map
# ggsave("Richmond_BuildingAge_2000.png",
#        path = "~/desktop",
#        width = 11,
#        height = 9,
#        units = "in",
#        dpi = 500)

# #Export object to shapefile
# st_write(Building_Age_2000, "~/Desktop/Building_Age_2000/Building_Age_2000.shp")




#----------------------------------------
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
#Table B25034 - Year Structure Built
#Filter Pre- and Post-CR housing, rename raw variables with codebook labels
HousingData_2010 <- get_acs(year = YR3,
                            geography = GEOG,
                            state = ST,
                            variables = c("Total_E" = "B25034_001E",
                                          "Total_MoE" = "B25034_001M",
                                          "2005orlater_E" = "B25034_002E",
                                          "2005orlater_MoE" = "B25034_002M",
                                          "2000to2004_E" = "B25034_003E",
                                          "2000to2004_MoE" = "B25034_003M",
                                          "1990to1999_E" = "B25034_004E",
                                          "1990to1999_MoE" = "B25034_004M",
                                          "1980to1989_E" = "B25034_005E",
                                          "1980to1989_MoE" = "B25034_005M",
                                          "1970to1979_E" = "B25034_006E",
                                          "1970to1979_MoE" = "B25034_006M",
                                          "1960to1969_E" = "B25034_007E",
                                          "1960to1969_MoE" = "B25034_007M",
                                          "1950to1959_E" = "B25034_008E",
                                          "1950to1959_MoE" = "B25034_008M",
                                          "1940to1949_E" = "B25034_009E",
                                          "1940to1949_MoE" = "B25034_009M",
                                          "1939orearlier_E" = "B25034_010E",
                                          "1939orearlier_MoE" = "B25034_010M"),
                            output = "wide") %>%
  mutate(PostCR_Units = `2005orlater_E` + `2000to2004_E` + 
           `1990to1999_E` + `1980to1989_E` + `1970to1979_E`,
         PreCR_Units = `1960to1969_E` + `1950to1959_E` + `1940to1949_E` +
           `1939orearlier_E`) %>%
  mutate(PostCR_Percent = 100 * (PostCR_Units / Total_E),
         PreCR_Percent = 100 * (PreCR_Units / Total_E)) %>%
  mutate_all(~replace_na(., 0)) %>%
  mutate(Tract_Age = case_when(
    PostCR_Percent >= 75 ~ "Post-Civil Rights",
    between(PostCR_Percent, 0.1, 75) ~ "Pre-Civil Rights",
    between(PreCR_Percent, 75, 100) ~ "Pre-Civil Rights",
    PostCR_Percent == 0 & PreCR_Percent == 0 ~ "No housing")) %>%
  select(GEOID, PostCR_Units, PostCR_Percent, PreCR_Units, PreCR_Percent, Tract_Age) 

#Join tracts with the housing data
#Label the tracts within the central city as urban, and those outside it as suburban\
#Label the suburban tracts by building age
Building_Age_2010 <- Tracts_2010 %>%
  rename(GEOID = GEOID10) %>%
  left_join(HousingData_2010, by="GEOID") %>%
  filter(COUNTYFP10 %in% COUNTIES | COUNTYFP10 %in% PLACES) %>%
  mutate(Geography = case_when(COUNTYFP10 %in% COUNTIES ~ 'County',
                               COUNTYFP10 %in% PLACES ~ 'Independent City')) %>%
  mutate(Environment = case_when(Geography %in% 'County' ~ 'Suburban',
                                 Geography %in% 'Independent City' ~ 'Urban')) %>%
  mutate(Environment = coalesce(Environment, "Suburban")) %>%
  mutate(Landscape = if_else(Environment == "Suburban", Tract_Age, Environment)) %>%
  mutate(State_Name = ST,
         City = CBSA) %>%
  relocate(State_Name, .before = STATEFP10) %>%
  relocate(City, .before = COUNTYFP10) %>%
  mutate("Definition" = "Building Age",
         YEAR = YR3) %>%
  select(GEOID, State_Name, STATEFP10, City, COUNTYFP10, TRACTCE10, NAME10, NAMELSAD10,
         MTFCC10, FUNCSTAT10, ALAND10, AWATER10, INTPTLAT10, INTPTLON10, Shape_area, Shape_len,
         Landscape, Geography, Definition, YEAR)

#Erase water
#Building_Age_2010 <- erase_water(Tracts_2010)

#For initial visualizing of Richmond's non-city suburbs
ggplot() + 
  geom_sf(data = Building_Age_2010, aes(fill = Landscape), col = "white") + 
  # geom_sf(data = CentralCities_2010, fill = NA, aes(color = "blue")) + 
  # geom_sf(data = CBSA_2010, fill = NA, aes(color = "darkred"), linewidth = 0.5) + 
  theme_void() +
  scale_fill_manual(values=c("lightgrey", "#8da0cb", "#fc8d62", "black"), 
                    labels=c("No housing", "Post-Civil Rights", "Pre-Civil Rights", "Urban"), name = NULL,
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) +
  scale_color_manual(values=c("blue", "darkred", "white"), 
                     labels=c("Independent City", "MSA"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid", "solid"), 
                                                              fill  = c("white", "white")))) 

#Export object to shapefile
#st_write(Building_Age_2010, "~/Desktop/Building_Age_2010/Building_Age_2010.shp")

#To export map
# ggsave("Richmond_Building_Age_2010.png",
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

#Spatial filter of tracts for the the Richmond MSA
#Here we include all tracts that are within Richmond MSA, ending at the boundary
Tracts_2020 <- Tracts_2020[lengths(st_within(Tracts_2020,CBSA_2020)) > 0,]

#Download housing data for census tracts prior to join
#Table B25034 - Year Structure Built
#Filter Pre- and Post-CR housing, rename raw variables with codebook labels
HousingData_2020 <- get_acs(year = YR4,
                            geography = GEOG,
                            state = ST,
                            variables = c("Total_E" = "B25034_001E",
                                          "Total_MoE" = "B25034_001M",
                                          "2014orlater_E" = "B25034_002E",
                                          "2014orlater_MoE" = "B25034_002M",
                                          "2010to2013_E" = "B25034_003E",
                                          "2010to2013_MoE" = "B25034_003M",
                                          "2000to2009_E" = "B25034_004E",
                                          "2000to2009_MoE" = "B25034_004M",
                                          "1990to1999_E" = "B25034_005E",
                                          "1990to1999_MoE" = "B25034_005M",
                                          "1980to1989_E" = "B25034_006E",
                                          "1980to1989_MoE" = "B25034_006M",
                                          "1970to1979_E" = "B25034_007E",
                                          "1970to1979_MoE" = "B25034_007M",
                                          "1960to1969_E" = "B25034_008E",
                                          "1960to1969_MoE" = "B25034_008M",
                                          "1950to1959_E" = "B25034_009E",
                                          "1950to1959_MoE" = "B25034_009M",
                                          "1940to1949_E" = "B25034_010E",
                                          "1940to1949_MoE" = "B25034_010M",
                                          "1939orearlier_E" = "B25034_011E",
                                          "1939orearlier_MoE" = "B25034_011M"),
                            output = "wide") %>%
  mutate(PostCR_Units = `2014orlater_E` + `2010to2013_E` + `2000to2009_E` + 
           `1990to1999_E` + `1980to1989_E` + `1970to1979_E`,
         PreCR_Units = `1960to1969_E` + `1950to1959_E` + `1940to1949_E` +
           `1939orearlier_E`) %>%
  mutate(PostCR_Percent = 100 * (PostCR_Units / Total_E),
         PreCR_Percent = 100 * (PreCR_Units / Total_E)) %>%
  mutate_all(~replace_na(., 0)) %>%
  mutate(Tract_Age = case_when(
    PostCR_Percent >= 75 ~ "Post-Civil Rights",
    between(PostCR_Percent, 0.1, 75) ~ "Pre-Civil Rights",
    between(PreCR_Percent, 75, 100) ~ "Pre-Civil Rights",
    PostCR_Percent == 0 & PreCR_Percent == 0 ~ "No housing")) %>%
  select(GEOID, PostCR_Units, PostCR_Percent, PreCR_Units, PreCR_Percent, Tract_Age) 

#Join tracts with the housing data
#Label the tracts within the central city as urban, and those outside it as suburban\
#Label the suburban tracts by building age
Tracts_2020 <- Tracts_2020 %>%
  left_join(HousingData_2020, by="GEOID") %>%
  mutate(Environment = ifelse(st_within(., CentralCities_2020), "Urban")) %>%
  mutate(Environment = coalesce(Environment, "Suburban")) %>%
  mutate(Landscape = if_else(Environment == "Suburban", Tract_Age, Environment)) %>%
  select(!Tract_Age)

#Erase water
#Building_Age_2020 <- erase_water(Tracts_2020)

#General tidying
#Final line matches the coordinate system with the NHGIS code to enable comparison
Building_Age_2020  <- Tracts_2020 %>%
  mutate(State_Name = ST,
         City = CBSA) %>%
  relocate(State_Name, .before = STATEFP) %>%
  relocate(City, .before = COUNTYFP) %>%
  relocate(GEOID, .before = State_Name) %>%
  mutate("Definition" = "Building Age",
         YEAR = YR4) %>%
  st_transform(Building_Age_2020, crs = st_crs(Building_Age_1990))

#For initial visualizing of Richmond (CBSA and Central City boundaries optional)
ggplot() + 
  geom_sf(data = Building_Age_2020, aes(fill = Landscape), col = "white") + 
  # geom_sf(data = CentralCities_2020, fill = NA, aes(color = "blue")) + 
  # geom_sf(data = CBSA_2020, fill = NA, aes(color = "darkred"), linewidth = 0.5) + 
  theme_void() +
  scale_fill_manual(values=c("lightgrey", "#8da0cb", "#fc8d62", "black"), 
                    labels=c("No housing", "Post-Civil Rights", "Pre-Civil Rights", "Urban"), name = NULL,
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) +
  scale_color_manual(values=c("blue", "darkred", "white"), 
                     labels=c("Independent City", "MSA"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype = c("solid", "solid"), 
                                                              fill  = c("white", "white")))) 


#Export object to shapefile
#st_write(Building_Age_2020, "~/Desktop/Building_Age_2020/Building_Age_2020.shp")

#To export map
ggsave("Building_Age_2020.png",
       path = "~/desktop",
       width = 11,
       height = 9,
       units = "in",
       dpi = 500)


