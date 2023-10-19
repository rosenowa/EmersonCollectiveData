# load libraries for cleaning data
library(tidyverse)
library(tidycensus)
library(sf)
library(tidyverse)
library(urbnthemes)

# update census API key with your personal key: https://api.census.gov/data/key_signup.html
#census_api_key('2330d2d66e60a29868283ad737077ca35e502983')

# load ACS variables to identify the correct variable names for analysis 
acs21 <- load_variables(2021, "acs5", cache = TRUE)
View(acs21)

####-----------------pull acs data for vars at dif geos--------------------####


acs_pull <- function(geos, years, selected_vars){
  get_acs(geography = geos,
          year = years, 
          variables = selected_vars,
          geometry = TRUE, # include geometry for mapping
          state = "DC") %>% 
    mutate(year = years)
}



####-----------------old--------------------####

#--------block group--------#

# # load DC block group geography and relevant variables
# dc_bg <- get_acs(geography = "block group",
#                             variables = c("B01001_001", #total pop
#                                           "B01001_003", "B01001_004", "B01001_005","B01001_006", #sum of these is M pop under 18
#                                           "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", #sum of these is M pop 65-84
#                                           "B01001_025", #M pop over 85
#                                           "B01001_027", "B01001_028", "B01001_029", "B01001_030", #sum of these is F pop under 18
#                                           "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", #sum of these is F pop 65-84
#                                           "B01001_049", #F pop over 85
#                                           "B19013_001", #med income
#                                           "B03002_001", #total race pop
#                                           "B03002_002", #total nH 
#                                           "B03002_003", #total nH white
#                                           "B03002_004", #total nH Black
#                                           "B03002_005", #total nH AmerInd NativeAmer
#                                           "B03002_006", #total nH asian
#                                           "B03002_007", #toal nH NativeHawaiian PacificIslander
#                                           "B03002_012" #total Hisp
#                                           ), 
#                             year = 2021, # most recently available data
#                             geometry = TRUE, # include geometry of block groups for mapping
#                             state = "DC") %>%
#   pivot_wider(id_cols = c("GEOID", "NAME", "geometry"), # reshaping the data so that it's easier to manipulate
#                                                         # (feel free to run lines 14-31 before the %>% to see what it looked like)
#               names_from = "variable",
#               values_from = c("estimate", "moe")) %>%
#   mutate(total_pop = estimate_B01001_001, # create new variables for maps; update variable names so that they're recognizable
#          under18_pop =
#            estimate_B01001_003 + estimate_B01001_004 + estimate_B01001_005 + estimate_B01001_006 +
#            estimate_B01001_027 + estimate_B01001_028 + estimate_B01001_029 + estimate_B01001_030,
#          over18_pop = total_pop - under18_pop,#why 18?
#          over85_pop = estimate_B01001_025 + estimate_B01001_049,
#          under85_pop = total_pop - over85_pop,
#          over65_pop =
#            estimate_B01001_020 + estimate_B01001_021 + estimate_B01001_022 + estimate_B01001_023 + estimate_B01001_024 +
#            estimate_B01001_044 + estimate_B01001_045 + estimate_B01001_046 + estimate_B01001_047 + estimate_B01001_048 +
#            over85_pop,
#          under65_pop = total_pop - over65_pop,
#          under18_pop_perc = under18_pop / total_pop,
#          over18_pop_perc = over18_pop / total_pop,
#          under85_pop_perc = under85_pop / total_pop,
#          over85_pop_perc = over85_pop / total_pop,
#          under65_pop_perc = under65_pop / total_pop,
#          over65_pop_perc = over65_pop / total_pop,
#          med_inc = estimate_B19013_001,
#          total_race = estimate_B03002_001,
#          white_alone = estimate_B03002_003,
#          black = estimate_B03002_004,
#          amind_natalas = estimate_B03002_005,
#          asian = estimate_B03002_006,
#          nathi_pi = estimate_B03002_007,
#          hisp = estimate_B03002_012,
#          white_perc = white_alone / total_race,
#          black_perc = black / total_race,
#          amind_perc = amind_natalas / total_race,
#          asian_perc = asian / total_race,
#          nathi_perc = nathi_pi / total_race,
#          hisp_perc = hisp / total_race
#          ) %>%
#   st_transform("ESRI:102003") # set the coordinate reference system (crs) so that all of the map layers are aligned
#                               # more info on crs: https://urbaninstitute.github.io/r-at-urban/mapping.html#crs


#--------tract--------#
# 
# # load DC tract geography and relevant variables
# dc_tract <- get_acs(geography = "tract",
#                                 #vars for calculating housing conditions
#                  variables = c("B25123_001", #total own oc units
#                                "B25123_002", #total own oc units 
#                                "B25123_003", "B25123_004", "B25123_005", "B25123_006", #own oc w/ conditions
#                                "B25123_007", #owner oc, no conditions
#                                "B25123_008", #total rent oc units
#                                "B25123_009", "B25123_010", "B25123_011", "B25123_012", #rent oc w/ conditions
#                                "B25123_013", #renter oc, no conditions,
#                                
#                                #vars for overcrowding
#                                "B25014_001", #total 
#                                "B25014_006", "B25014_007", #together >1.5 occ per room (owners)
#                                "B25014_012", "B25014_013", #together >1.5 occ per room (renters)
#                                
#                                #vars for rent burden
#                                "B25070_001", #gross rent total units measured
#                                "B25070_007", "B25070_008", "B25070_009", "B25070_010", #gross rent >30% hh inc
#                                #annie, please also calculate severe rent burden (50%+)
#                                
#                               
#                                
#                                #vars for housing cost burden for owner-occ units
#                                "B25091_001", #Owner-occupied Housing Units
#                                "B25091_008", "B25091_009", "B25091_010", "B25091_011", "B25091_019", "B25091_020", "B25091_021", "B25091_022", #Owner-occupied Housing Units where housing costs are  >30% hh inc
#                                #annie, please also calculate severe cost burden (50%+)
#                                
#                                #vars for tenure
#                                "B25003_001", #total tenure
#                                "B25003_002", #own tenure
#                                "B25003_003", #rent tenure
#                                
#                                #vars for vacancy rates
#                                #B25002
#                                "B25002_001", # - Estimate!!Total:OCCUPANCY STATUS
#                                "B25002_002", # - Estimate!!Total:!!Occupied OCCUPANCY STATUS
#                                "B25002_003" # - Estimate!!Total:!!Vacant OCCUPANCY STATUS
#                                #B25004 for more details of vacancy, likely not nec. right now
#                                ),
#                  year = 2021, # most recently available data
#                  geometry = TRUE, # include geometry of block groups for mapping
#                  state = "DC") %>%
#   pivot_wider(id_cols = c("GEOID", "NAME", "geometry"), # reshaping the data so that it's easier to manipulate 
#               # (feel free to run lines 14-31 before the %>% to see what it looked like)
#               names_from = "variable",
#               values_from = c("estimate", "moe")) %>%
#         #vars for calculating housing conditions
#   mutate(perc_housing_conditions_own = 
#            (estimate_B25123_003 + estimate_B25123_004 + estimate_B25123_005 + estimate_B25123_006)/
#            estimate_B25123_002, #own oc w/ conditions
#          #own oc w/ conditions - calculating through subtracting "no conditions" units
#          #don't need both, you can decide
#          perc_housing_conditions_own2 = 
#            (estimate_B25123_002 - estimate_B25123_007)/ estimate_B25123_002,
#          
#          perc_housing_conditions_rent = 
#            (estimate_B25123_009 + estimate_B25123_010 + estimate_B25123_011 + estimate_B25123_012)/
#            estimate_B25123_008, #rent oc w/ conditions
#          #rent oc w/ conditions - calculating through subtracting "no conditions" units
#          #don't need both, you can decide
#          perc_housing_conditions_rent2 = 
#            (estimate_B25123_008 - estimate_B25123_013) / estimate_B25123_008,
#          
#          #vars for overcrowding
#          perc_crowd_own = (estimate_B25014_006 + estimate_B25014_007)/ estimate_B25123_002,
#          perc_crowd_rent = (estimate_B25014_012 + estimate_B25014_013)/ estimate_B25123_008,
#          
#          #vars for overcrowding
#          perc_burden_own = 
#            (estimate_B25091_008 + estimate_B25091_009 + estimate_B25091_010 + estimate_B25091_011 + 
#               estimate_B25091_019 + estimate_B25091_020 + estimate_B25091_021 + estimate_B25091_022)/
#            estimate_B25091_001,
#          perc_burden_rent =
#            (estimate_B25070_007 + estimate_B25070_008 + estimate_B25070_009 + estimate_B25070_010)/
#            estimate_B25070_001,
#          tenure_own = estimate_B25003_002 / estimate_B25070_001,
#          tenure_rent = estimate_B25003_003 / estimate_B25070_001,
#          
#          
#          #vars for vacancy rates
#          perc_vacant = 
#            estimate_B25002_003/ estimate_B25002_001,
#          ) %>%
#   st_transform("ESRI:102003") # set the coordinate reference system (crs) so that all of the map layers are aligned
# # more info on crs: https://urbaninstitute.github.io/r-at-urban/mapping.html#crs


### -----------------------call---------------------------------####

# study years to iterate over
# years <- c(2012:2022)
years <- c(2021)


#--------block group--------#
geos <- "block group"

block_group_vars <- c("B01001_001", #total pop
                      "B01001_003", "B01001_004", "B01001_005","B01001_006", #sum of these is M pop under 18
                      "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", #sum of these is M pop 65-84
                      "B01001_025", #M pop over 85
                      "B01001_027", "B01001_028", "B01001_029", "B01001_030", #sum of these is F pop under 18
                      "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", #sum of these is F pop 65-84
                      "B01001_049", #F pop over 85
                      "B19013_001", #med income
                      "B03002_001", #total race pop
                      "B03002_002", #total nH 
                      "B03002_003", #total nH white
                      "B03002_004", #total nH Black
                      "B03002_005", #total nH AmerInd NativeAmer
                      "B03002_006", #total nH asian
                      "B03002_007", #toal nH NativeHawaiian PacificIslander
                      "B03002_012" #total Hisp
                      )

selected_vars <- block_group_vars

# function call
bg_data <- map_df(years, ~acs_pull(geos, .x, selected_vars))%>%
  
  # here manipulate data and rename
  pivot_wider(id_cols = c("GEOID", "NAME", "geometry"), # reshaping the data so that it's easier to manipulate 
              # (feel free to run lines 14-31 before the %>% to see what it looked like)
              names_from = "variable",
              values_from = c("estimate", "moe")) %>%
  mutate(total_pop = estimate_B01001_001, # create new variables for maps; update variable names so that they're recognizable
         under18_pop =
           estimate_B01001_003 + estimate_B01001_004 + estimate_B01001_005 + estimate_B01001_006 +
           estimate_B01001_027 + estimate_B01001_028 + estimate_B01001_029 + estimate_B01001_030,
         over18_pop = total_pop - under18_pop,#why 18?
         over85_pop = estimate_B01001_025 + estimate_B01001_049,
         under85_pop = total_pop - over85_pop,
         over65_pop =
           estimate_B01001_020 + estimate_B01001_021 + estimate_B01001_022 + estimate_B01001_023 + estimate_B01001_024 +
           estimate_B01001_044 + estimate_B01001_045 + estimate_B01001_046 + estimate_B01001_047 + estimate_B01001_048 +
           over85_pop,
         under65_pop = total_pop - over65_pop,
         under18_pop_perc = under18_pop / total_pop,
         over18_pop_perc = over18_pop / total_pop,
         under85_pop_perc = under85_pop / total_pop,
         over85_pop_perc = over85_pop / total_pop,
         under65_pop_perc = under65_pop / total_pop,
         over65_pop_perc = over65_pop / total_pop,
         med_inc = estimate_B19013_001,
         total_race = estimate_B03002_001,
         white_alone = estimate_B03002_003,
         black = estimate_B03002_004,
         amind_natalas = estimate_B03002_005,
         asian = estimate_B03002_006,
         nathi_pi = estimate_B03002_007,
         hisp = estimate_B03002_012,
         white_perc = white_alone / total_race,
         black_perc = black / total_race,
         amind_perc = amind_natalas / total_race,
         asian_perc = asian / total_race,
         nathi_perc = nathi_pi / total_race,
         hisp_perc = hisp / total_race
  ) %>%
  st_transform("ESRI:102003")#%>% # set the coordinate reference system (crs) so that all of the map layers are aligned
  # select(-contains(MOE and estimate))
  # more info on crs: https://urbaninstitute.github.io/r-at-urban/mapping.html#crs

  
#--------tract--------#
geos <- "tract"

tract_vars <- c("B25123_001", #total own oc units
                "B25123_002", #total own oc units 
                "B25123_003", "B25123_004", "B25123_005", "B25123_006", #own oc w/ conditions
                "B25123_007", #owner oc, no conditions
                "B25123_008", #total rent oc units
                "B25123_009", "B25123_010", "B25123_011", "B25123_012", #rent oc w/ conditions
                "B25123_013", #renter oc, no conditions,
                
                #vars for overcrowding
                "B25014_001", #total 
                "B25014_006", "B25014_007", #together >1.5 occ per room (owners)
                "B25014_012", "B25014_013", #together >1.5 occ per room (renters)
                #may need to pull total owner occ and renter occ for overcrowding 
                #in the future, may want to pull by race 
                
                #vars for rent burden
                "B25070_001", #gross rent total units measured
                "B25070_007", "B25070_008", "B25070_009", "B25070_010", #gross rent >30% hh inc
                #annie, please also calculate severe rent burden (50%+) B25070_010
                
                
                
                #vars for housing cost burden for owner-occ units
                "B25091_001", #Owner-occupied Housing Units
                "B25091_008", "B25091_009", "B25091_010", "B25091_011", "B25091_019", "B25091_020", "B25091_021", "B25091_022", #Owner-occupied Housing Units where housing costs are  >30% hh inc
                #annie, please also calculate severe cost burden (50%+)
                
                #vars for tenure
                "B25003_001", #total tenure
                "B25003_002", #own tenure
                "B25003_003", #rent tenure
                
                #vars for vacancy rates
                #B25002
                "B25002_001", # - Estimate!!Total:OCCUPANCY STATUS
                "B25002_002", # - Estimate!!Total:!!Occupied OCCUPANCY STATUS
                "B25002_003" # - Estimate!!Total:!!Vacant OCCUPANCY STATUS
                #B25004 for more details of vacancy, likely not nec. right now
                )
#check variables to see if they are available at the block group level

selected_vars <- tract_vars

# function call
tract_data <- map_df(years, ~acs_pull(geos, .x, selected_vars)) %>%
  
  # here manipulate data and rename
  pivot_wider(id_cols = c("GEOID", "NAME", "geometry"), # reshaping the data so that it's easier to manipulate 
              # (feel free to run lines 14-31 before the %>% to see what it looked like)
              names_from = "variable",
              values_from = c("estimate", "moe")) %>%
  #vars for calculating housing conditions
  mutate(perc_housing_conditions_own = 
           (estimate_B25123_003 + estimate_B25123_004 + estimate_B25123_005 + estimate_B25123_006)/
           estimate_B25123_002, #own oc w/ conditions
         #own oc w/ conditions - calculating through subtracting "no conditions" units
         #don't need both, you can decide
         perc_housing_conditions_own2 = 
           (estimate_B25123_002 - estimate_B25123_007)/ estimate_B25123_002,
         
         perc_housing_conditions_rent = 
           (estimate_B25123_009 + estimate_B25123_010 + estimate_B25123_011 + estimate_B25123_012)/
           estimate_B25123_008, #rent oc w/ conditions
         #rent oc w/ conditions - calculating through subtracting "no conditions" units
         #don't need both, you can decide
         perc_housing_conditions_rent2 = 
           (estimate_B25123_008 - estimate_B25123_013) / estimate_B25123_008,
         
         #vars for overcrowding
         perc_crowd_own = (estimate_B25014_006 + estimate_B25014_007)/ estimate_B25123_002,
         perc_crowd_rent = (estimate_B25014_012 + estimate_B25014_013)/ estimate_B25123_008,
         
         #vars for overcrowding
         perc_burden_own = 
           (estimate_B25091_008 + estimate_B25091_009 + estimate_B25091_010 + estimate_B25091_011 + 
              estimate_B25091_019 + estimate_B25091_020 + estimate_B25091_021 + estimate_B25091_022)/
           estimate_B25091_001,
         perc_burden_rent =
           (estimate_B25070_007 + estimate_B25070_008 + estimate_B25070_009 + estimate_B25070_010)/
           estimate_B25070_001,
         tenure_own = estimate_B25003_002 / estimate_B25070_001,
         tenure_rent = estimate_B25003_003 / estimate_B25070_001,
         
         
         #vars for vacancy rates
         perc_vacant = 
           estimate_B25002_003/ estimate_B25002_001,
  ) %>%
  st_transform("ESRI:102003") # set the coordinate reference system (crs) so that all of the map layers are aligned
# more info on crs: https://urbaninstitute.github.io/r-at-urban/mapping.html#crs
#remove MOE and estimates  

### -----------------------geographies---------------------------------####

##### set geographies
# filter out block groups identified as relevant congress heights areas by the interns, plus saint elizabeth's campus
# we will use these block groups as a layer in the maps
congress_heights_bg <- bg_data %>%
  filter(GEOID %in% c("110010098041","110010098042","110010073041",
                      "110010073042","110010073043","110010104003"))

#tracts 104, 73.04, 98.04 - can add others as appropriate
parcel15_bg <- bg_data %>%
  filter(GEOID %in% c("110010098041","110010098042","110010073041",
                      "110010073042","110010073043","110010104003"))


#tracts 104, 73.04, 98.04 - can add others as appropriate
parcel15_tract <- tract_data %>%
  filter(GEOID %in% c("11001009804", "11001007304","11001010400"))

# load ward shapefiles from dc's open data
ward8 <- st_read("data/Wards_from_2022.shp") %>%
  filter(WARD == "8") %>%
  st_transform("ESRI:102003") # align crs

# create df that includes all block groups in ward 8
# could be useful if we want to compare characteristics of the selected block groups with any neighboring block groups
ward8_bg <- ward8 %>%
  st_intersection(bg_data) %>%
  st_transform("ESRI:102003") # align crs

# create df that includes all tracts in ward 8
ward8_tract <- ward8 %>%
  st_intersection(tract_data) %>%
  st_transform("ESRI:102003") # align crs


### currently unused data ######################################################

# shapefile of neighborhood clusters as defined by dc
nbhd_cluster <- st_read("data/Neighborhood_Clusters.shp") %>%
  filter(NBH_NAMES %in% c("Congress Heights, Bellevue, Washington Highlands",
                          "Saint Elizabeths")) %>%
  st_transform("ESRI:102003")

# point data on neighborhoods as defined by dc
nbhd_names <- st_read("data/Neighborhood_labels.shp") 






#other possible vars: housing cost burden, homeowners/renters

#########
# # load DC block group geography and relevant variables
# dc_bg <- get_acs(geography = "block group",
#                  variables = c("B01001_001", #total pop
#                                "B01001_003", "B01001_004", "B01001_005","B01001_006", #sum of these is M pop under 18
#                                "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", #sum of these is M pop 65-84
#                                "B01001_025", #M pop over 85
#                                "B01001_027", "B01001_028", "B01001_029", "B01001_030", #sum of these is F pop under 18
#                                "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", #sum of these is F pop 65-84
#                                "B01001_049", #F pop over 85
#                                "B19013_001", #med income
#                                "B03002_001", #total race pop
#                                "B03002_002", #total nH 
#                                "B03002_003", #total nH white
#                                "B03002_004", #total nH Black
#                                "B03002_005", #total nH AmerInd NativeAmer
#                                "B03002_006", #total nH asian
#                                "B03002_007", #toal nH NativeHawaiian PacificIslander
#                                "B03002_012", #total Hisp
#                                "B25123_002", #total own oc units
#                                "B25123_003", "B25123_004", "B25123_005", "B25123_006", #own oc w/ conditions
#                                "B25123_007", #owner oc, no conditions
#                                "B25123_008", #total rent oc units
#                                "B25123_009", "B25123_010", "B25123_011", "B25123_012", #rent oc w/ conditions
#                                "B25123_013", #renter oc, no conditions,
#                                "B25014_006", "B25014_007", #together >1.5 occ per room (owners)
#                                "B25014_012", "B25014_013", #together >1.5 occ per room (renters)
#                                "B25070_001", #gross rent total units measured
#                                "B25070_007", "B25070_008", "B25070_009", "B25070_010", #gross rent >30% hh inc
#                                "B25091_001", 
#                                "B25091_008", "B25091_009", "B25091_010", "B25091_011", "B25091_019", "B25091_020", "B25091_021", "B25091_022",
#                                "B25003_001", #total tenure
#                                "B25003_002", #own tenure
#                                "B25003_003" #rent tenure
#                  ), 
#                  year = 2021, # most recently available data
#                  geometry = TRUE, # include geometry of block groups for mapping
#                  state = "DC") %>%
#   pivot_wider(id_cols = c("GEOID", "NAME", "geometry"), # reshaping the data so that it's easier to manipulate 
#               # (feel free to run lines 14-31 before the %>% to see what it looked like)
#               names_from = "variable",
#               values_from = c("estimate", "moe")) %>%
#   mutate(total_pop = estimate_B01001_001, # create new variables for maps; update variable names so that they're recognizable
#          under18_pop =
#            estimate_B01001_003 + estimate_B01001_004 + estimate_B01001_005 + estimate_B01001_006 +
#            estimate_B01001_027 + estimate_B01001_028 + estimate_B01001_029 + estimate_B01001_030,
#          over18_pop = total_pop - under18_pop,#why 18?
#          over85_pop = estimate_B01001_025 + estimate_B01001_049,
#          under85_pop = total_pop - over85_pop,
#          over65_pop =
#            estimate_B01001_020 + estimate_B01001_021 + estimate_B01001_022 + estimate_B01001_023 + estimate_B01001_024 +
#            estimate_B01001_044 + estimate_B01001_045 + estimate_B01001_046 + estimate_B01001_047 + estimate_B01001_048 +
#            over85_pop,
#          under65_pop = total_pop - over65_pop,
#          under18_pop_perc = under18_pop / total_pop,
#          over18_pop_perc = over18_pop / total_pop,
#          under85_pop_perc = under85_pop / total_pop,
#          over85_pop_perc = over85_pop / total_pop,
#          under65_pop_perc = under65_pop / total_pop,
#          over65_pop_perc = over65_pop / total_pop,
#          med_inc = estimate_B19013_001,
#          total_race = estimate_B03002_001,
#          white_alone = estimate_B03002_003,
#          black = estimate_B03002_004,
#          amind_natalas = estimate_B03002_005,
#          asian = estimate_B03002_006,
#          nathi_pi = estimate_B03002_007,
#          hisp = estimate_B03002_012,
#          white_perc = white_alone / total_race,
#          black_perc = black / total_race,
#          amind_perc = amind_natalas / total_race,
#          asian_perc = asian / total_race,
#          nathi_perc = nathi_pi / total_race,
#          hisp_perc = hisp / total_race,
#          perc_housing_conditions_own = 
#            (estimate_B25123_003 + estimate_B25123_004 + estimate_B25123_005 + estimate_B25123_006)/
#            estimate_B25123_002, #own oc w/ conditions
#          perc_housing_conditions_own2 = 
#            (estimate_B25123_002 - estimate_B25123_007)/ estimate_B25123_002,
#          perc_housing_conditions_rent = 
#            (estimate_B25123_009 + estimate_B25123_010 + estimate_B25123_011 + estimate_B25123_012)/
#            estimate_B25123_008,
#          perc_housing_conditions_rent2 = 
#            (estimate_B25123_008 - estimate_B25123_013) / estimate_B25123_008,
#          perc_crowd_own = (estimate_B25014_006 + estimate_B25014_007)/ estimate_B25123_002,
#          perc_crowd_rent = (estimate_B25014_012 + estimate_B25014_013)/ estimate_B25123_008,
#          perc_burden_own = 
#            (estimate_B25091_008 + estimate_B25091_009 + estimate_B25091_010 + estimate_B25091_011 + 
#               estimate_B25091_019 + estimate_B25091_020 + estimate_B25091_021 + estimate_B25091_022)/
#            estimate_B25091_001,
#          perc_burden_rent =
#            (estimate_B25070_007 + estimate_B25070_008 + estimate_B25070_009 + estimate_B25070_010)/
#            estimate_B25070_001,
#          tenure_own = estimate_B25003_002 / estimate_B25070_001,
#          tenure_rent = estimate_B25003_003 / estimate_B25070_001
#   ) %>%
#   st_transform("ESRI:102003") # set the coordinate reference system (crs) so that all of the map layers are aligned
# # more info on crs: https://urbaninstitute.github.io/r-at-urban/mapping.html#crs
