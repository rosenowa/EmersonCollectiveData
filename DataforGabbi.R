# load libraries for cleaning data
library(tidyverse)
library(tidycensus)
library(sf)
library(urbnthemes)

# update census API key with your personal key: https://api.census.gov/data/key_signup.html
#Annie's census api key:('25e7c711e84c77deb722fd007f9152d745a46027')
#census_api_key('2330d2d66e60a29868283ad737077ca35e502983')

# load ACS variables to identify the correct variable names for analysis 
acs21 <- load_variables(2021, "acs5", cache = TRUE)
View(acs21)

####-----------------pull acs data for vars at dif geos--------------------####


acs_pull_gabbi <- function(geos, years, block_group_vars_gabbi){
  get_acs(geography = geos,
          year = years, 
          variables = selected_vars,
          geometry = TRUE, # include geometry for mapping
          state = "DC") %>% 
    mutate(year = years)}
  
  ### -----------------------call---------------------------------####
  
  # study years to iterate over
  # years <- c(2012:2022)
  years <- c(2021)
  
  
  #--------block group--------#
  geos <- "block group"
  
  block_group_vars_gabbi <- c("B01001_003", #total M under 5 years
                        "B01001_004", #total M 5-9 years
                        "B01001_005", #total M 10-14 years
                        "B01001_006", #total M 15 to 17 years
                        "B01001_007", #total M 18 to 19 years
                        "B01001_008", #total M 20 years
                        "B01001_009", #total M 21 years
                        "B01001_010", #total M 22 to 24 years
                        "B01001_011", #total M 25 to 29 years
                        "B01001_012", #total M 30 to 34 years
                        "B01001_013", #total M 35 to 39 years
                        "B01001_014", #total M 40 to 44 years
                        "B01001_015", #total M 45 to 49 years
                        "B01001_016", #total M 50 to 54 years
                        "B01001_017", #total M 55 to 59 years
                        "B01001_018", #total M 60 to 61 years
                        "B01001_019", #total M 62 to 64 years
                        "B01001_020", #total M 65 to 66 years
                        "B01001_021", #total M 67 to 69 years
                        "B01001_022", #total M 70 to 74 years
                        "B01001_023", #total M 75 to 79 years
                        "B01001_024", #total M 80 to 84 years
                        "B01001_025", #M pop over 85
                        #AGE BY FEMALE
                        "B01001_027", #total F under 5 years
                        "B01001_028", #total F 5-9 years
                        "B01001_029", #total F 10-14 years
                        "B01001_030", #total F 15 to 17 years
                        "B01001_031", #total F 18 to 19 years
                        "B01001_032", #total F 20 years
                        "B01001_033", #total F 21 years
                        "B01001_034", #total F 22 to 24 years
                        "B01001_035", #total F 25 to 29 years
                        "B01001_036", #total F 30 to 34 years
                        "B01001_037", #total F 35 to 39 years
                        "B01001_038", #total F 40 to 44 years
                        "B01001_039", #total F 45 to 49 years
                        "B01001_040", #total F 50 to 54 years
                        "B01001_041", #total F 55 to 59 years
                        "B01001_042", #total F 60 to 61 years
                        "B01001_043", #total F 62 to 64 years
                        "B01001_044", #total F 65 to 66 years
                        "B01001_045", #total F 67 to 69 years
                        "B01001_046", #total F 70 to 74 years
                        "B01001_047", #total F 75 to 79 years
                        "B01001_048", #total F 80 to 84 years
                        "B01001_049", #F pop over 85
                        #median age
                        "B01002_002", #median age M
                        "B01002_002", #median age F
                        #total population count by sex
                        "B01003_001", #total pop
                        "B01001_002", #total M 
                        "B01001_026", #total F
                        "B03002_001", #total race pop.    
                        "B03002_002", #total nH 
                        "B03002_003", #total nH white
                        "B03002_004", #total nH Black
                        "B03002_005", #total nH AmerInd NativeAmer
                        "B03002_006", #total nH asian
                        "B03002_007", #toal nH NativeHawaiian PacificIslander
                        "B03002_012" #total Hisp
                        )
  
  
  # function call
  bg_datagabbi <- map_df(years, ~acs_pull(geos, .x, block_group_vars_gabbi))%>%
    
    # here manipulate data and rename
    pivot_wider(id_cols = c("GEOID", "NAME", "geometry"), # reshaping the data so that it's easier to manipulate 
                # (feel free to run lines 14-31 before the %>% to see what it looked like)
                names_from = "variable",
                values_from = c("estimate", "moe")) %>%
    mutate(total_pop = estimate_B01003_001, # create new variables for maps; update variable names so that they're recognizable
           under5_pop = estimate_B01001_003 + estimate_B01001_027, #M + F
           between5_17 = estimate_B01001_004 + estimate_B01001_005 + estimate_B01001_006 + 
             estimate_B01001_028 + estimate_B01001_029 + estimate_B01001_030,
           between18_24 = estimate_B01001_007 + estimate_B01001_008 + estimate_B01001_009 + estimate_B01001_010 +
             estimate_B01001_031 + estimate_B01001_032 + estimate_B01001_033 + estimate_B01001_034, 
           between25_65 = 	estimate_B01001_035 + 
             estimate_B01001_011 + estimate_B01001_012 + estimate_B01001_013 + estimate_B01001_014 + estimate_B01001_015 +
             estimate_B01001_016 + estimate_B01001_017 + estimate_B01001_018 + estimate_B01001_019 +
             estimate_B01001_035 + estimate_B01001_036 + estimate_B01001_037 + estimate_B01001_038 + estimate_B01001_039 + 
             estimate_B01001_040 + estimate_B01001_041 + estimate_B01001_0342 + estimate_B01001_043, 
           between65_85 = 
             estimate_B01001_020 + estimate_B01001_021 + estimate_B01001_022 + estimate_B01001_023 + estimate_B01001_024 +
             estimate_B01001_044 + estimate_B01001_045 + estimate_B01001_046 + estimate_B01001_047 + estimate_B01001_048, 
           over85 = estimate_B01001_025 + estimate_B01001_049, 
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
           )%>%
    st_transform("ESRI:102003")%>% # set the coordinate reference system (crs) so that all of the map layers are aligned
  # select(-contains(MOE and estimate))
  # more info on crs: https://urbaninstitute.github.io
  
  ### -----------------------geographies---------------------------------####
  
  ##### set geographies
  # filter out block groups identified as relevant congress heights areas by the interns, plus saint elizabeth's campus
  # we will use these block groups as a layer in the maps
  congress_heights_bg <- bg_datagabbi %>%
    filter(GEOID %in% c("110010098041","110010098042","110010073041",
                        "110010073042","110010073043","110010104003"))
  
  #tracts 104, 73.04, 98.04 - can add others as appropriate
  parcel15_bg <- bg_datagabbi %>%
    filter(GEOID %in% c("110010098041","110010098042","110010073041",
                        "110010073042","110010073043","110010104003"))