# load libraries for cleaning data
library(tidyverse)
library(tidycensus)
library(sf)
library(urbnthemes)
library(dplyr)

# update census API key with your personal key: https://api.census.gov/data/key_signup.html
#census_api_key('063c3c50651249b77ec616eec973ae7ab5fde4dc')
acs_data <- load_variables(year = 2021,
                                dataset = "acs5", 
                           cache = TRUE) #data detail tables
head(acs_data)

#searching for specific concepts within the ACS df
acs_data[grep(x = acs_data$concept, "Sex"),
         c("concept", "label")]

ch_acs_detail <- get_acs(geography = "tract",
          state = "DC",
          county = "001",
          year = 2021,
          variables= c( "B01001_003", #total M under 5 years
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
                        "B01002_002", #median age M
                        "B01002_003", #median age F
                        "B01001_001", #total pop
                        "B01001_002", #total M
                        "B01001_026", #total F
                        "B03002_001", #total race pop.
                        "B03002_002", #total nH
                        "B03002_003", #total nH white
                        "B03002_004", #total nH Black
                        "B03002_005", #total nH AmerInd NativeAmer
                        "B03002_006", #total nH asian
                        "B03002_007", #toal nH NativeHawaiian PacificIslander
                        "B03002_012", #total Hisp
                        "B07013_003",#renters
                        "B07013_002" #owners
          ), 
          geometry = F,
          output= "wide")

#filtering it so that it only shows congress height tracks
ch_acs_detail <- filter(ch_acs_detail, GEOID==11001009804 | GEOID==11001007304 | GEOID==11001010400) 

#dropping Margin of Error 
ch_acs_detail <-ch_acs_detail %>% select(ends_with(c("D","E")))

#grouping variables and creating new names
ch_acs_detail <-ch_acs_detail %>% mutate(total_pop = B01001_001E,
                                         under5 = B01001_003E + B01001_027E,
                                         pop5_17 = B01001_004E + B01001_005E + B01001_006E + 
                                           B01001_028E + B01001_029E + B01001_030E,
                                         pop18_24 =
                                           B01001_007E + B01001_008E + B01001_009E + B01001_010E +
                                           B01001_031E + B01001_032E + B01001_033E + B01001_034E,
                                         pop25_65 =
                                           B01001_011E + B01001_012E + B01001_013E + B01001_014E + B01001_015E +
                                           B01001_016E + B01001_017E + B01001_018E + B01001_019E +
                                           B01001_035E + B01001_036E + B01001_037E + B01001_038E + B01001_039E +
                                           B01001_040E + B01001_041E + B01001_042E + B01001_043E,
                                         pop65_85 =
                                           B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E +
                                           B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E,
                                         popover85 = B01001_025E + B01001_049E,
                                         male_tract= B01001_002E, 
                                         female_tract= B01001_026E,
                                         total_race = B03002_001E,
                                         white_alone = B03002_003E, 
                                         black = B03002_004E,
                                         amind_natalas = B03002_005E,
                                         asian = B03002_006E, 
                                         nathi_pi = B03002_007E,
                                         hisp = B03002_012E,
                                         median_ageM = B01002_002E,
                                         median_ageF = B01002_003E,
                                         renters = B07013_003E,
                                         owners = B07013_002E)


# #I want to create one row that summarizes everything but have yet to do that
# summarise (ch_acs_detail)
# #trying to add a new column to represent the total amount across tracts
# CH_total %>%
#   bind_rows(summarise_all(ch_acs_detail, ~if(is.numeric(.)) sum(.) else "Total")

#exporting ch_acs_detail df 
write.csv(ch_acs_detail, "/Users/annierosenow/Library/CloudStorage/Box-Box/Emerson 2022/Mapping/data/for Gabbi/ch_acs_gabbi.csv", 
          row.names=FALSE)
write.csv(ch_acs_detail,file="ch_acs_detail.csv") 

# creating age table
ch_acs_AGE <- ch_acs_detail %>% 
  select(under5,pop5_17,pop18_24,pop25_65,pop65_85,popover85) %>% 
  summarize(totalunder5 =sum(under5),
            totalpop5_17 = sum(pop5_17),
            totalpop18_24 = sum(pop18_24),
            totalpop25_65 = sum(pop25_65),
            totalpop65_85 = sum(pop65_85),
            totalpopover85 = sum(popover85))%>% 
  summarize(perc_under5= totalunder5/ (totalunder5 + totalpop5_17 + totalpop18_24 + totalpop25_65 + totalpop65_85 + totalpopover85), 
            perc_5_17= totalpop5_17/ (totalunder5 + totalpop5_17 + totalpop18_24 + totalpop25_65 + totalpop65_85 + totalpopover85), 
            perc_18_24= totalpop18_24/ (totalunder5 + totalpop5_17 + totalpop18_24 + totalpop25_65 + totalpop65_85 + totalpopover85),
            perc_25_65= totalpop25_65/ (totalunder5 + totalpop5_17 + totalpop18_24 + totalpop25_65 + totalpop65_85 + totalpopover85),
            perc_65_85= totalpop65_85/ (totalunder5 + totalpop5_17 + totalpop18_24 + totalpop25_65 + totalpop65_85 + totalpopover85),
            perc_over85= totalpopover85/ (totalunder5 + totalpop5_17 + totalpop18_24 + totalpop25_65 + totalpop65_85 + totalpopover85) 
            )

#exporting ch_acs_AGE df 
write.csv(ch_acs_AGE, "/Users/annierosenow/Library/CloudStorage/Box-Box/Emerson 2022/Mapping/data/for Gabbi/ch_acs_AGE.csv", row.names=FALSE)
write.csv(ch_acs_AGE,file="ch_acs_AGE.csv") 

# creating F and M table
ch_acs_SEX <- ch_acs_detail %>% 
  select(male_tract, female_tract) %>% 
  summarize(totalMpop =sum(male_tract),
            totalFpop = sum(female_tract)) %>% 
  summarize(perc_male= totalMpop/(totalMpop + totalFpop),
            perc_female= totalFpop/(totalMpop + totalFpop)) 
  

# creating demographics table
ch_acs_demograhics<- ch_acs_detail %>% 
  select(total_race, white_alone, black, amind_natalas,asian, nathi_pi,hisp) %>% 
    summarize(CHtotalrace =sum(total_race),
            totalwhite = sum(white_alone),
            totalblack = sum(black),
            totalamind_natalas = sum(amind_natalas),
            totalasian = sum(asian),
            totalnathi_pi = sum(nathi_pi),
            totalhisp = sum(hisp)) %>% 
    summarize(perc_white= totalwhite/ (totalwhite + totalblack + totalamind_natalas + totalasian + totalnathi_pi + totalhisp), 
              perc_black= totalblack/ (totalwhite + totalblack + totalamind_natalas + totalasian + totalnathi_pi + totalhisp),  
              perc_amind_natalas= totalamind_natalas/ (totalwhite + totalblack + totalamind_natalas + totalasian + totalnathi_pi + totalhisp),
              perc_asian= totalasian/ (totalwhite + totalblack + totalamind_natalas + totalasian + totalnathi_pi + totalhisp),
              perc_nathi_pi= totalnathi_pi/ (totalwhite + totalblack + totalamind_natalas + totalasian + totalnathi_pi + totalhisp),
              perc_hisp= totalhisp/ (totalwhite + totalblack + totalamind_natalas + totalasian + totalnathi_pi + totalhisp)
            )

# creating renter vs owner table
ch_acs_renters_owners <- ch_acs_detail %>% 
  select(owners, renters) %>% 
  summarize(totalrenters =sum(renters),
            totalowners = sum(owners)) %>% 
  summarize(perc_renters= totalrenters/(totalowners + totalrenters),
            perc_owners= totalowners/(totalowners + totalrenters))

#leftjoin could add the data table to 
#write.csv(df, "C:/Users/VSulkaHewes/Box/Emerson CEE/Mapping/data/for Gabbi/file_name.csv", row.names=FALSE)
                                      
