# load libraries relevant to mapping
library(tidyverse)
library(ggplot2)
library(urbnthemes)

# set mapping format for all maps to urban styles
set_urbn_defaults(style = "map")


# map of calculated share Black population
perc_housing_conditions_rent_parcel15 <- 
  ggplot() +
  geom_sf(data = ward8_tract,
          fill = "white",
          color = "black") + 
  geom_sf(data = parcel15_tract,
          aes(fill = perc_housing_conditions_rent),
          color = "black") + 
  scale_fill_gradientn(guide = guide_legend(reverse = T),
                       labels = scales::percent_format(),
                       name = "Percent of Units Occupied by Renters Expiriencing Housing Conditions")

ggsave("images/perc_housing_conditions_rent_parcel15.png")


#-------------------AGE MAPS------------------------

#-------------------18------------------------
# map of counts of population under 18
under18 <- 
  ggplot() +
  geom_sf(data = ward8_bg, # this map layer includes all of the block groups in ward 8 
          fill = "white", # all of the block group shapes are filled in with white...
          color = "black") + # ...and outlined in black
  geom_sf(data = congress_heights_bg, # this layer includes the block groups in the parcel 15 area
          aes(fill = under18_pop),
          color = "black") + 
  scale_fill_gradientn(guide = guide_legend(reverse = T)) # this reverses the ordering of the legend so that the colors go from light to dark

# map of counts of population over 18
over18 <-
  ggplot() +
  geom_sf(data = ward8_bg,
          fill = "white",
          color = "black") + 
  geom_sf(data = congress_heights_bg,
          aes(fill = over18_pop),
          color = "black") + 
  scale_fill_gradientn(guide = guide_legend(reverse = T)) 

# map of calculated percentage of population under 18
under18_perc <- 
  ggplot() +
  geom_sf(data = ward8_bg,
          fill = "white",
          color = "black") + 
  geom_sf(data = congress_heights_bg,
          aes(fill = under18_pop_perc),
          color = "black") + 
  scale_fill_gradientn(guide = guide_legend(reverse = T),
                       labels = scales::percent_format(),
                       name = "Percent of population under 18") 

ggsave("images/perc_under18_parcel15.png", under18_perc) # save map to images folder on Box

# map of calculated percentage of population over 18
over18_perc <- 
  ggplot() +
  geom_sf(data = ward8_bg,
          fill = "white",
          color = "black") + 
  geom_sf(data = congress_heights_bg,
          aes(fill = over18_pop_perc),
          color = "black") + 
  scale_fill_gradientn(guide = guide_legend(reverse = T),
                       labels = scales::percent_format(),
                       name = "Percent of population over 18")

ggsave("images/perc_over18_parcel15_2.0.png", over18_perc)


#-------------------65------------------------

# 65+
# map of counts of population under 65
under65 <- 
  ggplot() +
  geom_sf(data = ward8_bg, # this map layer includes all of the block groups in ward 8 
          fill = "white", # all of the block group shapes are filled in with white...
          color = "black") + # ...and outlined in black
  geom_sf(data = congress_heights_bg, # this layer includes the block groups in the parcel 15 area
          aes(fill = under65_pop),
          color = "black") + 
  scale_fill_gradientn(guide = guide_legend(reverse = T)) # this reverses the ordering of the legend so that the colors go from light to dark

# map of counts of population over 65
over65 <-
  ggplot() +
  geom_sf(data = ward8_bg,
          fill = "white",
          color = "black") + 
  geom_sf(data = congress_heights_bg,
          aes(fill = over65_pop),
          color = "black") + 
  scale_fill_gradientn(guide = guide_legend(reverse = T)) 

# map of calculated percentage of population under 65
under65_perc <- 
  ggplot() +
  geom_sf(data = ward8_bg,
          fill = "white",
          color = "black") + 
  geom_sf(data = congress_heights_bg,
          aes(fill = under65_pop_perc),
          color = "black") + 
  scale_fill_gradientn(guide = guide_legend(reverse = T),
                       labels = scales::percent_format(),
                       name = "Percent of population under 65") 

ggsave("images/perc_under65_parcel15.png", under65_perc) # save map to images folder on Box

# map of calculated percentage of population over 65
over65_perc <- 
  ggplot() +
  geom_sf(data = ward8_bg,
          fill = "white",
          color = "black") + 
  geom_sf(data = congress_heights_bg,
          aes(fill = over65_pop_perc),
          color = "black") + 
  scale_fill_gradientn(guide = guide_legend(reverse = T),
                       labels = scales::percent_format(),
                       name = "Percent of population over 65")

ggsave("images/perc_over65_parcel15.png", over65_perc)

#-------------------85------------------------

# 85+
# map of counts of population under 85
under85 <- 
  ggplot() +
  geom_sf(data = ward8_bg, # this map layer includes all of the block groups in ward 8 
          fill = "white", # all of the block group shapes are filled in with white...
          color = "black") + # ...and outlined in black
  geom_sf(data = congress_heights_bg, # this layer includes the block groups in the parcel 15 area
          aes(fill = under85_pop),
          color = "black") + 
  scale_fill_gradientn(guide = guide_legend(reverse = T)) # this reverses the ordering of the legend so that the colors go from light to dark

# map of counts of population over 85
over85 <-
  ggplot() +
  geom_sf(data = ward8_bg,
          fill = "white",
          color = "black") + 
  geom_sf(data = congress_heights_bg,
          aes(fill = over85_pop),
          color = "black") + 
  scale_fill_gradientn(guide = guide_legend(reverse = T)) 

# map of calculated percentage of population under 85
under85_perc <- 
  ggplot() +
  geom_sf(data = ward8_bg,
          fill = "white",
          color = "black") + 
  geom_sf(data = congress_heights_bg,
          aes(fill = under85_pop_perc),
          color = "black") + 
  scale_fill_gradientn(guide = guide_legend(reverse = T),
                       labels = scales::percent_format(),
                       name = "Percent of population under 85") 

ggsave("images/perc_under85_parcel15.png", under85_perc) # save map to images folder on Box

# map of calculated percentage of population over 85
over85_perc <- 
  ggplot() +
  geom_sf(data = ward8_bg,
          fill = "white",
          color = "black") + 
  geom_sf(data = congress_heights_bg,
          aes(fill = over85_pop_perc),
          color = "black") + 
  scale_fill_gradientn(guide = guide_legend(reverse = T),
                       labels = scales::percent_format(),
                       name = "Percent of population over 85")

ggsave("images/perc_over85_parcel15.png", over85_perc)




#----------------------RACE MAPS-----------------------

# map of calculated share Black population
black_perc_parcel15_2 <- 
  ggplot() +
  geom_sf(data = ward8_bg,
          fill = "white",
          color = "black") + 
  geom_sf(data = congress_heights_bg,
          aes(fill = black_perc),
          color = "black") + 
  scale_fill_gradientn(guide = guide_legend(reverse = T),
                       labels = scales::percent_format(),
                       name = "Percent Black")

ggsave("images/perc_black_parcel15_2.png")

### maps to explore overall characteristics of ward 8 ##########################

# map of calculated shares Black population for all of ward 8
black_perc_ward8 <- 
  ggplot() +
  geom_sf(data = ward8_bg,
          aes(fill = black_perc),
          #fill = "white",
          color = "black") + 
  scale_fill_gradientn(guide = guide_legend(reverse = T),
                       labels = scales::percent_format(),
                       name = "Percent Black")

ggsave("images/perc_black_ward8.png")

# map of calculated shares hispanic population for all of ward 8
hisp_perc_ward8 <- 
  ggplot() +
  geom_sf(data = ward8_bg,
          aes(fill = hisp_perc),
          #fill = "white",
          color = "black") + 
  scale_fill_gradientn(guide = guide_legend(reverse = T),
                       labels = scales::percent_format(),
                       name = "Percent Hispanic")

ggsave("images/perc_hisp_ward8.png")

# map of median hh incomes for all of ward 8
med_hh_inc_ward8_test <- 
  ggplot() +
  geom_sf(data = ward8_bg,
          aes(fill = med_inc),
          #fill = "white",
          color = "black") + 
  scale_fill_gradientn(guide = guide_legend(reverse = T),
                       name = "Median Household Income in DC Ward 8",
                       na.value = "white")

ggsave("images/med_HH_inc_ward8_test.png")

