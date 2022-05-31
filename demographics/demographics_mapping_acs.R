################################################################################
# Policy Consultant Code for Everett Housing Authority (EHA)
# Author: Katherine Chang
# Date: Winter/Spring 2022

# Purpose: The following code generates city racial demographic maps for Everett
# using the U.S. Census Bureau's American Community Survey 5-Year Estimates
# for 2015-2019.

################################################################################

################################################################################
# STEP 1: Load packages 
################################################################################

rm(list=ls())

library(ggpubr)
library(here)
library(leaflet)
library(sf)
library(tidycensus)
library(tidyverse)
library(tigris)
library(tmap)
options(tigris_use_cache = TRUE)

readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")
# census_api_key("####", overwrite=TRUE, install=TRUE) 
# Uncomment line 30 and update #### with unique key
# Get key at: https://api.census.gov/data/key_signup.html

here::i_am("./demographics_mapping_acs.R")

################################################################################
# STEP 2: Load and Select ACS 2014-2019 ACS Data Variables List 
################################################################################

# ACS MAP
# https://cran.r-project.org/web/packages/tidycensus/tidycensus.pdf

# Download the Variable dictionary
df_var<- load_variables(2019, "acs5", cache = TRUE)

var_selection <- c(
  pop = "B02001_001",
  white = "B02001_002",
  black = "B02001_003",
  asian="B02001_005",
  hispanic="B03002_012",
  #native="B02001_004", #Uncomment for Native American demographics
  #hawaii_pi="B02001_006", #Uncomment for API demographics
  total_units ="B25008_001",
  owner_units = "B25008_002",
  rental_units = "B25008_003"
)

# If adding new variables in var_selection, adjust variable selection in Step 4.

################################################################################
# STEP 3: Load Variables of Interest
################################################################################

# Geography level, 35 census tracts in Everett vs 95 census block groups
df_raw_block <- get_acs("block group",
                        variables = var_selection,
                        year=2019,
                        state = "WA", #FIPS code 53
                        county = "Snohomish", #FIPS code 061
                        geometry = FALSE, #no geometry here to help with pivot_wider
                        moe = 95, #margin of error
                        cache_table = TRUE)

################################################################################
# STEP 4: Reshape Data for Mapping
################################################################################

# Problem w/ tidyr::pivot_wider with geometry, 
# A solution can use gather.sf but I just called two ACS tables 
# (one w/ geometry and one w/out) and joined by GEOID

df_raw_block_wide <- df_raw_block %>% 
  pivot_wider(names_from = variable, 
              values_from = estimate)

df_raw_block_wide <- df_raw_block_wide%>%
  select(GEOID, pop, white, black, asian, hispanic, 
         total_units, owner_units, rental_units)%>%
  group_by(GEOID) %>% 
  summarize(across(everything(), ~ first(na.omit(.))))

df_geometry <- get_acs("block group",
                       variables = "B02001_003", #any variable is fine here
                       year=2019,
                       state = "WA", #FIPS code 53
                       county = "Snohomish", #FIPS code 033
                       geometry = TRUE, #this df has geometry
                       cache_table = TRUE)

df_geometry <- df_geometry%>%
  select(GEOID, geometry)

df_block <- full_join(df_raw_block_wide, df_geometry, by = c("GEOID" = "GEOID"), copy=TRUE) 

df_block_sf <- df_block %>%
  mutate(
    white_pct = white/pop,
    black_pct = black/pop,
    asian_pct= asian/pop,
    hispanic_pct=hispanic/pop,
    owner_units_prop = owner_units/total_units,
    rental_units_prop = rental_units/total_units
    )%>%
  st_as_sf() #convert to sf

################################################################################
# STEP 5: Filter Map Down to Everett
################################################################################

wa <- places("WA", year = 2019, class = "sf")
sno_water <- area_water("WA", "Snohomish", class = "sf") 
sno_water_union <- st_union(sno_water) #This line of code takes a while

everett <- 
  wa %>%
  filter(NAME == "Everett")

ggplot(everett) + 
  geom_sf(fill = "transparent") + 
  theme_minimal() 

everett_block <- 
  df_block_sf %>%
  st_intersection(everett)%>%
  st_difference(sno_water_union) 

# Remove block groups outside main city boundaries

everett_block <- everett_block%>%
  filter(GEOID != "530610522071")%>%
  filter(GEOID != "530610536023")%>%
  filter(GEOID != "530610538034")

# Base plot 

ggplot(everett_block) + 
  geom_sf(fill = "white") + 
  theme_minimal() 

################################################################################
# STEP 6: Generate and Save Maps for Demographics
################################################################################

pct_labels <- c("0", "20%", "40%", "60%", "80%", "100%")
pct_breaks <- c(0, 0.20, 0.40, 0.60, 0.80, 1)

everett_block %>%
  ggplot() +
  geom_sf(aes(fill = pop), size = .25) +
  scale_fill_gradient(low = "white", 
                      high="#85754d",
                      name= "Total \nPopulation\n",
                      limits= c(0, 3000),
                      labels = c("0", "1,000", "2,000", "3,000"),
                      breaks = c(0, 1000, 2000, 3000)) +
  theme_void() 

ggsave( "./outputs/demo_population.png",
        plot = last_plot(), bg = "#FFFFFF", dpi = 300)

everett_block %>%
  ggplot() +
  geom_sf(aes(fill = black_pct), size = .25) +
  scale_fill_gradient2(low = "#FFFFFF", 
                       mid = "#8c96c6",
                       high="#4b2e83",
                       midpoint = 0.50,
                       name= "Percentage of\nBlack Residents\n",
                       limits= c(0, 1),
                       labels = pct_labels,
                       breaks = pct_breaks) +
  theme_void() 

ggsave( "./outputs/demo_black_pct.png",
        plot = last_plot(), bg = "#FFFFFF", dpi = 300)

everett_block %>%
  ggplot() +
  geom_sf(aes(fill = asian_pct), size = .25) +
  scale_fill_gradient2(low = "#FFFFFF", 
                       mid = "#8c96c6",
                       high="#4b2e83",
                       midpoint = 0.50,
                       name= "Percentage of\nAsian Residents\n",
                       limits= c(0, 1),
                       labels = pct_labels,
                       breaks = pct_breaks) +
  theme_void() 

ggsave( "./outputs/demo_asian_pct.png",
        plot = last_plot(), bg = "#FFFFFF", dpi = 300)

everett_block %>%
  ggplot() +
  geom_sf(aes(fill = hispanic_pct), size = .25) +
  scale_fill_gradient2(low = "#FFFFFF", 
                       mid = "#8c96c6",
                       high="#4b2e83",
                       midpoint = 0.50,
                       name= "Percentage of\nHispanic Residents\n",
                       limits= c(0, 1),
                       labels = pct_labels,
                       breaks = pct_breaks) +
  theme_void() 

ggsave( "./outputs/demo_hispanic_pct.png",
        plot = last_plot(), bg = "#FFFFFF", dpi = 300)

everett_block %>%
  ggplot() +
  geom_sf(aes(fill = white_pct), size = .25) +
  scale_fill_gradient2(low = "#FFFFFF", 
                       mid = "#8c96c6",
                       high="#4b2e83",
                       midpoint = 0.50,
                       name= "Percentage of\nWhite Residents\n",
                       limits= c(0, 1),
                       labels = pct_labels,
                       breaks = pct_breaks) +
  theme_void() 

ggsave( "./outputs/demo_white_pct.png",
        plot = last_plot(), bg = "#FFFFFF", dpi = 300)

################################################################################
# OPTIONAL: Write Data to ESRI Shapefile to Work With ArcMap
################################################################################

# st_write(everett_block, "./outputs/acs_2019_block_group.shp")

