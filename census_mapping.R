library(tidycensus)
library(jsonlite)
library(tidyverse)
library(sf)
library(leaflet)
library(tmap)
library(tigris)
library(ggpubr)
options(tigris_use_cache = TRUE)

readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")
#census_api_key("####", overwrite=TRUE, install=TRUE) 
#Uncomment line 13 and update ####with unique key
#Get key at: https://api.census.gov/data/key_signup.html

#### ACS MAP
#https://cran.r-project.org/web/packages/tidycensus/tidycensus.pdf

# Download the Variable dictionary
df_var<- load_variables(2019, "acs5", cache = TRUE)

var_selection <- c(
  pop = "B02001_001",
  white = "B02001_002",
  black = "B02001_003",
  #native="B02001_004",
  asian="B02001_005",
  #hawaii_pi="B02001_006",
  hispanic="B03002_012",
  total_units ="B25008_001",
  owner_units = "B25008_002",
  rental_units = "B25008_003"
)

#https://walker-data.com/tidycensus/articles/basic-usage.html
#Geography level, ~35 census tracts in Everett vs 501 census block groups
df_raw_block <- get_acs("block group",
                        variables = var_selection,
                        year=2019,
                        state = "WA", #FIPS code 53
                        county = "Snohomish", #FIPS code 061
                        geometry = FALSE, #no geometry here to help with pivot_wider
                        moe = 95, #margin of error
                        cache_table = TRUE)

#Problem w/ tidyr::pivot_wider with geometry, 
#A solution involves using gather.sf but I just called two ACS tables (one w/ geometry and one w/out) and joined by GEOID
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

#Test out sf to write
df_block_sf%>%
  ggplot() +
  geom_sf(aes(fill = asian_pct)) +
  scale_fill_gradient(low = "white", high="black") +
  theme_void()

df_block_sf%>%
  ggplot() +
  geom_sf(aes(fill = rental_units_prop)) +
  scale_fill_gradient(low = "white", high="black") +
  theme_void()

#### FILTER TO Everett
# load places in Washington
wa <- places("WA", year = 2019, class = "sf")
# Get the water bodies of King County
sno_water <- area_water("WA", "Snohomish", class = "sf") 
sno_water_union <- st_union(sno_water) #This takes forever

# filter to Everett
everett <- 
  wa %>%
  filter(NAME == "Everett")

ggplot(everett) + 
  geom_sf(fill = "transparent") + 
  theme_minimal() 

everett_block <- 
  df_block_sf %>%
  # cut the outline of Everett
  st_intersection(everett)%>%
  # remove water areas from the map
  st_difference(sno_water_union) 

ggplot(everett_block) + 
  geom_sf(fill = "white") + 
  theme_minimal() 

everett_block %>%
  ggplot() +
  geom_sf(aes(fill = black_pct), size = .25) +
  scale_fill_gradient(low = "white", high="black", name="Percent") +
  theme_void() 

everett_block %>%
  ggplot() +
  geom_sf(aes(fill = asian_pct), size = .25) +
  scale_fill_gradient(low = "white", high="black", name="Percent") +
  theme_void() 

everett_block %>%
  ggplot() +
  geom_sf(aes(fill = rental_units_prop), size = .25) +
  scale_fill_gradient(low = "white", high="black", name="Count of Rental Units") +
  theme_void() 

## Write ESRI shapefile to work with ArcMap 

st_write(everett_block, "acs_2019_block_group.shp")



