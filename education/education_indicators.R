################################################################################
# POLICY CONSULTANT CODE FOR EVERETT HOUSING AUTHORITY (EHA)
# AUTHOR: Katherine Chang (kachang@uw.edu)
# DATE: Winter/Spring 2022

# PURPOSE: The following code is intended to provide an illustrative example for
# EHA's implementation of their Communities of Opportunity initiative.
# This file walks through a single neighborhood characteristic consideration:
# education. The file includes code for data processing, descriptive analysis,
# and mapping to help drive EHA internal decision-making on what indicators 
# meaningfully make up education opportunity within a neighborhood.

# A complete literature review and case study analysis has been provided to
# supervising EHA policy analysts, which provide important context for racial
# and economic equity and meaningful metrics that appropriately summarize
# how to identify spatial education opportunity.

# LIMITATIONS: This analysis lacks data for EPS school attendance boundaries. 
# Physical school addresses are used but further analysis will require obtaining 
# most current school enrollment zones so identification of education access is 
# aligned with school enrollment policy and residential addresses.

# School attendance boundary maps in PDF form can be downloaded at: 
# https://www.everettsd.org/boundarymaps
# However, shapefiles should be requested from EPS to conduct further analysis 

################################################################################

################################################################################
# STEP 1: Load Packages
################################################################################

rm(list=ls())

library(censusxy)
library(corrplot)
library(ggpubr)
library(here)
library(jsonlite)
library(leaflet)
library(sf)
library(tidycensus)
library(tidyverse)
library(tigris)
library(tmap)
options(tigris_use_cache = TRUE)

################################################################################
# STEP 2: Load Data
################################################################################

here::i_am("education_indicators.R")

readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")
# census_api_key("####", overwrite=TRUE, install=TRUE) 
# Uncomment line 56 and update #### with unique key
# Obtain Census key at: https://api.census.gov/data/key_signup.html

#### Education Data
# Data currently hosted on Github has been pre-processed to filter to Snohomish County
# The following file further filters down to the City of Everett, but the loaded
# code is sufficient to conduct similar analysis and mapping to Snohomish County
# boundaries. 
# Data can be found in `./education/data/*`

assessment <- read_csv("./data/assessment_snohomish_2018_2019.csv")
attendance <- read_csv("./data/attendance_snohomish_2018_2019.csv")
discipline <- read_csv("./data/discipline_snohomish_2018_2019.csv")
graduation <- read_csv("./data/graduation_snohomish_2018_2019.csv")
teacher_quality <- read_csv("./data/teacher_quality_snohomish_2018_2019.csv")

################################################################################
# STEP 3: Filter and Clean Data
################################################################################

esd_assessment <- assessment%>%
  filter(DistrictCode == "31002")%>%
  filter(CurrentSchoolType == "P")%>%
  filter(StudentGroupType == "All")%>%
  filter(GradeLevel == "All Grades")%>%
  filter(`Test Administration (group)` == "General")%>%
# Filtered to general test administration
# due to suppression rules for smaller alternative student groups
  select(SchoolCode, SchoolName, TestSubject, PercentMetStandard)

esd_assessment_wide <- esd_assessment%>%
  pivot_wider(names_from= TestSubject, values_from = PercentMetStandard)

esd_attendance <- attendance%>%
  filter(DistrictCode == "31002")%>%
  filter(CurrentSchoolType == "P") %>%
  filter(StudentGroupType == "AllStudents")%>%
  filter(GradeLevel == "All Grades")%>%
  filter(Measures == "Regular Attendance")

esd_discipline <- discipline%>%
  filter(DistrictCode == "31002")%>%
  filter(CurrentSchoolType == "P")%>%
  filter(`Student Group` == "All Students")%>%
  filter(GradeLevel == "All")%>%
  select(SchoolCode, SchoolName, DisciplineRate)

esd_graduation <- graduation%>%
  filter(DistrictCode == "31002")

esd_tchqual_experienced <- teacher_quality%>%
  filter(DistrictCode == "31002")%>%
  filter(CurrentSchoolType == "P")%>%
  filter(ContentAreaId == 0)%>%
  filter(TeacherQualificationId == 4)%>% #Experience Status
  select(SchoolCode, SchoolName, TeacherPercent)%>%
  distinct()%>%
  mutate(SchoolCode = as.numeric(SchoolCode))

ed_qual_list <- list(esd_assessment_wide, esd_discipline, esd_tchqual_experienced)

ed_qual_merge <- ed_qual_list %>% 
  reduce(full_join, by="SchoolCode")%>%
  mutate(ela_pct = as.numeric(parse_number(`English Language Arts`)/100),
         math_pct = as.numeric(parse_number(`Math`)/100),
         science_pct = as.numeric(parse_number(`Science`)/100),
         discipline_pct = as.numeric(parse_number(DisciplineRate)/100),
         teacher_exp_pct = as.numeric(TeacherPercent)
         )%>%
  select(-SchoolName.x, -SchoolName.y,
         -"English Language Arts", -Math, 
         -Science, -DisciplineRate,
         -TeacherPercent)
         
# write_csv(ed_qual_merge, "./outputs/education.csv")

################################################################################
# STEP 4: Descriptive Analysis
################################################################################

# Correlations can help establish whether certain indicators are repetitive
# and give the same information. When working with a large number of potential
# indicators, a simple correlation point can provide information on what indicators
# offer potentially duplicated information.

png(height=417, width=507, 
    file="./outputs/edu_descriptive_corr.png", 
    type = "cairo")

corrplot(cor(ed_qual_merge[,3:7]), method = 'number')

dev.off()

# The results from this correlation plot demonstrates that subject matter tests
# have strong correlation values (>0.50) with each other, particularly
# math and science tests. This isn't surprising, but could indicate that
# including science test scores as an indicator of education quality would
# provide similar information as just math test scores.

# Additionally, there are moderate to high correlation values for 
# discipline rates and all other indicators on the list. 

ed_qual_merge%>%
  ggplot(aes(x=ela_pct)) +
  geom_histogram(bins=20, color="black", fill="gray")+
  geom_density(alpha=.2, fill="gray") +
  geom_vline(aes(xintercept=mean(ela_pct)),
             color="black", linetype="dashed", size=0.5) + 
  labs(x="English Language Arts (ELA) Proficiency (%)") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("./outputs/edu_descriptive_ela.png",
       plot = last_plot(), bg = "#FFFFFF", dpi = 300)

ed_qual_merge%>%
  ggplot(aes(x=math_pct)) +
  geom_histogram(bins=20, color="black", fill="gray")+
  geom_density(alpha=.2, fill="gray") +
  geom_vline(aes(xintercept=mean(math_pct)),
             color="black", linetype="dashed", size=0.5) + 
  labs(x="Math Proficiency (%)") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("./outputs/edu_descriptive_math.png",
       plot = last_plot(), bg = "#FFFFFF", dpi = 300)

ed_qual_merge%>%
  ggplot(aes(x=discipline_pct)) +
  geom_histogram(bins=20, color="black", fill="gray")+
  geom_density(alpha=.2, fill="gray") +
  geom_vline(aes(xintercept=mean(discipline_pct)),
             color="black", linetype="dashed", size=0.5) + 
  labs(x="Disicipline Rates (%)") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("./outputs/edu_descriptive_discipline.png",
       plot = last_plot(), bg = "#FFFFFF", dpi = 300)

ed_qual_merge%>%
  ggplot(aes(x=teacher_exp_pct)) +
  geom_histogram(bins=20, color="black", fill="gray")+
  geom_density(alpha=.2, fill="gray") +
  geom_vline(aes(xintercept=mean(teacher_exp_pct)),
             color="black", linetype="dashed", size=0.5) + 
  labs(x="Experienced Teachers (%)") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("./outputs/edu_descriptive_teacher_exp.png",
       plot = last_plot(), bg = "#FFFFFF", dpi = 300)

################################################################################
# STEP 5: Create Base Map 

# This analysis uses Census Block Groups due to the smallest Census unit available
# EHA has indicated they are interested in Census Tracts, but the analysis
# include Census Block Groups to provide granularity. Aggregation back to
# Census Tracts is easily adaptable from this code.
################################################################################

school_address <- read_csv("./data/wa_public_school_address.csv")
address_sf <- read_sf("./data/wa_public_school_address/Washington_State_Public_Schools.shp")

esd_school_address <- address_sf%>%
  filter(LEACode == "31002")%>%
  filter(AYPCode == "P")%>%
  filter(GradeCateg != "Other")%>%
  filter(SchoolCode != 5570)%>%
  mutate(SchoolCode = as.numeric(SchoolCode))

ed_qual_address <- full_join(ed_qual_merge, esd_school_address, 
                             by = c("SchoolCode" = "SchoolCode"), 
                             copy=TRUE) 

# ACS MAP

df_var<- load_variables(2019, "acs5", cache = TRUE)

census_geometry <- get_acs("block group",
                       variables = "B02001_003", #Any variable is fine here
                       year=2019,
                       state = "WA", #FIPS code 53
                       county = "Snohomish", #FIPS code 033
                       geometry = TRUE, 
                       cache_table = TRUE)

census_geometry_sf <- census_geometry%>%
  select(GEOID, geometry)%>%
  st_as_sf() 

wa <- places("WA", year = 2019, class = "sf")
sno_water <- area_water("WA", "Snohomish", class = "sf") 
sno_water_union <- st_union(sno_water) #This line may take a while to run

everett <- 
  wa %>%
  filter(NAME == "Everett")

ggplot(everett) + 
  geom_sf(fill = "transparent") + 
  theme_minimal() 

everett_block <- 
  census_geometry_sf %>%
  st_intersection(everett)%>%
  st_difference(sno_water_union) 

ggplot(everett_block) + 
  geom_sf(fill = "white") + 
  theme_minimal() 

everett_block <- everett_block%>%
  filter(GEOID != "530610522071")%>%
  filter(GEOID != "530610536023")%>%
  filter(GEOID != "530610538034")

everett_base <- ggplot(everett_block) + 
  geom_sf(fill = "white") + 
  theme_void() 

# The following lines export cleaned data and shapefiles to work with ArcGIS

# st_write(everett_block, "./outputs/acs_2019_block_group.shp")
# st_write(ed_qual_address, "./outputs/ed_qual.shp")

################################################################################
# STEP 6: Map School Indicators Over Base Map 
################################################################################

# Everett Public School Locations
################################################################################

everett_base+
  geom_point(data=ed_qual_address,
             size=3,
             shape=4,
             stroke = 1,
             aes(x = Longitude, 
                 y = Latitude, 
                 shape=factor(GradeCateg, levels = c("Elementary School", 
                                                     "Middle School", 
                                                     "High School")),
                 color=factor(GradeCateg, levels = c("Elementary School", 
                                                     "Middle School", 
                                                     "High School"))
             )
  ) +
  theme(legend.title=element_blank())+
  theme(legend.position = c(0.2, 0.8))

ggsave("./outputs/edu_eps_address.png",
       plot = last_plot(), bg = "#FFFFFF", dpi = 300)

# English Language Arts Proficiency 
################################################################################

ed_qual_address$ela_pct_fct<- 
  cut(ed_qual_address$ela_pct, 
      breaks = c(0.5, 0.5999, 0.69999, 0.7999, 0.89, 1),
      labels = c("50-59%", "60-69%", "70-79%", "80-89%", "90-100%"),
      include.lowest = TRUE)

everett_base +
  geom_point(data=ed_qual_address,
             size=3,
             aes(x = Longitude, 
                 y = Latitude,
                 color=factor(ela_pct_fct)
                 )
             )+
  labs(color="ELA Proficiency") +
  theme(legend.position = c(0.2, 0.8))

ggsave("./outputs/edu_eps_ela.png",
       plot = last_plot(), 
       bg = "#FFFFFF", 
       dpi = 300)
  
# Math Proficiency 
################################################################################

ed_qual_address$math_pct_fct<- 
  cut(ed_qual_address$math_pct, 
      breaks = c(0.3, 0.4999, 0.5999, 0.6999, 0.7999, 1),
      labels = c("30-49%", "50-59%", "60-69%", "70-79%", "80-100%"),
      include.lowest = TRUE)

everett_base +
  geom_point(data=ed_qual_address,
             size=3,
             aes(x = Longitude, 
                 y = Latitude,
                 color=factor(math_pct_fct)
             )
  ) +
  labs(color="Math Proficiency")+
  theme(legend.position = c(0.2, 0.8))

ggsave("./outputs/edu_eps_math.png",
       plot = last_plot(), 
       bg = "#FFFFFF", 
       dpi = 300)

# Discipline Rate 
################################################################################

ed_qual_address$discipline_pct_fct<- 
  cut(ed_qual_address$discipline_pct, 
      breaks = c(0.0, 0.04999, 0.074999, 0.0999, 0.15),
      labels = c("0.0 - 4.9%", 
                 "5.0 - 7.49%", 
                 "7.5 - 9.9%", 
                 "10 - 15%"),
      include.lowest = TRUE)

everett_base +
  geom_point(data=ed_qual_address,
             shape=15,
             size=3,
             aes(x = Longitude, 
                 y = Latitude,
                 color=factor(discipline_pct_fct),
             )
  ) +
  labs(color="Discipline Rates")+
  theme(legend.position = c(0.2, 0.8))

ggsave("./outputs/edu_eps_discipline.png",
       plot = last_plot(), 
       bg = "#FFFFFF", 
       dpi = 300)

# Teacher Experience 
################################################################################

ed_qual_address$teacher_exp_pct_fct<- 
  cut(ed_qual_address$teacher_exp_pct, 
      breaks = c(0.5, 0.5999, 0.6999, 0.7999, 0.8999, 1),
      labels = c("50 - 59%", "60 - 69%", "70 - 79%", "80 - 89%", "90 - 100%"),
      include.lowest = TRUE)

everett_base +
  geom_point(data=ed_qual_address,
             shape=17,
             size=3,
             aes(x = Longitude, 
                 y = Latitude,
                 color=factor(teacher_exp_pct_fct),
             )
  ) +
  labs(color="Experienced Teachers")+
  theme(legend.position = c(0.2, 0.8))

ggsave("./outputs/edu_eps_teacher_exp.png",
       plot = last_plot(), 
       bg = "#FFFFFF", 
       dpi = 300)

################################################################################
# STEP 7: Identify Census Unit of Interest to ESD School Address

# Identification Phase: Once EHA has identified their education opportunity
# indicators and created a summative measure to identify the schools of interest,
# the following code can be used to map school addresses to a specific census unit.

# Crucially, a school's location in a census block group or census tract does not
# indicate education opportunity uniformly across the entire census unit. School
# enrollment zone data will be needed to identify which residential addresses within
# a census tract or census block group falls within a school's enrollment zone.
################################################################################

census_info <-   cxy_geography(
  ed_qual_address$Longitude[1],
  ed_qual_address$Latitude[1],
  benchmark = "Public_AR_Current",
  vintage = "Census2020_Current" # ACS 2019 1-year estimates, aligns with demo maps
  # Census2020_Current: Has block groups and census tracts,
  # ACS2021_Current: ACS only has census tracts
)

census_info%>%
  select(Census.Tracts.BASENAME,
         Census.Tracts.GEOID,
         Census.Tracts.TRACT,
         Census.Blocks.BASENAME,
         Census.Blocks.GEOID,
         Census.Blocks.OBJECTID
  )%>%
  print()

for(n in nrow(esd_school_address)) {
  census_single <- cxy_geography(
    esd_school_address$Longitude[n],
    esd_school_address$Latitude[n],
    benchmark = "Public_AR_Current",
    vintage = "Current_Current"
  )
  census_info <- census_info%>%
    add_row(census_single)
}

