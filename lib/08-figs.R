# Filename: 08-figs.R 
#
# Figures for Chapter 8 of vis4sds 
# 
# Author: Roger Beecham
#
#
#-----------------------------------------
# Contents
#-----------------------------------------
# 
# 1. Packages and data
# 2. Concepts graphics
# 3. Techniques graphics
#
#-----------------------------------------

# lisa-charlotte rost : https://github.com/datawrapper/snippets/blob/master/2020-03-coronavirus-charts/coronavirus.R
# data : https://joachim-gassen.github.io/tidycovid19/index.html
# coronavirus tracker example : https://robjhyndman.com/hyndsight/logratios-covid19/
# uk data vaccines etc. : https://twitter.com/Benj_Barr 

#-----------------------------------------
# 1. Packages and Data
#-----------------------------------------

# 1.1 Packages ---------------------------
# remotes::install_github("kjhealy/covdata@main")
library(covdata)
library(tidyverse)
library(sf)
library(lubridate)
library(RcppRoll)

county_data <- nytcovcounty %>% 
  # New York City recode to geoid : 36061
  mutate(
    fips=case_when(
      county == "New York City" ~ as.character(36061),
      TRUE ~ fips
    )
  ) %>% 
  # We focus up to end of May 2020 (as per Washington Post graphic).
  filter(date< "2020-05-26", county!="Unknown") %>% 
  group_by(fips) %>% 
  mutate(
    cases_cum=cumsum(cases),
    # 7-day rolling cases (though we don't use this).
    cases_mov_avg_local=roll_mean(cases,7,align="right", fill=0),
    # Find cases on May 3.
    cases_start=if_else(date=="2020-05-03", cases,0)
  ) %>% ungroup %>% 
  filter(date> "2020-05-02") %>% 
  group_by(fips) %>% 
  # Remove those where <20 cases on May 3. 
  mutate(remove=max(cases_start)) %>% ungroup %>% 
  filter(remove>19) %>% 
  mutate(
    growth_rate=cases/remove,
    end_rate=if_else(date=="2020-05-25", growth_rate,0),
    end_cases=if_else(date=="2020-05-25", cases,0)
  ) %>% 
  group_by(fips) %>% 
  mutate(
    end_rate=max(end_rate),
    end_cases=max(end_cases),
    binned_growth_rate=
      case_when(
        max(end_rate) > 7 ~ 4,
        max(end_rate) > 4 ~ 3,
        max(end_rate) > 2 ~ 2,
        TRUE ~ 1
      ),
    day_num=row_number()
  ) %>% ungroup() 

county_boundaries <- st_read(here("../", "data", "ch8", "county_boundaries", "cb_2018_us_county_20m.shp"))
st_write(county_boundaries, here("../", "data", "ch8", "county_boundaries.geojson"))
county_boundaries <- st_read(here("../", "data", "ch8", "county_boundaries.geojson"))                             
state_boundaries <- st_read(here("../", "data", "ch8", "state_boundaries.geojson")) 

# Load in county population data - for calculating relative case rates.
# From: https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html#par_textimage_70769902
county_pop <- 
  read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv") %>% 
  select(STATE, COUNTY, POPESTIMATE2019) %>% 
  transmute(GEOID=paste0(STATE,COUNTY), pop=POPESTIMATE2019) 

# Join state data to counties.
county_boundaries <- county_boundaries %>% 
  inner_join(state_boundaries %>% st_drop_geometry() %>%  select(STATEFP, NAME_STATE=NAME))

# Filter only on mainland states 
state_boundaries <- state_boundaries %>% filter(!NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))
# Use Albers Equal Area.
state_boundaries <- st_transform(state_boundaries, crs=5070)
st_crs(state_boundaries)
county_boundaries <- county_boundaries  %>% filter(!NAME_STATE %in% c("Alaska", "Hawaii", "Puerto Rico"))
# Use Albers Equal Area.
county_boundaries <- st_transform(county_boundaries, crs=5070)

# Calculate centroids of counties and states.
county_centroids <- county_boundaries %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  rename("x"="X", "y"="Y") %>% 
  add_column(GEOID = county_boundaries %>% pull(GEOID))
county_boundaries <- county_boundaries %>% left_join(county_centroids)   
state_centroids <- state_boundaries %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  rename("x"="X", "y"="Y") %>% 
  add_column(GEOID = state_boundaries %>% pull(GEOID))
state_boundaries <- state_boundaries %>% left_join(state_centroids) 

# Fix geog exceptions.
# Detailed in Healey's pages for covdata package: https://kjhealy.github.io/covdata/articles/new-york-times.html
# Merging NYC counties -- New York, Kings, Queens, Bronx and Richmond -- into New York.
county_pop <- county_pop %>% 
  inner_join(county_boundaries %>% st_drop_geometry() %>% select(GEOID, NAME, NAME_STATE)) %>% 
  mutate(
    GEOID=case_when(
      NAME_STATE=="New York" & NAME %in% c("New York", "Kings", "Queens", "Bronx", "Richmond") ~ "36061",
      TRUE ~ GEOID
    )
  ) %>% 
  group_by(GEOID) %>% 
  summarise(pop=sum(pop)) %>% ungroup 

# Join county population.
county_boundaries <- county_boundaries %>% inner_join(county_pop)

write_


#-----------------------------------------
# 1. Techniques grahics
#-----------------------------------------

# 3.1 Packages ---------------------------
