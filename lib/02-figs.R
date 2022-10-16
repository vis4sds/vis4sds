###############################################################################
# Figures for vis4sds 
# Chapter 2
# Author: Roger Beecham
###############################################################################

library(tidyverse) 
library(here) 
library(patchwork)
library(sf) 
library(lubridate) 
library(fst)

###############################################################################
# T H E M E S
###############################################################################

site_colours <- list(
  primary = "#003c8f",
  primary_selected = "#1565c0",
  secondary = "#8e0000",
  secondary_selected = "#c62828"
)

update_geom_defaults("label", list(family = "Avenir Next"))
update_geom_defaults("text", list(family = "Avenir Next"))

theme_v_gds <- function(base_size = 11, base_family = "Avenir Next") {
  return <- theme_minimal(base_size, base_family) +
    theme(plot.title = element_text(size = rel(1.2),
                                    family = "Avenir Next Demi Bold"),
          plot.subtitle = element_text(size = rel(1.1),
                                       family = "Avenir Next Medium"),
          plot.caption = element_text(size = rel(.8), color = "grey50",
                                      family = "Avenir Next",
                                      margin = margin(t = 10)),
          plot.tag = element_text(size = rel(.9), color = "grey50",
                                  family = "Avenir Next"),
          strip.text = element_text(size = rel(.9),
                                    family = "Avenir Next"),
          strip.text.x = element_text(margin = margin(t = 1, b = 1)),
          panel.border = element_blank(),
          plot.background = element_rect(fill="#eeeeee", colour = NA),
          axis.ticks = element_blank(),
          panel.grid = element_line(colour="#e0e0e0"),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)),
          #legend.margin = margin(t = 0),
          legend.title = element_text(size = rel(0.8)),
          legend.position = "bottom")
  
  return
}


# Set ggplot2 theme
theme_set(theme_v_gds())



###############################################################################
# C H    2
###############################################################################

remotes::install_github("ropensci/bikedata") # API to TfL's trip data. Uncomment to run.
library(bikedata)
# install.packages(DBI) # Uncomment to install.
# install.packages("SQLite") # Uncomment to install.

# Create subdirectory in data folder for storing bike data.
if(!dir.exists(here("bikedata"))) dir.create(here("bikedata"))
# Download data for June 2020.
dl_bikedata (city = "citibike",  data_dir = here("bikedata"), dates=202006)
# Read in and store in SQLite3 database.
bikedb <- file.path (tempdir (), "bikedb.sqlite") # Create sqlite db container.
store_bikedata (data_dir = here("bikedata"), bikedb = bikedb)
# Index dbase to speed up working.
index_bikedata_db(bikedb = bikedb)
con <- DBI::dbConnect(RSQLite::SQLite(), bikedb)
# Check tables that form the dbase.
DBI::dbListTables(con)
# Collect trips and stations tables for writing out to fst.
trips <- tbl(con, "trips") |>  collect()
stations <- tbl(con, "stations") |>  collect()
# Write trips out to .fst.
write_fst(trips, here("bikedata", "ny_trips.fst"))
# Write stations out to .csv
write_csv(stations, here("bikedata", "ny_stations.csv"))
# Clean workspace
bike_rm_db(bikedb)
rm(db,stations, trips, bikedb)

# Read in these local copies of the trips and stations data.
ny_trips <- read_fst(here("bikedata", "ny_trips.fst"))
ny_stations <- read_csv(here("bikedata", "ny_stations.csv"))


# Recode
ny_trips <- ny_trips |> 
  select(-c(city, gender)) |> 
  mutate_at(vars(start_station_id, end_station_id), ~as.integer(str_remove(., "ny"))) |> 
  mutate_at(vars(start_time, stop_time), ~as.POSIXct(., format="%Y-%m-%d %H:%M:%S")) |> 
  mutate(
    bike_id=as.integer(bike_id),
    user_type=case_when(
      user_type == 0 ~ "customer",
      user_type == 1 ~ "subscriber")
  ) 

ny_stations <- ny_stations |> 
  select(-city) |> 
  mutate(stn_id=as.integer(str_remove(stn_id, "ny"))) |> 
  mutate_at(vars(longitude, latitude), ~as.double(.))


# Plot trips by hod doy and by gender
ny_temporal <- ny_trips |> 
  mutate(
    day=wday(start_time, label=TRUE),
    hour=hour(start_time)) |> 
  group_by(user_type, day, hour) |> 
  summarise(count=n()) |> 
  ungroup()

plot <-
  ny_temporal |> 
  ggplot(aes(x=hour, y=count, group=user_type)) +
  geom_line(aes(colour=user_type), size=1.1) +
  scale_colour_manual(values=c("#e31a1c", "#1f78b4")) +
  facet_wrap(~day, nrow=1)+
  labs(
    title="Citibike trip counts by hour of day, day of week and user type",
    subtitle="--Jun 2020",
    caption="Data provided and owned by: NYC Bike Share, LLC and Jersey City Bike Share, LLC",
    x="", y="trip counts"
  )+
  theme_v_gds()

ggsave(filename=here("figs", "02", "hod_dow.svg"), plot=plot, width=9, height=5)


od_pairs <- ny_trips |>  select(start_station_id, end_station_id) |> unique() |> 
  left_join(ny_stations |>  select(stn_id, longitude, latitude), by=c("start_station_id"="stn_id")) |> 
  rename(o_lon=longitude, o_lat=latitude) |> 
  left_join(ny_stations |>  select(stn_id, longitude, latitude), by=c("end_station_id"="stn_id")) |> 
  rename(d_lon=longitude, d_lat=latitude) |> 
  rowwise() |>
  mutate(dist=geosphere::distHaversine(c(o_lat, o_lon), c(d_lat, d_lon))/1000) |> 
  ungroup()


ny_trips <- ny_trips |>
  mutate(od_pair=paste0(start_station_id,"-",end_station_id)) |>
  left_join(od_pairs |>
              mutate(od_pair=paste0(start_station_id,"-",end_station_id)) |>
              select(od_pair, dist)
  )

# Plot distance travelled
plot <-
  ny_trips |> 
  mutate(user_type=factor(user_type, levels=c("subscriber", "customer"))) |> 
  ggplot(aes(dist)) +
  geom_histogram(fill=site_colours$primary) +
  facet_wrap(~user_type)+
  labs(
    title="Citibike trip distance (straight-line km)",
    subtitle="--Jun 2020",
    caption="Data provided and owned by: NYC Bike Share, LLC and Jersey City Bike Share, LLC",
    x="distance = km", y="frequency"
  )+
  theme_v_gds()

ggsave(filename=here("figs", "02", "dist.svg"), plot=plot, width=9, height=4)



ny_trips |> # Take the ny_trips data frame.
  group_by(user_type) |> # Group by user type.
  summarise( # Summarise over the grouped rows, generate a new variable for each type of summary.
    count=n(),
    avg_duration=mean(trip_duration/60),
    median_duration=median(trip_duration/60),
    sd_duration=sd(trip_duration/60),
    min_duration=min(trip_duration/60),
    max_duration=max(trip_duration/60),
  ) |>
  arrange(desc(count))


ny_trips |> 
  mutate(perc_rank=percent_rank(trip_duration/60)) |> filter(user_type=="customer") |> 
  filter(perc_rank > .95) |> 
  View()

# Utility trips
get_age <- function(dob, now) {
  period <- lubridate::as.period(lubridate::interval(dob, now),unit = "year")
  return(period$year)
}

ny_trips <- ny_trips |> 
  mutate(age=get_age(as.POSIXct(birth_year, format="%Y"), as.POSIXct("2020", format="%Y")))
ny_trips <- ny_trips |> 
  mutate(duration_minutes=as.numeric(as.duration(stop_time-start_time),"minutes"))


t <- ny_trips |> 
  mutate(day=wday(start_time, label=TRUE), is_weekday=as.numeric(!day %in% c("Sat", "Sun"))) |>
  filter(
    is_weekday==1,
    start_station_id!=end_station_id,
    duration_minutes<=60,
    between(age, 16, 74)) |> 
  mutate(
    dist_bands=case_when(
      dist < 1.5 ~ "<1.5km",
      dist < 3 ~ ">1.5-3km",
      dist < 4.5 ~ ">3-4.5km",
      TRUE ~ ">4.5km"),
    age_band=if_else(age %% 10 > 4, ceiling(age/5)*5, floor(age/5)*5),
    speed=dist/(duration_minutes/60)
  ) |> 
  group_by(user_type, age_band, dist_bands) |> 
  summarise(speed=mean(speed), n=n())

t <- ny_trips |> 
  mutate(day=wday(start_time, label=TRUE), is_weekday=as.numeric(!day %in% c("Sat", "Sun"))) |>
  filter(
    is_weekday==1,
    start_station_id!=end_station_id,
    duration_minutes<=60,
    between(age, 16, 74)) |> 
  mutate(
    dist_bands=case_when(
      dist < 1.5 ~ "<1.5km",
      dist < 3 ~ ">1.5-3km",
      dist < 4.5 ~ ">3-4.5km",
      TRUE ~ ">4.5km"),
    age_band=if_else(age %% 10 > 4, ceiling(age/5)*5, floor(age/5)*5),
    speed=dist/(duration_minutes/60)
  ) |> 
  select(user_type, age_band, dist_bands, speed) |> 
  nest(data=everything()) |>
  mutate(boots=map(data, rsample::bootstraps, times=100, apparent=TRUE)) 

# suppress warning
detach("package:tidyverse", unload = TRUE)
options(tidyverse.quiet = TRUE)
library(tidyverse)
# summarise info
options(dplyr.summarise.inform = FALSE)

get_summary <- function(df) {
  return(
    df |> group_by(user_type, age_band, dist_bands) |> 
      summarise(speed=mean(speed), sample_size=n(), std=sd(speed)) |> ungroup() 
  )
} 


t <- ny_trips |> 
  mutate(day=wday(start_time, label=TRUE), is_weekday=as.numeric(!day %in% c("Sat", "Sun"))) |>
  filter(
    is_weekday==1,
    start_station_id!=end_station_id,
    duration_minutes<=60,
    between(age, 16, 74)) |> 
  mutate(
    dist_bands=case_when(
      dist < 1.5 ~ "<1.5km",
      dist < 3 ~ ">1.5-3km",
      dist < 4.5 ~ ">3-4.5km",
      TRUE ~ ">4.5km"),
    age_band=if_else(age %% 10 > 4, ceiling(age/5)*5, floor(age/5)*5),
    speed=dist/(duration_minutes/60)
  ) |> 
  select(user_type, age_band, dist_bands, speed) |> ungroup() |> 
  nest(data=everything()) |>
  mutate(boots=map(data, rsample::bootstraps, times=100, apparent=TRUE)) 



u <- t |> 
  select(-data) |> 
  unnest(boots) |> 
  mutate(
    summary=map(splits, ~analysis(.) |> get_summary())
    ) |> 
  select(-splits) |> 
  unnest(cols=summary)

# v <- u |> 
#   group_by(user_type, dist_bands, age_band) |> 
#   mutate(
#     position=round(cume_dist(speed),2),
#     std_error=sd(speed)/sqrt(sample_size),
#     is_high=position==.95, is_low=position==.05,
#     is_raw=id=="Apparent", rank=round(cume_dist(speed),2) 
#     ) |> ungroup() |> 
#   filter(is_high | is_low | is_raw) |> 
#   mutate(range_type=case_when(
#     is_raw ~ "raw",
#     is_high ~ "high",
#     is_low ~ "low"
#     )
#  ) |> group_by(user_type, dist_bands, age_band, range_type) |> 
#   slice_sample(n=1) |> ungroup() |> 
#   pivot_wider(names_from=range_type, values_from=speed) |> 
#   group_by(user_type, dist_bands, age_band) |>
#   summarise(speed_raw=min(raw, na.rm=TRUE), speed_high=min(high, na.rm=TRUE), speed_low=min(low, na.rm=TRUE))
#   
  
v <- u |> 
  group_by(user_type, dist_bands, age_band) |> 
  mutate(
    std_error=sd(speed)
  ) |> 
  filter(id=="Apparent")


plot <- v |> 
  # manually recode low where no .95 position
  #mutate(speed_high=if_else(!is.finite(speed_high),7.13, speed_high)) |> 
  ggplot(aes(x=age_band, y=speed))+
  geom_line(aes(colour=user_type))+
  geom_ribbon(aes(ymin=speed-(2*std_error),
                  ymax=speed+(2*std_error), fill=user_type), alpha=.2) +
  scale_colour_manual(values=c("#e31a1c", "#1f78b4")) +
  scale_fill_manual(values=c("#e31a1c", "#1f78b4")) +
  facet_wrap(~dist_bands, nrow=1) +
  labs(
    title="Citibike average trip speeds (approximate) by age, customer type and trip distance",
    subtitle="--Jun 2020",
    caption="Data provided and owned by: NYC Bike Share, LLC and Jersey City Bike Share, LLC",
    x="age - 5 year bands", y="speed - km/h "
  )+
  theme_v_gds()

ggsave(filename="./static/class/02-class_files/speeds.png", plot=plot,width=9, height=5, dpi=300)

ggsave(filename=here("figs", "02", "speeds.svg"), plot=plot,width=9, height=5, dpi=300)


