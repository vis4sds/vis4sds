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
url <- "https://vis4sds.github.io/data/ch2/ny_stations.csv"
ny_stations <- read_csv(url)
url <- "https://vis4sds.github.io/data/ch2/ny_trips.fst"
# download.file(url, here("../","data", "ch2", "ny_trips2.fst"))
ny_trips <- read_fst(here("../","data", "ch2", "ny_trips.fst"))


# Recode
ny_trips <- ny_trips |> 
  select(-c(city, gender)) |> 
  mutate_at(vars(start_station_id, end_station_id), ~as.integer(str_remove(., "ny"))) |> 
  mutate_at(vars(start_time, stop_time), ~as.POSIXct(., format="%Y-%m-%d %H:%M:%S")) |> 
  mutate(
    bike_id=as.integer(bike_id),
    # user_type=case_when(
    #   user_type == 0 ~ "customer",
    #   user_type == 1 ~ "subscriber")
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
  geom_line(aes(colour=user_type), size=1) +
  scale_colour_manual(values=c("#e31a1c", "#1f78b4")) +
  facet_wrap(~day, nrow=1)+
  labs(
    #title="Citibike trip counts by hour of day, day of week and user type",
    #subtitle="--Jun 2020",
    #caption="Data provided and owned by: NYC Bike Share, LLC and Jersey City Bike Share, LLC",
    x="hour of day", y="trip counts"
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
  mutate(user_type=factor(user_type, levels=c("Subscriber", "Customer"))) |> 
  ggplot(aes(dist)) +
  geom_histogram(fill=site_colours$primary) +
  facet_wrap(~user_type)+
  labs(
    #title="Citibike trip distance (straight-line km)",
    #subtitle="--Jun 2020",
    #caption="Data provided and owned by: NYC Bike Share, LLC and Jersey City Bike Share, LLC",
    x="distance = km", y="frequency"
  )+
  theme_v_gds()

ggsave(filename=here("figs", "02", "dist.svg"), plot=plot, width=8, height=4)



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
      summarise(avg_speed=mean(speed), sample_size=n(), std=sd(speed), std_n=std/sqrt(sample_size)) |> ungroup() 
  )
} 


t <- ny_trips |> 
  mutate(day=wday(start_time, label=TRUE), is_weekday=as.numeric(!day %in% c("Sat", "Sun"))) |>
  filter(
    is_weekday==1,
    start_station_id!=end_station_id,
    duration_minutes<=60,
    between(age, 16, 74),
    dist>.5
    ) |> 
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
    percentile=percent_rank(avg_speed),
    std_error=mean(std_n)
  ) |> 
  ungroup() |> 
  filter(id=="Apparent" | percentile %in% c(.99, .01)) |> 
  select(-c(std, std_n)) |> 
  mutate(percentile=if_else(!percentile %in% c(.99, .01), .5, percentile)) |> 
  pivot_wider(names_from=percentile, values_from=avg_speed) |> 
  group_by(user_type, dist_bands, age_band) |> 
  mutate(
    `0.01`=max(`0.01`, na.rm=TRUE),
    `0.99`=max(`0.99`, na.rm=TRUE),
    `0.5`=max(`0.5`, na.rm=TRUE)
  ) |> 
  filter(id=="Apparent")
     


plot <- v |> ungroup() |> 
  # manually recode low where no .95 position
  #mutate(speed_high=if_else(!is.finite(speed_high),7.13, speed_high)) |> 
  mutate(low_sample = 
           case_when(
             user_type == "Customer" & 
               dist_bands == ">3-4.5km" & 
               age_band > 60 ~ TRUE,
             user_type == "Customer" & 
                 dist_bands == ">4.5km" & 
                 age_band > 45 ~ TRUE,
             TRUE ~ FALSE
             )
         ) |> 
  ggplot(aes(x=age_band, y=`0.5`))+
  geom_line(data=. %>% filter(user_type=="Customer", low_sample | 
              (age_band == 60 | (age_band == 45 &  dist_bands == ">4.5km"))), 
            aes(colour=user_type, group=user_type), linetype=2, alpha=.6) +
  geom_line(data=. %>% filter(!low_sample), 
            aes(colour=user_type, group=user_type), linetype=1) +
  geom_ribbon(data=. %>% filter(!low_sample), aes(ymin=`0.01`,
                  ymax=`0.99`, fill=user_type), alpha=.2) +
  scale_colour_manual(values=c("#e31a1c", "#1f78b4")) +
  scale_fill_manual(values=c("#e31a1c", "#1f78b4")) +
  facet_wrap(~dist_bands, nrow=1) +
  labs(
    #title="Citibike average trip speeds (approximate) by age, customer type and trip distance",
    #subtitle="--Jun 2020",
    #caption="Data provided and owned by: NYC Bike Share, LLC and Jersey City Bike Share, LLC",
    x="age - 5 year bands", y="speed - km/h "
  )+
  theme_v_gds()

ggsave(filename="./static/class/02-class_files/speeds.png", plot=plot,width=9, height=5, dpi=300)

ggsave(filename=here("figs", "02", "speeds.svg"), plot=plot,width=8, height=4.5, dpi=300)

url <- "https://vis4sds.github.io/data/ch2/ny_spread_rows.csv"
ny_spread_rows <- read_csv(url) 
url <- "https://vis4sds.github.io/data/ch2/ny_spread_columns.csv"
ny_spread_columns <- read_csv(url) 

ny_spread_rows |> 
  pivot_wider(names_from=summary_type, values_from=value) 

ny_spread_columns |> 
  pivot_longer(cols = count_weekend:duration_weekday) |> 
  separate(col = name, into = c("summary_type", "wkday"), sep = "_") |> 
  pivot_wider(names_from = summary_type, values_from = value)
