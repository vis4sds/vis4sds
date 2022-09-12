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
# S E S S I O N  2
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
trips <- tbl(con, "trips") %>%  collect()
stations <- tbl(con, "stations") %>%  collect()
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


od_pairs <- ny_trips |>  select(start_station_id, end_station_id) %>% unique() |> 
  left_join(ny_stations |>  select(stn_id, longitude, latitude), by=c("start_station_id"="stn_id")) |> 
  rename(o_lon=longitude, o_lat=latitude) |> 
  left_join(ny_stations |>  select(stn_id, longitude, latitude), by=c("end_station_id"="stn_id")) |> 
  rename(d_lon=longitude, d_lat=latitude) |> 
  rowwise() %>%
  mutate(dist=geosphere::distHaversine(c(o_lat, o_lon), c(d_lat, d_lon))/1000) |> 
  ungroup()


ny_trips <- ny_trips %>%
  mutate(od_pair=paste0(start_station_id,"-",end_station_id)) %>%
  left_join(od_pairs %>%
              mutate(od_pair=paste0(start_station_id,"-",end_station_id)) %>%
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
  mutate(day=wday(start_time, label=TRUE), is_weekday=as.numeric(!day %in% c("Sat", "Sun"))) %>%
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
  mutate(day=wday(start_time, label=TRUE), is_weekday=as.numeric(!day %in% c("Sat", "Sun"))) %>%
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
  mutate(day=wday(start_time, label=TRUE), is_weekday=as.numeric(!day %in% c("Sat", "Sun"))) %>%
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


###############################################################################
# S E S S I O N  3
###############################################################################

# install.packages("parlitools")
library(parlitools)

# Contituency boundaries -- simplified using mapshapr From -- https://geoportal.statistics.gov.uk/
constituency_boundaries <- st_read("./data/constituency_boundaries.geojson", crs=27700)

# dataset -- https://docs.evanodell.com/parlitools/articles/bes-2019.html
# reset_colours
# Reset colours to those used by Flourish :
# https://flourish.studio/2019/11/26/charts-for-the-uk-elections-2019/
# Author: Roger Beecham
###############################################################################

# Swing
bes_2019

data <- bes_2019 %>%
  filter(region != "Northern Ireland") %>%
  mutate(
    swing_con_lab=0.5*((con_19-con_17)-(lab_19-lab_17)),
    # Recode to 0 Chorley incoming speaker,Buckingham outgoing speaker --  uncontested seat.
    swing_con_lab=if_else(constituency_name %in% c("Chorley", "Buckingham"),0,swing_con_lab)
  )

# Con :
con <- "#0575c9"
# Lab :
lab <- "#ed1e0e"
# Other :
other <- "#bdbdbd"

# Lib dem :
lib_dem <- "#fe8300"
# SNP :
snp <- "#ebc31c"
# Greens :
greens <- "#78c31e"
# plaid
plaid <- "#4e9f2f"
# sinn fein
sinn_fein <- "#0a6240"
# dup
dup <- "#be1a40"
# Other :
other <- "#bdbdbd"


# con_1719 against  estimated constituency-level leave vote
elected_parties <- c(
  "Conservative",
  "Labour",
  "Other")

# Store as vector and recode elected variable as factor for use in scale_colour_manual.
t <- bes_2019 %>%
  mutate(
    elected=if_else(!winner_19 %in% elected_parties, "Other", winner_19),
    elected=factor(elected,
                   levels=c("Conservative","Labour", "Other"))
  )
colours <- c(con, lab, other)
names(colours) <- levels(t$elected)


plot <- bes_2019 %>%
  filter(region != "Northern Ireland", constituency_name != "Chorley") %>%
  ggplot(mapping=aes(x=leave_hanretty, y=con_1719)) +
  geom_point(alpha=.8) +
  labs(
    # title="Conservative gain in vote shares by Leave vote (estimated).",
    #  subtitle="-- Constituencies in Great Britain. Gain is diff in vote shares between 2017-2019 GE.",
    #  caption="Data: Published by House of Commons Library, accessed via parlitools package",
    x="% Leave", y="% gain in Conservative vote share"
  )+
  theme_v_gds()

plot2 <- t %>%
  filter(constituency_name != "Chorley", region != "Northern Ireland") %>%
  ggplot(mapping=aes(x=leave_hanretty, y=con_1719)) +
  geom_point(mapping=aes(colour=elected), alpha=.8)+
  scale_colour_manual(values=colours)+
  labs(
    #title="Conservative gain in vote shares by Leave vote (estimated)",
    #subtitle="-- Constituencies in Great Britain. Gain is diff in vote shares between 2017-2019 GE.",
    #caption="Data: Published by House of Commons Library, accessed via parlitools package",
    x="% Leave", y="% gain in Conservative vote share"
  )+
  guides(colour=guide_legend(title="Winning party"))+
  theme_v_gds()

# shape -- on flipped
t %>%
  filter(constituency_name != "Chorley", region != "Northern Ireland") %>%
  mutate(is_flipped=seat_change_1719=="Conservative gain from Labour",
         is_flipped=if_else(is.na(is_flipped), FALSE, is_flipped)) %>%
  ggplot(mapping=aes(x=leave_hanretty, y=con_1719)) +
  geom_point(mapping=aes(colour=elected, pch=is_flipped))+
  scale_colour_manual(values=colours)+
  labs(
    #title="Conservative gain in vote shares by Leave vote (estimated)",
    #subtitle="-- Constituencies in Great Britain. Gain is diff in vote shares between 2017-2019 GE.",
    #caption="Data: Published by House of Commons Library, accessed via parlitools package",
    x="% Leave", y="% gain in Conservative vote share"
  )+
  theme_v_gds()

t %>%
  filter(constituency_name != "Chorley", region != "Northern Ireland") %>%
  mutate(is_flipped=seat_change_1719=="Conservative gain from Labour",
         is_flipped=if_else(is.na(is_flipped), FALSE, is_flipped)) %>%
  ggplot(mapping=aes(x=leave_hanretty, y=con_1719)) +
  geom_point(mapping=aes(colour=elected, pch=is_flipped))+
  scale_colour_manual(values=colours)+
  labs(
    #title="Conservative gain in vote shares by Leave vote (estimated)",
    #subtitle="-- Constituencies in Great Britain. Gain is diff in vote shares between 2017-2019 GE.",
    #caption="Data: Published by House of Commons Library, accessed via parlitools package",
    x="% Leave", y="% gain in Conservative vote share"
  )+
  theme_v_gds()

plot3 <- t %>%
  filter(constituency_name != "Chorley", region != "Northern Ireland") %>%
  mutate(is_flipped=seat_change_1719=="Conservative gain from Labour",
         is_flipped=if_else(is.na(is_flipped), FALSE, is_flipped)) %>%
  ggplot(mapping=aes(x=leave_hanretty, y=con_1719)) +
  geom_point(mapping=aes(colour=elected, alpha=is_flipped))+
  geom_vline(xintercept=50, size=.2)+
  scale_colour_manual(values=colours)+
  scale_alpha_ordinal(range=c(.25,1))+
  guides(colour=FALSE,
         alpha=guide_legend(title="Flipped Lab-to-Con"))+
  labs(
    #title="Conservative gain in vote shares by Leave vote (estimated)",
    #subtitle="-- Constituencies in Great Britain. Gain is diff in vote shares between 2017-2019 GE.",
    #caption="Data: Published by House of Commons Library, accessed via parlitools package",
    x="% Leave", y="% gain in Conservative vote share"
  )+
  theme_v_gds()

library(patchwork)

t %>%
  filter(constituency_name != "Chorley", region != "Northern Ireland") %>%
  ggplot(data=.) +
  geom_point(
    mapping=
      aes(x=leave_hanretty, y=con_1719)
  )

plot_export <-plot + plot2 + plot3 +  plot_layout(nrow=3) +
  plot_annotation(title="Conservative gain in vote shares by Leave vote (estimated)",subtitle="-- Constituencies in Great Britain. Gain is diff in vote shares between 2017-2019 GE.",
                  caption="Data: Published by House of Commons Library, accessed via parlitools package", theme=theme_v_gds())

ggsave(filename="./static/class/03-class_files/gog-demo.png", plot=plot_export,width=6, height=12, dpi=300)

# Preattentive

temp <- tibble(
  x = rep(0:39,5),
  y = c(rep(3, 40),rep(6, 40),rep(9, 40),rep(12, 40), rep(15, 40)),
  numbers = sample(0:9,200, replace=TRUE, prob=c(0.11,0.11,0.11,0.025, 0.11, 0.11,0.11,0.11,0.11,0.11))
)

none <- temp %>%
  ggplot(aes(x, y))+
  geom_text(aes(label=numbers), colour="#636363", angle=0, size=8,hjust = 0.5, vjust=-0.5, family="Roboto Condensed Light")+
  scale_y_continuous(limits = c(1, 20))+
  theme_v_gds() +
  theme(axis.text=element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(filename="./static/class/03-class_files/no-encoding.png", plot=none,width=8, height=3.2, dpi=300)

temp <- tibble(
  x = rep(0:39,5),
  y = c(rep(3, 40),rep(6, 40),rep(9, 40),rep(12, 40), rep(15, 40)),
  numbers = sample(0:9,200, replace=TRUE, prob=c(0.11,0.11,0.11,0.025, 0.11, 0.11,0.11,0.11,0.11,0.11))
)

area <- temp %>%
  ggplot(aes(x, y))+
  #geom_point(colour="#2b8cbe", fill="#2b8cbe", pch=21) +
  geom_text(data=subset(temp, numbers!=3),
            aes(label=numbers), colour="#636363", angle=0, size=8,hjust = 0.5, vjust=-0.5, family="Roboto Condensed Light")+
  geom_text(data=subset(temp, numbers==3),
            aes(label=numbers), colour="#636363", angle=0, size=9.5,hjust = 0.5, vjust=-0.5, family="Roboto Condensed Light")+
  scale_y_continuous(limits = c(1, 20))+
  theme_v_gds() +
  theme(axis.text=element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(filename="./static/class/03-class_files/area-encoding.png", plot=area,width=8, height=3.2, dpi=300)

temp <- tibble(
  x = rep(0:39,5),
  y = c(rep(3, 40),rep(6, 40),rep(9, 40),rep(12, 40), rep(15, 40)),
  numbers = sample(0:9,200, replace=TRUE, prob=c(0.11,0.11,0.11,0.025, 0.11, 0.11,0.11,0.11,0.11,0.11))
)

angle <- temp %>%
  ggplot(aes(x, y))+
  #geom_point(colour="#2b8cbe", fill="#2b8cbe", pch=21) +
  geom_text(data=subset(temp, numbers!=3),
            aes(label=numbers), colour="#636363", angle=0, size=8,hjust = 0.5, vjust=-0.5, family="Roboto Condensed Light")+
  geom_text(data=subset(temp, numbers==3 & y %% 2==0),
            aes(label=numbers), colour="#636363", angle=-15, size=8,hjust = 0.5, vjust=-0.5, family="Roboto Condensed Light")+
  geom_text(data=subset(temp, numbers==3 & y %% 2!=0),
            aes(label=numbers), colour="#636363", angle=15, size=8,hjust = 0.5, vjust=-0.5, family="Roboto Condensed Light")+
  labs(x="x",y="y")+
  scale_y_continuous(limits = c(1, 20))+
  theme_v_gds() +
  theme(axis.text=element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(filename="./static/class/03-class_files/angle-encoding.png", plot=angle,width=8, height=3.2, dpi=300)

temp <- tibble(
  x = rep(0:39,5),
  y = c(rep(3, 40),rep(6, 40),rep(9, 40),rep(12, 40), rep(15, 40)),
  numbers = sample(0:9,200, replace=TRUE, prob=c(0.11,0.11,0.11,0.025, 0.11, 0.11,0.11,0.11,0.11,0.11))
)


hue <- temp %>%
  ggplot(aes(x, y))+
  geom_text(data=subset(temp, numbers!=3),
            aes(label=numbers), colour="#636363", angle=0, size=8,hjust = 0.5, vjust=-0.5, family="Roboto Condensed Light")+
  geom_text(data=subset(temp, numbers==3),
            aes(label=numbers), colour="#de2d26", angle=0, size=8,hjust = 0.5, vjust=-0.5, family="Roboto Condensed Light")+
  scale_y_continuous(limits = c(1, 20))+
  theme_v_gds() +
  theme(axis.text=element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(filename="./static/class/03-class_files/hue-encoding.png", plot=hue,width=8, height=3.2, dpi=300)

temp <- tibble(
  x = rep(0:39,5),
  y = c(rep(3, 40),rep(6, 40),rep(9, 40),rep(12, 40), rep(15, 40)),
  numbers = sample(0:9,200, replace=TRUE, prob=c(0.11,0.11,0.11,0.025, 0.11, 0.11,0.11,0.11,0.11,0.11))
)

temp_region <- temp %>% filter(!(y==3 & x>34))

temp_region <- temp_region %>%
  add_row(x=0,y=21,numbers=3) %>%
  add_row(x=1,y=21,numbers=3) %>%
  add_row(x=2,y=21,numbers=3) %>%
  add_row(x=3,y=21,numbers=3) %>%
  add_row(x=4,y=21,numbers=3)

spatial <- temp_region %>%
  ggplot(aes(x, y))+
  geom_text(data=subset(temp_region, numbers!=3),
            aes(label=numbers), colour="#636363", angle=0, size=8,hjust = 0.5, vjust=-0.5, family="Roboto Condensed Light")+
  geom_text(data=subset(temp_region, numbers==3),
            aes(label=numbers), colour="#636363", angle=0, size=8,hjust = 0.5, vjust=-0.5, family="Roboto Condensed Light")+
  labs(x="x",y="y")+
  scale_y_continuous(limits = c(1, 25))+
  theme_v_gds() +
  theme(axis.text=element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(filename="./static/class/03-class_files/spatial-encoding.png", plot=spatial,width=8, height=3.5, dpi=300)



hue <- temp %>%
  ggplot(aes(x, y))+
  geom_text(data=subset(temp, numbers!=3),
            aes(label=numbers), colour="#636363", angle=0, size=8,hjust = 0.5, vjust=-0.5, family="Roboto Condensed Light")+
  geom_text(data=subset(temp, numbers==3),
            aes(label=numbers), colour="#de2d26", angle=0, size=8,hjust = 0.5, vjust=-0.5, family="Roboto Condensed Light")+
  scale_y_continuous(limits = c(1, 20))+
  theme_v_gds() +
  theme(axis.text=element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(filename="./static/class/03-class_files/hue-encoding.png", plot=hue,width=8, height=3.2, dpi=300)


temp <- tibble(
  x = seq(0,5,.01),
  length = x,
  flannery = x^.87,
  stevens = x^.7
)

power_laws <- temp %>%
  pivot_longer(cols=c(length,stevens, flannery), names_to="stimulus", values_to="perception") %>%
  ggplot() +
  geom_path(aes(x=x,y=perception, group=stimulus), colour=site_colours$primary, size=0.8, alpha=.8) +
  scale_x_continuous(limits=c(-.3,6))+
  #scale_y_continuous(limits=c(-.5,5))+
  annotate("segment", x=0, xend=5, y=0, yend=0, arrow=arrow(ends="last", type="closed", length = unit(.15, "cm")), size=.35)+
  annotate("segment", x=0, xend=0, y=0, yend=5, arrow=arrow(ends="last", type="closed", length = unit(.15, "cm")), size=.35)+
  annotate("text", x=-.25, y=5, label="perceived size", hjust=1, angle=90)+
  annotate("text", x=5, y=-.3, label="graphical size", hjust=1)+
  annotate("text", x=5.1, y=max(temp$flannery)+.1, label="0.87 Flannery", hjust=0, size=3.4)+
  annotate("text", x=5.1, y=max(temp$flannery)-.2, label="circle area", hjust=0, size=3)+
  annotate("text", x=5.1, y=max(temp$x)+.1, label="1.0 Linear", hjust=0, size=3.4)+
  annotate("text", x=5.1, y=max(temp$x)-.2, label="line/bar length", hjust=0, size=3)+
  annotate("text", x=5.1, y=max(temp$stevens)+.1, label="0.7 Stevens", hjust=0, size=3.4)+
  annotate("text", x=5.1, y=max(temp$stevens)-.2, label="rectangle area", hjust=0, size=3)+
  theme_v_gds()+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(filename="./static/class/03-class_files/power-laws.png", plot=power_laws,width=5, height=3.5, dpi=300)


temp <- tibble(
  r=c(10,4.5),
  x=c(0,20),
  y=c(0,-5),
  area=pi*r^2,
  width=sqrt(area)
)

library(ggforce)
circles <- temp %>%
  ggplot()+
  geom_circle(aes(x0=x,y0=y, r=r), fill=site_colours$primary, colour=site_colours$primary, alpha=.7, size=1.2)+
  coord_equal()+
  theme_v_gds()+
  theme(axis.text = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(filename="./static/class/03-class_files/circles.png", plot=circles,width=5, height=3.5, dpi=300)

rectangles <- ggplot()+
  geom_rect(aes(xmin=0,ymin=0, xmax=17.7, ymax=17.7), fill=site_colours$primary, colour=site_colours$primary,size=1.2, alpha=.7)+
  geom_rect(aes(xmin=23,ymin=0, xmax=30.98, ymax=7.98), fill=site_colours$primary, colour=site_colours$primary, size=1.2, alpha=.7)+
  coord_equal() +
  theme_v_gds()+
  theme(axis.text = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(filename="./static/class/03-class_files/rectangles.png", plot=rectangles,width=5, height=3.5, dpi=300)

bars <- ggplot()+
  geom_rect(aes(xmin=0,ymin=0, xmax=4, ymax=31.4), fill=site_colours$primary, colour=site_colours$primary, alpha=.7)+
  geom_rect(aes(xmin=7,ymin=0, xmax=12, ymax=6.36), fill=site_colours$primary, colour=site_colours$primary, alpha=.7)+
  scale_x_continuous(limits=c(0,70))+
  coord_equal() +
  theme_v_gds()+
  theme(axis.text = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(filename="./static/class/03-class_files/bars.png", plot=bars,width=5, height=3.5, dpi=300)

bars | (circles / rectangles)  + plot_layout(widths=c(1,5))



# Number of constituencies won by party.
bes_2019 %>%
  group_by(winner_19) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

# Share of vote by party.
bes_2019 %>%
  select(constituency_name, total_vote_19, con_vote_19:alliance_vote_19) %>%
  pivot_longer(cols=con_vote_19:alliance_vote_19, names_to="party", values_to="votes") %>%
  mutate(party=str_extract(party, "[^_]+")) %>%
  group_by(party) %>%
  summarise(vote_share=sum(votes, na.rm=TRUE)/sum(total_vote_19)) %>%
  arrange(desc(vote_share))

data_gb <- bes_2019 %>%
  filter(region!="Northern Ireland")

data_gb %>%
  summarise(
    min_swing=min(swing_con_lab),
    max_swing=max(swing_con_lab),
    median_swing=median(swing_con_lab),
    num_swing=sum(swing_con_lab>0),
    num_landslide_con=sum(con_19>50, na.rm=TRUE),
    num_landslide_lab=sum(lab_19>50, na.rm=TRUE)
  )


hist_1 <- data_gb %>%
  ggplot() +
  geom_histogram(mapping=aes(swing_con_lab)) +
  labs() +
  theme_v_gds()

hist_2 <- data_gb %>%
  ggplot(mapping=aes(swing_con_lab)) +
  geom_histogram(fill="#003c8f") +
  labs(
    title="Butler two-party Labour-Conservative Swing for Constituencies in GB",
    subtitle="-- 2019 versus 2017 election",
    caption="Data published by House of Commons Library, accessed via `parlitools`",
    x="Swing", y="count"
  )+
  theme_v_gds()

hist_3 <- data_gb %>%
  ggplot(mapping=aes(swing_con_lab)) +
  geom_histogram(fill="#003c8f") +
  geom_vline(xintercept=4.44, size=.3)+
  labs(
    title="Butler two-party Labour-Conservative Swing for Constituencies in GB",
    subtitle="-- 2019 versus 2017 election",
    caption="Data published by House of Commons Library, accessed via `parlitools`",
    x="Swing", y="count"
  )+
  facet_wrap(~region)+
  theme_v_gds()

plot = (hist_1 + hist_2)
ggsave(filename="./static/class/03-class_files/hist.png", plot=plot,width=9, height=4, dpi=300)

ggsave(filename="./static/class/03-class_files/hist-region.png", plot=hist_3,width=8, height=5, dpi=300)




# Share of vote by party.
party_shares <- data_gb %>%
  select(constituency_name, region, total_vote_19, con_vote_19:alliance_vote_19) %>%
  pivot_longer(cols=con_vote_19:alliance_vote_19, names_to="party", values_to="votes") %>%
  mutate(party=str_extract(party, "[^_]+")) %>%
  group_by(party) %>%
  summarise(vote_share=sum(votes, na.rm=TRUE)/sum(total_vote_19)) %>%
  filter(vote_share>0) %>%
  ggplot(aes(x=reorder(party, -vote_share), y=vote_share)) +
  geom_col(fill="#003c8f") +
  labs(
    title="Vote share by party in GB",
    subtitle="-- 2019 UK General Election",
    caption="Data published by House of Commons Library, accessed via `parlitools`",
    x="party", y="vote share"
  )+
  theme_v_gds()


party_shares_rotate <- data_gb %>%
  select(constituency_name, region, total_vote_19, con_vote_19:alliance_vote_19) %>%
  pivot_longer(cols=con_vote_19:alliance_vote_19, names_to="party", values_to="votes") %>%
  mutate(party=str_extract(party, "[^_]+")) %>%
  group_by(party) %>%
  summarise(vote_share=sum(votes, na.rm=TRUE)/sum(total_vote_19)) %>%
  filter(vote_share>0) %>%
  ggplot(aes(x=reorder(party, vote_share), y=vote_share)) +
  geom_col(fill="#003c8f") +
  coord_flip() +
  labs(
    title="Vote share by party in GB",
    subtitle="-- 2019 UK General Election",
    caption="Data published by House of Commons Library, accessed via `parlitools`",
    x="party", y="vote share"
  )+
  theme_v_gds()

party_shares_colour <- temp_party_shares %>%
  ggplot(aes(x=reorder(party, vote_share), y=vote_share)) +
  geom_col(aes(fill=party)) +
  coord_flip() +
  labs(
    title="Vote share by party in GB",
    subtitle="-- 2019 UK General Election",
    caption="Data published by House of Commons Library, accessed via `parlitools`",
    x="party", y="vote share"
  )+
  scale_fill_manual(values=party_colours)+
  theme_v_gds()


temp_party_shares <- data_gb %>%
  select(constituency_name, region, total_vote_19, con_vote_19:alliance_vote_19) %>%
  pivot_longer(cols=con_vote_19:alliance_vote_19, names_to="party", values_to="votes") %>%
  mutate(party=str_extract(party, "[^_]+")) %>%
  group_by(party) %>%
  summarise(vote_share=sum(votes, na.rm=TRUE)/sum(total_vote_19)) %>%
  filter(vote_share>0) %>%
  mutate(party=factor(party, levels=c("con", "lab", "ld", "snp", "green", "brexit", "pc")))

plot = party_shares + party_shares_rotate
ggsave(filename="./static/class/03-class_files/bars.png", plot=plot,width=9, height=4, dpi=300)

temp_party_shares_region <- data_gb %>%
  select(constituency_name, region, total_vote_19, con_vote_19:alliance_vote_19) %>%
  pivot_longer(cols=con_vote_19:alliance_vote_19, names_to="party", values_to="votes") %>%
  mutate(party=str_extract(party, "[^_]+")) %>%
  group_by(party, region) %>%
  summarise(vote_share=sum(votes, na.rm=TRUE)/sum(total_vote_19)) %>%
  filter(party %in% c("con", "lab", "ld", "snp", "green", "brexit", "pc")) %>%
  mutate(party=factor(party, levels=c("con", "lab", "ld", "snp", "green", "brexit", "pc")))

# Con :
con <- "#0575c9"
# Lab :
lab <- "#ed1e0e"
# Lib dem :
ld <- "#fe8300"
# SNP :
snp <- "#ebc31c"
# Greens :
green <- "#78c31e"
# Plaid
pc <- "#4e9f2f"
# Brexit
brexit <- "#25b6ce"
# Other : recode brexit to other as they didn't win a seat.
other <- "#bdbdbd"

party_colours <- c(con, lab, ld, snp, green, brexit, pc)
names(party_colours) <- levels(temp_party_shares$party)

party_shares_region <- temp_party_shares_region %>%
  ggplot(aes(x=reorder(party, vote_share), y=vote_share)) +
  geom_col(aes(fill=party)) +
  scale_fill_manual(values=party_colours) +
  coord_flip() +
  labs(
    title="Vote share by party in GB, grouped by Region",
    subtitle="-- 2019 UK General Election",
    caption="Data published by House of Commons Library, accessed via `parlitools`",
    x="party", y="vote share"
  )+
  facet_wrap(~region) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_v_gds()

ggsave(filename="./static/class/03-class_files/bars-region.png", plot=party_shares_region,width=10, height=6, dpi=300)


plot_scatters <- data_gb %>%
  mutate(winner_19=case_when(
    winner_19 == "Conservative" ~ "Conservative",
    winner_19 == "Labour" ~ "Labour",
    TRUE ~ "Other"
  )) %>%
  ggplot(aes(x=con_17, y=con_19)) +
  geom_point(aes(colour=winner_19), alpha=.8) +
  geom_abline(intercept = 0, slope = 1) +
  scale_colour_manual(values=c(con,lab,other)) +
  #facet_wrap(~region) +
  theme_v_gds()


plot_scatters_con <- data_gb %>%
  mutate(is_flipped=seat_change_1719=="Conservative gain from Labour",
         is_flipped=if_else(is.na(is_flipped), FALSE, is_flipped),
         winner_19=case_when(
           winner_19 == "Conservative" ~ "Conservative",
           winner_19 == "Labour" ~ "Labour",
           TRUE ~ "Other"
         )) %>%
  ggplot(aes(x=con_17, y=con_19)) +
  geom_point(aes(colour=winner_19, alpha=is_flipped, shape=is_flipped)) +
  geom_abline(intercept = 0, slope = 1, size=.3) +
  scale_colour_manual(values=c(con,lab,other)) +
  scale_alpha_ordinal(range=c(.5,1)) +
  scale_x_continuous(limits=c(0,90)) +
  annotate("text", x=40, y=90, label="Conservative", hjust=0.5, size=4.5) +
  annotate("text", x=20, y=60, label="vote share > than 2019", hjust=0.5, size=3.5) +
  annotate("text", x=80, y=20, label="vote share < than 2019", hjust=0.5, size=3.5) +
  theme_v_gds() +
  labs(x="vote share 2017 ", y="vote share 2019")

plot_scatters_lab <- data_gb %>%
  mutate(is_flipped=seat_change_1719=="Conservative gain from Labour",
         is_flipped=if_else(is.na(is_flipped), FALSE, is_flipped),
         winner_19=case_when(
           winner_19 == "Conservative" ~ "Conservative",
           winner_19 == "Labour" ~ "Labour",
           TRUE ~ "Other"
         )) %>%
  ggplot(aes(x=lab_17, y=lab_19)) +
  geom_point(aes(colour=winner_19, alpha=is_flipped, shape=is_flipped)) +
  geom_abline(intercept=0, slope=1, size=.3) +
  annotate("text", x=40, y=90, label="Labour", hjust=0.5, size=4.5) +
  annotate("text", x=20, y=60, label="vote share > than 2019", hjust=0.5, size=3.5) +
  annotate("text", x=80, y=20, label="vote share < than 2019", hjust=0.5, size=3.5) +
  scale_colour_manual(values=c(con,lab,other)) +
  scale_alpha_ordinal(range=c(.5,1)) +
  scale_x_continuous(limits=c(0,90)) +
  guides(fill=FALSE, alpha=FALSE, shape=FALSE, colour=FALSE) +
  theme_v_gds() +
  labs(x="vote share 2017 ", y="vote share 2019")


+
  labs(
    title="Conservative vote share in 2019 against Conservative vote share in 2017",
    subtitle="-- Constituencies in Great Britain.",
    caption="Data published by House of Commons Library, accessed via `parlitools`",
    x="Conservative vote share 2017 ", y="Conservative vote share 2019"
  )

plot <- plot_scatters_con / plot_scatters_lab + plot_annotation(
  title="Conservative vote share in 2019 against vote share in 2017",
  subtitle="-- Constituencies in Great Britain.",
  caption="Data published by House of Commons Library, accessed via `parlitools`",
  theme = theme_v_gds())


ggsave(filename="./static/class/03-class_files/scatters-con.png", plot=plot,width=9, height=10, dpi=300)



plot <- data_gb %>%
  mutate(is_flipped=seat_change_1719=="Conservative gain from Labour",
         is_flipped=if_else(is.na(is_flipped), FALSE, is_flipped),
         winner_19=case_when(
           winner_19 == "Conservative" ~ "Conservative",
           winner_19 == "Labour" ~ "Labour",
           TRUE ~ "Other"
         )) %>%
  ggplot(aes(x=lab_17, y=lab_19)) +
  geom_point(aes(colour=winner_19, alpha=is_flipped, shape=is_flipped)) +
  geom_abline(intercept=0, slope=1, size=.3) +
  #annotate("text", x=40, y=90, label="Labour", hjust=0.5, size=4.5) +
  annotate("text", x=20, y=60, label="vote share > than 2019", hjust=0.5, size=3.5) +
  annotate("text", x=80, y=20, label="vote share < than 2019", hjust=0.5, size=3.5) +
  scale_colour_manual(values=c(con,lab,other)) +
  scale_alpha_ordinal(range=c(.5,1)) +
  scale_x_continuous(limits=c(0,90)) +
  guides(fill=FALSE, alpha=FALSE, shape=FALSE, colour=FALSE) +
  theme_v_gds() +
  labs(x="vote share 2017 ", y="vote share 2019", title="Labour vote share in 2019 against Labour vote share in 2017",
       subtitle="-- Constituencies in Great Britain.",
       caption="Data published by House of Commons Library, accessed via `parlitools`")

ggsave(filename="./static/class/03-class_files/scatters-lab.png", plot=plot,width=9, height=5, dpi=300)

plot_scatters_region <- data_gb %>%
  mutate(is_flipped=seat_change_1719=="Conservative gain from Labour",
         is_flipped=if_else(is.na(is_flipped), FALSE, is_flipped),
         winner_19=case_when(
           winner_19 == "Conservative" ~ "Conservative",
           winner_19 == "Labour" ~ "Labour",
           TRUE ~ "Other"
         )) %>%
  ggplot(aes(x=con_17, y=con_19)) +
  geom_point(aes(colour=winner_19, fill=winner_19, alpha=is_flipped, shape=is_flipped)) +
  geom_abline(intercept=0, slope=1, size=.2) +
  scale_colour_manual(values=c(con,lab,other)) +
  scale_fill_manual(values=c(con,lab,other)) +
  scale_alpha_ordinal(range=c(.5,1)) +
  facet_wrap(~region) +
  theme_v_gds() +
  labs(
    title="Conservative vote share in 2019 against Conservative vote share in 2017, grouped by Region",
    subtitle="-- Constituencies in Great Britain.",
    caption="Data published by House of Commons Library, accessed via `parlitools`",
    x="Conservative vote share 2017 ", y="Conservative vote share 2019"
  )

plot <- plot_scatters_emph

ggsave(filename="./static/class/03-class_files/scatters-region.png", plot=plot_scatters_region,width=11, height=7, dpi=300)

data_gb %>%
  mutate(is_flipped=seat_change_1719=="Conservative gain from Labour",
         is_flipped=if_else(is.na(is_flipped), FALSE, is_flipped),
         winner_19=case_when(
           winner_19 == "Conservative" ~ "Conservative",
           winner_19 == "Labour" ~ "Labour",
           TRUE ~ "Other"
         )) %>%
  ggplot(aes(x=con_17, y=con_19)) +
  geom_point(aes(alpha=is_flipped, colour=winner_19)) +
  geom_abline(intercept = 0, slope = 1) +
  scale_alpha_ordinal(range=c(.25,1)) +
  scale_colour_manual(values=c(con,lab,other)) +
  #facet_wrap(~region) +
  theme_v_gds() +
  labs(
    title="Conservative vote share in 2019 against Conservative vote share in 2017, grouped by Region",
    subtitle="-- Constituencies in Great Britain.",
    caption="Data published by House of Commons Library, accessed via `parlitools`",
    x="Labour vote share 2017 ", y="Labour vote share 2019"
  )


ggsave(filename="./static/class/03-class_files/bars-region.png", plot=party_shares_region,width=12, height=8, dpi=300)


url <- "https://www.roger-beecham.com/datasets/cons_outline.geojson"
url <- "/Users/roger/Downloads/constituencies_outline/Westminster_Parliamentary_Constituencies_(December_2019)_Boundaries_UK_BUC.shp"
cons_outline <- st_read(url, crs=27700)

data_gb <- cons_outline %>%
  inner_join(data_gb, by=c("pcon19cd"="ons_const_id"))

class(data_gb)

data_gb <- data_gb %>%
  mutate(
    winner_19=if_else(winner_19=="Speaker", "Other", winner_19),
    winner_19=as_factor(winner_19))


party_colours <- c(con, lab, ld, green, other, snp, pc)
names(party_colours) <- levels(data_gb$winner_19)


map_1 <- data_gb %>%
  ggplot() +
  geom_sf(aes(fill=winner_19), colour="#eeeeee", size=0.01)+
  coord_sf(crs=27700, datum=NA) +
  guides(fill=FALSE)+
  theme_v_gds() +
  scale_fill_manual(values=party_colours)


map_2 <- data_gb %>%
  ggplot() +
  geom_sf(aes(fill=winner_19), colour="#eeeeee", size=0.01)+
  geom_sf(data=. %>% group_by(region) %>% summarise(), colour="#eeeeee", fill="transparent", size=0.08)+
  coord_sf(crs=27700, datum=NA) +
  theme_v_gds() +
  theme(legend.position = "right") +
  scale_fill_manual(values=party_colours)


plot <- map_1 + map_2 + plot_annotation(
  title="Winning parties for in 2019 General Election",
  subtitle="-- Constituencies in Great Britain.",
  caption="Data published by House of Commons Library, accessed via `parlitools`",
  theme = theme_v_gds())

ggsave(filename="./static/class/03-class_files/map-winners.png", plot=plot,width=8.6, height=7, dpi=300)



max_shift <- max(abs(data_gb$swing_con_lab))
min_shift <- -max_shift
# Calculate bounding boxes for use in annotation_custom().
london_bbox <- st_bbox(data_gb %>% filter(region=="London"))
london_width <- unname(london_bbox$xmax)-unname(london_bbox$xmin)
london_height <- unname(london_bbox$ymax)-unname(london_bbox$ymin)
london_aspect <- london_width/london_height
uk_bbox <- st_bbox(data_gb)
uk_width <- unname(uk_bbox$xmax)-unname(uk_bbox$xmin)
uk_height <- unname(uk_bbox$ymax)-unname(uk_bbox$ymin)

# Annotate constituencies that *really* defied expectation (discussed in the EPA paper).
bassetlaw <- data_gb %>% filter(constituency_name == "Bassetlaw")
redcar <-  data_gb %>% filter(constituency_name == "Redcar")
sedgefield <- data_gb %>% filter(constituency_name == "Sedgefield")
stoke <- data_gb %>% filter(constituency_name == "Stoke-On-Trent Central")

# Convert degrees to radians.
get_radians <- function(degrees) {
  (degrees * pi) / (180)
}
# Rescaling function.
map_scale <- function(value, min1, max1, min2, max2) {
  return  (min2+(max2-min2)*((value-min1)/(max1-min1)))
}
# Position subclass for centred geom_spoke as per --
# https://stackoverflow.com/questions/55474143/how-to-center-geom-spoke-around-their-origin
position_center_spoke <- function() PositionCenterSpoke
PositionCenterSpoke <- ggplot2::ggproto('PositionCenterSpoke', ggplot2::Position,
                                        compute_panel = function(self, data, params, scales) {
                                          data$x <- 2*data$x - data$xend
                                          data$y <- 2*data$y - data$yend
                                          data$radius <- 2*data$radius
                                          data
                                        }
)


party_colours <- c(con, lab, other)
names(party_colours) <- c("Conservative", "Labour", "Other")

gb <- data_gb %>%
  filter(region!="London") %>%
  mutate(is_flipped=seat_change_1719 %in% c("Conservative gain from Labour","Labour gain from Conservative"),
         is_flipped=if_else(is.na(is_flipped), FALSE, is_flipped),
         elected=if_else(!winner_19 %in% c("Conservative", "Labour"), "Other", as.character(winner_19))
  ) %>%
  ggplot()+
  geom_sf(aes(fill=elected), colour="#636363", alpha=0.2, size=0.01)+
  geom_sf(data=. %>% group_by(region) %>% summarise(), colour="#eeeeee", fill="transparent", size=0.08)+
  coord_sf(crs=27700, datum=NA,
           xlim = c(unname(uk_bbox$xmin), unname(uk_bbox$xmax)+6*london_width),
           ylim = c(unname(uk_bbox$ymin), unname(uk_bbox$ymax)-0.22*uk_height)
  )+
  geom_spoke(
    aes(x=bng_e, y=bng_n, angle=get_radians(map_scale(swing_con_lab,min_shift,max_shift,135,45)), colour=elected, size=is_flipped),
    radius=7000, position="center_spoke", lineend="round"
  )+
  scale_size_ordinal(range=c(.3,.9))+
  scale_colour_manual(values=party_colours)+
  scale_fill_manual(values=party_colours)+
  annotate(geom="segment", xend=bassetlaw$bng_e, yend=bassetlaw$bng_n, x=bassetlaw$bng_e+0.15*uk_width, y=bassetlaw$bng_n, size=.2)+
  annotate(geom="text", x=bassetlaw$bng_e+0.16*uk_width, y=bassetlaw$bng_n, hjust="left", label=paste0(bassetlaw$constituency_name), family="Roboto Condensed", size=3.5)+
  annotate(geom="segment", xend=sedgefield$bng_e, yend=sedgefield$bng_n, x=sedgefield$bng_e+0.06*uk_width, y=sedgefield$bng_n+0.02*uk_height, size=.2)+
  annotate(geom="text", x=sedgefield$bng_e+0.07*uk_width, y=sedgefield$bng_n+0.02*uk_height, hjust="left", label=paste0(sedgefield$constituency_name), family="Roboto Condensed", size=3.5)+
  annotate(geom="segment", xend=redcar$bng_e, yend=redcar$bng_n, x=redcar$bng_e+0.05*uk_width, y=redcar$bng_n, size=.2)+
  annotate(geom="text", x=redcar$bng_e+0.06*uk_width, y=redcar$bng_n, hjust="left", label=paste0(redcar$constituency_name), family="Roboto Condensed", size=3.5)+
  annotate(geom="segment", xend=stoke$bng_e, yend=stoke$bng_n, x=stoke$bng_e-.15*uk_width, y=stoke$bng_n+0.05*uk_height, size=.2)+
  annotate(geom="text", x=stoke$bng_e-0.16*uk_width, y=stoke$bng_n+0.05*uk_height, hjust="right", label=paste0(stoke$constituency_name), family="Roboto Condensed", size=3.5)+
  guides(colour=FALSE, fill=FALSE, size=FALSE)+
  theme_v_gds() +
  theme(axis.title.x= element_blank(), axis.title.y= element_blank())

data_gb %>%
  mutate(
    is_flipped=seat_change_1719 %in% c("Conservative gain from Labour","Labour gain from Conservative"),
    elected=if_else(!winner_19 %in% c("Conservative", "Labour"), "Other", as.character(winner_19))
  ) %>%
  ggplot()+
  geom_sf(aes(fill=elected), colour="#636363", alpha=0.2, size=0.01)+
  geom_spoke(
    aes(x=bng_e, y=bng_n, angle=get_radians(map_scale(swing_con_lab,min_shift,max_shift,135,45)), colour=elected, size=is_flipped),
    radius=7000, position="center_spoke", lineend="round"
  )+
  coord_sf(crs=27700, datum=NA)+
  scale_size_ordinal(range=c(.3,.9))+
  scale_colour_manual(values=party_colours)+
  scale_fill_manual(values=party_colours)+
  theme_v_gds()

london <- data_gb %>%
  filter(region=="London") %>%
  mutate(is_flipped=seat_change_1719 %in% c("Conservative gain from Labour","Labour gain from Conservative"),
         is_flipped=if_else(is.na(is_flipped), FALSE, is_flipped),
         elected=if_else(!winner_19 %in% c("Conservative", "Labour"), "Other", as.character(winner_19))
  ) %>%
  ggplot()+
  geom_sf(aes(fill=elected), colour="#636363", alpha=0.2, size=0.01)+
  coord_sf(datum=NA)+
  geom_spoke(
    aes(x=bng_e, y=bng_n, angle=get_radians(map_scale(swing_con_lab,min_shift,max_shift,135,45)), colour=elected, size=is_flipped),
    radius=7000/5, position="center_spoke", lineend="round"
  )+
  scale_size_ordinal(range=c(.3,.9))+
  scale_colour_manual(values=party_colours)+
  scale_fill_manual(values=party_colours)+
  guides(colour=FALSE, fill=FALSE, size=FALSE)+
  theme_v_gds() +
  theme(axis.title.x= element_blank(), axis.title.y= element_blank())



# Use of angle to encode swing.
swing <-  ggplot()+
  geom_spoke(aes(x=0, y=-.35,angle=get_radians(90)),radius=0.55, size=0.2, colour="#636363", lineend="round")+
  geom_spoke(aes(x=0, y=-.35,angle=get_radians(135)),radius=0.55, size=0.2,colour="#636363", linetype = "dashed", lineend="round")+
  geom_spoke(aes(x=0, y=-.35,angle=get_radians(45)),radius=0.55,size=0.2,colour="#636363",linetype = "dashed", lineend="round")+
  geom_text(aes(label="+18% to \n Con",x=.5, y=0), angle=45,hjust="right", family="Roboto Condensed", size=3, colour="#636363")+
  geom_text(aes(label="+18% to \n Lab",x=-.5, y=0), angle=315,hjust="left", family="Roboto Condensed", size=3, colour="#636363")+
  geom_curve(aes(x=-.04, y=.2, xend=-.3, yend=.08), size=0.3, curvature = 0.2, arrow=arrow(type="closed", length = unit(.03, "inches")), colour="#636363")+
  geom_curve(aes(x=.04, y=.2, xend=.3, yend=.08), size=0.3, curvature = -0.2, arrow=arrow(type="closed", length = unit(.03, "inches")), colour="#636363")+
  xlim(-0.5,0.5)+
  ylim(-0.35,0.35)+
  coord_equal() +
  theme_void()
theme_v_gds() +
  theme(axis.title.x= element_blank(), axis.title.y= element_blank(), axis.text = element_blank())
# Use colour to encode party.
temp_dat <-tibble(
  elected=names(party_colours),
  y=c(3,2,1),
  x=c(1,1,1)
)

# Use thickness to show flips.
line <-  ggplot()+
  geom_spoke(aes(x=-0.2, y=-.35,angle=get_radians(90)),radius=0.55, size=0.2, lineend="round")+
  geom_spoke(aes(x=0.2, y=-.35,angle=get_radians(90)),radius=0.55, size=0.8, lineend="round")+
  xlim(-0.5,0.5)+
  ylim(-0.35,0.35)+
  theme_void()


theme_v_gds() +
  theme(axis.title.x= element_blank(), axis.title.y= element_blank(), axis.text = element_blank())

# Party colours for legend
party <- temp_dat %>%
  ggplot()+
  geom_spoke(aes(x=x, y=y,angle=get_radians(90), colour=elected),radius=0.7, size=1, lineend="round")+
  scale_colour_manual(values=party_colours)+
  geom_text(aes(label=elected,x=x+0.03, y=y+0.2),hjust="left",vjust="middle", family="Roboto Condensed", size=4, colour="#636363")+
  guides(colour=FALSE)+
  xlim(1,5)+
  ylim(-4,4)+
  theme_void()
theme_v_gds() +
  theme(axis.title.x= element_blank(), axis.title.y= element_blank(), axis.text = element_blank())


# Use annotation_custom to organise grobs in legend.
legend <- ggplot()+
  geom_text(aes(label="Each constituency is a line -- Butler Con-Lab swing",x=0, y=6), hjust="left", vjust="top", family="Roboto Condensed", size=5)+
  geom_text(aes(label="Colour hue -- winning party",x=0, y=5), hjust="left", vjust="top", family="Roboto Condensed", size=4)+
  geom_text(aes(label="Thick line -- \n constituency flipped \n winning party \n from 2017",x=4.5, y=5), hjust="left", vjust="top", family="Roboto Condensed", size=4)+
  geom_text(aes(label="Line angle -- \n Butler % swing \n in vote share \n from 2017 -- \n Con-Lab",x=4.5, y=2.5), hjust="left", vjust="top", family="Roboto Condensed", size=4)+
  annotation_custom(grob=ggplotGrob(swing),xmin=7,xmax=10,ymin=0,ymax=2.5)+
  annotation_custom(ggplotGrob(line),xmin=7,xmax=10,ymin=4.2,ymax=3.3)+
  annotation_custom(ggplotGrob(party),xmin=0,xmax=6,ymin=0,ymax=4.9)+
  xlim(0,10)+
  ylim(0,6.25) +
  theme_void() +
  theme(plot.background = element_rect(fill="#eeeeee", colour = NA))

# Assemble with annotation_custom.
map <- gb +
  annotation_custom(
    grob=ggplotGrob(london),
    xmin=unname(uk_bbox$xmax +1*london_width),
    xmax=unname(uk_bbox$xmax) + 6*london_width,
    ymin=unname(uk_bbox$ymin) +1*london_height,
    ymax=unname(uk_bbox$ymin) + 6*london_height
  )

ggsave(filename="./static/class/03-class_files/spoke-legend.png", plot=legend,width=8, height=4, dpi=300)

ggsave(filename="./static/class/03-class_files/spoke-map.png", plot=map,width=11, height=7, dpi=300)



# Map Swing

plot <- data_gb %>%
  mutate(margin_leave=leave_hanretty-50) %>%
  ggplot() +
  geom_sf(aes(fill=margin_leave), colour="#eeeeee", size=0.01)+
  geom_sf(data=. %>% group_by(region) %>% summarise(), colour="#636363", fill="transparent", size=0.04)+
  geom_sf(data=. %>% summarise(), colour="#636363", fill="transparent", size=0.08)+
  coord_sf(crs=27700, datum=NA) +
  guides(fill=guide_legend(title="Majority Leave:Remain")) +
  theme_v_gds() +
  theme(legend.position = "right") +
  scale_fill_distiller(palette="BrBG", limits=c(-30,30), labels=c("Heavy Remain", "", "", "No majority", "","",  "Heavy Leave")) +
  labs(
    title="Vote margin for Leave:Remain in Great Britain",
    subtitle="-- Estimated by Constituency via Hanretty 2017.",
    caption="Data published by House of Commons Library, accessed via `parlitools`"
  )

ggsave(filename="./static/class/03-class_files/referendum-map.png", plot=plot,width=8.5, height=7, dpi=300)
