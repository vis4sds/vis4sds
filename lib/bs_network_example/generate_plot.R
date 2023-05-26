# Dependencies
library(here)
library(sf)
library(fst)
library(tidyverse)
library(lubridate)
# install.packages("ggforce")

##########  D A T A   /   S E T U P  ##########

# 1m sample of OD pairs.
sampled_data <- read_fst(here("lib", "bs_network_example", "data", "sampled_data.fst"))

# Sampled ODs : unique OD pairs derived from sampled_data. 
# Corrected and cleaned E/N data and with same OD removed.
sampled_ods <- read_fst(here("lib", "bs_network_example","data", "sampled_ods.fst"))

# River, parks and building outlines (from OSM).
rivers <- st_read(here("lib", "bs_network_example","data", "rivers.json")) %>%  st_transform(crs=27700)
parks <- st_read(here("lib", "bs_network_example","data", "parks.json")) %>%  st_transform(crs=27700)
buildings <- st_read(here("lib", "bs_network_example","data", "buildings.json")) %>%  st_transform(crs=27700)

stations <- read_csv(here("lib", "bs_network_example","data", "stations_cleaned.csv"))

# Helper function for generating asymmetric beziers.
source(here("lib", "bs_network_example","code", "bezier_path.R"))

# Set to ggplot theme_void for view composition.
theme_set(theme_void(base_family="Avenir Book"))

##########  D A T A   P L O T  ##########

# Build data frame of asymmetric trajectories (unique OD pair with controls). 
# Only needs to be calculated once.
od_trajectories_bezier <- sampled_ods %>% 
  nest(data=-row_id) %>% 
  mutate(
    trajectory=purrr::map(data,
                          ~get_trajectory(.x))
  ) %>% 
  select(-data) %>% unnest(trajectory)

# Filter and counts trajectories in am peak.
od_trajectories_am <- od_trajectories_bezier %>% 
  left_join(sampled_data %>% 
              mutate(od_pair=paste0(start_station_id,"-",end_station_id),
                     start_time=as.POSIXct(start_time, format="%Y-%m-%d %H:%M:%S"),
                     day=wday(start_time, label=TRUE),
                     is_wday=if_else(day %in% c("Sun","Sat"), 0,1),
                     hour=hour(start_time),
                     am_peak=if_else(hour %in% c(7,8,9,10), 1,0),
                     pm_peak=if_else(hour %in% c(16,17,18,19), 1,0)
              ) %>% 
              filter(is_wday==1, am_peak==1) %>%
              group_by(od_pair) %>%
              summarise(count=n())) 

# Find spatial extent of London bs area.
bbox <- st_bbox(buildings)
width <- unname(bbox$xmax)-unname(bbox$xmin) 
height <- unname(bbox$ymax)-unname(bbox$ymin) 
aspect <- width/height

# Define landmarks to annotate.
landmarks <- 
  tibble(
    name=c("Hyde Park", "Waterloo", "King's Cross", "Liverpool Street", "Regent's Park", "Olympic Park"),
    easting=c(527014,531000,530456, 533151, 528161, 537662),
    northing=c(180317,179916,183134, 181605, 182880, 184719)
  )


##########  P L O T  ##########

# Explore a weighting factor for penalizing less frequent OD pairs.
# This is controlled by f_od -- and removes the hairball.
# Linearity of weighting factor controlled with exponent (^): 
# >1 (increases penalty), <1 (decreases penalty)
od_trajectories_am <- od_trajectories_am %>% 
  filter(!is.na(count)) %>%
  mutate(f_od=((count/max(count))^0.7))

# Map.
plot_bezier_am_peak <- ggplot() +
  geom_sf(data=parks, fill="#EFF4E1",  colour="#EFF4E1")+
  geom_sf(data=rivers, fill="#E2EDF6",  colour="#E2EDF6", linewidth=2)+
  geom_point(data=stations, aes(x=x, y=y), size=0.1, colour="#2171b5", alpha=0.5)+
  ggforce::geom_bezier0(
    aes(x=x, y=y, group=od_pair, alpha=f_od, colour=f_od, size=f_od), 
    data=od_trajectories_am %>% filter(count>20, !is.na(x))) +
  coord_sf(crs=st_crs(parks), datum=NA) +
  geom_text(
    aes(x=easting, y=northing, label=name), 
    colour="#252525", alpha=0.6, size=3, show.legend=FALSE, family="Avenir Book", hjust=0.5, vjust=0.5, 
    data=landmarks)+
  scale_size_continuous(range=c(0.1,0.9))+
  guides(colour="none", alpha="none", size="none")+
  scale_colour_distiller(palette="Blues", direction=1)+
  theme(plot.title = element_text(size=18),
        plot.subtitle = element_text(size=12),
        plot.caption = element_text(size=10),
        plot.background = element_rect(fill="#ffffff", colour = "#737373"),
        axis.title.x = element_blank(), axis.title.y = element_blank())

# Context : temporal.
colours <- c("#c6dbef", "#2171b5")
plot_hod <-  sampled_data %>%
  mutate(
    start_time=as.POSIXct(start_time, format="%Y-%m-%d %H:%M:%S"),
    day=wday(start_time, label=TRUE),
    hour=hour(start_time),
    am_peak=if_else(hour %in% c(7,8,9,10), TRUE,FALSE),
    pm_peak=if_else(hour %in% c(16,17,18,19), TRUE,FALSE)) %>%
  filter(!is.na(day), day!="Sat", day!="Sun") %>% 
  group_by(hour) %>%
  summarise(total=n(), am_peak=first(am_peak), pm_peak=first(pm_peak)) %>%
  ggplot(aes(x=hour, y=total, group=1)) +
  geom_col(aes(fill=am_peak)) +
  labs(title="Journey counts by hour of day\nfilter : am peak")+ 
  scale_fill_manual(values=colours)+
  guides(fill="none")+
  theme(plot.title = element_text(size=8, hjust=1, family="Avenir Book"),
        plot.background = element_rect(fill="#ffffff", colour="#ffffff"),
        panel.grid = element_blank(), axis.text.y = element_blank(), axis.title.y=element_blank(),
        axis.title.x=element_blank(), axis.text.x = element_blank(), axis.line = element_blank())

# Legend : OD pair.
dat <- tibble(od_pair="demo", o_east=0, d_east=1, o_north=1,d_north=1)
dat <- get_trajectory(dat)
dat <- dat %>% mutate(row=row_number(),
                      type=if_else(row==1,"origin", if_else(row==2,"mid", "destination")))

legend <- ggplot() +
  geom_point(data=dat %>% filter(row!=2), aes(x=x, y=y), size=1, colour="#737373", alpha=0.5)+
  ggforce::geom_bezier0(data=dat, aes(x=x, y=y, group=od_pair), colour="#737373")+
  coord_equal()+
  geom_text(data=dat %>% filter(row!=2),
            aes(x=x, y=y-0.1, label=type), 
            colour="#252525", size=2.5, show.legend=FALSE, 
            family="Avenir Book", hjust="Middle", vjust="Top")+
  scale_x_continuous(limits=c(-0.1,1.3))+
  scale_y_continuous(limits=c(0.8,1.19))+
  theme(plot.background = element_rect(fill="#ffffff", colour="#ffffff"), axis.line = element_blank(), 
        panel.grid = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank())

# Compose views. 
plot <- plot_bezier_am_peak +
  annotation_custom(
    grob=ggplotGrob(plot_hod),
    xmin=unname(bbox$xmax-0.25*width),
    xmax=unname(bbox$xmax),
    ymin=unname(bbox$ymin),
    ymax=unname(bbox$ymin)+0.2*height
  ) +
  annotation_custom(
    grob=ggplotGrob(legend),
    xmin=unname(bbox$xmin-+0.05*width),
    xmax=unname(bbox$xmin+0.2*width),
    ymin=unname(bbox$ymax)-0.1*height,
    ymax=unname(bbox$ymax)
  )

# Aspect ratio (for saving).
aspect <- height/width

# Save output as a .png to your local directory : this takes some time to draw -- for this reason it is worth perhaps drawing pairs > stated freq.
ggsave(here("figs", "05", "flowline_bike.png"), plot = plot, width=10, height=10*aspect)

