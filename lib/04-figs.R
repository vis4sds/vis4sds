# Filename: 04-figs.R 
#
# Figures for Chapter 4 of vis4sds 
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
#-----------------------------------------


#-----------------------------------------
# 1. Packages and Data
#-----------------------------------------

# 1.1 Packages ---------------------------
library(tidyverse) 
library(here) 
library(patchwork)
library(ggtext)

library(sf)
library(distributional)
library(ggdist)
library(ggmosaic)
library(stats19)
library(trafficalmr)
library(lubridate)
library(fst)
library(gridmappr)
library(magick)

# 1.2 Data -----------------------------
ped_veh <- 
  read_fst(here("../", "data", "ch4", "ped_veh.fst"))



#-----------------------------------------
# 2. Concept graphics
#-----------------------------------------


# 2.1 Plot distributions -----------------

plot_data <- ped_veh |> 
  filter(!is.na(age_of_casualty), age_of_casualty>=0) |> sample_n(10000)

dots <- plot_data |> 
  ggplot(aes(age_of_casualty, y="1")) +
  geom_jitter(colour=site_colours$primary, fill=site_colours$primary, alpha=.05) +
  scale_x_continuous(limits=c(0,100)) +
  labs(x="casualty age", subtitle = "strip plot") +
  coord_flip() +
  theme(
    axis.title.x = element_blank(),  
    axis.text.x= element_blank(),   
    panel.grid.major.y=element_blank(), 
    panel.grid.minor = element_blank(),
    plot.subtitle = element_text(family="Avenir Book")
    ) 

histogram <- plot_data |>   
  ggplot(aes(age_of_casualty)) +
  geom_histogram(colour=site_colours$primary, fill=site_colours$primary, alpha=.2, size=.3) +
  scale_x_continuous(limits=c(0,100)) +
  coord_flip() +
  labs(subtitle = "histogram") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),  
    axis.text= element_blank(),   
    panel.grid.major.y=element_blank(), 
    panel.grid.minor = element_blank(),
    plot.subtitle = element_text(family="Avenir Book")
    ) 

get_mode <- function(d) {
  uniqd <- unique(d)
  uniqd[which.max(tabulate(match(d, uniqd)))]
}
get_mode(plot_data$age_of_casualty)
 
density <- plot_data |> 
  ggplot(aes(age_of_casualty)) +
  geom_density(colour=site_colours$primary, fill=site_colours$primary, alpha=.2, size=.3) +
  scale_x_continuous(limits=c(0,100)) +
  annotate("text", label="mode : 11 years",
           vjust="centre", hjust="left", family="Avenir Book",size=3.6,
           x=10, y=.001)+
  annotate("segment", x=13, xend=13, y=0.001, yend=.021, colour=site_colours$primary, size=.15) +
  coord_flip() +
  labs(subtitle = "density plot") +
  theme(
    plot.subtitle = element_text(family="Avenir Book"), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),  
    axis.text= element_blank(),   
    panel.grid.major.y=element_blank(), 
    panel.grid.minor = element_blank()
    ) 

box_plot <- plot_data |>  
  ggplot(aes(age_of_casualty)) +
  geom_boxplot(colour=site_colours$primary, fill=site_colours$primary, alpha=.2, size=.3, width=.5) +
  scale_x_continuous(limits=c(0,100)) +
  scale_y_discrete(c("1","2")) +
  annotate("text", label="median :\n 30 years",
           vjust="centre", hjust="left", family="Avenir Book",size=3.6,
           x=30, y=-1.2)+
  coord_flip() +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(),axis.text.x = element_blank(),   panel.grid.major.y=element_blank(), panel.grid.minor = element_blank()) + 
  labs(subtitle = "boxplot") +
  theme(
    plot.subtitle = element_text(family="Avenir Book"), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),  
    axis.text= element_blank(),   
    panel.grid.major.y=element_blank(), 
    panel.grid.minor = element_blank()
  ) 

plot <-  
  dots + density + histogram + box_plot +  plot_layout(widths=c(1.2, 2,2.2, 2.2), nrow=1) 

ggsave(here("figs", "04", "univariate-plots.png"), plot=plot,width=8, height=4.5, dpi=300)

# 2.2 Compare distributions -----------------

plot_data <- ped_veh |> 
  filter(!is.na(age_of_casualty), age_of_casualty>=0) 

order_type <- plot_data |> 
  group_by(vehicle_type) |>
  summarise(median=median(age_of_casualty)) |> arrange(median) |> pull(vehicle_type)

plot2_density <- plot_data |> mutate(day=lubridate::wday(datetime, label=TRUE)) |>
  mutate(
    vehicle_type=factor(vehicle_type, levels=order_type)
  ) |> 
  select(age_of_casualty, age_of_driver, vehicle_type) |> 
  rename(age_of_pedestrian=age_of_casualty) |> 
  pivot_longer(cols=-vehicle_type, names_to="individual_type", values_to="age") |> 
  filter(age>0) |> 
  ggplot(aes(x=age, y=vehicle_type)) +
  
  stat_halfeye(
    aes(fill=individual_type, colour=individual_type),
    slab_alpha=.35, point_alpha=1, .width=0, trim=TRUE, shape='', size=6, scale=1.3
  ) +
  
  scale_fill_manual(values=c("#e31a1c", "#1f78b4"))+
  scale_colour_manual(values=c("#e31a1c", "#1f78b4"))+
  scale_x_continuous(limits=c(0,100)) +
  guides(colour="none", fill="none")+
  labs(y="vehicle type", x="casualty age")+
  theme(axis.title.y = element_blank(), axis.text.y= element_blank(),   panel.grid.major.y=element_blank(), panel.grid.minor = element_blank())


plot2_density_annotations <- ggplot() +
  annotate("rect", xmin=.5, xmax=15, ymin=8.4, ymax=8.8, fill="#1f78b4", colour="#1f78b4", alpha=.5, size=.15)+
  annotate("rect", xmin=.5, xmax=15, ymin=7.9, ymax=8.3, fill="#e31a1c", colour="#e31a1c", alpha=.5, size=.15)+
  annotate("segment", x=0, xend=40, y=3, yend=3, size=.15) +
  annotate("text", label="car, taxi and bus drivers\noften older than pedestrians\nthey crash into and injur",
           vjust="centre", hjust="left", family="Avenir Book",size=3.6,
           x=0, y=2)+
  annotate("text", label="pedestrian age",
           vjust="centre", hjust="left", family="Avenir Book",size=3.6,
           x=16.5, y=8.6)+
  annotate("text", label="driver age",
           vjust="centre", hjust="left", family="Avenir Book",size=3.6,
           x=16.5, y=8.1)+
  scale_x_continuous(limits=c(0,100)) +
  scale_y_continuous(limits=c(0,9)) +
  theme_void()
  
  
plot2_box <- plot_data |> mutate(day=lubridate::wday(datetime, label=TRUE)) |>
  mutate(
    vehicle_type=factor(vehicle_type, levels=order_type)
  ) |> 
  select(age_of_casualty, age_of_driver, vehicle_type) |> 
  rename(pedestrian=age_of_casualty, driver=age_of_driver) |> 
  pivot_longer(cols=-vehicle_type, names_to="individual_type", values_to="age") |> 
  filter(age>0) |> 
  ggplot(aes(x=age, y=vehicle_type)) +  
  geom_boxplot(aes(fill=individual_type, colour=individual_type), alpha=.2, size=.3) +
  scale_fill_manual(values=c("#e31a1c", "#1f78b4"), guide="none")+
  scale_colour_manual(values=c("#e31a1c", "#1f78b4"), guide="none")+
  scale_x_continuous(limits=c(0,100)) +
  labs(y="vehicle type", x="age casualty age")

plot <- plot2_box + plot2_density + plot2_density_annotations

ggsave(filename=here("figs", "04", "boxplot-by-class.png"), plot=plot,width=9, height=4.2, dpi=300)


# 2.3 Plot bars -----------------

bar1 <- plot_data |> 
  sample_n(50000) |> 
  group_by(vehicle_type) |> 
  summarise(count=n()) |> 
  ggplot() +
  geom_col(aes(x=vehicle_type, y=count), fill=site_colours$primary)+
  scale_y_continuous(
    breaks=c(0:4*10000), 
    labels = scales::comma_format(scale = .001)) +
  labs(x="vehicle type", y="crash count in thousands")+
  theme(panel.grid.major.x=element_blank(), panel.grid.minor = element_blank())

bar2 <- plot_data |>
  sample_n(50000) |> 
  group_by(vehicle_type) |> 
  summarise(count=n()) |> 
  ggplot() +
  geom_col(aes(x=vehicle_type, y=count), fill=site_colours$primary)+
  scale_y_continuous(
    breaks=c(0:4*10000), 
    labels = scales::comma_format(scale = .001)) +
  labs(x="vehicle type", y="crash count in thousands")+
  theme(panel.grid.major.y=element_blank(), panel.grid.minor = element_blank()) +
  coord_flip()

bar3 <- plot_data |> 
  sample_n(50000) |> 
  group_by(vehicle_type) |> 
  summarise(count=n()) |> 
  ggplot() +
  geom_col(aes(x=reorder(vehicle_type, count), y=count), fill=site_colours$primary)+
  scale_y_continuous(
    breaks=c(0:4*10000), 
    labels = scales::comma_format(scale = .001)) +
  labs(x="vehicle type", y="crash count in thousands")+
  theme(panel.grid.major.y=element_blank(), panel.grid.minor = element_blank()) +
  coord_flip()

plot <- bar1 + bar2 +  bar3 
ggsave(filename=here("figs", "04", "bars.png"), plot=plot,width=11, height=3.5, dpi=300)


# 2.4 Plot borough freqs -----------------

borough_counts <- ped_veh |> filter(police_force == "Metropolitan Police" | police_force == "City of London", local_authority_district!="London Airport (Heathrow)") |> 
  mutate(is_inner=if_else(local_authority_district %in% c("Camden",
                                                          "Greenwich",
                                                          "Hackney",
                                                          "Hammersmith and Fulham",
                                                          "Islington",
                                                          "Kensington and Chelsea",
                                                          "Lambeth",
                                                          "Lewisham",
                                                          "Southwark",
                                                          "Tower Hamlets",
                                                          "Wandsworth",
                                                          "Westminster", "City of London"), "inner", "outer")) |> 
  group_by(local_authority_district) |> 
  summarise(count=n(), is_inner=first(is_inner)) |> 
  ggplot(aes(x=count,y=reorder(local_authority_district, count)))+
  geom_segment(aes(x=0, y=reorder(local_authority_district, count), xend=count, yend=reorder(local_authority_district, count)), colour="#525252", linewidth=.2)+
  geom_point(colour=site_colours$primary, fill=site_colours$primary, shape=21)+
  theme(
    panel.grid.major.y=element_blank(), 
    strip.text = element_blank(), 
    #axis.text.x = element_text(angle=270, hjust=0, size=8), 
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    panel.spacing.x = unit(-.1, "lines")
    )+
  facet_grid(is_inner~., scales="free_y", space="free_y")+
  labs(x="", y="") 


borough_counts_period <- 
  ped_veh |>
  filter(police_force == "Metropolitan Police" | police_force == "City of London", local_authority_district!="London Airport (Heathrow)") |>
  mutate(
    year=lubridate::year(datetime),
    day=lubridate::wday(datetime,label=TRUE),
    hod=lubridate::hour(datetime),
    is_night=if_else((hod < 6 | hod > 20), "night", "day"),
    is_weekend=if_else(day %in% c("Sat", "Sun"), "weekend", "weekday"),
    is_inner=if_else(local_authority_district %in% c("Camden",
                                                     "Greenwich",
                                                     "Hackney",
                                                     "Hammersmith and Fulham",
                                                     "Islington",
                                                     "Kensington and Chelsea",
                                                     "Lambeth",
                                                     "Lewisham",
                                                     "Southwark",
                                                     "Tower Hamlets",
                                                     "Wandsworth",
                                                     "Westminster", "City of London"), "inner", "outer")
  ) |>
  filter(!is.na(hod)) |>
  group_by(local_authority_district) |>
  mutate(borough_count=n()) |> ungroup() |>
  group_by(local_authority_district, is_weekend) |>
  mutate(weekend_count=n()) |> ungroup() |>
  group_by(local_authority_district, is_weekend) |>
  summarise(is_inner=first(is_inner), weekend_count=first(weekend_count), borough_count=first(borough_count)) |> ungroup() |>
  pivot_wider(names_from=c(is_weekend), values_from=c("weekend_count")) |>
  mutate(prop_weekend=weekend/borough_count, exp_weekend=(sum(weekend)/sum(borough_count))*borough_count) |>
  pivot_longer(c(weekday, weekend), names_to="period", values_to="count") |>
  filter(period %in% c("weekday", "weekend")) |>
  ungroup() |> 

  mutate(
    period_type=if_else(period %in% c("day", "night"),
                        "<span style = 'color: #252525;'>night </span> -- <span style = 'color: #2171b5;'> day </span>",
                        "<span style = 'color: #08306b;'>weekend </span> -- <span style = 'color: #6baed6;'> weekday </span>"),
  ) |>
  
  
  ggplot(
    aes(x=count,y=reorder(local_authority_district, borough_count))
  ) +
  geom_segment(data=. %>% group_by(local_authority_district, period_type) %>%
                 summarise(min_x=min(count), max_x=max(count), borough_count=first(borough_count),
                           is_inner=first(is_inner)),
               aes(x=min_x, y=reorder(local_authority_district, borough_count), xend=max_x,
                   yend=reorder(local_authority_district, borough_count)), colour="#252525", linewidth=.2)+
  
  geom_point(aes(fill=period), shape=21,colour="#252525") +
  
  facet_grid(is_inner~period_type, scales="free_y", space="free_y") +
  scale_fill_manual(values=c("#6baed6", "#08306b", "#6baed6",  "#08306b")) +
  guides(fill="none")+
  #labs(y="", x="pedestrain crash counts in thousands") +
  scale_x_continuous(
    breaks=c(0:4*1000), 
    labels = scales::comma_format(scale = .001)) +
  theme(
    axis.text.x = element_blank(),
    legend.position = "right", panel.grid.major.y=element_blank(),
    strip.text.x = ggtext::element_markdown(),
    axis.title.x = element_blank(), axis.title.y = element_blank()
  )

borough_counts_period <- borough_counts_period +
  geom_text(data = . %>% filter(local_authority_district == "Hackney", period == "weekday"),
          aes(x=count+600, y=local_authority_district),
          label=str_wrap("28% of Hackney's pedestrian crashes are on weekends", 18),
          size=3.5, hjust="left") +
  
  geom_text(data = . %>% filter(local_authority_district == "Bromley", period == "weekday"),
            aes(x=count+550, y=local_authority_district),
            label=str_wrap("18%-20% weekends for Hilingdon, Havering, Bromley", 18),
            size=3.5, hjust="left") 
  
  
vehicle_order <- ped_veh |>
  group_by(vehicle_type) |> 
  summarise(count=n()) |> arrange(desc(count)) |> pull(vehicle_type)

borough_counts_vehicle <- ped_veh |> filter(police_force == "Metropolitan Police" | police_force == "City of London", local_authority_district!="London Airport (Heathrow)") |> 
  mutate(
    is_inner=if_else(local_authority_district %in% c("Camden",
                                                     "Greenwich",
                                                     "Hackney",
                                                     "Hammersmith and Fulham",
                                                     "Islington",
                                                     "Kensington and Chelsea",
                                                     "Lambeth",
                                                     "Lewisham",
                                                     "Southwark",
                                                     "Tower Hamlets",
                                                     "Wandsworth",
                                                     "Westminster", "City of London"), "inner", "outer"),
    day=lubridate::wday(datetime,label=TRUE),
    is_weekend=factor(
      if_else(day %in% c("Sat", "Sun"), "weekend", "weekday"),
      levels=c("weekend", "weekday")
      )
  ) |> 
  # filter(is_weekend) |> 
  group_by(local_authority_district) |> 
  mutate(borough_count=n()) |> ungroup() |> 
  group_by(local_authority_district, vehicle_type, is_weekend) |> 
  summarise(count=n(), borough_count=first(borough_count), prop=count/borough_count, is_inner=first(is_inner)) |> ungroup() |> 
  mutate(vehicle_type=factor(vehicle_type, levels=vehicle_order)) |> 
  ggplot(aes(x=vehicle_type,y=reorder(local_authority_district, borough_count), fill=count)) +
  geom_tile(colour="#707070", size=.2) +
  facet_grid(is_inner~is_weekend, scales="free_y", space="free_y") +
  scale_fill_distiller(palette="Blues", direction=1) +
  guides(fill="none")+
  labs(y="") +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle=270, hjust=0, size=8), panel.spacing.x = unit(.5, "lines"),
    legend.position = "right", panel.grid.major.y=element_blank(), strip.text.y=element_blank(), axis.title.x = element_blank(), axis.line = element_blank(),
  )

annotate_text <- "Dominance of cars makes patterns in other vehicle types difficult to see. \n\n 
Inner London boroughs, especially Westminster, contain crashes involving taxis, buses, bikes. \n\n
But this is quite imperceptible when mapping raw frequencies to colour value."

borough_counts_vehicle_annotate <- ggplot() +
  annotate("text", 0,1, label=
             str_wrap(annotate_text, 15),
           hjust="left", vjust="top", size=3.5) + 
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(limits=c(0,1)) +
  theme_void()

# plot <-  borough_counts + borough_counts_period + borough_counts_vehicle + 
#   plot_layout(widths = c(1,1, 1.4)) 
# 
# plot <-  borough_counts + borough_counts_period + borough_counts_vehicle + 
#   plot_layout(widths = c(1,1, 1.4)) 

plot <- borough_counts_period + borough_counts_vehicle + borough_counts_vehicle_annotate +
  plot_layout(widths = c(1, 1, .8)) 


ggsave(filename=here("figs", "04", "borough-freqs.png"), plot=plot,width=11, height=6.5, dpi=300)






# 2.5 Plot mosaics etc. -----------------

# Data for plots
vehicle_order <- ped_veh |>
  group_by(vehicle_type) |> 
  summarise(count=n()) |> arrange(desc(count)) |> pull(vehicle_type)

vehicle_period_cross <- #vehicle_pedestrians |>
  ped_veh |> 
  mutate(
    day=lubridate::wday(datetime,label=TRUE),
    is_weekend=factor(if_else(day %in% c("Sat", "Sun"), "weekend", "weekday"),
                      levels=c("weekend", "weekday"))
  ) |> 
  group_by(vehicle_type, is_weekend) |> 
  summarise(count=n()) |> ungroup() |> 
  mutate(`Vehicle type`=factor(vehicle_type, levels=vehicle_order)) |>  arrange(`Vehicle type`) |> 
  pivot_wider(names_from =  is_weekend, values_from = count) |> select(-vehicle_type) |> rowwise() |> 
  mutate(`Row Total` = sum(weekday, weekend)) |> 
  ungroup()

col_totals <- vehicle_period_cross |> summarise_at(vars(weekday:weekend), sum)

vehicle_period_cross <- vehicle_period_cross |> 
  add_row(`Vehicle type`= "Column Total", 
          weekday=col_totals |> pull(weekday), 
          weekend=col_totals |> pull(weekend))

write_csv(vehicle_period_cross, here::here("files","csv","vehicle_period_cross.csv"))
vehicle_period_cross <- read_csv( here::here("files","csv","vehicle_period_cross.csv"))


bar_freq <- vehicle_period_cross |> select(-`Row Total`) |> 
  pivot_longer(cols=c(weekday:weekend), names_to="period", values_to="count") |> 
  filter(`Vehicle type`!="Column Total") |> 
  mutate(
    vehicle_type=factor(`Vehicle type`, levels=vehicle_order),
    period=factor(period, levels=c("weekday", "weekend"))) |>
  ggplot(aes(x=count, y=fct_rev(vehicle_type))) +
  geom_col(aes(fill=period))+
  scale_fill_manual(values=c("#c6dbef", "#08519c"))+
  scale_x_continuous(
    breaks=c(0:2*100000), 
    labels = scales::comma_format(scale = .001)) +
  labs(y="", x="crash count in thousands")+
  guides(fill=FALSE)+
  labs(subtitle="stacked bar") +
  theme(
    plot.subtitle = element_text(family="Avenir Book", size=14),
    panel.grid.major.y=element_blank(), panel.grid.minor = element_blank()
    )
  

bar_prop <- vehicle_period_cross |> select(-`Row Total`) |> 
  pivot_longer(cols=c(weekday:weekend), names_to="period", values_to="count") |> 
  filter(`Vehicle type`!="Column Total") |> 
  mutate(
    vehicle_type=factor(`Vehicle type`, levels=vehicle_order),
    period=factor(period, levels=c("weekday", "weekend"))) |>
  ggplot(aes(x=count, y=fct_rev(vehicle_type))) +
  geom_col(aes(fill=period), position="fill")+
  annotate("segment", x=.22, xend=.22, y=0.4, yend=8.6, colour="#08306b")+
  scale_x_continuous(breaks=c(0,0.5, 1)) +
  #annotate("text", y=9, x=.24, label="expected weekend", hjust=0.5,vjust=1, family="Avenir Book", size=3.5, colour="#08306b")+
  scale_fill_manual(values=c("#c6dbef", "#08519c"))+
  labs(y="", x="prop weekend")+
  guides(fill=FALSE)+
  labs(subtitle="standardised bar")+
  theme(
    plot.subtitle = element_text(family="Avenir Book", size=14),
    panel.grid.major.y=element_blank(), panel.grid.minor = element_blank(), axis.text.y=element_blank()
  )


mosaic <-  ped_veh |> 
  mutate(
    day=lubridate::wday(datetime,label=TRUE),
    is_weekend=factor(if_else(day %in% c("Sat", "Sun"), "weekend", "weekday"),
                      levels=c("weekend", "weekday"))
  ) |> 
  filter(police_force == "Metropolitan Police" | police_force == "City of London", local_authority_district!="London Airport (Heathrow)") |>
  group_by(is_weekend) |> 
  mutate(vehicle_type=factor(vehicle_type, levels=vehicle_order)) |> 
   ungroup() |> 
  select(vehicle_type, is_weekend) |>
  mutate(vehicle_type=fct_rev(vehicle_type)) |> 
  ggplot() +
  geom_mosaic(aes(x=product(vehicle_type), fill=is_weekend, colour=is_weekend, divider = "vspine"), offset = 0.008, alpha=1)+
  scale_fill_manual(values=c("#08519c", "#c6dbef"))+
  theme(panel.grid.major.y=element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) +
  coord_flip()

# annotate labels
plot_data <- ggplot_build(mosaic)$data |> as.data.frame() |>
  group_by(x__vehicle_type) |>
  summarise(xmin=min(xmin),xmax=max(xmax), ymin=min(ymin),ymax=max(ymax),
            count=sum(.wt))

mosaic_plot <- mosaic + 
  geom_text(
    data=plot_data, 
    aes(x=xmin+0.5*(xmax-xmin), y=ymin+0.5*(ymax-ymin),
        label=x__vehicle_type, size=count), family= "Avenir Book", alpha=0.5)+
  annotate("segment", y=.22, yend=.22, x=0, xend=1, colour="#08306b")+
  scale_size(range = c(1.5, 9))+
  guides(size=FALSE)+
  theme(plot.subtitle = element_text(family="Avenir Book", size=14), legend.position = "right", axis.text=element_blank(), legend.title = element_blank())+
  labs(y="prop weekend", x="crash count", subtitle="mosaic plot") 


plot<- bar_freq + bar_prop + mosaic_plot + plot_layout(widths=c(1,1,1.1)) 


ggsave(filename=here("figs", "04", "bars-assoc.png"), plot=plot,width=10.5, height=3.2, dpi=700)
ggsave(filename=here("figs", "04", "bars-assoc.svg"), plot=plot,width=12, height=4)

# 2.5 Residuals plot -----------------

observed_vehicle <- ped_veh |> filter(police_force == "Metropolitan Police" | police_force == "City of London", local_authority_district!="London Airport (Heathrow)") |> 
  mutate(
    is_inner=if_else(local_authority_district %in% c("Camden",
                                                     "Greenwich",
                                                     "Hackney",
                                                     "Hammersmith and Fulham",
                                                     "Islington",
                                                     "Kensington and Chelsea",
                                                     "Lambeth",
                                                     "Lewisham",
                                                     "Southwark",
                                                     "Tower Hamlets",
                                                     "Wandsworth",
                                                     "Westminster", "City of London"), "inner", "outer"),
    vehicle_type=factor(vehicle_type, levels=order_type)
  ) |> 
  group_by(local_authority_district) |> 
  mutate(row_total=n()) |> ungroup() |> 
  group_by(vehicle_type) |> 
  mutate(col_total=n()) |> ungroup() |> 
  mutate(grand_total=n()) |> 
  group_by(local_authority_district, vehicle_type) |> 
  
  summarise(
    observed=n(), 
    row_total=first(row_total), 
    col_total=first(col_total),
    grand_total=first(grand_total),
    expected=(row_total*col_total)/grand_total,
    prop=observed/row_total,
    resid=(observed-expected)/sqrt(expected),
    is_inner=first(is_inner)) |> 
  ungroup() |> 
  
  # censor resids to 25
  mutate(
    resid=pmax(pmin(resid, 25),-25)
  ) |> 
  
  ggplot(aes(x=vehicle_type,y=reorder(local_authority_district, row_total), fill=observed)) +
  geom_tile(colour="#707070", size=.2) +
  facet_grid(is_inner~., scales="free_y", space="free_y") +
  scale_fill_distiller(palette="Blues", direction=1) +
  guides(fill="none")+
  labs(y="", subtitle="", fill="resids") +
  theme(
    axis.text.x = element_text(angle=270, hjust=0, size=8), panel.spacing.x = unit(-.1, "lines"),
    axis.title.x=element_blank(),
    panel.grid.major.y=element_blank(),
    strip.text = element_blank(),
    plot.margin = unit(c(0,0,0,0), "cm"),
    legend.key.size = unit(.4, 'cm'),
    legend.text=element_text(size=4)
  )

resids_vehicle <- ped_veh |> filter(police_force == "Metropolitan Police" | police_force == "City of London", local_authority_district!="London Airport (Heathrow)") |> 
  mutate(
    is_inner=if_else(local_authority_district %in% c("Camden",
                                                     "Greenwich",
                                                     "Hackney",
                                                     "Hammersmith and Fulham",
                                                     "Islington",
                                                     "Kensington and Chelsea",
                                                     "Lambeth",
                                                     "Lewisham",
                                                     "Southwark",
                                                     "Tower Hamlets",
                                                     "Wandsworth",
                                                     "Westminster", "City of London"), "inner", "outer"),
    vehicle_type=factor(vehicle_type, levels=order_type)
  ) |> 
  group_by(local_authority_district) |> 
  mutate(row_total=n()) |> ungroup() |> 
  group_by(vehicle_type) |> 
  mutate(col_total=n()) |> ungroup() |> 
  mutate(grand_total=n()) |> 
  group_by(local_authority_district, vehicle_type) |> 
  
  summarise(
    observed=n(), 
    row_total=first(row_total), 
    col_total=first(col_total),
    grand_total=first(grand_total),
    expected=(row_total*col_total)/grand_total,
    prop=observed/row_total,
    resid=(observed-expected)/sqrt(expected),
    is_inner=first(is_inner)) |> 
  ungroup() |> 
  
  # censor resids to 25
  mutate(
    resid=pmax(pmin(resid, 25),-25)
  ) |> 
  
  ggplot(aes(x=vehicle_type,y=reorder(local_authority_district, row_total), fill=resid)) +
  geom_tile(colour="#707070", size=.2) +
  facet_grid(is_inner~., scales="free_y", space="free_y") +
  scale_fill_distiller(palette="RdBu", direction=-1, limits=c(-25,25)) +
  #labs(y="", subtitle="Colour: crash counts by borough <br>  independent of vehicle type") +
  guides(fill="none")+
 
  theme(
    axis.text.x = element_text(angle=270, hjust=0), panel.spacing.x = unit(-.1, "lines"),
    axis.title.x=element_blank(), axis.title.y = element_blank(),
    panel.grid.major.y=element_blank(),
    plot.subtitle = ggtext::element_markdown(size=10.5, hjust=0, family="Avenir Book", halign=0),
    plot.margin = unit(c(0,0,0,0), "cm"),
    legend.key.size = unit(.4, 'cm'),
    legend.text=element_text(size=4),
    axis.line = element_blank(),
    strip.text.y = element_blank()
  )

resids_period <- ped_veh |> 
  filter(police_force == "Metropolitan Police" | police_force == "City of London", local_authority_district!="London Airport (Heathrow)") |> 
  mutate(
    day=lubridate::wday(datetime,label=TRUE),
    hod=lubridate::hour(datetime), 
    is_night=if_else((hod < 6 | hod > 20), "night", "day"),
    is_weekend=if_else(day %in% c("Sat", "Sun"), "weekend", "weekday"),
    is_inner=if_else(local_authority_district %in% c("Camden",
                                                     "Greenwich",
                                                     "Hackney",
                                                     "Hammersmith and Fulham",
                                                     "Islington",
                                                     "Kensington and Chelsea",
                                                     "Lambeth",
                                                     "Lewisham",
                                                     "Southwark",
                                                     "Tower Hamlets",
                                                     "Wandsworth",
                                                     "Westminster", "City of London"), "inner", "outer"),
    vehicle_type=factor(vehicle_type, levels=order_type)
  ) |> 
  filter(!is.na(hod)) |> 
  group_by(local_authority_district, is_weekend) |> 
  mutate(row_total=n()) |> ungroup() |> 
  group_by(local_authority_district, vehicle_type) |> 
  mutate(col_total=n()) |>  ungroup() |> 
  group_by(local_authority_district) |> 
  mutate(grand_total=n()) |> 
  group_by(local_authority_district, is_weekend, vehicle_type) |> 
  summarise(
    observed=n(), 
    row_total=first(row_total), 
    col_total=first(col_total),
    grand_total=first(grand_total),
    expected=(row_total*col_total)/grand_total,
    prop=observed/row_total,
    resid=(observed-expected)/sqrt(expected),
    is_inner=first(is_inner)
  ) |> ungroup() |> 
  
  ggplot(
    aes(x=vehicle_type,y=reorder(local_authority_district, row_total), fill=resid)
  ) +
  geom_tile(colour="#707070", size=.2) +
  facet_grid(is_inner~., scales="free_y", space="free_y") +
  scale_fill_distiller(palette="RdBu", direction=1, limits=c(-5.43,5.43)) +
  guides(fill="none")+
  #labs(y="", x="",
    #   subtitle="Colour: crash counts by vehicle type<br>
     #  independent of period in week") +
  theme(
    axis.text.x = element_text(angle=270, hjust=0), panel.spacing.x = unit(-.1, "lines"),
    axis.text.y=element_blank(),
    strip.text.x = ggtext::element_markdown(),
    plot.subtitle = ggtext::element_markdown(size=10.5, hjust=0, family="Avenir Book", halign=.0),
    axis.title.x = element_blank(), axis.title.y=element_blank(),
    plot.margin = unit(c(0,0,0,0), "cm"),
    legend.key.size = unit(.4, 'cm'),
    legend.text=element_text(size=4),
    axis.line = element_blank(), 
  )

plot <- resids_vehicle + resids_period +
  plot_layout(widths = c(1, 1), nrow=1) 

ggsave(filename="figs/04/borough-freqs-resids-raw.png", plot=plot,width=7, height=6, dpi=500)


# Load in as image to annotate.
img <- image_read(here("figs", "04", "borough-freqs-resids-raw.png")) |> 
  image_fill('none') |> 
  as.raster()

aspect <- 30/35

plot <- ggplot() +
  annotation_raster(img, 0, 1, 0, 1*aspect) +
  annotate("richtext",
           label= "Vehicle types other than cars <br> <span style = 'color: #b2182b;'>over-represented</span> in inner London<br> crashes",
           x=-.02, y=.9, fill = NA, label.color = NA, size=2.8, hjust="left", family="Avenir Book"
  ) +
  
  
  annotate("richtext", 
           label="Colour: crash counts by borough <br>  independent of vehicle type",
           x=.62, y=.87, fill = NA, label.color = NA, size=2.5, hjust="right", family="Avenir Book"
           
    )+
  annotate("richtext", 
           label="Colour: crash counts by vehicle type <br>  independent of period in week",
           x=.95, y=.87, fill = NA, label.color = NA, size=2.5, hjust="right", family="Avenir Book"
           
  )+
  annotate(
    geom = "curve",
    x = .28, xend = .35,
    y = .89, yend = .85,
    curvature = -.5,
    angle = 35, size=.2
  ) +
  
  annotate("richtext",
           label= "Cars very <span style = 'color: #2166ac;'>under-<br>represented</span> in <br>weekday crashes, <br> especially in <br> inner London",
           x=.96, y=.55, fill = NA, label.color = NA, size=2.8, hjust="left", family="Avenir Book"
  ) +
  
  annotate(
    geom = "curve",
    x = .96, xend = .7,
    y = .6, yend = .6,
    curvature = .3,
    angle = 135, size=.2
  ) +
  
  
  scale_x_continuous(limits=c(-0.05, 1.2)) +
  scale_y_continuous(limits=c(0, 1)) +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
  
ggsave(here("figs", "04", "borough-freqs-resids.png"), plot, dpi=700, width = 8, height=6.5)


# 2.6 Gridmap plot boroughs -----------------

n_row <- 8
n_col <- 8
pts <- london_boroughs |>
  st_drop_geometry() |>
  select(area_name, x = easting, y = northing)
solution <- points_to_grid(pts, n_row, n_col, compactness = .65)
grid_all <- make_grid(london_boroughs, n_row, n_col) 
grid <- grid_all |>
  inner_join(solution)

exp <- ped_veh |>
  filter(
    vehicle_type %in% c("Car", "Motorcycle","Taxi", "Bicycle"),
    police_force == "Metropolitan Police" | police_force == "City of London", local_authority_district!="London Airport (Heathrow)" 
    ) |> 
  mutate(
    day=lubridate::wday(datetime, label=TRUE),
    is_weekend=day %in% c("Sat", "Sun"), 
    glob_exp=mean(is_weekend)
  ) |> 
  group_by(local_authority_district, vehicle_type) |> 
  summarise(
    glob_exp=first(glob_exp), 
    freq=n(), 
    obs=mean(is_weekend),
    exp=glob_exp,
    diff=obs-exp,
    resid=(obs-exp)/sqrt(exp)
  ) |> ungroup() |> 
  group_by(vehicle_type) |> 
  mutate(freq_global=max(freq)) 

plot_data <- exp |> ungroup() |> 
  left_join(grid, by=c("local_authority_district"="area_name")) |> 
  filter(!is.na(col)) |> 
  st_as_sf()

cell_width <- abs((grid |> filter(col==5, row==2) |> pull(x)) - (grid |> filter(col==6, row==2) |> pull(x)))
cell_height <- abs((grid |> filter(col==5, row==2) |> pull(y)) - (grid |> filter(col==5, row==3) |> pull(y)))

cell_width = cell_width * .95
cell_height = cell_height * .95

max_x <- max(plot_data$x)
max_y <- max(plot_data$y)

grids_car <- plot_data |> 
  mutate(vehicle_type=factor(vehicle_type, levels=c("Car", "Motorcycle", "Bicycle", "Taxi"))) |> 
  filter(vehicle_type %in% c("Car")) |> 
  ggplot() +
  geom_sf(fill="#d9d9d9", colour="#ffffff") +
  # prop weekend
  geom_rect(aes(ymin=y-.5*cell_height, ymax=y+(freq/freq_global-.5)*cell_height,
    xmin=x-.5*cell_width, xmax=x+(obs-.5)*cell_width), fill=site_colours$primary,
    alpha=.5
    ) +
  # frequency 
  geom_rect(aes(ymin=y-.5*cell_height, ymax=y+(freq/freq_global-.5)*cell_height,
                xmin=x-.5*cell_width, xmax=x+.5*cell_width), fill=site_colours$primary,
            alpha=.5
   ) +
 geom_text(aes(x = x, y = y, label = str_extract(local_authority_district, "^.{3}")), size = 3.5, alpha=.8, family="Avenir Medium") +
 annotate("text", x=max_x -4.5 *cell_width, y=max_y+.5*cell_height, vjust="bottom", hjust="right", label="Car", size=4) +
 annotate("text", x=max_x-1.25*cell_width, y=max_y, vjust="bottom", hjust="left", label="", size=4) +
  coord_sf() +
  facet_wrap(~vehicle_type, nrow=1) +
  #labs(caption= "<span style = 'color: #4A679D;'> weekend </span> | <span style = 'color: #3B7EBA;'>weekday </span>") +
  theme(
    strip.text.x = element_blank(),
    panel.grid.major=element_blank(), panel.grid.minor = element_blank(), axis.title.y=element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), axis.line = element_blank(),
    plot.caption=ggtext::element_markdown(family = "Avenir Medium", size=13), plot.margin = unit(c(0,0,0,0), "cm")
)


grids_bike <- plot_data |> 
  mutate(vehicle_type=factor(vehicle_type, levels=c("Car", "Motorcycle", "Bicycle", "Taxi"))) |> 
  filter(vehicle_type %in% c("Bicycle")) |> 
  ggplot() +
  geom_sf(fill="#d9d9d9", colour="#ffffff") +
  # prop weekend
  geom_rect(aes(ymin=y-.5*cell_height, ymax=y+(freq/freq_global-.5)*cell_height,
                xmin=x-.5*cell_width, xmax=x+(obs-.5)*cell_width), fill=site_colours$primary,
            alpha=.5
  ) +
  # frequency 
  geom_rect(aes(ymin=y-.5*cell_height, ymax=y+(freq/freq_global-.5)*cell_height,
                xmin=x-.5*cell_width, xmax=x+.5*cell_width), fill=site_colours$primary,
            alpha=.5
  ) +
  geom_text(aes(x = x, y = y, label = str_extract(local_authority_district, "^.{3}")), size = 3, alpha=.8) +
  annotate("text", x=max_x -2.5 *cell_width, y=max_y+1.5*cell_height, vjust="bottom", hjust="right", label="Bicycle", size=4) +
  coord_sf() +
  facet_wrap(~vehicle_type, nrow=1) +
  #labs(caption= "<span style = 'color: #4A679D;'> weekend </span> | <span style = 'color: #3B7EBA;'>weekday </span>") +
  theme(
    strip.text.x = element_blank(),
    panel.grid.major=element_blank(), panel.grid.minor = element_blank(), axis.title.y=element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), axis.line = element_blank(),
    plot.caption=ggtext::element_markdown(family = "Avenir Medium", size=13),plot.margin = unit(c(0,0,0,0), "cm")
  )


grids_motobike <- plot_data |> 
  mutate(vehicle_type=factor(vehicle_type, levels=c("Car", "Motorcycle", "Bicycle", "Taxi"))) |> 
  filter(vehicle_type %in% c("Motorcycle")) |> 
  ggplot() +
  geom_sf(fill="#d9d9d9", colour="#ffffff") +
  # prop weekend
  geom_rect(aes(ymin=y-.5*cell_height, ymax=y+(freq/freq_global-.5)*cell_height,
                xmin=x-.5*cell_width, xmax=x+(obs-.5)*cell_width), fill=site_colours$primary,
            alpha=.5
  ) +
  # frequency 
  geom_rect(aes(ymin=y-.5*cell_height, ymax=y+(freq/freq_global-.5)*cell_height,
                xmin=x-.5*cell_width, xmax=x+.5*cell_width), fill=site_colours$primary,
            alpha=.5
  ) +
  geom_text(aes(x = x, y = y, label = str_extract(local_authority_district, "^.{3}")), size = 3.5, alpha=.8, family="Avenir Medium") +
  annotate("text", x=max_x -4.5 *cell_width, y=max_y+.5*cell_height, vjust="bottom", hjust="right", label="Motorcycle", size=4) +
  coord_sf() +
  facet_wrap(~vehicle_type, nrow=1) +
  #labs(caption= "<span style = 'color: #4A679D;'> weekend </span> | <span style = 'color: #3B7EBA;'>weekday </span>") +
  theme(
    strip.text.x = element_blank(),
    panel.grid.major=element_blank(), panel.grid.minor = element_blank(), axis.title.y=element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), axis.line = element_blank(),
    plot.caption=ggtext::element_markdown(family = "Avenir Medium", size=13),plot.margin = unit(c(0,0,0,0), "cm")
  )

labels <- grid |> 
  ggplot() +
  geom_sf(fill="#d9d9d9",colour="#ffffff", linewidth=.4) +
  geom_text(aes(x = x, y = y-.15*cell_height, label = word(area_name, 1)), size = 2) +
  geom_text(aes(x = x, y = y+.15*cell_height, label = str_extract(area_name, "^.{3}")), size = 3, family="Avenir Medium") +
  annotate("text", x=max_x -4.5 *cell_width, y=max_y+.5*cell_height, vjust="bottom", hjust="right", label="Gridmap layout", size=4) +
  
  theme(
    panel.grid.major=element_blank(), panel.grid.minor = element_blank(), axis.title.y=element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), axis.line = element_blank()
  )


grids_cam <- plot_data |> 
  filter(local_authority_district=="Camden", vehicle_type=="Car") |> 
  ggplot() +
  geom_sf(fill="#d9d9d9", colour="#ffffff") +
  # prop weekend
  geom_rect(aes(ymin=y-.5*cell_height, ymax=y+(freq/freq_global-.5)*cell_height,
                xmin=x-.5*cell_width, xmax=x+(obs-.5)*cell_width), fill="#838485",
            alpha=.5
  ) +
  # frequency 
  geom_rect(aes(ymin=y-.5*cell_height, ymax=y+(freq/freq_global-.5)*cell_height,
                xmin=x-.5*cell_width, xmax=x+.5*cell_width), fill="#838485",
            alpha=.5
  ) +
  
  annotate("segment", 
           x=521840.9-.49*cell_width, xend=521840.9+.5*cell_width,
           y=181198-.58*cell_height, yend=181198-.58*cell_height, size=.2
           ) +
  annotate("segment", 
           x=521840.9-.49*cell_width, xend=521840.9-.22*cell_width,
           y=181198-.58*cell_height, yend=181198-.58*cell_height, size=.7
  ) +
  
  annotate("segment", 
           x=521840.9-.58*cell_width, xend=521840.9-.58*cell_width,
           y=181198-.5*cell_height, yend=181198+.5*cell_height, size=.2
  ) +
  annotate("segment", 
           x=521840.9-.58*cell_width, xend=521840.9-.58*cell_width,
           y=181198-.5*cell_height, yend=181198+.12*cell_height, size=.7
  ) +
  
  annotate("text", x=521840.9-.65*cell_width, y=181198+.1*cell_height, 
           label="num\ncrashes", hjust="right", size=5.5) +
  annotate("text", x=521840.9-.01*cell_width, y=181198-.9*cell_height, 
           label="proportion\nweekend", hjust="centre", size=5.5) +
  
  # annotate(
  #   geom = "curve",
  #   xend = 521840.9-.42*cell_width, x = 521840.9-.25*cell_width,
  #   y = 181198-.7*cell_height, yend = 181198-.6*cell_height,
  #   curvature = -.3,
  #   angle = 60, size=.2
  # ) +
  # 
  # annotate(
  #   geom = "curve",
  #   xend = 521840.9-.85*cell_width, x = 521840.9-.62*cell_width,
  #   yend = 181198-0*cell_height, y = 181198-.15*cell_height,
  #   curvature = -.3,
  #   angle = 60, size=.2
  # ) +
  # 
  coord_sf(xlim=c(521840.9-2*cell_width, 521840.9+.49*cell_width), 
           ylim= c(181198-1.5*cell_height, 181198+.5*cell_height), default_crs = sf::st_crs(27700)) +
  theme(
    panel.grid.major=element_blank(), panel.grid.minor = element_blank(), axis.title.y=element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), axis.line = element_blank(),
    plot.caption=ggtext::element_markdown(family = "Avenir Medium", size=13)
  )

ggsave(filename= here("figs", "04", "grids_camx.png"), plot=grids_cam, width=3.2, height=2.8, dpi=800)

# remotes::install_github("wilkelab/cowplot")
# library(cowplot)

img <- image_read(here("figs", "04", "grids_camx.png")) |> 
  image_fill('none') |> 
  as.raster()


min_x <- 507258.2
min_y <- 198096.9

# p1 <- grids_car +
#   annotation_raster(img, min_x-1.2*cell_width, min_x + 1.1*cell_width, max_y-.5*cell_height, max_y+1.8*cell_height)

p1 <- labels + 
  annotate("text", x=max_x + 8 *cell_width, y=max_y, label="", size=1) +
  # annotate("text", x=max_x +3.1 *cell_width, y=max_y-4.2*cell_height, label="Mosaic encoding", size=3.5) +
  # annotation_raster(img, max_x+1.8*cell_width, max_x + 4.5*cell_width, max_y-4.6*cell_height, max_y-7.1*cell_height)
  annotate("text", x=max_x +3.1 *cell_width, y=max_y+.5*cell_height, label="Mosaic encoding", size=4) +
  annotation_raster(img, max_x+1.8*cell_width, max_x + 4.5*cell_width, max_y-0.2*cell_height, max_y-2.7*cell_height)

plot <- (p1 +  grids_bike) / 
  (labels  + grids_motobike)         
ggsave(filename= here("figs", "04", "grids_vehicle.png"), plot=plot,
       width=9.5, height=8, dpi=900)

plot <- (grids_car + grids_motobike)/ p1  + plot_layout(heights=c(1.05,1))


ggsave(filename= here("figs", "04", "grids_vehicle_test.png"), plot=plot,
       width=9.5, height=8, dpi=600)

#-----------------------------------------
# 3. Techniques graphics
#-----------------------------------------


# 3.1 Data -----------------

ped_veh_complete <- ped_veh |> 
  filter(
    !is.na(crash_quintile),
    !is.na(casualty_quintile), 
    casualty_quintile != "Data missing or out of range", 
    driver_quintile != "Data missing or out of range"
    ) 

ped_veh_missing <- ped_veh |> 
  anti_join(ped_veh_complete |> select(accident_index)) 

# 3.2 Study missing -----------------
ped_veh |> 
  mutate(
    is_complete=accident_index %in% (ped_veh_complete |> pull(accident_index))
    ) |> 
  group_by(accident_severity) |> 
  summarise(prop_complete=mean(is_complete))

ped_veh |> 
  mutate(
    is_complete=accident_index %in% (ped_veh_complete |> pull(accident_index))
  ) |> 
  group_by(crash_quintile) |> 
  summarise(prop_complete=mean(is_complete))

plot <- ped_veh |> 
  mutate(
    is_complete=accident_index %in% (ped_veh_complete |> pull(accident_index)),
    is_ksi=if_else(accident_severity != "Slight", "Fatal | Serious", "Slight")
  ) |> 
  group_by(crash_quintile, is_ksi) |> 
  summarise(prop_complete=mean(is_complete)) |> 
  filter(!is.na(crash_quintile)) |> 
  ggplot() +
  geom_point(aes(y=crash_quintile, x=prop_complete, colour=is_ksi), size=3) +
  scale_colour_manual(values=c("#67000d", "#fb6a4a")) +
  scale_x_continuous(limits=c(0.1,.4)) +
  labs(x="proportion complete", y="IMD crash location") +
  theme(legend.title = element_blank())

ggsave(filename= here("figs", "04", "completeness.png"), plot=plot,width=5, height=3.5, dpi=300)  


# 3.3 Plot freqs -----------------

plot_data <- ped_veh_complete |> 
  # inner_join(imd |> select(lsoa_code, total_pop), by=c("lsoa_of_accident_location"="lsoa_code"))  |>
  select(driver_quintile, casualty_quintile, crash_quintile) |> 
  pivot_longer(cols=everything(), names_to="location_type", values_to="imd") |>
  group_by(location_type, imd) |> 
  summarise(count=n()) |> ungroup() |> 
  separate(col=location_type, into="type", sep="_", extra = "drop") |> 
  mutate(
    type=case_when(
      type=="casualty" ~ "pedestrian",
      type=="crash" ~ "location",
      TRUE ~ type)
  ) 
freqs <- ped_veh_complete |> 
  # inner_join(imd |> select(lsoa_code, total_pop), by=c("lsoa_of_accident_location"="lsoa_code"))  |>
  select(driver_quintile, casualty_quintile, crash_quintile) |> 
  pivot_longer(cols=everything(), names_to="location_type", values_to="imd") |>
  group_by(location_type, imd) |> 
  summarise(count=n()) |> ungroup() |> 
  separate(col=location_type, into="type", sep="_", extra = "drop") |> 
  mutate(
    type=case_when(
      type=="casualty" ~ "pedestrian",
      type=="crash" ~ "location",
      TRUE ~ type),
    type=factor(type, levels=c("location", "pedestrian", "driver"))
  ) |> 
  ggplot() + 
  geom_col(aes(x=imd, y=count), fill="#003c8f") +
  scale_x_discrete(labels=c("most","", "mid", "", "least")) +
  labs(y="crash count in thousands", x="IMD") +
  scale_y_continuous(
    breaks=c(0:3*5000), 
    labels = scales::comma_format(scale = .001)) +
  facet_wrap(~type)


plot_imd_driver <- plot_data |> 
  ggplot(aes(x=casualty_quintile, y=driver_quintile)) +
  geom_tile(aes(fill=observed), fill="transparent", colour="#707070", size=.1) +
  annotate("text", x=5,y=0.2, label="least", hjust=.5, size=3) +
  annotate("segment", x=.5, y = 5.7, xend = 5.2, yend = 5.7,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")), linewidth=.15) +
  annotate("segment", x=5.7, y = 0.5, xend = 5.7, yend = 5.2,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")), linewidth=.15) +
  annotate("text", x=1,y=0.2, label="most", hjust=0.5, size=3) +
  annotate("text", x=3,y=0.2, label="mid", hjust=0.5, size=3) +
  annotate("text", y=5,x=0.2, label="least", hjust=.5, size=3, angle=90) +
  annotate("text", y=1,x=0.2, label="most", hjust=0.5, size=3, angle=90) +
  annotate("text", y=3,x=0.2, label="mid", hjust=0.5, size=3, angle=90) +
  annotate("text", y=5,x=0, label="", hjust=.5, size=2) +
  annotate("text", x=6.5,y=0, label="", hjust=1, size=2) +
  annotate("text", y=3,x=5.9, label="IMD of driver", hjust=.5, vjust=0, size=3, angle=270) +
  annotate("text", y=6.3,x=3, label="IMD of pedestrian", hjust=.5, vjust=1, size=3) +
  scale_fill_distiller(palette="Blues", direction=1, guide="none") +
  coord_equal() +
  theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    plot.subtitle=element_text(family = "Avenir Book", size=12),
    axis.text.x=element_blank(), 
    axis.text.y = element_blank(), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position="right",
    axis.line.x = element_blank()
  )


plot <- freqs + plot_spacer() + plot_imd_driver + plot_layout(widths=c(1,.05, .4))
ggsave(filename= here("figs", "04", "freqs_imd.png"), plot=plot,width=7, height=3.5, dpi=300) 


# 3.4 Plot freqs -----------------

model_ped_veh <- ped_veh_complete |> 
  # Record the grand_total: total pedestrian crashes.
  mutate(grand_total=n()) |> 
  # Record the row_total: total crashes for each IMD class of driver.
  group_by(driver_quintile) |> 
  mutate(row_total=n()) |>  ungroup() |> 
  # Record the col_total: total crashes for each IMD class of pedestrian.
  group_by(casualty_quintile) |> 
  mutate(col_total=n()) |>  ungroup() |> 
  # Calculate over observed cells: each ped-driver IMD class combination.
  group_by(casualty_quintile, driver_quintile) |> 
  summarise(
    # Observed crashes per ped-driver combination cell.
    observed=n(),
    # row_total for that cell.
    row_total=first(row_total),
    # col_total for that cell.
    col_total=first(col_total),
    grand_total=first(grand_total),
    # expected counts as per chi-square assumption of independence.
    expected=(row_total*col_total)/grand_total,
    # Residuals measure relative difference biased towards larger numbers
    # due to sqrt() transformation in denominator.
    resid=(observed-expected)/sqrt(expected),
  ) %>% ungroup()


plot_data <- ped_veh_complete |>  
  mutate(grand_total=n()) |> 
  group_by(driver_quintile) |> 
  mutate(row_total=n()) |> ungroup() |> 
  group_by(casualty_quintile) |> 
  mutate(col_total=n()) |> ungroup() |> 
  group_by(casualty_quintile, driver_quintile) |> 
  summarise(
    observed=n(), 
    row_total=first(row_total), 
    col_total=first(col_total),
    grand_total=first(grand_total),
    expected=(row_total*col_total)/grand_total,
    expected_share_row=expected/row_total,
    row_marginal=row_total,
    col_marginal=col_total,
    expected_share_col=expected/col_total,
    resid=(observed-expected)/sqrt(expected),
    max_resid=max(abs(resid))
  ) |> ungroup()


model_data <- ped_veh_complete |> 
  mutate(grand_total=n()) |> 
  group_by(driver_quintile) |> 
  mutate(row_total=n()) |> ungroup() |> 
  group_by(casualty_quintile) |> 
  mutate(col_total=n()) |> ungroup() |> 
  group_by(casualty_quintile, driver_quintile) |> 
  summarise(
    observed=n(), 
    row_total=first(row_total), 
    col_total=first(col_total),
    grand_total=first(grand_total),
    expected=(row_total*col_total)/grand_total,
    resid=(observed-expected)/sqrt(expected),
  ) 

model_data |> 
  # Censor max values to 40.
  #mutate(resid=if_else(resid>0, pmin(resid, 40), pmax(resid, -40))) |> 
  ggplot(aes(x=casualty_quintile, y=driver_quintile)) +
  geom_tile(aes(fill=resid), colour="#707070", size=.2) +
  scale_fill_distiller(
    palette="RdBu", direction=-1, limits=c(- max(model_data$resid), max(model_data$resid))) +
  coord_equal()


plot_imd_driver <- plot_data |> 
  ggplot(aes(x=casualty_quintile, y=driver_quintile)) +
  geom_point()+
  geom_tile(aes(fill=observed), colour="#707070", size=.2) +
  
  # annotate("segment", x=.5, y = 5.7, xend = 5.2, yend = 5.7,
  #          arrow = arrow(type = "closed", length = unit(0.02, "npc")), linewidth=.15) +
  # annotate("segment", x=5.7, y = 0.5, xend = 5.7, yend = 5.2,
  #          arrow = arrow(type = "closed", length = unit(0.02, "npc")), linewidth=.15) +
  
  annotate("text", x=1,y=0.2, label="most", hjust=0.5, size=3) +
  annotate("text", x=3,y=0.2, label="mid", hjust=0.5, size=3) +
  annotate("text", y=5,x=0.2, label="least", hjust=.5, size=3, angle=90) +
  annotate("text", y=1,x=0.2, label="most", hjust=0.5, size=3, angle=90) +
  annotate("text", y=3,x=0.2, label="mid", hjust=0.5, size=3, angle=90) +
  annotate("text", x=5,y=0.2, label="least", hjust=.5, size=3) +
  
  annotate("text", x=6.5,y=0, label="", hjust=1, size=2) +
  annotate("text", y=5,x=0, label="", hjust=.5, size=2) +
  
  annotate("text", y=3,x=5.8, label="IMD of driver", hjust=.5, vjust=0, size=3.2, angle=270) +
  annotate("text", y=6.15,x=3, label="IMD of pedestrian", hjust=.5, vjust=1, size=3.2) +
  scale_fill_distiller(
    palette="Blues", direction=1, name="count\nthousands",
    labels = scales::comma_format(scale = .001)
    ) +
  labs(subtitle="Observed") +
  coord_equal() +
  theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    plot.subtitle=element_text(family = "Avenir Book", size=10),
    axis.text.x=element_blank(), 
    axis.text.y = element_blank(), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position="bottom",
    legend.key.height = unit(.4, "cm"),
    legend.key.width = unit(.5, "cm"),
    legend.text = element_text(size=8),
    legend.title = element_text(size=8),
    axis.line.x = element_blank()
  )

plot_imd_expected <- plot_data |> 
  ggplot(aes(x=casualty_quintile, y=driver_quintile)) +
  geom_point()+
  geom_tile(aes(fill=expected), colour="#707070", size=.1) +
  
  # annotate("text", x=1,y=0.2, label="most", hjust=0.5, size=3) +
  # annotate("text", x=3,y=0.2, label="mid", hjust=0.5, size=3) +
  # annotate("text", y=5,x=0.2, label="least", hjust=.5, size=3, angle=90) +
  # annotate("text", y=1,x=0.2, label="most", hjust=0.5, size=3, angle=90) +
  # annotate("text", y=3,x=0.2, label="mid", hjust=0.5, size=3, angle=90) +
  # annotate("text", x=5,y=0.2, label="least", hjust=.5, size=3) +
  
  annotate("text", x=6.5,y=0, label="", hjust=1, size=2) +
  annotate("text", y=5,x=0, label="", hjust=.5, size=2) +
  
  annotate("text", y=3,x=-.1, label="expected", hjust=0.5, size=2.5, angle=90) +

  # annotate("text", y=3,x=5.8, label="IMD of driver", hjust=.5, vjust=0, size=3.5, angle=270) +
  # annotate("text", y=6,x=3, label="IMD of pedestrian", hjust=.5, vjust=1, size=3.5) +
  scale_fill_distiller(palette="Blues", direction=1, guide ="none") +
  
  coord_equal() +

  theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    plot.subtitle=element_text(family = "Avenir Book", size=8),
    axis.text.x=element_blank(), 
    axis.text.y = element_blank(), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line = element_blank(),
    legend.position="right")

plot_imd_marginal_row <- plot_data |> 
  ggplot(aes(x=casualty_quintile, y=driver_quintile)) +
  geom_point()+
  geom_tile(aes(fill=row_marginal), colour="#707070", size=.1) +
  
  # annotate("text", x=1,y=0.2, label="most", hjust=0.5, size=3) +
  # annotate("text", x=3,y=0.2, label="mid", hjust=0.5, size=3) +
  # annotate("text", y=5,x=0.2, label="least", hjust=.5, size=3, angle=90) +
  # annotate("text", y=1,x=0.2, label="most", hjust=0.5, size=3, angle=90) +
  # annotate("text", y=3,x=0.2, label="mid", hjust=0.5, size=3, angle=90) +
  # annotate("text", x=5,y=0.2, label="least", hjust=.5, size=3) +
  
  annotate("text", y=3,x=-0.1, label="row marginals", hjust=0.5, size=2.5, angle=90) +
  
  annotate("text", x=6.5,y=0, label="", hjust=1, size=2) +
  annotate("text", y=5,x=0, label="", hjust=.5, size=2) +
  
  annotate("text", y=3,x=-.5, label="", hjust=0.5, size=4, angle=90) +
  
  # annotate("text", y=3,x=5.8, label="IMD of driver", hjust=.5, vjust=0, size=3.5, angle=270) +
  # annotate("text", y=6,x=3, label="IMD of pedestrian", hjust=.5, vjust=1, size=3.5) +
  scale_fill_distiller(palette="Blues", direction=1, guide="none") +
  
  coord_equal() +

  theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    plot.subtitle=element_text(family = "Avenir Book", size=8),
    axis.text.x=element_blank(), 
    axis.text.y = element_blank(), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line = element_blank(),
    legend.position="right")




plot_imd_expected <- plot_data |> 
  ggplot(aes(x=casualty_quintile, y=driver_quintile)) +
  geom_point()+
  geom_tile(aes(fill=expected), colour="#707070", size=.1) +
  
  # annotate("text", x=1,y=0.2, label="most", hjust=0.5, size=3) +
  # annotate("text", x=3,y=0.2, label="mid", hjust=0.5, size=3) +
  # annotate("text", y=5,x=0.2, label="least", hjust=.5, size=3, angle=90) +
  # annotate("text", y=1,x=0.2, label="most", hjust=0.5, size=3, angle=90) +
  annotate("text", y=3,x=-.1, label="expected", hjust=0.5, size=2.5, angle=90) +
  # annotate("text", x=5,y=0.2, label="least", hjust=.5, size=3) +
  annotate("text", y=3,x=-.5, label="", hjust=0.5, size=4, angle=90) +
  
  annotate("text", x=6.5,y=0, label="", hjust=1, size=2) +
  annotate("text", y=5,x=0, label="", hjust=.5, size=2) +
  
  # annotate("text", y=3,x=5.8, label="IMD of driver", hjust=.5, vjust=0, size=3.5, angle=270) +
  # annotate("text", y=6,x=3, label="IMD of pedestrian", hjust=.5, vjust=1, size=3.5) +
  scale_fill_distiller(palette="Blues", direction=1, guide="none") +
  
  coord_equal() +
  #labs(subtitle="Col marginals") +
  theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    plot.subtitle=element_text(family = "Avenir Book", size=8),
    axis.text.x=element_blank(), 
    axis.text.y = element_blank(), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line= element_blank(),
    legend.position="right")




plot_imd_marginal_col <- plot_data |> 
  ggplot(aes(x=casualty_quintile, y=driver_quintile)) +
  geom_point()+
  geom_tile(aes(fill=col_marginal), colour="#707070", size=.1) +
  
  # annotate("text", x=1,y=0.2, label="most", hjust=0.5, size=3) +
  # annotate("text", x=3,y=0.2, label="mid", hjust=0.5, size=3) +
  # annotate("text", y=5,x=0.2, label="least", hjust=.5, size=3, angle=90) +
  # annotate("text", y=1,x=0.2, label="most", hjust=0.5, size=3, angle=90) +
  annotate("text", y=3,x=-.1, label="col marginals", hjust=0.5, size=2.5, angle=90) +
  # annotate("text", x=5,y=0.2, label="least", hjust=.5, size=3) +
  annotate("text", y=3,x=-.5, label="", hjust=0.5, size=4, angle=90) +
  
  annotate("text", x=6.5,y=0, label="", hjust=1, size=2) +
  annotate("text", y=5,x=0, label="", hjust=.5, size=2) +
  
  # annotate("text", y=3,x=5.8, label="IMD of driver", hjust=.5, vjust=0, size=3.5, angle=270) +
  # annotate("text", y=6,x=3, label="IMD of pedestrian", hjust=.5, vjust=1, size=3.5) +
  scale_fill_distiller(palette="Blues", direction=1, guide="none") +

 coord_equal() +
  #labs(subtitle="Col marginals") +
  theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    plot.subtitle=element_text(family = "Avenir Book", size=8),
    axis.text.x=element_blank(), 
    axis.text.y = element_blank(), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line= element_blank(),
    legend.position="right")



plot_imd_driver_resid <- plot_data |> 
  # Censor max values to 40.
  #mutate(resid=if_else(resid>0, pmin(resid, 40), pmax(resid, -40))) |> 
  ggplot(aes(x=casualty_quintile, y=driver_quintile)) +
  geom_tile(aes(fill=resid), colour="#707070", size=.2) +
  
  annotate("text", x=1,y=0.2, label="most", hjust=0.5, size=3) +
  annotate("text", x=3,y=0.2, label="mid", hjust=0.5, size=3) +
  annotate("text", y=5,x=0.2, label="least", hjust=.5, size=3, angle=90) +
  annotate("text", y=1,x=0.2, label="most", hjust=0.5, size=3, angle=90) +
  annotate("text", y=3,x=0.2, label="mid", hjust=0.5, size=3, angle=90) +
  annotate("text", x=5,y=0.2, label="least", hjust=.5, size=3) +
  
  annotate("text", x=6.5,y=0, label="", hjust=1, size=2) +
  annotate("text", y=5,x=0, label="", hjust=.5, size=2) +
  
  scale_fill_distiller(
    palette="RdBu", direction=-1, 
    limits=c(- max(plot_data$max_resid), max(plot_data$max_resid)),
    name="residuals"
    ) +
  annotate("text", y=5.5,x=0, label="", hjust=1, size=2) +
  annotate("text", x=6.5,y=0, label="", hjust=1, size=2) +
  # annotate("text", y=3,x=5.8, label="IMD of driver", hjust=.5, vjust=0, size=3.5, angle=270) +
  # annotate("text", y=6,x=3, label="IMD of pedestrian", hjust=.5, vjust=1, size=3.5) +
  annotate("text", y=3,x=5.8, label="", hjust=.5, vjust=0, size=3.5, angle=270) +
  annotate("text", y=6,x=3, label="", hjust=.5, vjust=1, size=3.5) +
  labs(x="", subtitle="Obs vs Exp") +
  theme(
    legend.text = element_text(size=8),
    legend.title = element_text(size=8),
    legend.position="bottom",
    legend.key.height = unit(.4, "cm"),
    legend.key.width = unit(.5, "cm"),
    plot.subtitle=element_text(family = "Avenir Book", size=10),
    axis.text.x=element_blank(), axis.text.y = element_blank(),  axis.title.y = element_blank(), axis.line = element_blank(),
        axis.title.x=element_blank(), plot.margin = unit(c(0,0,0,0), "cm")
    ) +
  coord_equal()

plot_cols <- plot_grid(plot_imd_expected / plot_imd_marginal_row / plot_imd_marginal_col) 

plot <- 
  plot_grid(plot_imd_driver,  
            plot_imd_driver_resid, plot_cols,  
            rel_widths = c(1, 1, .75), 
            rel_heights = c(1,1,1),
            nrow=1)


ggsave(here("figs", "04", "imd_driver_cas.png"), plot=plot,width=4.5, height=3.8, dpi=600)


# 3.6 Plot model 2 -----------------

plot_data <- ped_veh_complete |> 
  mutate(
    grand_total=n()
  ) |> 
  group_by(casualty_quintile, driver_quintile) |> 
  mutate(row_total=n()) |> ungroup() |> 
  group_by(crash_quintile) |> 
  mutate(col_total=n()) |> ungroup() |> 
  group_by(casualty_quintile, driver_quintile, crash_quintile) |> 
  summarise(
    observed=n(), 
    row_total=first(row_total), 
    col_total=first(col_total),
    grand_total=first(grand_total),
    expected=(row_total*col_total)/grand_total,
    resid=(observed-expected)/sqrt(expected),
    # Censor extreme effect sizes.
    resid_censored=if_else(resid<0, pmax(resid, -40), pmin(resid, 40)
    ))  |> ungroup() |> 
  mutate(max_resid_censored=max(resid_censored), max_resid=max(resid))

plot_imd_driver_area_obs <- ped_veh_complete |> 
  group_by(crash_quintile, casualty_quintile, driver_quintile) |> 
  mutate(
    observed=n()
  ) |> 
  group_by(crash_quintile) |> 
  mutate(
    total_crash_quintile=n(),
    observed_max=max(observed),
    observed_rescaled=observed/observed_max
  ) |> ungroup() |> 
  ggplot(aes(x=casualty_quintile, y=driver_quintile)) +
  geom_tile(aes(fill=observed), colour="#707070", size=.2) +
  scale_fill_distiller(palette="Blues", direction=1) +
  facet_wrap(~crash_quintile, nrow=1) +
  labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Observed") +
  theme(axis.text.x=element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.line = element_blank(), legend.title=element_blank(), legend.position = "right",
        plot.subtitle=element_text(family = "Avenir Book", size=13)) +
  coord_equal()


demog_distances <- ped_veh_complete |> 
  mutate(
    # Derive numeric values from IMD classes (ordered factor variable).
    across(c(casualty_quintile, driver_quintile, crash_quintile),
      .fns=list(num=~as.numeric(
        factor(., levels=c(c("1 most deprived", "2 more deprived", "3 mid deprived", "4 less deprived", "5 least deprived")))
        ))
    ),
    # Calculate demog_distance.
    demog_dist=sqrt(
      (casualty_quintile_num-driver_quintile_num)^2 +
        (casualty_quintile_num-crash_quintile_num)^2 +
        (driver_quintile_num-crash_quintile_num)^2
    )
  ) |> 
  # Calculate over observed cells: each ped-driver IMD class combination.
  group_by(casualty_quintile, driver_quintile, crash_quintile) |> 
  summarise(crash_count=n(), demog_dist=first(demog_dist)) |> ungroup()


# Model crash count against demographic distance allowing the intercept to vary
# on crash quintile. There may be some biasing in the distribution of counts due
# to crash quintile (linked to systematic differences in rurality) and so we
# want to observe the association net of this.
model <- lme4::glmer(crash_count ~ demog_dist + ( 1 | crash_quintile),
                     data=demog_distances, family=poisson, nAGQ = 100)

# Extract model residuals.
demog_distances <- demog_distances %>%
  mutate(ml_resids=residuals(model, type="pearson"))

# Model 2.
plot_imd_driver_area_model <- demog_distances |> 
  ggplot(aes(x=casualty_quintile, y=driver_quintile)) +
  geom_tile(aes(fill=ml_resids), colour="#707070", size=.2) +
  # Make colour scale symmetrical on 0.
  scale_fill_distiller(
    palette="RdBu", direction=-1,
    limits=c(-max(demog_distances$ml_resids %>% abs()), max(demog_distances$ml_resids) %>% abs())
  )+
  facet_wrap(~crash_quintile, nrow=1) +
  coord_equal() +
  theme(axis.line = element_blank()) +
  labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Obs vs Exp") +
  theme(axis.text.x=element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.line = element_blank(), legend.title=element_blank(), legend.position = "right",
        plot.subtitle=element_text(family = "Avenir Book", size=13)) +
  coord_equal()


plot_demog_dists <- demog_distances |> 
  ggplot(aes(x=casualty_quintile, y=driver_quintile)) +
  geom_tile(aes(fill=demog_dist), colour="#707070", size=.2) +
  scale_fill_distiller(palette="Blues", direction=-1) +
  facet_wrap(~crash_quintile, nrow=1) +
  labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Geodemographic distance") +
  theme(axis.text.x=element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.line = element_blank(), legend.title=element_blank(), legend.position = "right",
        plot.subtitle=element_text(family = "Avenir Book", size=13)) +
  coord_equal()


plot <- plot_imd_driver_area_obs / plot_demog_dists / plot_imd_driver_area_model


ggsave(filename= here("figs", "04", "model_imd_location.png"), plot=plot,width=8.5, height=6.8, dpi=500) 


# plot_imd_driver_area_obs_local <- ped_veh |> 
#   filter(
#     !is.na(casualty_imd_decile), !is.na(driver_imd_decile),
#     casualty_imd_decile!="Data missing or out of range", 
#     driver_imd_decile!="Data missing or out of range") |>  
#   group_by(crash_quintile, casualty_imd_quintile, driver_imd_quintile) |> 
#   mutate(
#     observed=n()
#   ) |> 
#   group_by(crash_quintile) |> 
#   mutate(
#     total_crash_quintile=n(),
#     observed_max=max(observed),
#     observed_rescaled=observed/observed_max
#   ) |> ungroup() |> 
#   ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
#   geom_tile(aes(fill=observed_rescaled), colour="#707070", size=.2) +
#   scale_fill_distiller(palette="Blues", direction=1) +
#   facet_wrap(~crash_quintile, nrow=1) +
#   theme_v_gds() +
#   labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Observed - local scaling") +
#   theme(axis.text.x=element_blank(), axis.text.y = element_blank(), 
#         axis.title.x = element_blank(), axis.title.y = element_blank())
# 
# plot <- plot_imd_driver_area_obs_local + plot_annotation(title="Pedestrian casualties by IMD quintile of homeplace of pedestrian and driver",
#                                                        subtitle="--Grouped and compared by IMD quintile of crash location",
#                                                        caption="Stats19 data accessed via `stats19` package",
#                                                        theme = theme_v_gds())
# 
# ggsave(filename="./static/class/04-class_files/imd-driver-cas-crash-local.png", plot=plot,width=9, height=4, dpi=300)



# plot_driver_area_resid <-  plot_data |> 
#   ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
#   geom_tile(aes(fill=resid), colour="#707070", size=.2) +
#   scale_fill_distiller(
#     palette="PRGn", direction=1, 
#     limits=c(-max(plot_data$max_resid), max(plot_data$max_resid))
#     )+
#   facet_wrap(~crash_quintile, nrow=1) +
#   theme_v_gds() +
#   labs(subtitle="Obs vs Exp", x="Null: distribute in cells independently of crash location") +
#   theme(
#     axis.text.x=element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
#     axis.title.x = element_text(size=10, hjust=1, family="Roboto Condensed Light")
#     ) +
#   coord_equal()
#   
# 
# 
# plot_driver_area_exp <-  plot_data |> 
#   group_by(crash_quintile) |> 
#   mutate(expected_rescaled=expected/max(expected)) |> 
#   ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
#   geom_tile(aes(fill=expected), colour="#707070", size=.2) +
#   scale_fill_distiller(palette="Blues", direction=1) +
#   facet_wrap(~crash_quintile, nrow=1) +
#   theme_v_gds() +
#   guides(fill=FALSE) +
#   labs(subtitle="Expected", x="") +
#   theme(
#     axis.text.x=element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
#     axis.title.x = element_text(size=10, hjust=1, family="Roboto Condensed Light")
#   ) +
#   coord_equal()

# plot_driver_area_row_total <-  plot_data |> 
#   group_by(crash_quintile) |> 
#   mutate(expected_rescaled=expected/max(expected)) |> 
#   ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
#   geom_tile(aes(fill=row_total), colour="#707070", size=.2) +
#   scale_fill_distiller(palette="Blues", direction=1) +
#   facet_wrap(~crash_quintile, nrow=1) +
#   theme_v_gds() +
#   labs(subtitle="Row marginals", x="") +
#   guides(fill=FALSE) +
#   theme(
#     axis.text.x=element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
#     axis.title.x = element_text(size=10, hjust=1, family="Roboto Condensed Light")
#   ) +
#   coord_equal()
# 
# 
# plot_driver_area_col_total <-  plot_data |> 
#   group_by(crash_quintile) |> 
#   mutate(expected_rescaled=expected/max(expected)) |> 
#   ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
#   geom_tile(aes(fill=col_total), colour="#707070", size=.2) +
#   scale_fill_distiller(palette="Blues", direction=1) +
#   facet_wrap(~crash_quintile, nrow=1) +
#   theme_v_gds() +
#   labs(subtitle="Col marginals", x="") +
#   guides(fill=FALSE) +
#   theme(
#     axis.text.x=element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
#     axis.title.x = element_text(size=10, hjust=1, family="Roboto Condensed Light")
#   ) +
# coord_equal()


  
# plot <- plot_imd_driver_area_obs + plot_driver_area_resid + plot_spacer()+theme_v_gds() +
#   plot_driver_area_exp + plot_driver_area_row_total + plot_driver_area_col_total +  plot_layout(nrow=6) +
#   plot_layout(heights=c(1,1,.1,.8,.8,.8)) +
#   plot_annotation(title="Pedestrian casualties by IMD quintile of homeplace of pedestrian and driver",
#                   subtitle="--Grouped and compared by IMD quintile of crash location",
#                   caption="Stats19 data accessed via `stats19` package",
#                   theme = theme_v_gds())
# 
# 
# ggsave(filename="./static/class/04-class_files/imd-driver-cas-crash.png", plot=plot,width=9, height=14, dpi=300)


# demog_distances <- ped_veh_complete |>
#   mutate(
#     across(
#       c(casualty_quintile, driver_quintile, crash_quintile),
#       .fns=list(num=~as.numeric(factor(., levels=c(c("1 most deprived", "2 more deprived", "3 mid deprived", "4 less deprived", "5 least deprived")))))
#     ),
#     demog_dist=sqrt(
#        (casualty_quintile_num-driver_quintile_num)^2 +
#         (casualty_quintile_num-crash_quintile_num)^2 +
#         (driver_quintile_num-crash_quintile_num)^2
#     )
#   ) |>
#   group_by(demog_dist, casualty_quintile, driver_quintile, crash_quintile) |>
#   summarise(crash_count=n()) |>  ungroup() |>
#   # We want to know how often each demographic distance *occurs*.
#   group_by(demog_dist, casualty_quintile, driver_quintile, crash_quintile) |>
#   mutate(occurrences=n()) |> ungroup() |>
#   mutate(total_crashes=sum(crash_count)) |>
#   # Then for each demographic distance, compute counts and bin probabilities
#   # that are to be spread out to cells of matrix (so we devide by number of times that those distances exist).
#   group_by(demog_dist) |>
#   mutate(crash_count_dist=sum(crash_count), occurrences=sum(occurrences), bin_prob=(crash_count_dist/total_crashes)/occurrences) |> ungroup() |>
#   # For expected counts we need to weight according to size of quintile (crashes). But as there is an association between geodemographic distance
#   # and counts, we need to additionally weight according to the sum of the bin probabilities.
#   group_by(crash_quintile) |>
#   mutate(prop=sum(crash_count)/total_crashes, weight=prop/sum(bin_prob)) |> ungroup()
# 
# 
# # So we can estimate the probability of crashes occurring in each bin, weighting
# # on the number of times those bins appear overall. These are our *global probabilities*.
# demog_distances |> select(demog_dist, crash_count, occurrences, bin_prob, weight) |> unique
# 
# # Our expected counts assume that counts are a function of demographic distance,
# # but *not* the IMD class in which they occur. That is, expected counts distribute
# # by geodemographic distance independently of the geodemographic quintile in which the
# # crash occurred.
# # For each cell in the matrix we arrive at a bin probability or likelihood.
# # However, we need to *weight* according to the relative size of each quintile in terms of crashes and sum of geographic distance.
# plot_data <-  ped_veh |>
#   filter(
#     !is.na(casualty_imd_decile), !is.na(driver_imd_decile),
#     casualty_imd_decile!="Data missing or out of range",
#     driver_imd_decile!="Data missing or out of range", !is.na(crash_quintile)) |>
#   left_join(demog_distances) |>
#   mutate(
#     grand_total=n(),
#     row_total=bin_prob,
#   ) |> ungroup |>
#   group_by(crash_quintile) |>
#   mutate(col_total=n()) |> ungroup |>
#   group_by(casualty_imd_quintile, driver_imd_quintile, crash_quintile) |>
#   summarise(
#     col_total=first(weight),
#     observed=n(),
#     row_total=first(row_total),
#     col_total=first(col_total),
#     grand_total=first(grand_total),
#     #expected=(row_total/first(occurrences)*col_total)/grand_total,
#     expected=(row_total*col_total)/grand_total,
#     expected=(row_total*col_total)*grand_total,
#     #expected=(row_total/first(occurrences)*col_total)/grand_total,
#     resid=(observed-expected)/sqrt(expected),
#     # Censor extreme effect sizes.
#     resid_censored=if_else(resid<0, pmax(resid, -40), pmin(resid, 40)),
#     occurrences=first(occurrences),
#     distance=first(demog_dist),
#     bin_prob=first(bin_prob),
#     #prop=first(crash_prop),
#     #crash_prop_adj=first(crash_prop_adj)
#   )  |> ungroup |>
#   mutate(max_resid_censored=max(resid_censored), max_resid=max(resid))
# 
# plot_data |> summarise(expected=sum(expected), observed=sum(observed), bin_prob=sum(bin_prob))
# 
# plot_data |>
#   group_by(crash_quintile) |>
#   summarise(expected=sum(expected), observed=sum(observed), diff=observed-expected, bin_prob=sum(bin_prob))
# 
# 
# plot_social_dist <- demog_distances |>
#   ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
#   geom_tile(aes(fill=demog_dist), colour="#707070", size=.2) +
#   scale_fill_distiller(palette="Blues", direction=1) +
#   facet_wrap(~crash_quintile, nrow=1) +
#   guides(fill=FALSE) +
#   labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Geodemographic distance") +
#   theme(axis.text.x=element_blank(), axis.text.y = element_blank(),
#         axis.title.x = element_blank(), axis.title.y = element_blank()) +
#   coord_equal()
# 
# 
# plot_exp_dist <- plot_data |>
#   ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
#   geom_tile(aes(fill=expected), colour="#707070", size=.2) +
#   scale_fill_distiller(palette="Blues", direction=1) +
#   facet_wrap(~crash_quintile, nrow=1) +
#   guides(fill=FALSE) +
#   labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Expected") +
#   theme(axis.text.x=element_blank(), axis.text.y = element_blank(),
#         axis.title.x = element_blank(), axis.title.y = element_blank()) +
#   coord_equal()
# 
# plot_row_total_dist <- plot_data |>
#   ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
#   geom_tile(aes(fill=row_total), colour="#707070", size=.2) +
#   scale_fill_distiller(palette="Blues", direction=1) +
#   facet_wrap(~crash_quintile, nrow=1) +
#   guides(fill=FALSE) +
#   labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Cell probabilities") +
#   theme(axis.text.x=element_blank(), axis.text.y = element_blank(),
#         axis.title.x = element_blank(), axis.title.y = element_blank()) +
#   coord_equal()
# 
# plot_col_total_dist <- plot_data |>
#   ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
#   geom_tile(aes(fill=col_total), colour="#707070", size=.2) +
#   scale_fill_distiller(palette="Blues", direction=1) +
#   facet_wrap(~crash_quintile, nrow=1) +
#   guides(fill=FALSE) +
#   labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Quintile sizes") +
#   theme(axis.text.x=element_blank(), axis.text.y = element_blank(),
#         axis.title.x = element_blank(), axis.title.y = element_blank()) +
#   coord_equal()
# 
# plot_resid_dist <-  plot_data |>
#   ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
#   geom_tile(aes(fill=resid), colour="#707070", size=.2) +
#   scale_fill_distiller(
#     palette="PRGn", direction=1,
#     limits=c(-max(plot_data$max_resid), max(plot_data$max_resid))
#   )+
#   facet_wrap(~crash_quintile, nrow=1) +
#   labs(subtitle="Observed vs Expected", x="Null: distribute by 'geodemographic distance' independently of crash location") +
#   theme(
#     axis.text.x=element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
#     axis.title.x = element_text(size=10, hjust=1, family="Roboto Condensed Light")
#   ) +
#   coord_equal()
# 
# 
# plot <- plot_resid_dist + plot_spacer() +
#   plot_exp_dist + plot_row_total_dist + plot_col_total_dist +  plot_layout(nrow=5) +
#   plot_layout(heights=c(1,.1,.8,.8,.8)) +
#   plot_annotation(title="Pedestrian casualties by IMD quintile of homeplace of pedestrian and driver",
#                   subtitle="--Grouped and compared by IMD quintile of crash location",
#                   caption="Stats19 data accessed via `stats19` package")
# 
# 
# ggsave(filename="./static/class/04-class_files/imd-driver-cas-crash-dist-full.png", plot=plot,width=9, height=10, dpi=300)
# 
# plot_data <-  ped_veh |> 
#   filter(
#     !is.na(casualty_imd_decile), !is.na(driver_imd_decile),
#     casualty_imd_decile!="Data missing or out of range", 
#     driver_imd_decile!="Data missing or out of range", !is.na(crash_quintile)) |>   
#   left_join(demog_distances) |> 
#   mutate(
#     grand_total=n()
#   ) |> 
#   group_by(ranked_dist) |> 
#   mutate(row_total=n()) |> ungroup |> 
#   group_by(crash_quintile) |> 
#   mutate(col_total=n()) |> ungroup |> 
#   group_by(casualty_imd_quintile, driver_imd_quintile, crash_quintile) |> 
#   summarise(
#     distance=first(ranked_dist),
#     prop=first(crash_prop_dist),
#     prop_adjusted=prop/ranked_occurrences,
#     row_total=prop_adjusted,
#     col_total=first(col_total),
#     expected=prop_adjusted*col_total,
#     observed=n(), 
#     resid=(observed-expected)/sqrt(expected),
#     # Censor extreme effect sizes.
#     resid_censored=if_else(resid<0, pmax(resid, -40), pmin(resid, 40)),
#     ranked_occurrences=first(ranked_occurrences),
#   )  |> ungroup |> 
#   mutate(max_resid_censored=max(resid_censored), max_resid=max(resid))
# 
# 
# 
# plot_exp_dist <- plot_data |> 
#   ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
#   geom_tile(aes(fill=expected), colour="#707070", size=.2) +
#   scale_fill_distiller(palette="Blues", direction=1) +
#   facet_wrap(~crash_quintile, nrow=1) +
#   guides(fill=FALSE) +
#   theme_v_gds() +
#   labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Expected") +
#   theme(axis.text.x=element_blank(), axis.text.y = element_blank(), 
#         axis.title.x = element_blank(), axis.title.y = element_blank()) +
#   coord_equal()
# 
# 
# plot_row_total_dist <- plot_data |> 
#   ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
#   geom_tile(aes(fill=row_total/ranked_occurrences), colour="#707070", size=.2) +
#   scale_fill_distiller(palette="Blues", direction=1) +
#   facet_wrap(~crash_quintile, nrow=1) +
#   guides(fill=FALSE) +
#   theme_v_gds() +
#   labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Row total") +
#   theme(axis.text.x=element_blank(), axis.text.y = element_blank(), 
#         axis.title.x = element_blank(), axis.title.y = element_blank()) +
#   coord_equal()
# 
# 
# plot_col_total_dist <- plot_data |> 
#   ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
#   geom_tile(aes(fill=col_total), colour="#707070", size=.2) +
#   scale_fill_distiller(palette="Blues", direction=1) +
#   facet_wrap(~crash_quintile, nrow=1) +
#   guides(fill=FALSE) +
#   theme_v_gds() +
#   labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Col total") +
#   theme(axis.text.x=element_blank(), axis.text.y = element_blank(), 
#         axis.title.x = element_blank(), axis.title.y = element_blank()) +
#   coord_equal()
# 
# plot_resid_dist <-  plot_data |> 
#   ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
#   geom_tile(aes(fill=resid), colour="#707070", size=.2) +
#   scale_fill_distiller(
#     palette="PRGn", direction=1, 
#     limits=c(-max(plot_data$max_resid), max(plot_data$max_resid))
#   )+
#   facet_wrap(~crash_quintile, nrow=1) +
#   theme_v_gds() +
#   labs(subtitle="Obs vs Exp", x="Null: no difference in proportional distribution in 'social distance' by crash location") +
#   theme(
#     axis.text.x=element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
#     axis.title.x = element_text(size=10, hjust=1, family="Roboto Condensed Light")
#   ) + 
#   coord_equal()
# 
# 
# plot_dist <- demog_distances |> 
#   ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
#   geom_tile(aes(fill=demog_dist), colour="#707070", size=.2) +
#   scale_fill_distiller(palette="Blues", direction=1) +
#   facet_wrap(~crash_quintile, nrow=1) +
#   guides(fill=FALSE) +
#   theme_v_gds() +
#   labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Demographic distance") +
#   theme(axis.text.x=element_blank(), axis.text.y = element_blank(), 
#         axis.title.x = element_blank(), axis.title.y = element_blank())
# 
# +
#   coord_equal()
# 
# plot <- plot_dist + plot_annotation(title="IMD quintile of homeplace of pedestrian and driver, grouped by IMD class of crash location",
#                                                          subtitle="--'Geodemographic distance' variable is mapped to each cell",
#                                                          caption="Stats19 data accessed via `stats19` package",
#                                                          theme = theme_v_gds())
# 
# ggsave(filename="./static/class/04-class_files/imd-geodemog-dist.png", plot=plot,width=9, height=3.2, dpi=300)
# 
# 
# 
# plot <- plot_imd_driver_area_obs + plot_resid_dist + plot_spacer()+theme_v_gds() +
#   plot_exp_dist + plot_row_total_dist + plot_col_total_dist +  plot_layout(nrow=6) +
#   plot_layout(heights=c(1,1,.1,.8,.8,.8)) +
#   plot_annotation(title="Pedestrian casualties by IMD quintile of homeplace of pedestrian and driver",
#                   subtitle="--Grouped and compared by IMD quintile of crash location",
#                   caption="Stats19 data accessed via `stats19` package",
#                   theme = theme_v_gds())
# 
# 
# 
# ggsave(filename="./static/class/04-class_files/imd-driver-cas-crash-dist-prop.png", plot=plot,width=9, height=14, dpi=300)
# 
# 
# 

# plot_imd_driver_area <- ped_veh |> 
#   filter(
#     !is.na(casualty_imd_decile), !is.na(driver_imd_decile),
#     casualty_imd_decile!="Data missing or out of range", 
#     driver_imd_decile!="Data missing or out of range", !is.na(crash_quintile)) |>   
#   mutate(grand_total=n()) |> 
#   group_by(casualty_imd_quintile, driver_imd_quintile) |> 
#   mutate(row_total=n()) |> ungroup |> 
#   group_by(crash_quintile) |> 
#   mutate(col_total=n()) |> ungroup |> 
#   group_by(casualty_imd_quintile, driver_imd_quintile, crash_quintile) |> 
#   summarise(
#     observed=n(), 
#     row_total=first(row_total), 
#     col_total=first(col_total),
#     grand_total=first(grand_total),
#     expected=(row_total*col_total)/grand_total,
#     resid=(observed-expected)/sqrt(expected)
#   ) |> 
#   # Censor max values to 40.
#   mutate(resid=if_else(resid>0, pmin(resid, 40), pmax(resid, -40))) |> 
#   ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
#   geom_tile(aes(fill=resid), colour="#707070", size=.2) +
#   scale_fill_distiller(palette="PRGn", direction=1, limits=c(-40,40)) +
#   facet_wrap(~crash_quintile, nrow=1) +
#   theme_v_gds() +
#   labs(x="IMD quintile of casualty", y="IMD quintile of driver", title="Obs vs Exp", subtitle="--Null: distribute in cells independently of crash location") +
#   theme(axis.text.x=element_blank(), axis.text.y = element_blank(), 
#         axis.title.x = element_blank(), axis.title.y = element_blank())+
#   coord_equal()


plot_layout(ncol=2, nrow=2, widths=c(1.5,5), heights = c(1,1)) +
  plot_annotation(title="Pedestrian casualties by IMD quintile of homeplace of pedestrian and driver",
                  subtitle="--Grouped and compared by IMD quintile of crash location",
                  caption="Stats19 data accessed via `stats19` package",
                  theme = theme_v_gds())


ggsave(filename="./static/class/04-class_files/imd-driver-cas.png", plot=plot,width=12, height=7, dpi=300)


######### March 3rd Revised cell probabilities


demog_distances <- ped_veh |>
  filter(
    !is.na(casualty_imd_decile), !is.na(driver_imd_decile),
    casualty_imd_decile!="Data missing or out of range",
    driver_imd_decile!="Data missing or out of range", !is.na(crash_quintile)) |>
  mutate(
    across(
      c(casualty_imd_quintile, driver_imd_quintile, crash_quintile),
      .fns=list(num=~as.numeric(factor(., levels=c(c("1 most deprived", "2 more deprived", "3 mid deprived", "4 less deprived", "5 least deprived")))))
    ),
    demog_dist=sqrt(
      (casualty_imd_quintile_num-driver_imd_quintile_num)^2 +
        (casualty_imd_quintile_num-crash_quintile_num)^2 +
        (driver_imd_quintile_num-crash_quintile_num)^2
    )
  ) |>
  group_by(demog_dist, casualty_imd_quintile, driver_imd_quintile, crash_quintile) |>
  summarise(crash_count=n()) |>  ungroup() |>
  # We want to know how often each demographic distance *occurs*.
  group_by(demog_dist, casualty_imd_quintile, driver_imd_quintile, crash_quintile) |>
  mutate(occurrences=n()) |> ungroup |>
  mutate(total_crashes=sum(crash_count)) |>
  # Then for each demographic distance, compute counts and bin probabilities
  # that are to be spread out to cells of matrix (so we devide by number of times that those distances exist).
  group_by(demog_dist) |>
  mutate(crash_count_dist=sum(crash_count), occurrences=sum(occurrences), bin_prob=(crash_count_dist/total_crashes)/occurrences) |> ungroup() |>
  # For expected counts we need to weight according to size of quintile (crashes). But as there is an association between geodemographic distance
  # and counts, we need to additionally weight according to the sum of the bin probabilities.
  group_by(crash_quintile) |>
  mutate(prop=sum(crash_count)/total_crashes, weight=prop/sum(bin_prob)) |> ungroup()


# So we can estimate the probability of crashes occurring in each bin, weighting
# on the number of times those bins appear overall. These are our *global probabilities*.
demog_distances |> select(demog_dist, crash_count, occurrences, bin_prob, weight) |> unique

# Our expected counts assume that counts are a function of demographic distance,
# but *not* the IMD class in which they occur. That is, expected counts distribute
# by geodemographic distance independently of the geodemographic quintile in which the
# crash occurred.
# For each cell in the matrix we arrive at a bin probability or likelihood.
# However, we need to *weight* according to the relative size of each quintile in terms of crashes and sum of geographic distance.
plot_data <-  ped_veh |>
  filter(
    !is.na(casualty_imd_decile), !is.na(driver_imd_decile),
    casualty_imd_decile!="Data missing or out of range",
    driver_imd_decile!="Data missing or out of range", !is.na(crash_quintile)) |>
  left_join(demog_distances) |>
  mutate(
    grand_total=n(),
    row_total=bin_prob,
  ) |> ungroup |>
  group_by(crash_quintile) |>
  mutate(col_total=n()) |> ungroup |>
  group_by(casualty_imd_quintile, driver_imd_quintile, crash_quintile) |>
  summarise(
    col_total=first(weight),
    observed=n(),
    row_total=first(row_total),
    col_total=first(col_total),
    grand_total=first(grand_total),
    #expected=(row_total/first(occurrences)*col_total)/grand_total,
    expected=(row_total*col_total)/grand_total,
    expected=(row_total*col_total)*grand_total,
    #expected=(row_total/first(occurrences)*col_total)/grand_total,
    resid=(observed-expected)/sqrt(expected),
    # Censor extreme effect sizes.
    resid_censored=if_else(resid<0, pmax(resid, -40), pmin(resid, 40)),
    occurrences=first(occurrences),
    distance=first(demog_dist),
    bin_prob=first(bin_prob),
    #prop=first(crash_prop),
    #crash_prop_adj=first(crash_prop_adj)
  )  |> ungroup |>
  mutate(max_resid_censored=max(resid_censored), max_resid=max(resid))

plot_data |> summarise(expected=sum(expected), observed=sum(observed), bin_prob=sum(bin_prob))

plot_data |>
  group_by(crash_quintile) |>
  summarise(expected=sum(expected), observed=sum(observed), diff=observed-expected, bin_prob=sum(bin_prob))


plot_social_dist <- demog_distances |>
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_tile(aes(fill=demog_dist), colour="#707070", size=.2) +
  scale_fill_distiller(palette="Blues", direction=1) +
  facet_wrap(~crash_quintile, nrow=1) +
  guides(fill=FALSE) +
  labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Geodemographic distance") +
  theme(axis.text.x=element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  coord_equal()


plot <- plot_social_dist +
  plot_annotation(
    title="Geodemographic distance between IMD quintile of homeplace of pedestrian, driver and crash location"
  )


ggsave(filename="./static/class/04-class_files/imd-geodemog-dist.png", plot=plot,width=11, height=3.4, dpi=300)




plot_obs_dist <- plot_data |>
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_tile(aes(fill=observed), colour="#707070", size=.2) +
  scale_fill_distiller(palette="Blues", direction=1) +
  facet_wrap(~crash_quintile, nrow=1) +
  #guides(fill=FALSE) +
  labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Observed") +
  theme(axis.text.x=element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  coord_equal()



plot_exp_dist <- plot_data |>
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_tile(aes(fill=expected), colour="#707070", size=.2) +
  scale_fill_distiller(palette="Blues", direction=1) +
  facet_wrap(~crash_quintile, nrow=1) +
  guides(fill=FALSE) +
  labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Expected") +
  theme(axis.text.x=element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  coord_equal()

plot_row_total_dist <- plot_data |>
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_tile(aes(fill=row_total), colour="#707070", size=.2) +
  scale_fill_distiller(palette="Blues", direction=1) +
  facet_wrap(~crash_quintile, nrow=1) +
  guides(fill=FALSE) +
  labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Cell probabilities") +
  theme(axis.text.x=element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  coord_equal()

plot_col_total_dist <- plot_data |>
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_tile(aes(fill=col_total), colour="#707070", size=.2) +
  scale_fill_distiller(palette="Blues", direction=1) +
  facet_wrap(~crash_quintile, nrow=1) +
  guides(fill=FALSE) +
  labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Quintile sizes") +
  theme(axis.text.x=element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  coord_equal()

plot_resid_dist <-  plot_data |>
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_tile(aes(fill=resid), colour="#707070", size=.2) +
  scale_fill_distiller(
    palette="PRGn", direction=1,
    limits=c(-max(plot_data$max_resid), max(plot_data$max_resid))
  )+
  facet_wrap(~crash_quintile, nrow=1) +
  labs(subtitle="Observed vs Expected", x="Null: distribute by 'geodemographic distance' independently of crash location") +
  theme(
    axis.text.x=element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
    axis.title.x = element_text(size=10, hjust=1, family="Avenir Book")
  ) +
  coord_equal()


plot <- plot_obs_dist + plot_resid_dist + plot_spacer() +
  plot_exp_dist + plot_row_total_dist + plot_col_total_dist +  plot_layout(nrow=6) +
  plot_layout(heights=c(1, 1,.1,.8,.8,.8)) +
  plot_annotation(title="Pedestrian casualties by IMD quintile of homeplace of pedestrian and driver",
                  subtitle="--Grouped and compared by IMD quintile of crash location",
                  caption="Stats19 data accessed via `stats19` package")


ggsave(filename="./static/class/04-class_files/imd-driver-cas-crash-dist-full.png", plot=plot,width=9, height=13, dpi=300)



# Bring in adjustments
t <- get_stats19_adjustments()
u <- ped_veh |> 
  left_join(
    t |> select(accident_index, Casualty_Reference, Adjusted_Serious, Adjusted_Slight),
    by=c("accident_index"="accident_index", "casualty_reference"="Casualty_Reference")
    ) |> 
  mutate(
    fatal=if_else(casualty_severity=="Fatal", 1,0),
    ksi=fatal+Adjusted_Serious,
    slight=Adjusted_Slight) |> 
  select(-c(Adjusted_Slight, Adjusted_Serious))

ggsave(filename="./static/class/04-class_files/ped-cas-year.png", plot=plot,width=9, height=4.5, dpi=300)


freq_by_year <- u |>
  mutate(
    year=lubridate::year(date)
  ) |>
  group_by(year) |>
  summarise(ksi=sum(ksi, na.rm=TRUE), slight=sum(slight, na.rm=TRUE))

min_slight <- min(freq_by_year$slight)
max_slight <- max(freq_by_year$slight)
range_slight <- max_slight-min_slight

plot_slight_year <- freq_by_year |>
  ggplot(aes(x=year, y=slight)) +
  geom_line(colour="#fee0d2", size=1.5) +
  geom_label(aes(label=scales::comma(slight, accuracy=1)), label.size = 0, family="Roboto Condensed", fill="#eeeeee") +
  scale_y_continuous(limits = c(min_slight-.05*range_slight,max_slight+.05*range_slight)) +
  scale_x_continuous(breaks=seq(2009, 2020, 1), position="top")+
  theme_v_gds()+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
        panel.grid.major.y=element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank())

  
min_ksi <- min(freq_by_year$ksi)
max_ksi <- max(freq_by_year$ksi)
range_ksi <- max_ksi-min_ksi
plot_ksi_year <- freq_by_year |>
  ggplot(aes(x=year, y=ksi)) +
  geom_line(colour="#de2d26", size=1.5) +
  geom_label(aes(label=scales::comma(ksi, accuracy=1)), label.size = 0, family="Roboto Condensed",fill="#eeeeee" ) +
  scale_y_continuous(limits = c(min_ksi,max_ksi+.85*range_ksi)) +
  scale_x_continuous(breaks=seq(2009, 2020, 1))+
  theme_v_gds()+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_blank(),
        panel.grid.major.y=element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank())


plot <- 
  plot_slight_year + plot_ksi_year + plot_layout(nrow=2) + 
  plot_annotation(
    title="Pedestrian casualties 2010-2019 by KSI (dark red) and slight (light red)",
    subtitle="--Severity classification using DfT adjustment",
    caption="Stats19 data accessed via `stats19` package",
    theme = theme_v_gds())






mosaic <- vehicle_pedestrians |> group_by(casualty_severity) |> 
  mutate(
    vehicle_type=factor(vehicle_type, levels=vehicle_order),
    severity=if_else(casualty_severity=="Slight", "Slight", "KSI")
  ) |> ungroup |> 
  select(vehicle_type, severity) |>
  mutate(vehicle_type=fct_rev(vehicle_type)) |> 
  ggplot() +
  geom_mosaic(aes(x=product(vehicle_type), fill=severity, colour=severity, divider = "vspine"), offset = 0.008, alpha=1)+
  scale_fill_manual(values=c("#de2d26","#fee0d2"))+
  theme_v_gds() +
  theme(panel.grid.major.y=element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) +
  coord_flip()

# annotate labels
plot_data <- ggplot_build(mosaic)$data |> as.data.frame() |>
  group_by(x__vehicle_type) |>
  summarise(xmin=min(xmin),xmax=max(xmax), ymin=min(ymin),ymax=max(ymax),
            count=sum(.wt))

mosaic_plot <- mosaic + 
  geom_text(
    data=plot_data, 
    aes(x=xmin+0.5*(xmax-xmin), y=ymin+0.5*(ymax-ymin),
        label=x__vehicle_type, size=count), family= "Roboto Condensed Regular", alpha=0.5)+
  scale_size(range = c(2, 13))+
  guides(size=FALSE)+
  theme_v_gds()+
  theme(legend.position = "right", axis.text=element_blank())+
  labs(y="prop severity", x="crash count", subtitle="mosaic plot")


plot<- bar_freq + bar_prop + mosaic_plot + plot_layout(widths=c(1,1,1.1)) +
  plot_annotation(title="Pedestrian casualties by severity and vehicle type",
                  subtitle="-- Stats19 crashes 2010-2019",
                  caption="Stats19 data accessed via `stats19` package",
                  theme = theme_v_gds())


ggsave(filename="./static/class/04-class_files/bars-assoc.png", plot=plot,width=11, height=4.5, dpi=300)

#