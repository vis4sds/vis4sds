###############################################################################
# Figures for vis4sds 
# Chapter 4
# Author: Roger Beecham
###############################################################################

library(tidyverse) 
library(here) 
library(patchwork)

library(stats19)
library(trafficalmr)
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
# S E S S I O N  4
###############################################################################

ped_veh <- 
  read_fst(here("data", "ped_veh.fst"))

plot_data <- ped_veh |> 
  filter(!is.na(age_of_casualty)) |>  sample_n(10000)

dots <- plot_data |> 
  ggplot(aes(age_of_casualty, y="1")) +
  geom_jitter(colour=site_colours$primary, fill=site_colours$primary, alpha=.05) +
  scale_x_continuous(limits=c(0,100)) +
  theme_v_gds() +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(),   panel.grid.major.y=element_blank(), panel.grid.minor = element_blank()) +
  labs(x="age of casualty") 

histogram <- plot_data |>   
  ggplot(aes(age_of_casualty)) +
  geom_histogram(colour=site_colours$primary, fill=site_colours$primary, alpha=.2) +
  scale_x_continuous(limits=c(0,100)) +
  theme_v_gds() +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),  axis.text= element_blank(),   panel.grid.major.y=element_blank(), panel.grid.minor = element_blank()) 


density <- plot_data |> 
  ggplot(aes(age_of_casualty)) +
  geom_density(colour=site_colours$primary, fill=site_colours$primary, alpha=.2) +
  scale_x_continuous(limits=c(0,100)) +
  theme_v_gds() +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),  axis.text= element_blank(),   panel.grid.major.y=element_blank(), panel.grid.minor = element_blank()) 

box_plot <- plot_data |>  
  ggplot(aes(age_of_casualty)) +
  geom_boxplot(colour=site_colours$primary, fill=site_colours$primary, alpha=.2) +
  scale_x_continuous(limits=c(0,100)) +
  theme_v_gds() +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),  axis.text= element_blank(),   panel.grid.major.y=element_blank(), panel.grid.minor = element_blank()) 

plot <-  box_plot + density + histogram + dots +  plot_layout(heights=c(.6, 2,2.2, .9), nrow=4) +
  plot_annotation(
    title="Plots of univariate distribution: age of casualty in Stats19 dataset",
    subtitle="-- Strip-plot, histogram, density plot, boxplot  |  mean 36 years - median 33 years - mode 21 years",
    caption="Stats19 data accessed via `stats19` package",
    theme = theme_v_gds())


ggsave(here("figs", "04", "univariate-plots.png"), plot=plot,width=8.5, height=6, dpi=300)
ggsave(here("figs", "04", "univariate-plots.svg"), plot=plot,width=8.5, height=6)


plot_data <- ped_veh |> 
  filter(!is.na(age_of_casualty)) |>  sample_n(100000)


order_type <- plot_data |> 
  group_by(vehicle_type) |>
  summarise(median=median(age_of_casualty)) |> arrange(median) |> pull(vehicle_type)


plot1 <- plot_data |> mutate(day=lubridate::wday(date, label=TRUE)) |>
  mutate(
    vehicle_type=factor(vehicle_type, levels=order_type)
  ) |> 
  select(age_of_casualty, age_of_driver, vehicle_type) |> 
  pivot_longer(cols=-vehicle_type, names_to="individual_type", values_to="age") |> 
  filter(age>0) |> 
  ggplot(aes(x=age, y=vehicle_type)) +
  geom_boxplot(fill=site_colours$primary,colour=site_colours$primary, alpha=.2, width=0.5) +
  scale_x_continuous(limits=c(0,100)) +
  labs(y="vehicle type", x="age of individual involved")+
  theme_v_gds() +
  theme( panel.grid.major.y=element_blank(), panel.grid.minor = element_blank())


plot2 <- plot_data |> mutate(day=lubridate::wday(date, label=TRUE)) |>
  mutate(
    vehicle_type=factor(vehicle_type, levels=order_type)
  ) |> 
  select(age_of_casualty, age_of_driver, vehicle_type) |> 
  pivot_longer(cols=-vehicle_type, names_to="individual_type", values_to="age") |> 
  filter(age>0) |> 
  ggplot(aes(x=age, y=vehicle_type)) +
  geom_boxplot(aes(fill=individual_type, colour=individual_type), alpha=.2) +
  scale_fill_manual(values=c("#e31a1c", "#1f78b4"))+
  scale_colour_manual(values=c("#e31a1c", "#1f78b4"))+
  scale_x_continuous(limits=c(0,100)) +
  #facet_wrap(~casualty_class) +
  labs(y="vehicle type", x="age of casualty")+
  theme_v_gds() +
  theme(axis.title.y = element_blank(), axis.text.y= element_blank(),   panel.grid.major.y=element_blank(), panel.grid.minor = element_blank())


plot <- plot1 + plot2 +  plot_annotation(
  title="Box plots of age of casualty by vehicle type, coloured by invdividual type",
  subtitle="-- Random sample of 100k Stats19 Pedestrian-Vehicle crashes",
  caption="Stats19 data accessed via `stats19` package",
  theme = theme_v_gds())

ggsave(filename=here("figs", "04", "boxplot-by-class.png"), plot=plot,width=8.5, height=6, dpi=300)
ggsave(filename=here("figs", "04", "boxplot-by-class.svg"), plot=plot,width=8.5, height=6)




plot_data_3 <- ped_veh |> 
  select(accident_index, casualty_type, age_of_casualty, age_of_driver, vehicle_type, datetime, urban_or_rural_area, casualty_imd_decile) |> 
  filter_at(vars(age_of_casualty, age_of_driver), ~ !is.na(.)) |> 
  group_by(accident_index) |> 
  mutate(age_of_driver=max(age_of_driver)) |> ungroup |> 
  filter(age_of_driver !=-1, age_of_casualty !=-1)  

plot_data_3 |> 
  mutate(hod=hour(datetime), 
         is_daytime=case_when(hod < 6 ~ "night",
                              hod < 19 ~ "day",
                              TRUE ~ "night")) |> filter(urban_or_rural_area!="Unallocated") |> 
  filter(vehicle_type=="Car", urban_or_rural_area=="Urban") |> 
  group_by(is_daytime) |> 
  sample_n(500) |> 
  ggplot(aes(x=age_of_driver, age_of_casualty)) +
  geom_point(colour=site_colours$primary, alpha=.2)+
  labs(x="age of driver", y="age of pedestrian") +
  facet_wrap(~is_daytime) +
  theme_v_gds()


plot_data_3 |> 
  mutate(hod=hour(datetime), 
         is_daytime=case_when(hod < 6 ~ "night",
                              hod < 19 ~ "day",
                              TRUE ~ "night")) |> filter(urban_or_rural_area!="Unallocated") |> 
  # filter(vehicle_type=="Car", urban_or_rural_area=="Urban") |> 
  filter(vehicle_type %in% c("Bicycle", "Motorcycle", "Taxi", "Bus"), is_daytime=="day") |> 
  group_by(vehicle_type) |> 
  sample_n(500) |> 
  summarise(cor(age_of_casualty, age_of_driver))


plot <- plot_data_3 |> 
  mutate(hod=hour(datetime), 
         is_daytime=case_when(hod < 6 ~ "night",
                              hod < 19 ~ "day",
                              TRUE ~ "night")) |> filter(urban_or_rural_area!="Unallocated") |> 
  # filter(vehicle_type=="Car", urban_or_rural_area=="Urban") |> 
  filter(vehicle_type %in% c("Bicycle", "Motorcycle", "Taxi", "Bus")) |> 
  group_by(vehicle_type) |> 
  sample_n(500) |> 
  ungroup() |> 
  mutate(vehicle_type=factor(vehicle_type, levels=c("Bicycle", "Motorcycle", "Bus", "Taxi"))) |> 
  ggplot(aes(x=age_of_driver, age_of_casualty)) +
  geom_point(colour=site_colours$primary, alpha=.2)+
  geom_smooth(method='lm', colour=site_colours$primary, fill=site_colours$primary, alpha=.1, se=FALSE)+
  labs(x="age of driver", y="age of pedestrian") +
  facet_wrap(~vehicle_type, nrow=1) +
  labs(
    title="Scatterplots of pedestrian age by driver age and grouped by vehicle type",
    subtitle="-- Random sample of Stats19 pedestrian-vehicle crashes stratified by vehicle type",
    caption="Stats19 data accessed via `stats19` package"
  )+
  theme_v_gds()


plot_data_3 |> 
  mutate(hod=hour(datetime), 
         is_daytime=case_when(hod < 6 ~ "night",
                              hod < 19 ~ "day",
                              TRUE ~ "night")) |> filter(urban_or_rural_area!="Unallocated") |> 
  # filter(vehicle_type=="Car", urban_or_rural_area=="Urban") |> 
  #filter(vehicle_type %in% c("Bicycle", "Motorcycle", "Taxi", "Bus")) |> 
  # filter(casualty_imd_decile==" Most deprived 10%") |> 
  group_by(casualty_imd_decile) |> 
  sample_n(500) |> 
  # ungroup() |> 
  mutate(vehicle_type=factor(vehicle_type, levels=c("Bicycle", "Motorcycle", "Bus", "Taxi"))) |> 
  ggplot(aes(x=age_of_driver, age_of_casualty)) +
  geom_point(colour=site_colours$primary, alpha=.2)+
  geom_smooth(method='lm', colour=site_colours$primary, fill=site_colours$primary, alpha=.1, se=FALSE)+
  labs(x="age of driver", y="age of pedestrian") +
  facet_wrap(~casualty_imd_decile, nrow=1) +
  labs(
    title="Scatterplots of pedestrian age by driver age and grouped by vehicle type",
    subtitle="-- Random sample of Stats19 pedestrian-vehicle crashes stratified by vehicle type",
    caption="Stats19 data accessed via `stats19` package"
  )+
  theme_v_gds()


ggsave(filename=here("figs", "04", "scatter-by-type.png"), plot=plot,width=11, height=4, dpi=300)
ggsave(filename=here("figs", "04", "scatter-by-type.svg"), plot=plot,width=11, height=4)

bar1 <- plot_data |> 
  sample_n(50000) |> 
  group_by(vehicle_type) |> 
  summarise(count=n()) |> 
  ggplot() +
  geom_col(aes(x=vehicle_type, y=count), fill=site_colours$primary)+
  theme_v_gds()+
  labs(x="vehicle type", y="crash count")+
  theme(panel.grid.major.x=element_blank(), panel.grid.minor = element_blank())


bar2 <- plot_data |>
  sample_n(50000) |> 
  group_by(vehicle_type) |> 
  summarise(count=n()) |> 
  ggplot() +
  geom_col(aes(x=vehicle_type, y=count), fill=site_colours$primary)+
  theme_v_gds()+
  labs(x="vehicle type", y="crash count")+
  theme(panel.grid.major.y=element_blank(), panel.grid.minor = element_blank()) +
  coord_flip()

bar3 <- plot_data |> 
  sample_n(50000) |> 
  group_by(vehicle_type) |> 
  summarise(count=n()) |> 
  ggplot() +
  geom_col(aes(x=reorder(vehicle_type, count), y=count), fill=site_colours$primary)+
  theme_v_gds()+
  labs(x="vehicle type", y="crash count")+
  theme(panel.grid.major.y=element_blank(), panel.grid.minor = element_blank()) +
  coord_flip()


plot <- bar1 + bar2 +  bar3 + plot_annotation(
  title="Bars of crash frequencies by vehicle type",
  subtitle="-- Random sample of 50k Stats19 crashes",
  caption="Stats19 data accessed via `stats19` package",
  theme = theme_v_gds())

ggsave(filename=here("figs", "04", "bars.png"), plot=plot,width=11, height=4.5, dpi=300)
ggsave(filename=here("figs", "04", "bars.svg"), plot=plot,width=11, height=4.5)


ped_veh |> sample_n(50000) |> 
  select(speed_limit, road_type, light_conditions, weather_conditions, road_surface_conditions) |> 
  pivot_longer(cols=c(speed_limit:road_surface_conditions), names_to="context", values_to="context_value") |> 
  group_by(context, context_value) |> 
  summarise(count=n()) |> 
  filter(!is.na(context_value)) |> 
  ggplot(aes(x=count,y=context_value))+
  geom_segment(aes(x=0, y=context_value, xend=count, yend=context_value), colour=site_colours$primary)+
  geom_point(colour=site_colours$primary, fill=site_colours$primary, shape=21)+
  theme_v_gds()+
  # Facet the plot on country to display group freq by destination country.
  facet_grid(context~., scales="free_y", space="free_y")+
  theme(  strip.text.y = element_text(angle=0))

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
  geom_segment(aes(x=0, y=reorder(local_authority_district, count), xend=count, yend=reorder(local_authority_district, count)), colour=site_colours$primary, size=.2)+
  geom_point(colour=site_colours$primary, fill=site_colours$primary, shape=21)+
  theme_v_gds()+
  theme(panel.grid.major.y=element_blank(), strip.text = element_blank())+
  facet_grid(is_inner~., scales="free_y", space="free_y")+
  labs(x="crash count", y="")


borough_counts_day <- ped_veh |> filter(police_force == "Metropolitan Police" | police_force == "City of London", local_authority_district!="London Airport (Heathrow)") |> 
  mutate(
    day=lubridate::wday(datetime,label=TRUE),
    hod=lubridate::hour(datetime), 
    period=case_when(hod < 6 ~ "night",
                     hod < 10 ~ "am peak",
                     hod < 16 ~ "midday",
                     hod < 20 ~ "pm peak",
                     TRUE ~ "night"),
    period=factor(period, levels=c("am peak", "midday", "pm peak", "night")), 
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
  filter(!is.na(day), !is.na(hod)) |> 
  group_by(local_authority_district) |> 
  mutate(borough_count=n()) |> ungroup |> 
  group_by(local_authority_district, day, period) |> 
  summarise(count=n(), borough_count=first(borough_count), prop=count/borough_count, is_inner=first(is_inner)) |> ungroup |> 
  ggplot(aes(x=period,y=reorder(local_authority_district, borough_count), fill=count)) +
  geom_tile(colour="#707070", size=.2) +
  facet_grid(is_inner~day, scales="free_y", space="free_y") +
  scale_fill_distiller(palette="Blues", direction=1) +
  labs(y="") +
  theme_v_gds() +
  theme(
    axis.text.x = element_text(angle=270, hjust=0, size=8), panel.spacing.x = unit(-.1, "lines"),
    legend.position = "right", panel.grid.major.y=element_blank(),
    axis.text.y=element_blank()
  )

plot <-  borough_counts + borough_counts_day + plot_layout(widths = c(.8,2)) + 
  plot_annotation(
    title="Crash frequencies by London borough and period of day",
    subtitle="-- Stats19 crashes 2010-2019",
    caption="Stats19 data accessed via `stats19` package",
    theme = theme_v_gds())


ggsave(filename=here("figs", "04", "borough-freqs.png"), plot=plot,width=12, height=8, dpi=300)
ggsave(filename=here("figs", "04", "borough-freqs.svg"), plot=plot,width=12, height=8)




# Facet the plot on country to display group freq by destination country.
facet_grid(context~., scales="free_y", space="free_y")+
  theme(strip.text.y = element_text(angle=0))

# Vehicle-pedestrain crashes, casualties are pedestrians. 
# vehicle_pedestrians <- crashes_all |> 
#   left_join(casualties_all |> select(accident_index, age_of_casualty, casualty_type, casualty_imd_decile, casualty_severity)) |> 
#   left_join(vehicles_all |> select(accident_index, age_of_driver, vehicle_type)) |> 
#   select(accident_index, casualty_type, age_of_casualty, age_of_driver, vehicle_type, datetime, urban_or_rural_area, casualty_imd_decile, casualty_severity) |> 
#   group_by(accident_index) |> 
#   mutate(age_of_driver=max(age_of_driver), na.rm=TRUE) |> ungroup |>
#   filter(casualty_type=="Pedestrian")  

vehicle_order <- ped_veh |>
  group_by(vehicle_type) |> 
  summarise(count=n()) |> arrange(desc(count)) |> pull(vehicle_type)

vehicle_severity_cross <- #vehicle_pedestrians |>
  ped_veh |> 
  group_by(vehicle_type, casualty_severity) |> 
  summarise(count=n()) |> ungroup |> 
  mutate(`Vehicle type`=factor(vehicle_type, levels=vehicle_order)) |>  arrange(`Vehicle type`) |> 
  pivot_wider(names_from =  casualty_severity, values_from = count) |> select(-vehicle_type) |> rowwise() |> 
  mutate(KSI=sum(Fatal, Serious), Slight=Slight) |> select(`Vehicle type`, KSI, Slight) |> 
  mutate(`Row Total` = sum(KSI, Slight)) |> 
  ungroup()

col_totals <- vehicle_severity_cross |> summarise_at(vars(KSI:Slight), sum)

vehicle_severity_cross <- vehicle_severity_cross |> 
  add_row(`Vehicle type`= "Column Total", 
          KSI=col_totals |> pull(KSI), 
          Slight=col_totals |> pull(Slight))



write_csv(vehicle_severity_cross, here::here("static","csv","vehicle_severity_cross.csv"))

vehicle_severity_cross <- read_csv( here::here("static","csv","vehicle_severity_cross.csv"))

kableExtra::kbl(vehicle_severity_cross) |> row_spec(9, bold=T) |> column_spec(4,bold = T) 

vehicle_severity_cross |> select(-`Row Total`) |> 
  pivot_longer(cols=c(KSI:Slight), names_to="severity", values_to="count") |> 
  filter(`Vehicle type`!="Column Total") |> 
  mutate(vehicle_type=factor(`Vehicle type`, levels=vehicle_order)) |> 
  ggplot(aes(x=count, y=fct_rev(vehicle_type))) +
  geom_col(fill=site_colours$primary)+
  theme_v_gds()+
  labs(y="vehicle type", x="crash count")+
  theme(panel.grid.major.y=element_blank(), panel.grid.minor = element_blank())

bar_freq <- vehicle_severity_cross |> select(-`Row Total`) |> 
  pivot_longer(cols=c(KSI:Slight), names_to="severity", values_to="count") |> 
  filter(`Vehicle type`!="Column Total") |> 
  mutate(
    vehicle_type=factor(`Vehicle type`, levels=vehicle_order),
    severity=factor(severity, levels=c("Slight", "KSI"))) |>
  ggplot(aes(x=count, y=fct_rev(vehicle_type))) +
  geom_col(aes(fill=severity))+
  theme_v_gds()+
  scale_fill_manual(values=c("#fee0d2", "#de2d26"))+
  labs(y="", x="crash count")+
  guides(fill=FALSE)+
  theme(panel.grid.major.y=element_blank(), panel.grid.minor = element_blank())+
  labs(subtitle="stacked bar")


vehicle_severity_cross |> select(-`Row Total`) |> 
  pivot_longer(cols=c(KSI:Slight), names_to="severity", values_to="count") |> 
  filter(`Vehicle type`!="Column Total") |> 
  mutate(
    vehicle_type=factor(`Vehicle type`, levels=vehicle_order),
    severity=factor(severity, levels=c("Slight", "KSI"))) |> 
  pivot_wider(names_from=severity, values_from = count) |> 
  mutate(rate=KSI/(KSI+Slight))



bar_prop <- vehicle_severity_cross |> select(-`Row Total`) |> 
  pivot_longer(cols=c(KSI:Slight), names_to="severity", values_to="count") |> 
  filter(`Vehicle type`!="Column Total") |> 
  mutate(
    vehicle_type=factor(`Vehicle type`, levels=vehicle_order),
    severity=factor(severity, levels=c("Slight", "KSI"))) |>
  ggplot(aes(x=count, y=fct_rev(vehicle_type))) +
  geom_col(aes(fill=severity), position="fill")+
  annotate("segment", x=.24, xend=.24, y=0.4, yend=8.6, colour="#a50f15")+
  #geom_vline(xintercept=.245, colour="#de2d26")+
  #geom_label(aes(y=8.9, x=.245), label="expectation", hjust=0.5,vjust=1, family="Roboto Condensed Light", fill="#eeeeee", label.size = 0, size=3)+
  annotate("text", y=8.9, x=.24, label="expectation", hjust=0.5,vjust=1, family="Roboto Condensed Light", size=2.5, colour="#a50f15")+
  theme_v_gds()+
  scale_fill_manual(values=c("#fee0d2", "#de2d26"))+
  labs(y="", x="prop severity")+
  guides(fill=FALSE)+
  theme(panel.grid.major.y=element_blank(), panel.grid.minor = element_blank())+
  labs(subtitle="standardised bar")

devtools::install_github("haleyjeppson/ggmosaic")


mosaic <- #vehicle_pedestrians |> group_by(casualty_severity) |> 
  ped_veh |> group_by(casualty_severity) |> 
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


ggsave(filename=here("figs", "04", "bars-assoc.png"), plot=plot,width=11, height=4.5, dpi=300)
ggsave(filename=here("figs", "04", "bars-assoc.svg"), plot=plot,width=11, height=4.5)


vehicle_severity_cross <- readr::read_csv(here::here("static","csv","vehicle_severity_cross.csv"))
kbl(vehicle_severity_cross, caption = "Pedestrian casualties by vehicle involved and injury severity.") |> row_spec(9, bold=T) |> column_spec(4,bold = T)


# L O N D O N 

vehicle_order <- ped_veh |> filter(local_authority_district=="Westminster") |> 
  group_by(vehicle_type) |> 
  summarise(count=n()) |> arrange(desc(count)) |> pull(vehicle_type)


mosaic <- ped_veh |> filter(local_authority_district=="Westminster") |> 
  mutate(
    is_weekend=if_else(day_of_week %in% c("Saturday", "Sunday"), "weekend", "weekday"),
    vehicle_type=factor(vehicle_type, levels=vehicle_order)
    ) |> 
  select(vehicle_type, is_weekend) |>
  mutate(vehicle_type=fct_rev(vehicle_type)) |> 
  ggplot() +
  geom_mosaic(aes(x=product(vehicle_type), fill=is_weekend, colour=is_weekend, divider = "vspine"), offset = 0.008, alpha=1)+
  scale_fill_manual(values=c("#377eb8","#ff7f00"))+
  theme_v_gds() +
  theme(panel.grid.major.y=element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) +
  coord_flip()

# annotate labels
plot_data <- ggplot_build(mosaic)$data |> as.data.frame() |>
  group_by(x__vehicle_type) |>
  summarise(xmin=min(xmin),xmax=max(xmax), ymin=min(ymin),ymax=max(ymax),
            count=sum(.wt))

westminster  <- mosaic + 
  geom_text(
    data=plot_data, 
    aes(x=xmin+0.5*(xmax-xmin), y=ymin+0.5*(ymax-ymin),
        label=x__vehicle_type, size=count), family= "Roboto Condensed Regular", alpha=0.6)+
  scale_size(range = c(2, 13))+
  guides(size=FALSE)+
  theme_v_gds()+
  theme(legend.position = "right", axis.text=element_blank())+
  labs(title="Westminster", x="", y="")


vehicle_order <- ped_veh |> filter(local_authority_district=="Harrow") |> 
  group_by(vehicle_type) |> 
  summarise(count=n()) |> arrange(desc(count)) |> pull(vehicle_type)


mosaic <- ped_veh |> filter(local_authority_district=="Harrow") |> 
  mutate(
    is_weekend=if_else(day_of_week %in% c("Saturday", "Sunday"), "weekend", "weekday")) |> 
    #,
    #vehicle_type=fct_rev(vehicle_type)) |> 
  select(vehicle_type, is_weekend) |>
  mutate(vehicle_type=fct_rev(vehicle_type)) |> 
  ggplot() +
  geom_mosaic(aes(x=product(vehicle_type), fill=is_weekend, colour=is_weekend, divider = "vspine"), offset = 0.008, alpha=1)+
  scale_fill_manual(values=c("#377eb8","#ff7f00"))+
  theme_v_gds() +
  theme(panel.grid.major.y=element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) +
  coord_flip()


# annotate labels
plot_data <- ggplot_build(mosaic)$data |> as.data.frame() |>
  group_by(x__vehicle_type) |>
  summarise(xmin=min(xmin),xmax=max(xmax), ymin=min(ymin),ymax=max(ymax),
            count=sum(.wt))

harrow <- mosaic + 
  geom_text(
    data=plot_data, 
    aes(x=xmin+0.5*(xmax-xmin), y=ymin+0.5*(ymax-ymin),
        label=x__vehicle_type, size=count), family= "Roboto Condensed Regular", alpha=0.6)+
  scale_size(range = c(2, 13))+
  guides(size=FALSE)+
  theme_v_gds()+
  theme(legend.position = "right", axis.text=element_blank())+
  labs(title="Harrow", x="", y="")


plot <- westminster + harrow + 
  plot_annotation(title="Pedestrian casualties by vehicle type and period in week",
                  subtitle="--Stats19 crashes 2010-2019",
                  caption="Stats19 data accessed via `stats19` package",
                  theme = theme_v_gds())

ggsave(filename="./static/class/04-class_files/mosaic_harrow_westminster.png", plot=plot,width=10, height=5, dpi=300)



vehicle_order <- ped_veh |> filter(police_force=="Metropolitan Police" | police_force=="City of London") |> 
  group_by(vehicle_type) |> 
  summarise(count=n()) |> arrange(desc(count)) |> pull(vehicle_type)

ped_veh <- ped_veh |> mutate(vehicle_type=factor(vehicle_type, levels = vehicle_order))

# Upload london_squared layout: https://aftertheflood.com/projects/future-cities-catapult/
london_squared <- read_csv(here("static", "csv","london_squared.csv")) |> select(-panel)

london_squared <- ped_veh |> select(local_authority_district) |> 
  inner_join(london_squared, by=c("local_authority_district"="authority")) |>
  filter(!is.na(fX)) |>
  group_by(BOR, fX, fY, local_authority_district) |>
  summarise(bor_total=n()) |>
  ungroup()


t <- ped_veh |>
  inner_join(london_squared, by=c("local_authority_district"="authority")) |>
  filter(!is.na(fX)) |>
  group_by(BOR) |>
  mutate(bor_total=n()) |>
  ungroup() |>
  mutate(
    bor_total_rescale=scales::rescale(bor_total, to=c(0.2, 0.9),from=c( min(bor_total), max(bor_total))),
    severity=if_else(casualty_severity=="Slight", "Slight", "KSI")
  ) |>
  group_by(BOR, severity, vehicle_type) |>
  summarise(
    panel=first(panel),
    bor_total_rescale=first(bor_total_rescale),
    bor_total=first(bor_total)) |> ungroup |>
  arrange(panel)


# Create record of all combinations of vehicle_type and casualty_type.
vt <- t |> pull(vehicle_type) |>  unique
cs <- t |> pull(severity) |>  unique
bor <- t |> pull(BOR) |> unique
temp_t <- tibble(
  vehicle_type=rep(rep(vt,each=length(cs), times=1),times=33),
  severity=rep(rep(cs,each=length(vt), times=1),times=33),
  BOR=rep(bor, 16)
)

mosaic <- ped_veh |>
 left_join(london_squared) |>
 filter(!is.na(BOR)) |> 
  mutate(
    is_weekend=if_else(day_of_week %in% c("Saturday", "Sunday"), "weekend", "weekday"),
    vehicle_type=fct_rev(vehicle_type)) |> 
  ggplot() +
  geom_mosaic(aes(x=product(is_weekend, vehicle_type), fill=is_weekend, colour=is_weekend), offset = 0.008, alpha=1)+
  scale_fill_manual(values=c("#377eb8","#ff7f00"))+
  theme_v_gds() +
  facet_wrap(~BOR, ncol=7)+
  theme(
    panel.grid.major.y=element_blank(), panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank(),
    axis.title.y = element_blank(), axis.title.x = element_blank(), legend.position = "right"
    ) +
  coord_flip()

plot_data <- ggplot_build(mosaic)$data |> as.data.frame() |> 
  group_by(PANEL) |>
  mutate(bor_total=sum(.wt)) |> ungroup() |>
  group_by(PANEL, x__vehicle_type) |>
  summarise(xmin=min(xmin),xmax=max(xmax), ymin=min(ymin),ymax=max(ymax),
            vehicle_prop_bor=sum(.wt)/sum(bor_total), bor_total=min(bor_total)) |> ungroup() |> 
  left_join(london_squared)

borough_alpha <- mosaic + 
  geom_text(
    data=plot_data, 
    aes(
      x=xmin+0.5*(xmax-xmin), y=ymin+0.5*(ymax-ymin),label=x__vehicle_type, size=vehicle_prop_bor
      ), 
    alpha=.7,family="Roboto Condensed")+
  scale_size(range=c(0, 7))+
  scale_alpha(range=c(0.3,0.9))+
  guides(size=FALSE, alpha=FALSE)+
  #theme_v_gds()+
  theme(
    panel.grid.major.y=element_blank(), panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank(),
    axis.title.y = element_blank(), axis.title.x = element_blank(), 
    #legend.position = "right",
    panel.spacing=unit(-0.1, "lines"),
    legend.text=element_text(size=6), legend.key.size = unit(.4, 'cm'), legend.title = element_text(size=7)
  ) 


mosaic <- ped_veh |>
  inner_join(london_squared) |>
  filter(!is.na(BOR)) |> 
  mutate(
    is_weekend=if_else(day_of_week %in% c("Saturday", "Sunday"), "weekend", "weekday"),
    vehicle_type=fct_rev(vehicle_type)) |> 
  ggplot() +
  geom_mosaic(aes(x=product(is_weekend, vehicle_type), fill=vehicle_type, colour=vehicle_type, alpha=is_weekend), offset = 0.008)+
  scale_fill_brewer(palette="Set1", direction=-1)+
  scale_alpha_ordinal(range=c(.3,.9))+
  theme_v_gds() +
  labs(fill="vehicle type")+
  facet_wrap(~BOR, ncol=7)+
  guides(fill=guide_legend(reverse = TRUE))+
  theme(
    panel.grid.major.y=element_blank(), panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank(),
    axis.title.y = element_blank(), axis.title.x = element_blank(),
    legend.position = "right",
  ) +
  coord_flip()

borough_alpha_colour <- mosaic + 
  geom_text(
    data=plot_data, 
    aes(
      x=xmin+0.5*(xmax-xmin), y=ymin+0.5*(ymax-ymin),label=x__vehicle_type, size=vehicle_prop_bor
    ), 
    alpha=.7,family="Roboto Condensed")+
  scale_size(range=c(0, 7))+
  #scale_alpha(range=c(0.3,0.9))+
  guides(size=FALSE)+
  #theme_v_gds()+
  theme(
    panel.grid.major.y=element_blank(), panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank(),
    axis.title.y = element_blank(), axis.title.x = element_blank(), 
    #legend.position = "right",
    panel.spacing=unit(-0.1, "lines"),
    legend.text=element_text(size=6), legend.key.size = unit(.4, 'cm'), legend.title = element_text(size=7)
  ) 


borough_alpha + borough_alpha_colour

plot <- borough_alpha + borough_alpha_colour + plot_layout(nrow=2) +
  plot_annotation(title="Pedestrian casualties by vehicle type and period in week by London Borough",
                  subtitle="--Stats19 crashes 2010-2019",
                  caption="Stats19 data accessed via `stats19` package",
                  theme = theme_v_gds())

ggsave(filename="./static/class/04-class_files/mosaic_boroughs.png", plot=plot,width=9, height=13, dpi=300)

library(ggtext)
# Animate between two states.
displacement <- grid_real_sf |> 
  ggplot()+
  geom_sf(fill="#cfcfcf", colour="#9e9e9e", size=0.1)+
  coord_sf(crs=27700, datum=NA)+
  geom_text(aes(x=east, y=north, label=BOR), size=2, alpha=.7, show.legend=FALSE, family="Roboto Condensed")+
  annotate("text", x=558297.5+.8*38642.8, y=197713.7, label="")+
 # annotate("text", x=558297.5, y=159070.9-.15*38642.8 , label="See: github.com/aftertheflood/londonsquared", hjust=1, size=1.2, family="Roboto Condensed Light")+
  #annotate("text", x=558297.5, y=159070.9-.15*38642.8 , label="LondonSquared's After the Flood layout", hjust=1, size=1.2, family="Roboto Condensed Light")+
  transition_states(type, 1, 2)+
  labs(title="Demonsrating relaxed spatial layout of London Boroughs",
       subtitle="--LondonSquared's After the Flood layout",
 caption="See: github.com/aftertheflood/londonsquared")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(size=7), plot.subtitle = element_text(size=5),
        plot.caption = element_text(size=4)
        #plot.caption = element_markdown(size=4, margin=margin(t=-100,b=0,r=130,l=0, unit="pt"))
  )

#caption="<img src='./static/class/04-class_files/mosaic_boroughs_spatial.png'height='150' />")+

animate(displacement, duration=5, fps=10, width=886, height=640, res=300, renderer=gifski_renderer("./static/class/04-class_files/anim_real_grid.gif"))

8/6.5
#animate(displacement, duration=1, fps=1, width=1350, height=770, res=300, renderer=gifski_renderer("./static/class/04-class_files/anim_real_grid.gif"))

max(east) min(east) max(north) min(north)                       
1  558297.5  507258.2   197713.7   159070.9 

38642.8

mosaic <- ped_veh |>
  inner_join(london_squared, by=c("local_authority_district"="local_authority_district")) |>
  filter(!is.na(BOR)) |> 
  mutate(
    is_weekend=if_else(day_of_week %in% c("Saturday", "Sunday"), "weekend", "weekday"),
    vehicle_type=fct_rev(vehicle_type)) |> 
  ggplot() +
  geom_mosaic(aes(x=product(is_weekend, vehicle_type), fill=vehicle_type, colour=vehicle_type, alpha=is_weekend), offset = 0.008)+
  scale_fill_brewer(palette="Set1", direction=-1)+
  scale_alpha_ordinal(range=c(.3,.9))+
  theme_v_gds() +
  labs(fill="vehicle type")+
  facet_grid(-fY~fX)+
  guides(fill=guide_legend(reverse = TRUE))+
  theme(
    panel.grid.major.y=element_blank(), panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank(),
    axis.title.y = element_blank(), axis.title.x = element_blank(),
    legend.position = "right"
  ) +
  coord_flip()

plot_data <- ggplot_build(mosaic)$data |> as.data.frame() |>
  group_by(PANEL) |>
  mutate(bor_total=sum(.wt)) |> ungroup() |>
  group_by(PANEL, x__fill__vehicle_type) |>
  summarise(xmin=min(xmin),xmax=max(xmax), ymin=min(ymin),ymax=max(ymax),
            vehicle_prop_bor=sum(.wt)/sum(bor_total), bor_total=min(bor_total)) |> ungroup() |> 
  left_join(london_squared, by=c("bor_total"='bor_total'))


borough_alpha_colour <- mosaic + 
  geom_text(
    data=plot_data, 
    aes(
      x=xmin+0.5*(xmax-xmin), y=ymin+0.5*(ymax-ymin),label=x__fill__vehicle_type, size=vehicle_prop_bor
    ), 
    alpha=.7,family="Roboto Condensed")+
  scale_size(range=c(0, 7))+
  #scale_alpha(range=c(0.3,0.9))+
  guides(size=FALSE)+
  #theme_v_gds()+
  theme(
    panel.grid.major.y=element_blank(), panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank(),
    axis.title.y = element_blank(), axis.title.x = element_blank(), legend.position = "right",
    legend.text=element_text(size=6), legend.key.size = unit(.4, 'cm'), 
    legend.title = element_text(size=7), strip.text=element_blank(), strip.text.x = element_blank(), panel.grid=element_blank(),
    panel.spacing=unit(-0.2, "lines")
    )
   

plot <- borough_alpha_colour +
  plot_annotation(
  title="Pedestrian casualties by vehicle type and period of week",
       subtitle="--Relaxed spatial layout of London boroughs",
       caption="Stats19 data accessed via `stats19` package")

ggsave(filename="./static/class/04-class_files/mosaic_boroughs_spatial.png", plot=plot,width=8, height=6.5, dpi=350)



plot <- borough_alpha_colour + 
  plot_annotation(title="Pedestrian casualties by vehicle type and period in week by London Borough",
                  subtitle="--After the Flood's LondonSquared layout is used",
                  caption="Stats19 data accessed via `stats19` package",
                  theme = theme_v_gds())


ggsave(filename="./static/class/04-class_files/mosaic_boroughs_spatial.png", plot=plot,width=8, height=6.5, dpi=300)





grand_total <- vehicle_severity_cross |> 
  select(-c(`Row Total`)) |> 
  filter(`Vehicle type`!="Column Total") |> 
  pivot_longer(cols=c(KSI, Slight)) |> 
  summarise(grand_total=sum(value)) |> pull

vehicle_severity_cross_resids <- 
  vehicle_severity_cross |> mutate(row_total=`Row Total`) |> rowwise() |> 
  mutate(
    `KSI Exp`=(row_total*57918)/grand_total, 
    `Slight Exp`=(row_total*183742)/grand_total,
    `KSI Resid`=round((KSI-`KSI Exp`) / sqrt(`KSI Exp`),2), 
    `Slight Resid`=round((Slight-`Slight Exp`) / sqrt(`Slight Exp`),2),
    `KSI Exp`=round(`KSI Exp`,0), 
    `Slight Exp`=round(`Slight Exp`)
  ) |> select(-row_total) |> ungroup

write_csv(vehicle_severity_cross_resids, here::here("static","csv","vehicle_severity_cross_resids.csv"))

readr::read_csv(here::here("static","csv","vehicle_severity_cross_resids.csv")) |> 
  kbl(caption = "Pedestrian casualties by vehicle involved and injury severity: contingency table with signed chi-scores.") |>
  row_spec(9, bold=T) |> column_spec(4,bold = T) |>
  add_header_above(c(" ", "Observed" = 3, "Expected" = 2, "Signed chi-scores"=2))



counts_day <- crashes_all |> filter(police_force == "Metropolitan Police" | police_force == "City of London", local_authority_district!="London Airport (Heathrow)") |> 
  mutate(
    day=lubridate::wday(datetime,label=TRUE),
    hod=lubridate::hour(datetime), 
    period=case_when(hod < 6 ~ "night",
                     hod < 10 ~ "am peak",
                     hod < 16 ~ "midday",
                     hod < 20 ~ "pm peak",
                     TRUE ~ "night"),
    period=factor(period, levels=c("am peak", "midday", "pm peak", "night")), 
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
                                                     "Westminster",
                                                     "City of London"), "inner", "outer")
  ) |> 
  filter(!is.na(day), !is.na(hod)) |> 
  group_by(local_authority_district) |> 
  mutate(row_total=n()) |> ungroup |> 
  group_by(day) |> 
  mutate(col_total=n()) |> ungroup |> 
  mutate(grand_total=n()) |> 
  group_by(local_authority_district, day) |> 
  summarise(
    observed=n(), 
    row_total=first(row_total), 
    col_total=first(col_total),
    grand_total=first(grand_total),
    expected=(row_total*col_total)/grand_total,
    prop=observed/row_total,
    resid=(observed-expected)/sqrt(expected),
    is_inner=first(is_inner)) |> ungroup |> 
  ggplot(aes(x=day,y=reorder(local_authority_district, row_total), fill=observed)) +
  geom_tile(colour="#707070", size=.2) +
  facet_grid(is_inner~., scales="free_y", space="free_y") +
  scale_fill_distiller(palette="Blues", direction=1) +
  labs(y="", subtitle="Obs", fill="count") +
  theme_v_gds() +
  theme(
    axis.text.x = element_text(angle=270, hjust=0, size=8), panel.spacing.x = unit(-.1, "lines"),
    #legend.position = "right", 
    panel.grid.major.y=element_blank(),#,
    axis.title.x=element_blank(),
    #axis.text.y=element_blank()
    strip.text = element_blank(),
    plot.margin = unit(c(0,0,0,0), "cm"),
    legend.key.size = unit(.4, 'cm'),
    legend.text=element_text(size=4)
  )

resids_day <- crashes_all |> filter(police_force == "Metropolitan Police" | police_force == "City of London", local_authority_district!="London Airport (Heathrow)") |> 
  mutate(
    day=lubridate::wday(datetime,label=TRUE),
    hod=lubridate::hour(datetime), 
    period=case_when(hod < 6 ~ "night",
                     hod < 10 ~ "am peak",
                     hod < 16 ~ "midday",
                     hod < 20 ~ "pm peak",
                     TRUE ~ "night"),
    period=factor(period, levels=c("am peak", "midday", "pm peak", "night")), 
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
                                                     "Westminster",
                                                     "City of London"), "inner", "outer")
  ) |> 
  filter(!is.na(day), !is.na(hod)) |> 
  group_by(local_authority_district) |> 
  mutate(row_total=n()) |> ungroup |> 
  group_by(day) |> 
  mutate(col_total=n()) |> ungroup |> 
  mutate(grand_total=n()) |> 
  group_by(local_authority_district, day) |> 
  summarise(
    observed=n(), 
    row_total=first(row_total), 
    col_total=first(col_total),
    grand_total=first(grand_total),
    expected=(row_total*col_total)/grand_total,
    prop=observed/row_total,
    resid=(observed-expected)/sqrt(expected),
    is_inner=first(is_inner)) |> ungroup |> 
  ggplot(aes(x=day,y=reorder(local_authority_district, row_total), fill=resid)) +
  geom_tile(colour="#707070", size=.2) +
  facet_grid(is_inner~., scales="free_y", space="free_y") +
  scale_fill_distiller(palette="PRGn", direction=1, limits=c(-8.9,8.9)) +
  labs(y="", subtitle="Obs vs Exp", fill="resids") +
  theme_v_gds() +
  theme(
    axis.text.x = element_text(angle=270, hjust=0, size=8), panel.spacing.x = unit(-.1, "lines"),
    panel.grid.major.y=element_blank(),
    axis.text.y=element_blank(),
    axis.title.x=element_blank(),
    strip.text = element_blank(),
    plot.margin = unit(c(0,0,0,0), "cm"),
    legend.key.size = unit(.4, 'cm'),
    legend.text=element_text(size=4)
  )



counts_period <- crashes_all |> filter(police_force == "Metropolitan Police" | police_force == "City of London", local_authority_district!="London Airport (Heathrow)") |> 
  mutate(
    day=lubridate::wday(datetime,label=TRUE),
    hod=lubridate::hour(datetime), 
    period=case_when(hod < 6 ~ "night",
                     hod < 10 ~ "am peak",
                     hod < 16 ~ "midday",
                     hod < 20 ~ "pm peak",
                     TRUE ~ "night"),
    period=factor(period, levels=c("am peak", "midday", "pm peak", "night")), 
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
                                                     "Westminster",
                                                     "City of London"), "inner", "outer")
  ) |> 
  filter(!is.na(day), !is.na(hod)) |> 
  group_by(local_authority_district) |> 
  mutate(row_total=n()) |> ungroup |> 
  group_by(period) |> 
  mutate(col_total=n()) |> ungroup |> 
  mutate(grand_total=n()) |> 
  group_by(local_authority_district, period) |> 
  summarise(
    observed=n(), 
    row_total=first(row_total), 
    col_total=first(col_total),
    grand_total=first(grand_total),
    expected=(row_total*col_total)/grand_total,
    prop=observed/row_total,
    resid=(observed-expected)/sqrt(expected),
    is_inner=first(is_inner)) |> ungroup |> 
  ggplot(aes(x=period,y=reorder(local_authority_district, row_total), fill=observed)) +
  geom_tile(colour="#707070", size=.2) +
  facet_grid(is_inner~., scales="free_y", space="free_y") +
  scale_fill_distiller(palette="Blues", direction=1) +
  labs(y="", subtitle="Obs", fill="count") +
  theme_v_gds() +
  theme(
    axis.text.x = element_text(angle=270, hjust=0, size=8), panel.spacing.x = unit(-.1, "lines"),
    #legend.position = "right", 
    panel.grid.major.y=element_blank(),
    axis.text.y=element_blank(),
    axis.title.x=element_blank(),
    strip.text = element_blank(),
    plot.margin = unit(c(0,0,0,0), "cm"),
    legend.key.size = unit(.4, 'cm'),
    legend.text=element_text(size=4)
  )

resids_period <- crashes_all |> filter(police_force == "Metropolitan Police" | police_force == "City of London", local_authority_district!="London Airport (Heathrow)") |> 
  mutate(
    day=lubridate::wday(datetime,label=TRUE),
    hod=lubridate::hour(datetime), 
    period=case_when(hod < 6 ~ "night",
                     hod < 10 ~ "am peak",
                     hod < 16 ~ "midday",
                     hod < 20 ~ "pm peak",
                     TRUE ~ "night"),
    period=factor(period, levels=c("am peak", "midday", "pm peak", "night")), 
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
                                                     "Westminster",
                                                     "City of London"), "inner", "outer")
  ) |> 
  filter(!is.na(day), !is.na(hod)) |> 
  group_by(local_authority_district) |> 
  mutate(row_total=n()) |> ungroup |> 
  group_by(period) |> 
  mutate(col_total=n()) |> ungroup |> 
  mutate(grand_total=n()) |> 
  group_by(local_authority_district, period) |> 
  summarise(
    observed=n(), 
    row_total=first(row_total), 
    col_total=first(col_total),
    grand_total=first(grand_total),
    expected=(row_total*col_total)/grand_total,
    prop=observed/row_total,
    resid=(observed-expected)/sqrt(expected),
    is_inner=first(is_inner)) |> ungroup |> 
  ggplot(aes(x=period,y=reorder(local_authority_district, row_total), fill=resid)) +
  geom_tile(colour="#707070", size=.2) +
  facet_grid(is_inner~., scales="free_y", space="free_y") +
  scale_fill_distiller(palette="PRGn", direction=1, limits=c(-11.8,11.8)) +
  labs(y="", subtitle="Obs vs Exp", fill="resids") +
  theme_v_gds() +
  theme(
    axis.text.x = element_text(angle=270, hjust=0, size=8), panel.spacing.x = unit(-.1, "lines"),
    axis.title.x=element_blank(),
    panel.grid.major.y=element_blank(),
    axis.text.y=element_blank(),
    strip.text = element_blank(),
    plot.margin = unit(c(0,0,0,0), "cm"),
    legend.key.size = unit(.4, 'cm'),
    legend.text=element_text(size=4)
  )


plot <- counts_day + resids_day + counts_period + resids_period +
  plot_layout(widths = c(1.7,1.7,1,1), nrow=1) +
  plot_annotation(
    title="Crash frequencies by London borough and day of week | period of day",
    subtitle="-- Stats19 crashes 2010-2019",
    caption="Stats19 data accessed via `stats19` package",
    theme = theme_v_gds()
  )


ggsave(filename="./static/class/04-class_files/borough-freqs-resids.png", plot=plot,width=8, height=8, dpi=300)



# Technical element -- analysis

# Adjusted KSI data
plot <- u |>
  # Crashes in Wales are excluded as English IMD
  inner_join(imd |> select(LSOA11CD, crash_quintile), by=c("lsoa_of_accident_location"="LSOA11CD")) |>
  mutate(
    year=lubridate::year(date)
  ) |>
  group_by(year,  crash_quintile) |>
  summarise(ksi=sum(ksi, na.rm=TRUE), slight=sum(slight, na.rm=TRUE), 
            total=ksi+slight) |> ungroup() |> 
  mutate(
    crash_quintile=factor(
      crash_quintile, levels=c("5 least deprived","4 less deprived","3 mid deprived", "2 more deprived", "1 most deprived")
    )
  )|> 
  #pivot_longer(cols=c(ksi,slight), names_to="severity", values_to="counts") |> 
  ggplot(aes(x=year, y=total)) +
  geom_col(aes(fill=crash_quintile),colour="#8d8d8d", size=.1 ) +
  scale_fill_brewer(palette = "Blues", type="seq") +
  scale_x_continuous(breaks=seq(2009, 2020,1))+
  labs(x="year", y="crash count", fill="IMD of crash location",
       title="Pedestrian casualties 2010-2019 by IMD of crash location",
       subtitle="--Stacked bars are used to compare IMD class",
       caption="Stats19 data accessed via `stats19` package")+
  #facet_wrap(~crash_quintile, nrow=1)+
  theme_v_gds()




plot <- ped_veh |> 
  mutate(
    year_month=floor_date(date, "month")
  ) |>
  group_by(year_month,  crash_quintile) |>
  summarise(total=n()) |> 
  mutate(
    crash_quintile=factor(
      crash_quintile, levels=c("5 least deprived","4 less deprived","3 mid deprived", "2 more deprived", "1 most deprived")
    )
  ) |> 
  ggplot(aes(x=year_month, y=total)) +
  geom_col(aes(fill=crash_quintile),colour="#8d8d8d", size=.1 ) +
  scale_fill_brewer(palette = "Blues", type="seq") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m")+
  labs(x="month-on-month", y="crash count", fill="IMD of crash location",
       title="Pedestrian casualties 2010-2019 by IMD of crash location",
       subtitle="--Stacked bars are used to compare IMD class",
       caption="Stats19 data accessed via `stats19` package")+
  #facet_wrap(~crash_quintile, nrow=1)+
  theme_v_gds() +
  theme(axis.text.x = element_text(angle=90))

ggsave(filename="./static/class/04-class_files/plot-year-imd.png", plot=plot,width=10, height=5.5, dpi=300)


dates <- ped_veh |> 
  mutate(
    year_month=floor_date(date, "month"),
    year_month_format=format(as.Date(year_month), "%Y-%m")
  ) |>
  select(year_month, year_month_format) |> unique() |> arrange(year_month) |> pull(year_month_format)

selected_dates <- dates[seq(1, length(dates), by=24)]

plot1 <- ped_veh |> 
  mutate(
    year_month=floor_date(date, "month"),
    year_month=format(as.Date(year_month), "%Y-%m")
  ) |>
  group_by(year_month,  crash_quintile) |>
  summarise(total=n()) |> 
  mutate(
    crash_quintile=factor(
      crash_quintile, levels=c("5 least deprived","4 less deprived","3 mid deprived", "2 more deprived", "1 most deprived")
    ),
    crash_quintile=fct_rev(crash_quintile)
  ) |> 
  ggplot(aes(x=year_month, y=total)) +
  geom_col(fill=site_colours$primary, width=1) +
  scale_x_discrete(breaks=selected_dates) +
  #scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  facet_wrap(~crash_quintile)+
  theme_v_gds() 
  
  
plot2 <-  ped_veh |> 
  mutate(
    year_month=floor_date(date, "month"),
    year_month=format(as.Date(year_month), "%Y-%m")
  ) |>
  # mutate(
  #   year_month=floor_date(date, "month")
  # ) |>
  group_by(year_month,  crash_quintile) |>
  summarise(total=n()) |> 
  mutate(
    crash_quintile=factor(
      crash_quintile, levels=c("5 least deprived","4 less deprived","3 mid deprived", "2 more deprived", "1 most deprived")
    )
  ) |> 
  ggplot(aes(x=year_month, y=total)) +
  geom_col(fill=site_colours$primary, width=1) +
  #geom_col(aes(fill=crash_quintile),colour="#8d8d8d", size=.1 ) +
  scale_fill_brewer(palette = "Blues", type="seq", direction=1) +
  scale_x_discrete(breaks=selected_dates) +
  #scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  facet_grid(crash_quintile~., space="free_y", scales="free_y")+
  guides(fill=FALSE)+
  theme_v_gds()+
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(),
    axis.text.x=element_blank(),
    panel.grid.major.y=element_blank(), panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(),
    strip.text = element_text(size=5))
    

months <- ped_veh |> 
  mutate(
    month=month(date, label=TRUE)
  ) |>
  select(month) |> unique() |> pull(month)

selected_months <- months[seq(1, length(dates), by=3)]

plot <- ped_veh |> 
  mutate(
    year=year(date),
    month=month(date, label=TRUE),
  ) |>
  group_by(year, month,  crash_quintile) |>
  summarise(total=n()) |> ungroup() |> 
  group_by(year, crash_quintile) |> 
  mutate(month_avg=mean(total)) |> ungroup |> 
  mutate(
    crash_quintile=factor(
      crash_quintile, levels=c("5 least deprived","4 less deprived","3 mid deprived", "2 more deprived", "1 most deprived")
    )
  ) |> 
  ggplot(aes(x=month, y=total)) +
  geom_col(fill=site_colours$primary, width=1, alpha=.6)+
  geom_line(aes(y=month_avg, group=interaction(year, crash_quintile)), colour=site_colours$primary) +
  scale_x_discrete(breaks=c("Jan", "Apr", "Jul", "Oct")) +
  facet_grid(crash_quintile~year, scales="free_y")+
  theme_v_gds() +
  theme(panel.spacing.x = unit(-.01, "lines")) +
  labs(x="month-on-month", y="crash count", fill="IMD of crash location",
       title="Pedestrian casualties 2010-2019 by IMD of crash location",
       subtitle="--Horizontal line is monthly avg for the year, local scaling by IMD used",
       caption="Stats19 data accessed via `stats19` package")

ggsave(filename="./static/class/04-class_files/plot-monthyear-imd.png", plot=plot,width=11, height=6.5, dpi=300)

plot <- ped_veh |> 
  mutate(
    year=year(date),
    month=month(date, label=TRUE),
  ) |>
  group_by(year, month,  crash_quintile) |>
  summarise(total=n()) |> ungroup() |> 
  group_by(year, crash_quintile) |> 
  mutate(month_avg=mean(total)) |> ungroup |> 
  mutate(
    crash_quintile=factor(
      crash_quintile, levels=c("5 least deprived","4 less deprived","3 mid deprived", "2 more deprived", "1 most deprived")
    )
  ) |> 
  ggplot(aes(x=month, y=total)) +
  geom_col(fill=site_colours$primary, width=1, alpha=.6)+
  geom_line(aes(y=month_avg, group=interaction(year, crash_quintile)), colour=site_colours$primary) +
  scale_fill_brewer(palette = "Blues", type="seq", direction=1) +
  scale_x_discrete(breaks=c("Jan", "Apr", "Jul", "Oct")) +
  facet_grid(crash_quintile~year, scales="free_y")+
  theme_v_gds() +
  labs(x="month-on-month", y="crash count", fill="IMD of crash location",
       title="Pedestrian casualties 2010-2019 by IMD of crash location",
       subtitle="--Horizontal line is monthly avg for the year, local scaling by IMD used",
       caption="Stats19 data accessed via `stats19` package")

    
plot <- plot1 + plot2 + plot_layout(widths=c(4,1))+
  plot_annotation(
    title="Pedestrian casualties 2010-2019 by IMD of crash location",
    subtitle="--Bars are faceted to compare IMD class",
    caption="Stats19 data accessed via `stats19` package",
    theme=theme_v_gds())

ggsave(filename="./static/class/04-class_files/plot-year-imd-facet.png", plot=plot,width=10, height=6, dpi=300)


order_purpose <- ped_veh |> 
  mutate(
    year=year(date),
    driver_purpose=
           case_when(
             journey_purpose_of_driver == "Pupil riding to/from school" ~ "school_run",
             journey_purpose_of_driver == "Taking pupil to/from school" ~ "school_run",
             journey_purpose_of_driver == "Commuting to/from work" ~ "commute",
             journey_purpose_of_driver == "Journey as part of work" ~ "commute",
             journey_purpose_of_driver == "Other" ~ "other",
             TRUE ~ "not_provided"
           )
         ) |>
  filter(year>2010, driver_purpose != "not_provided") |> 
  group_by(driver_purpose) |> 
  summarise(total=n()) |> arrange(total) |> pull(driver_purpose)
  
           
plot <- ped_veh |> 
  mutate(
    year=year(date),
    month=month(date, label=TRUE),
    driver_purpose=
      case_when(
        journey_purpose_of_driver == "Pupil riding to/from school" ~ "school_run",
        journey_purpose_of_driver == "Taking pupil to/from school" ~ "school_run",
        journey_purpose_of_driver == "Commuting to/from work" ~ "commute",
        journey_purpose_of_driver == "Journey as part of work" ~ "commute",
        journey_purpose_of_driver == "Other" ~ "other",
        TRUE ~ "not_provided"
      )
  ) |>
  filter(year>2010, driver_purpose != "not_provided") |> 
  group_by(year, month,  crash_quintile, driver_purpose) |>
  summarise(total=n()) |> ungroup() |> 
  mutate(
    crash_quintile=factor(
      crash_quintile, levels=c("5 least deprived","4 less deprived","3 mid deprived", "2 more deprived", "1 most deprived")),
      driver_purpose=factor(driver_purpose, levels=c("other", "school_run", "commute"))
  ) |> 
  ggplot(aes(x=month, y=total, fill=driver_purpose)) +
  geom_col(width=1, alpha=.6)+
  scale_fill_brewer(palette="Set1")+
  #scale_fill_brewer(palette = "Blues", type="seq", direction=1) +
  scale_x_discrete(breaks=c("Jan", "Apr", "Jul", "Oct")) +
  facet_grid(crash_quintile~year, scales="free_y")+
  theme_v_gds() +
  theme(panel.spacing.x = unit(-.01, "lines")) +
  labs(x="month-on-month", y="crash count", fill="Journey purpose of driver",
       title="Pedestrian casualties 2010-2019 by IMD of crash location and journey purpose of driver",
       subtitle="--Colour is journey purpose of driver",
       caption="Stats19 data accessed via `stats19` package")


ggsave(filename="./static/class/04-class_files/plot-year-imd-facet-purpose.png", plot=plot,width=10, height=7, dpi=300)

ped_veh |> 
  mutate(
    year=year(date),
    month=month(date, label=TRUE),
    driver_purpose=
      case_when(
        journey_purpose_of_driver == "Pupil riding to/from school" ~ "school_run",
        journey_purpose_of_driver == "Taking pupil to/from school" ~ "school_run",
        journey_purpose_of_driver == "Commuting to/from work" ~ "commute",
        journey_purpose_of_driver == "Journey as part of work" ~ "commute",
        journey_purpose_of_driver == "Other" ~ "other",
        TRUE ~ "not_provided"
      )
    ) |>
  filter(year==2019, crash_quintile %in% c("1 most deprived", "5 least deprived")) |> 
  group_by(driver_purpose, crash_quintile) |> 
  summarise(count=n()) |> ungroup() |> 
  pivot_wider(names_from=crash_quintile, values_from=count) |> 
  summarise_at(vars(c(`1 most deprived`, `5 least deprived`)), sum)

      

ped_veh |> 
  mutate(
    year=year(date),
    month=month(date, label=TRUE),
    driver_purpose=
      case_when(
        journey_purpose_of_driver == "Pupil riding to/from school" ~ "school_run",
        journey_purpose_of_driver == "Taking pupil to/from school" ~ "school_run",
        journey_purpose_of_driver == "Commuting to/from work" ~ "commute",
        journey_purpose_of_driver == "Journey as part of work" ~ "commute",
        journey_purpose_of_driver == "Other" ~ "other",
        TRUE ~ "not_provided"
      )



plot <- u |>
  # Crashes in Wales are excluded as English IMD
  inner_join(imd |> select(LSOA11CD, crash_quintile), by=c("lsoa_of_accident_location"="LSOA11CD")) |>
  mutate(
    year=lubridate::year(date)
  ) |>
  filter(year>2010, journey_purpose_of_driver!="Not known") |> 
  mutate(journey_purpose_of_driver=factor(journey_purpose_of_driver, levels=order_purpose)) |>


plot1 <- u |>
  # Crashes in Wales are excluded as English IMD
  inner_join(imd |> select(LSOA11CD, crash_quintile), by=c("lsoa_of_accident_location"="LSOA11CD")) |>
  mutate(
    year=lubridate::year(date)
  ) |>
  group_by(year,  crash_quintile) |>
  summarise(ksi=sum(ksi, na.rm=TRUE), slight=sum(slight, na.rm=TRUE), 
            total=ksi+slight) |> ungroup() |> 
  mutate(
    crash_quintile=factor(
      crash_quintile, levels=c("5 least deprived","4 less deprived","3 mid deprived", "2 more deprived", "1 most deprived")),
    crash_quintile=fct_rev(crash_quintile)
  ) |> 
  #pivot_longer(cols=c(ksi,slight), names_to="severity", values_to="counts") |> 
  ggplot(aes(x=year, y=total)) +
  geom_col(aes(fill=crash_quintile),colour="#8d8d8d", size=.1 ) +
  scale_fill_brewer(palette = "Blues", type="seq", direction=-1) +
  scale_x_continuous(breaks=seq(2009, 2020,3))+
  facet_wrap(~crash_quintile, nrow=1)+
  labs(x="year", y="crash count", fill="IMD of crash location")+
  theme_v_gds()


plot2 <- u |>
  # Crashes in Wales are excluded as English IMD
  inner_join(imd |> select(LSOA11CD, crash_quintile), by=c("lsoa_of_accident_location"="LSOA11CD")) |>
  mutate(
    year=lubridate::year(date)
  ) |>
  group_by(year,  crash_quintile) |>
  summarise(ksi=sum(ksi, na.rm=TRUE), slight=sum(slight, na.rm=TRUE), 
            total=ksi+slight) |> ungroup() |> 
  mutate(
    crash_quintile=factor(
      crash_quintile, levels=c("5 least deprived","4 less deprived","3 mid deprived", "2 more deprived", "1 most deprived"))
  ) |> 
  #pivot_longer(cols=c(ksi,slight), names_to="severity", values_to="counts") |> 
  ggplot(aes(x=year, y=total)) +
  geom_col(aes(fill=crash_quintile),colour="#8d8d8d", size=.1 ) +
  scale_fill_brewer(palette = "Blues", type="seq", direction=1) +
  scale_x_continuous(breaks=seq(2009, 2020,3)) +
  facet_grid(crash_quintile~., space="free_y", scales="free_y")+
  guides(fill=FALSE)+
  theme_v_gds()+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
        panel.grid.major.y=element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
        strip.text = element_text(size=4.5), axis.text.x=element_blank())






plot <- u |>
  # Crashes in Wales are excluded as English IMD
  inner_join(imd |> select(LSOA11CD, crash_quintile), by=c("lsoa_of_accident_location"="LSOA11CD")) |>
  mutate(
    year=lubridate::year(date)
  ) |>
  #filter(urban_or_rural_area=="Urban") |> 
  group_by(year,  crash_quintile) |>
  summarise(ksi=sum(ksi, na.rm=TRUE), slight=sum(slight, na.rm=TRUE)) |>
  pivot_longer(cols=c(ksi,slight), names_to="severity", values_to="counts") |> 
  mutate(severity=factor(severity, levels=c("slight", "ksi"))) |> 
  ggplot(aes(x=year, y=counts)) +
  geom_col(aes(fill=severity), size=1.5, position="fill") +
  scale_fill_manual(values=c("#fee0d2","#de2d26"))+
  scale_x_continuous(breaks=seq(2009, 2020, 4))+
  facet_wrap(~crash_quintile, nrow=1)+
  labs(
    title="Pedestrian casualties 2010-2019 by severity and IMD of crash location",
    subtitle="--Standardised bars to compare proportions KSI by IMD class",
    caption="Stats19 data accessed via `stats19` package", y="proportions"
  )+
  theme_v_gds()

ggsave(filename="./static/class/04-class_files/plot-year-imd-facet-severity.png", plot=plot,width=10, height=5.5, dpi=300)



order_type <- u |> mutate(vehicle_type=as.character(vehicle_type)) |> 
  group_by(vehicle_type) |>
  summarise(total=n()) |> arrange(total) |> pull(vehicle_type)


order_purpose <- u |> 
  group_by(journey_purpose_of_driver) |> 
  summarise(total=n()) |> arrange(total) |> pull(journey_purpose_of_driver)

plot <- u |>
  # Crashes in Wales are excluded as English IMD
  inner_join(imd |> select(LSOA11CD, crash_quintile), by=c("lsoa_of_accident_location"="LSOA11CD")) |>
  mutate(
    year=lubridate::year(date)
  ) |>
  filter(year>2010, journey_purpose_of_driver!="Not known") |> 
  mutate(journey_purpose_of_driver=factor(journey_purpose_of_driver, levels=order_purpose)) |> 
  #filter(urban_or_rural_area=="Urban") |> 
  group_by(year,  crash_quintile, journey_purpose_of_driver) |>
  summarise(ksi=sum(ksi, na.rm=TRUE), slight=sum(slight, na.rm=TRUE), total=ksi+slight) |> ungroup |> 
  #mutate(vehicle_type=factor(vehicle_type, levels=order_type)) |> 
  ggplot(aes(x=year, y=total)) +
  geom_col(aes(fill=journey_purpose_of_driver), size=1.5, position="fill") +
  scale_fill_brewer(palette="Set1", direction=-1)+
  #scale_fill_manual(values=c("#fee0d2","#de2d26"))+
  scale_x_continuous(breaks=seq(2010, 2020, 4))+
  facet_wrap(~crash_quintile, nrow=1)+
  labs(
    title="Pedestrian casualties 2011-2019: journey purpose of driver by IMD of crash location",
    subtitle="--Standardised bars to compare proportions within-purpose by IMD class",
    caption="Stats19 data accessed via `stats19` package", y="proportions", fill="Driver journey purpose"
  )+
  theme_v_gds()


ggsave(filename="./static/class/04-class_files/plot-year-imd-purpose.png", plot=plot,width=10, height=5.5, dpi=300)


u |>
  # Crashes in Wales are excluded as English IMD
  inner_join(imd |> select(LSOA11CD, crash_quintile), by=c("lsoa_of_accident_location"="LSOA11CD")) |>
  mutate(
    year=lubridate::year(date)
  ) |>
  #filter(urban_or_rural_area=="Urban") |> 
  group_by(year,  crash_quintile, vehicle_type) |>
  summarise(ksi=sum(ksi, na.rm=TRUE), slight=sum(slight, na.rm=TRUE), total=ksi+slight) |> ungroup |> 
  mutate(vehicle_type=factor(vehicle_type, levels=order_type)) |> 
  ggplot(aes(x=year, y=total)) +
  geom_col(aes(fill=vehicle_type), size=1.5) +
  scale_fill_brewer(palette="Set1")+
  #scale_fill_manual(values=c("#fee0d2","#de2d26"))+
  scale_x_continuous(breaks=seq(2009, 2020, 4))+
  facet_grid(vehicle_type~crash_quintile, space="free_y", scales="free_y")+
  theme_v_gds()




plot_imd_driver <- ped_veh |> 
  filter(
    !is.na(casualty_imd_decile), !is.na(driver_imd_decile),
    casualty_imd_decile!="Data missing or out of range", 
    driver_imd_decile!="Data missing or out of range") |>   
  mutate(grand_total=n()) |> 
  # Crashes in Wales are excluded as English IMD
  group_by(casualty_imd_quintile, driver_imd_quintile) |> 
  summarise(
    count=n(), 
  ) |> 



+
  coord_equal()


plot_data <- ped_veh |> 
  filter(
    !is.na(casualty_imd_decile), !is.na(driver_imd_decile),
    casualty_imd_decile!="Data missing or out of range", 
    driver_imd_decile!="Data missing or out of range", !is.na(crash_quintile)) |>   
  mutate(grand_total=n()) |> 
  group_by(driver_imd_quintile) |> 
  mutate(row_total=n()) |> ungroup |> 
  group_by(casualty_imd_quintile) |> 
  mutate(col_total=n()) |> ungroup |> 
  group_by(casualty_imd_quintile, driver_imd_quintile) |> 
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
  ) |> ungroup


plot_imd_driver <- plot_data |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_point()+
  geom_tile(aes(fill=expected), colour="#707070", size=.2) +
  #annotate("segment", x=0.5,xend=5.5, y=0.2,yend=0.2, arrow=arrow(type="closed", length = unit(0.2, "cm")), size=.1) +
  annotate("text", x=5.5,y=0.2, label="least deprived", hjust=1, size=3, family="Roboto Condensed Light") +
  annotate("text", x=0.5,y=0.2, label="most deprived", hjust=0, size=3, family="Roboto Condensed Light") +
  annotate("text", y=5.5,x=0.2, label="least deprived", hjust=1, size=3, angle=90, family="Roboto Condensed Light") +
  annotate("text", y=0.5,x=0.2, label="most deprived", hjust=0, size=3, angle=90, family="Roboto Condensed Light") +
  annotate("text", y=5.5,x=0, label="", hjust=1, size=2) +
  annotate("text", x=6.5,y=0, label="", hjust=1, size=2) +
  annotate("text", y=3,x=5.8, label="IMD of driver", hjust=.5, vjust=0, size=3.5, family="Roboto Condensed Light", angle=270) +
  annotate("text", y=6,x=3, label="IMD of pedestrian", hjust=.5, vjust=1, size=3.5, family="Roboto Condensed Light") +
  scale_fill_distiller(palette="Blues", direction=1) +
  theme_v_gds() +
  labs(subtitle="Obvs") +
  theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    axis.text.x=element_blank(), 
    axis.text.y = element_blank(), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position="right")


plot_data |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_point()+
  geom_tile(aes(fill=observed), colour="#707070", size=.2) +
  scale_fill_distiller(palette="Blues", direction=1)

plot_data |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_point()+
  geom_tile(aes(fill=resid), colour="#707070", size=.2) +
  scale_fill_distiller(
    palette="PRGn", direction=1, limits=c(-max(plot_data$max_resid), max(plot_data$max_resid)))


plot_imd_expected <- plot_data |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_point()+
  geom_tile(aes(fill=expected), colour="#707070", size=.2) +
  #annotate("segment", x=0.5,xend=5.5, y=0.2,yend=0.2, arrow=arrow(type="closed", length = unit(0.2, "cm")), size=.1) +
  annotate("text", x=5.5,y=0.2, label="least deprived", hjust=1, size=3, family="Roboto Condensed Light") +
  annotate("text", x=0.5,y=0.2, label="most deprived", hjust=0, size=3, family="Roboto Condensed Light") +
  annotate("text", y=5.5,x=0.2, label="least deprived", hjust=1, size=3, angle=90, family="Roboto Condensed Light") +
  annotate("text", y=0.5,x=0.2, label="most deprived", hjust=0, size=3, angle=90, family="Roboto Condensed Light") +
  annotate("text", y=5.5,x=0, label="", hjust=1, size=2) +
  annotate("text", x=6.5,y=0, label="", hjust=1, size=2) +
  annotate("text", y=3,x=5.8, label="IMD of driver", hjust=.5, vjust=0, size=3.5, family="Roboto Condensed Light", angle=270) +
  annotate("text", y=6,x=3, label="IMD of pedestrian", hjust=.5, vjust=1, size=3.5, family="Roboto Condensed Light") +
  scale_fill_distiller(palette="Blues", direction=1) +
  theme_v_gds() +
  labs(subtitle="Expected") +
  theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    axis.text.x=element_blank(), 
    axis.text.y = element_blank(), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position="right")

plot_imd_marginal_row <- plot_data |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_point()+
  geom_tile(aes(fill=row_marginal), colour="#707070", size=.2) +
  #annotate("segment", x=0.5,xend=5.5, y=0.2,yend=0.2, arrow=arrow(type="closed", length = unit(0.2, "cm")), size=.1) +
  annotate("text", x=5.5,y=0.2, label="least deprived", hjust=1, size=3, family="Roboto Condensed Light") +
  annotate("text", x=0.5,y=0.2, label="most deprived", hjust=0, size=3, family="Roboto Condensed Light") +
  annotate("text", y=5.5,x=0.2, label="least deprived", hjust=1, size=3, angle=90, family="Roboto Condensed Light") +
  annotate("text", y=0.5,x=0.2, label="most deprived", hjust=0, size=3, angle=90, family="Roboto Condensed Light") +
  annotate("text", y=5.5,x=0, label="", hjust=1, size=2) +
  annotate("text", x=6.5,y=0, label="", hjust=1, size=2) +
  annotate("text", y=3,x=5.8, label="IMD of driver", hjust=.5, vjust=0, size=3.5, family="Roboto Condensed Light", angle=270) +
  annotate("text", y=6,x=3, label="IMD of pedestrian", hjust=.5, vjust=1, size=3.5, family="Roboto Condensed Light") +
  scale_fill_distiller(palette="Blues", direction=1) +
  theme_v_gds() +
  labs(subtitle="Row marginals") +
  theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    axis.text.x=element_blank(), 
    axis.text.y = element_blank(), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position="right")


plot_imd_expected<- plot_data |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_point()+
  geom_tile(aes(fill=expected), colour="#707070", size=.2) +
  #annotate("segment", x=0.5,xend=5.5, y=0.2,yend=0.2, arrow=arrow(type="closed", length = unit(0.2, "cm")), size=.1) +
  annotate("text", x=5.5,y=0.2, label="least deprived", hjust=1, size=3, family="Roboto Condensed Light") +
  annotate("text", x=0.5,y=0.2, label="most deprived", hjust=0, size=3, family="Roboto Condensed Light") +
  annotate("text", y=5.5,x=0.2, label="least deprived", hjust=1, size=3, angle=90, family="Roboto Condensed Light") +
  annotate("text", y=0.5,x=0.2, label="most deprived", hjust=0, size=3, angle=90, family="Roboto Condensed Light") +
  annotate("text", y=5.5,x=0, label="", hjust=1, size=2) +
  annotate("text", x=6.5,y=0, label="", hjust=1, size=2) +
  annotate("text", y=3,x=5.8, label="IMD of driver", hjust=.5, vjust=0, size=3.5, family="Roboto Condensed Light", angle=270) +
  annotate("text", y=6,x=3, label="IMD of pedestrian", hjust=.5, vjust=1, size=3.5, family="Roboto Condensed Light") +
  scale_fill_distiller(palette="Blues", direction=1) +
  theme_v_gds() +
  labs(subtitle="Expected") +
  theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    axis.text.x=element_blank(), 
    axis.text.y = element_blank(), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position="right")


plot_imd_marginal_col <- plot_data |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_point()+
  geom_tile(aes(fill=col_marginal), colour="#707070", size=.2) +
  #annotate("segment", x=0.5,xend=5.5, y=0.2,yend=0.2, arrow=arrow(type="closed", length = unit(0.2, "cm")), size=.1) +
  annotate("text", x=5.5,y=0.2, label="least deprived", hjust=1, size=3, family="Roboto Condensed Light") +
  annotate("text", x=0.5,y=0.2, label="most deprived", hjust=0, size=3, family="Roboto Condensed Light") +
  annotate("text", y=5.5,x=0.2, label="least deprived", hjust=1, size=3, angle=90, family="Roboto Condensed Light") +
  annotate("text", y=0.5,x=0.2, label="most deprived", hjust=0, size=3, angle=90, family="Roboto Condensed Light") +
  annotate("text", y=5.5,x=0, label="", hjust=1, size=2) +
  annotate("text", x=6.5,y=0, label="", hjust=1, size=2) +
  annotate("text", y=3,x=5.8, label="IMD of driver", hjust=.5, vjust=0, size=3.5, family="Roboto Condensed Light", angle=270) +
  annotate("text", y=6,x=3, label="IMD of pedestrian", hjust=.5, vjust=1, size=3.5, family="Roboto Condensed Light") +
  scale_fill_distiller(palette="Blues", direction=1) +
  theme_v_gds() +
  labs(subtitle="Col marginals") +
  theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    axis.text.x=element_blank(), 
    axis.text.y = element_blank(), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position="right")


col_marginals <- plot_data |> select(casualty_imd_quintile, col_total) |> unique() |> 
  ggplot(aes(x=casualty_imd_quintile, y=-col_total))+
  geom_col(fill="#aeaeae") +
  annotate("text", x=5.5,y=0, label="", hjust=1, size=2) +
  annotate("text", y=5.5,x=0, label="", hjust=1, size=2) +
  annotate("text", y=1000,x=3, label="IMD of pedestrian", hjust=0.5, size=3.5, family="Roboto Condensed Light") +
  theme_v_gds() +
  theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(),
    axis.text.x=element_blank(),
    panel.grid.major.y=element_blank(), panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank())

row_marginals <- plot_data |> select(driver_imd_quintile, row_total) |> unique() |> 
  ggplot(aes(x=driver_imd_quintile, y=row_total))+
  geom_col(fill="#aeaeae") +
  theme_v_gds() +
  annotate("text", x=5.5,y=0, label="", hjust=1, size=2) +
  annotate("text", y=5.5,x=0, label="", hjust=1, size=2) +
  annotate("text", y=-1300,x=3, label="IMD of driver", hjust=0.5, vjust=0, size=3.5, family="Roboto Condensed Light", angle=270) +
  coord_flip() +
  theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    axis.title.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.title.y = element_blank(), 
    axis.text.x=element_blank(),
    panel.grid.major.y=element_blank(), panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank())
    


plot_imd_driver_exp <- plot_data |> 
  # Censor max values to 40.
  #mutate(resid=if_else(resid>0, pmin(resid, 40), pmax(resid, -40))) |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_tile(aes(fill=expected), colour="#707070", size=.2) +
  scale_fill_distiller(palette="Blues", direction=1) +
  theme_v_gds() +
  annotate("text", y=5.5,x=0, label="", hjust=1, size=2) +
  annotate("text", x=6.5,y=0, label="", hjust=1, size=2) +
  annotate("text", y=3,x=5.8, label="IMD of driver", hjust=.5, vjust=0, size=3.5, family="Roboto Condensed Light", angle=270) +
  annotate("text", y=6,x=3, label="IMD of pedestrian", hjust=.5, vjust=1, size=3.5, family="Roboto Condensed Light") +
  annotate("text", x=5.5,y=0.2, label="high-driv:low-cas", hjust=1, size=3, family="Roboto Condensed Light") +
  annotate("text", x=0.5,y=0.2, label="high-driv:high-cas", hjust=0, size=3, family="Roboto Condensed Light") +
  annotate("text", y=5.5,x=0.2, label="low-driv:high-cas", hjust=1, size=3, angle=90, family="Roboto Condensed Light") +
  annotate("text", y=0.5,x=0.2, label="high-driv:high-cas", hjust=0, size=3, angle=90, family="Roboto Condensed Light") +
  labs(x="", subtitle="Exp") +
  theme(axis.text.x=element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_text(size=9, hjust=1, family="Roboto Condensed Light"), axis.title.y = element_blank())+
  coord_equal()


plot_imd_driver_exp <- plot_data |> 
  # Censor max values to 40.
  #mutate(resid=if_else(resid>0, pmin(resid, 40), pmax(resid, -40))) |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_tile(aes(fill=expected_share), colour="#707070", size=.2) +
  scale_fill_distiller(palette="Blues", direction=1) +
  theme_v_gds() +
  annotate("text", y=5.5,x=0, label="", hjust=1, size=2) +
  annotate("text", x=6.5,y=0, label="", hjust=1, size=2) +
  annotate("text", y=3,x=5.8, label="IMD of driver", hjust=.5, vjust=0, size=3.5, family="Roboto Condensed Light", angle=270) +
  annotate("text", y=6,x=3, label="IMD of pedestrian", hjust=.5, vjust=1, size=3.5, family="Roboto Condensed Light") +
  annotate("text", x=5.5,y=0.2, label="high-driv:low-cas", hjust=1, size=3, family="Roboto Condensed Light") +
  annotate("text", x=0.5,y=0.2, label="high-driv:high-cas", hjust=0, size=3, family="Roboto Condensed Light") +
  annotate("text", y=5.5,x=0.2, label="low-driv:high-cas", hjust=1, size=3, angle=90, family="Roboto Condensed Light") +
  annotate("text", y=0.5,x=0.2, label="high-driv:high-cas", hjust=0, size=3, angle=90, family="Roboto Condensed Light") +
  labs(x="", subtitle="Exp") +
  theme(axis.text.x=element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_text(size=9, hjust=1, family="Roboto Condensed Light"), axis.title.y = element_blank())+
  coord_equal()

plot_imd_driver_exp <- plot_data |> 
  # Censor max values to 40.
  #mutate(resid=if_else(resid>0, pmin(resid, 40), pmax(resid, -40))) |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_tile(aes(fill=expected_share), colour="#707070", size=.2) +
  scale_fill_distiller(palette="Blues", direction=1) +
  theme_v_gds() +
  annotate("text", y=5.5,x=0, label="", hjust=1, size=2) +
  annotate("text", x=6.5,y=0, label="", hjust=1, size=2) +
  annotate("text", y=3,x=5.8, label="IMD of driver", hjust=.5, vjust=0, size=3.5, family="Roboto Condensed Light", angle=270) +
  annotate("text", y=6,x=3, label="IMD of pedestrian", hjust=.5, vjust=1, size=3.5, family="Roboto Condensed Light") +
  annotate("text", x=5.5,y=0.2, label="high-driv:low-cas", hjust=1, size=3, family="Roboto Condensed Light") +
  annotate("text", x=0.5,y=0.2, label="high-driv:high-cas", hjust=0, size=3, family="Roboto Condensed Light") +
  annotate("text", y=5.5,x=0.2, label="low-driv:high-cas", hjust=1, size=3, angle=90, family="Roboto Condensed Light") +
  annotate("text", y=0.5,x=0.2, label="high-driv:high-cas", hjust=0, size=3, angle=90, family="Roboto Condensed Light") +
  labs(x="", subtitle="Exp") +
  theme(axis.text.x=element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_text(size=9, hjust=1, family="Roboto Condensed Light"), axis.title.y = element_blank())+
  coord_equal()


plot_imd_driver_resid <- plot_data |> 
  # Censor max values to 40.
  #mutate(resid=if_else(resid>0, pmin(resid, 40), pmax(resid, -40))) |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_tile(aes(fill=resid), colour="#707070", size=.2) +
  scale_fill_distiller(
    palette="PRGn", direction=1, limits=c(- max(plot_data$max_resid), max(plot_data$max_resid))) +
  theme_v_gds() +
  annotate("text", y=5.5,x=0, label="", hjust=1, size=2) +
  annotate("text", x=6.5,y=0, label="", hjust=1, size=2) +
  annotate("text", y=3,x=5.8, label="IMD of driver", hjust=.5, vjust=0, size=3.5, family="Roboto Condensed Light", angle=270) +
  annotate("text", y=6,x=3, label="IMD of pedestrian", hjust=.5, vjust=1, size=3.5, family="Roboto Condensed Light") +
  annotate("text", x=5.5,y=0.2, label="high-driv:low-cas", hjust=1, size=3, family="Roboto Condensed Light") +
  annotate("text", x=0.5,y=0.2, label="high-driv:high-cas", hjust=0, size=3, family="Roboto Condensed Light") +
  annotate("text", y=5.5,x=0.2, label="low-driv:high-cas", hjust=1, size=3, angle=90, family="Roboto Condensed Light") +
  annotate("text", y=0.5,x=0.2, label="high-driv:high-cas", hjust=0, size=3, angle=90, family="Roboto Condensed Light") +
  labs(x, subtitle="Obvs vs Exp - independent of driver-passenger IMD") +
  theme(axis.text.x=element_blank(), axis.text.y = element_blank(),  axis.title.y = element_blank(), legend.position="right", 
        axis.title.x=element_blank(), plot.margin = unit(c(0,0,0,0), "cm"))

+
  coord_equal()


plot <- 
  plot_imd_driver + row_marginals + plot_spacer()+theme_v_gds() +  plot_imd_driver_resid +
  col_marginals + plot_spacer()+theme_v_gds() + plot_spacer()+theme_v_gds() + plot_spacer()+theme_v_gds() +
  plot_layout(widths = c(1,.5, .1, 1), heights=c(1,.5)) +
  plot_annotation(
    title="Pedestrian casualties by IMD quintile of homeplace of pedestrian and driver",
    subtitle="--Grouped and compared by IMD quintile of crash location",
    caption="Stats19 data accessed via `stats19` package",
    theme = theme_v_gds()
  )


plot <- plot_imd_driver |  plot_imd_driver_resid + plot_spacer()+theme_v_gds() +
  plot_imd_expected + plot_imd_marginal_row + plot_imd_marginal_col +
  plot_annotation(
    title="Pedestrian casualties by IMD quintile of homeplace of pedestrian and driver",
    subtitle="--Grouped and compared by IMD quintile of crash location",
    caption="Stats19 data accessed via `stats19` package",
    theme = theme_v_gds()
  )

plot <- (plot_imd_driver |  plot_imd_driver_resid |  plot_spacer()+theme_v_gds()) / 
  (plot_spacer()+theme_v_gds() | plot_spacer()+theme_v_gds() | plot_spacer()+theme_v_gds()) / 
  (plot_imd_expected | plot_imd_marginal_row | plot_imd_marginal_col) +
  plot_layout(heights=c(1,.1,1)) +
  plot_annotation(
    title="Pedestrian casualties by IMD quintile of homeplace of pedestrian and driver",
    subtitle="--Grouped and compared by IMD quintile of crash location",
    caption="Stats19 data accessed via `stats19` package",
    theme = theme_v_gds()
  )


ggsave(filename="./static/class/04-class_files/imd-driver-cas-overall.png", plot=plot,width=11, height=6.9, dpi=300)



plot_data <- ped_veh |> 
  filter(
    !is.na(casualty_imd_decile), !is.na(driver_imd_decile),
    casualty_imd_decile!="Data missing or out of range", 
    driver_imd_decile!="Data missing or out of range", !is.na(crash_quintile)) |>   
  mutate(
    grand_total=n()
  ) |> 
  group_by(casualty_imd_quintile, driver_imd_quintile) |> 
  mutate(row_total=n()) |> ungroup |> 
  group_by(crash_quintile) |> 
  mutate(col_total=n()) |> ungroup |> 
  group_by(casualty_imd_quintile, driver_imd_quintile, crash_quintile) |> 
  summarise(
    observed=n(), 
    row_total=first(row_total), 
    col_total=first(col_total),
    grand_total=first(grand_total),
    expected=(row_total*col_total)/grand_total,
    resid=(observed-expected)/sqrt(expected),
    # Censor extreme effect sizes.
    resid_censored=if_else(resid<0, pmax(resid, -40), pmin(resid, 40)
    ))  |> ungroup |> 
  mutate(max_resid_censored=max(resid_censored), max_resid=max(resid))



plot_imd_driver_area_obs <- ped_veh |> 
  filter(
    !is.na(casualty_imd_decile), !is.na(driver_imd_decile),
    casualty_imd_decile!="Data missing or out of range", 
    driver_imd_decile!="Data missing or out of range") |>  
  group_by(crash_quintile, casualty_imd_quintile, driver_imd_quintile) |> 
  mutate(
    observed=n()
  ) |> 
  group_by(crash_quintile) |> 
  mutate(
    total_crash_quintile=n(),
    observed_max=max(observed),
    observed_rescaled=observed/observed_max
  ) |> ungroup() |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_tile(aes(fill=observed), colour="#707070", size=.2) +
  scale_fill_distiller(palette="Blues", direction=1) +
  facet_wrap(~crash_quintile, nrow=1) +
  theme_v_gds() +
  labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Observed") +
  theme(axis.text.x=element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  coord_equal()




plot_imd_driver_area_obs_local <- ped_veh |> 
  filter(
    !is.na(casualty_imd_decile), !is.na(driver_imd_decile),
    casualty_imd_decile!="Data missing or out of range", 
    driver_imd_decile!="Data missing or out of range") |>  
  group_by(crash_quintile, casualty_imd_quintile, driver_imd_quintile) |> 
  mutate(
    observed=n()
  ) |> 
  group_by(crash_quintile) |> 
  mutate(
    total_crash_quintile=n(),
    observed_max=max(observed),
    observed_rescaled=observed/observed_max
  ) |> ungroup() |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_tile(aes(fill=observed_rescaled), colour="#707070", size=.2) +
  scale_fill_distiller(palette="Blues", direction=1) +
  facet_wrap(~crash_quintile, nrow=1) +
  theme_v_gds() +
  labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Observed - local scaling") +
  theme(axis.text.x=element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank())

plot <- plot_imd_driver_area_obs_local + plot_annotation(title="Pedestrian casualties by IMD quintile of homeplace of pedestrian and driver",
                                                       subtitle="--Grouped and compared by IMD quintile of crash location",
                                                       caption="Stats19 data accessed via `stats19` package",
                                                       theme = theme_v_gds())

ggsave(filename="./static/class/04-class_files/imd-driver-cas-crash-local.png", plot=plot,width=9, height=4, dpi=300)



plot_driver_area_resid <-  plot_data |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_tile(aes(fill=resid), colour="#707070", size=.2) +
  scale_fill_distiller(
    palette="PRGn", direction=1, 
    limits=c(-max(plot_data$max_resid), max(plot_data$max_resid))
    )+
  facet_wrap(~crash_quintile, nrow=1) +
  theme_v_gds() +
  labs(subtitle="Obs vs Exp", x="Null: distribute in cells independently of crash location") +
  theme(
    axis.text.x=element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
    axis.title.x = element_text(size=10, hjust=1, family="Roboto Condensed Light")
    ) +
  coord_equal()
  


plot_driver_area_exp <-  plot_data |> 
  group_by(crash_quintile) |> 
  mutate(expected_rescaled=expected/max(expected)) |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_tile(aes(fill=expected), colour="#707070", size=.2) +
  scale_fill_distiller(palette="Blues", direction=1) +
  facet_wrap(~crash_quintile, nrow=1) +
  theme_v_gds() +
  guides(fill=FALSE) +
  labs(subtitle="Expected", x="") +
  theme(
    axis.text.x=element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
    axis.title.x = element_text(size=10, hjust=1, family="Roboto Condensed Light")
  ) +
  coord_equal()



plot_driver_area_row_total <-  plot_data |> 
  group_by(crash_quintile) |> 
  mutate(expected_rescaled=expected/max(expected)) |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_tile(aes(fill=row_total), colour="#707070", size=.2) +
  scale_fill_distiller(palette="Blues", direction=1) +
  facet_wrap(~crash_quintile, nrow=1) +
  theme_v_gds() +
  labs(subtitle="Row marginals", x="") +
  guides(fill=FALSE) +
  theme(
    axis.text.x=element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
    axis.title.x = element_text(size=10, hjust=1, family="Roboto Condensed Light")
  ) +
  coord_equal()


plot_driver_area_col_total <-  plot_data |> 
  group_by(crash_quintile) |> 
  mutate(expected_rescaled=expected/max(expected)) |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_tile(aes(fill=col_total), colour="#707070", size=.2) +
  scale_fill_distiller(palette="Blues", direction=1) +
  facet_wrap(~crash_quintile, nrow=1) +
  theme_v_gds() +
  labs(subtitle="Col marginals", x="") +
  guides(fill=FALSE) +
  theme(
    axis.text.x=element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
    axis.title.x = element_text(size=10, hjust=1, family="Roboto Condensed Light")
  ) +
coord_equal()


  
plot <- plot_imd_driver_area_obs + plot_driver_area_resid + plot_spacer()+theme_v_gds() +
  plot_driver_area_exp + plot_driver_area_row_total + plot_driver_area_col_total +  plot_layout(nrow=6) +
  plot_layout(heights=c(1,1,.1,.8,.8,.8)) +
  plot_annotation(title="Pedestrian casualties by IMD quintile of homeplace of pedestrian and driver",
                  subtitle="--Grouped and compared by IMD quintile of crash location",
                  caption="Stats19 data accessed via `stats19` package",
                  theme = theme_v_gds())


ggsave(filename="./static/class/04-class_files/imd-driver-cas-crash.png", plot=plot,width=9, height=14, dpi=300)
# 
# demog_distances <- ped_veh |> 
#   filter(
#     !is.na(casualty_imd_decile), !is.na(driver_imd_decile),
#     casualty_imd_decile!="Data missing or out of range", 
#     driver_imd_decile!="Data missing or out of range", !is.na(crash_quintile)) |>   
#   mutate(
#     across(
#       c(casualty_imd_quintile, driver_imd_quintile, crash_quintile), 
#       .fns=list(num=~as.numeric(factor(., levels=c(c("1 most deprived", "2 more deprived", "3 mid deprived", "4 less deprived", "5 least deprived")))))
#     ),
#     demog_dist=sqrt(
#       (casualty_imd_quintile_num-driver_imd_quintile_num)^2 +
#         (casualty_imd_quintile_num-crash_quintile_num)^2 +
#         (driver_imd_quintile_num-crash_quintile_num)^2
#     ),
#     ranked_dist=dense_rank(demog_dist)
#   ) |>
#   group_by(ranked_dist) |> summarise(count=n()) |> ungroup |> 
#   mutate(prop=count/sum(count))
# 
# 
# 
# 
# demog_distances <- ped_veh |> 
#   filter(
#     !is.na(casualty_imd_decile), !is.na(driver_imd_decile),
#     casualty_imd_decile!="Data missing or out of range", 
#     driver_imd_decile!="Data missing or out of range", !is.na(crash_quintile)) |>   
#   mutate(
#     across(
#       c(casualty_imd_quintile, driver_imd_quintile, crash_quintile), 
#       .fns=list(num=~as.numeric(factor(., levels=c(c("1 most deprived", "2 more deprived", "3 mid deprived", "4 less deprived", "5 least deprived")))))
#     ),
#     demog_dist=sqrt(
#       (casualty_imd_quintile_num-driver_imd_quintile_num)^2 +
#         (casualty_imd_quintile_num-crash_quintile_num)^2 +
#         (driver_imd_quintile_num-crash_quintile_num)^2
#     ),
#     ranked_dist=dense_rank(demog_dist)
#   ) |>
#   group_by(casualty_imd_quintile, driver_imd_quintile, crash_quintile, ranked_dist, demog_dist) |> 
#   summarise(crash_count=n()) |> ungroup |> 
#   group_by(ranked_dist, crash_quintile) |> mutate(ranked_occurrences=n()) |> ungroup |> 
#   group_by(ranked_dist) |> 
#     mutate(
#         crash_count_dist=sum(crash_count)
#     ) |> 
#   ungroup() |> 
#   mutate(crash_prop_dist=crash_count_dist/sum(crash_count))
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
# group_by(ranked_dist) |> 
#   mutate(row_total=n()) |> ungroup |> 
# group_by(crash_quintile) |> 
#   mutate(col_total=n()) |> ungroup |> 
# group_by(casualty_imd_quintile, driver_imd_quintile, crash_quintile) |> 
#   summarise(
#     observed=n(), 
#     row_total=first(row_total),
#     col_total=first(col_total),
#     grand_total=first(grand_total),
#     expected=(row_total/first(ranked_occurrences)*col_total)/grand_total,
#     resid=(observed-expected)/sqrt(expected),
#     # Censor extreme effect sizes.
#     resid_censored=if_else(resid<0, pmax(resid, -40), pmin(resid, 40)),
#     ranked_occurrences=first(ranked_occurrences),
#     distance=first(ranked_dist),
#     prop=first(crash_prop_dist),
#     prop_adjusted=prop/ranked_occurrences
#     )  |> ungroup |> 
#   mutate(max_resid_censored=max(resid_censored), max_resid=max(resid))
# 
# 
# plot_social_dist <- demog_distances |> 
#   ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
#   geom_tile(aes(fill=demog_dist), colour="#707070", size=.2) +
#   scale_fill_distiller(palette="Blues", direction=1) +
#   facet_wrap(~crash_quintile, nrow=1) +
#   guides(fill=FALSE) +
#   theme_v_gds() +
#   labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Expected") +
#   theme(axis.text.x=element_blank(), axis.text.y = element_blank(), 
#         axis.title.x = element_blank(), axis.title.y = element_blank()) +
#   coord_equal()
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
#   labs(subtitle="Obs vs Exp", x="Null: distribute by 'social distance' independently of crash location") +
#   theme(
#     axis.text.x=element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
#     axis.title.x = element_text(size=10, hjust=1, family="Roboto Condensed Light")
#   ) + 
#   coord_equal()
# 
# 
# demog_distances |> 
#   ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
#   geom_tile(aes(fill=demog_dist), colour="#707070", size=.2) +
#   scale_fill_distiller(palette="Blues", direction=1) +
#   facet_wrap(~crash_quintile, nrow=1) +
#   guides(fill=FALSE) +
#   theme_v_gds() +
#   labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Col total") +
#   theme(axis.text.x=element_blank(), axis.text.y = element_blank(), 
#         axis.title.x = element_blank(), axis.title.y = element_blank()) +
#   coord_equal()
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
# ggsave(filename="./static/class/04-class_files/imd-driver-cas-crash-dist-full.png", plot=plot,width=9, height=14, dpi=300)


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
      #(casualty_imd_quintile_num-driver_imd_quintile_num)^2 +
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
    axis.title.x = element_text(size=10, hjust=1, family="Roboto Condensed Light")
  ) +
  coord_equal()


plot <- plot_resid_dist + plot_spacer() +
  plot_exp_dist + plot_row_total_dist + plot_col_total_dist +  plot_layout(nrow=5) +
  plot_layout(heights=c(1,.1,.8,.8,.8)) +
  plot_annotation(title="Pedestrian casualties by IMD quintile of homeplace of pedestrian and driver",
                  subtitle="--Grouped and compared by IMD quintile of crash location",
                  caption="Stats19 data accessed via `stats19` package")


ggsave(filename="./static/class/04-class_files/imd-driver-cas-crash-dist-full.png", plot=plot,width=9, height=10, dpi=300)

plot_data <-  ped_veh |> 
  filter(
    !is.na(casualty_imd_decile), !is.na(driver_imd_decile),
    casualty_imd_decile!="Data missing or out of range", 
    driver_imd_decile!="Data missing or out of range", !is.na(crash_quintile)) |>   
  left_join(demog_distances) |> 
  mutate(
    grand_total=n()
  ) |> 
  group_by(ranked_dist) |> 
  mutate(row_total=n()) |> ungroup |> 
  group_by(crash_quintile) |> 
  mutate(col_total=n()) |> ungroup |> 
  group_by(casualty_imd_quintile, driver_imd_quintile, crash_quintile) |> 
  summarise(
    distance=first(ranked_dist),
    prop=first(crash_prop_dist),
    prop_adjusted=prop/ranked_occurrences,
    row_total=prop_adjusted,
    col_total=first(col_total),
    expected=prop_adjusted*col_total,
    observed=n(), 
    resid=(observed-expected)/sqrt(expected),
    # Censor extreme effect sizes.
    resid_censored=if_else(resid<0, pmax(resid, -40), pmin(resid, 40)),
    ranked_occurrences=first(ranked_occurrences),
  )  |> ungroup |> 
  mutate(max_resid_censored=max(resid_censored), max_resid=max(resid))



plot_exp_dist <- plot_data |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_tile(aes(fill=expected), colour="#707070", size=.2) +
  scale_fill_distiller(palette="Blues", direction=1) +
  facet_wrap(~crash_quintile, nrow=1) +
  guides(fill=FALSE) +
  theme_v_gds() +
  labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Expected") +
  theme(axis.text.x=element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  coord_equal()


plot_row_total_dist <- plot_data |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_tile(aes(fill=row_total/ranked_occurrences), colour="#707070", size=.2) +
  scale_fill_distiller(palette="Blues", direction=1) +
  facet_wrap(~crash_quintile, nrow=1) +
  guides(fill=FALSE) +
  theme_v_gds() +
  labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Row total") +
  theme(axis.text.x=element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  coord_equal()


plot_col_total_dist <- plot_data |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_tile(aes(fill=col_total), colour="#707070", size=.2) +
  scale_fill_distiller(palette="Blues", direction=1) +
  facet_wrap(~crash_quintile, nrow=1) +
  guides(fill=FALSE) +
  theme_v_gds() +
  labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Col total") +
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
  theme_v_gds() +
  labs(subtitle="Obs vs Exp", x="Null: no difference in proportional distribution in 'social distance' by crash location") +
  theme(
    axis.text.x=element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
    axis.title.x = element_text(size=10, hjust=1, family="Roboto Condensed Light")
  ) + 
  coord_equal()


plot_dist <- demog_distances |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_tile(aes(fill=demog_dist), colour="#707070", size=.2) +
  scale_fill_distiller(palette="Blues", direction=1) +
  facet_wrap(~crash_quintile, nrow=1) +
  guides(fill=FALSE) +
  theme_v_gds() +
  labs(x="IMD quintile of casualty", y="IMD quintile of driver", subtitle="Demographic distance") +
  theme(axis.text.x=element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank())

+
  coord_equal()

plot <- plot_dist + plot_annotation(title="IMD quintile of homeplace of pedestrian and driver, grouped by IMD class of crash location",
                                                         subtitle="--'Geodemographic distance' variable is mapped to each cell",
                                                         caption="Stats19 data accessed via `stats19` package",
                                                         theme = theme_v_gds())

ggsave(filename="./static/class/04-class_files/imd-geodemog-dist.png", plot=plot,width=9, height=3.2, dpi=300)



plot <- plot_imd_driver_area_obs + plot_resid_dist + plot_spacer()+theme_v_gds() +
  plot_exp_dist + plot_row_total_dist + plot_col_total_dist +  plot_layout(nrow=6) +
  plot_layout(heights=c(1,1,.1,.8,.8,.8)) +
  plot_annotation(title="Pedestrian casualties by IMD quintile of homeplace of pedestrian and driver",
                  subtitle="--Grouped and compared by IMD quintile of crash location",
                  caption="Stats19 data accessed via `stats19` package",
                  theme = theme_v_gds())



ggsave(filename="./static/class/04-class_files/imd-driver-cas-crash-dist-prop.png", plot=plot,width=9, height=14, dpi=300)




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
#   labs(x="IMD quintile of casualty", y="IMD quintile of driver", title="Obvs vs Exp", subtitle="--Null: distribute in cells independently of crash location") +
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

