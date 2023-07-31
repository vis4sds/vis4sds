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
library(here)
library(lubridate)
library(RcppRoll)

# 1.2 Data ---------------------------
# county_data <- nytcovcounty |> 
#   # New York City recode to geoid : 36061
#   mutate(
#     fips=case_when(
#       county == "New York City" ~ as.character(36061),
#       TRUE ~ fips
#     )
#   ) |> 
#   # Focus up to end of May 2020 (as per Washington Post graphic).
#   filter(date< "2020-05-26", county!="Unknown") |> 
#   inner_join(county_boundaries |>  select(GEOID, x, y, pop), by=c("fips"="GEOID")) |> 
#   group_by(fips) |> 
#   mutate(
#     cases_cum=cumsum(cases),
#     # 7-day rolling cases : remove?.
#     cases_mov_avg_local=roll_mean(cases,7,align="right", fill=0),
#     # Find cases on May 3.
#     cases_start=if_else(date=="2020-05-03", cases,0)
#   ) |>  ungroup() |> 
#   filter(date> "2020-05-02") |>  
#   group_by(fips) |>  
#   # Remove those where <20 cases on May 3. 
#   mutate(remove=max(cases_start)) |>  ungroup() |>  
#   filter(remove>19) |>  
#   mutate(
#     growth_rate=cases/remove,
#     end_rate=if_else(date=="2020-05-25", growth_rate,0),
#     end_cases=if_else(date=="2020-05-25", cases,0)
#   ) |> 
#   group_by(fips) |> 
#   mutate(
#     end_rate=max(end_rate),
#     end_cases=max(end_cases),
#     binned_growth_rate=as.factor(
#       case_when(
#         max(end_rate) > 7 ~ 4,
#         max(end_rate) > 4 ~ 3,
#         max(end_rate) > 2 ~ 2,
#         TRUE ~ 1
#       )),
#     day_num=row_number(),
#     case_rate=end_cases/pop,
#     binned_case_rate=
#       case_when(
#         case_rate < 0.00208 ~ 1,
#         case_rate < 0.00437 ~ 2,
#         case_rate < .00862 ~ 3,
#         case_rate < 0.0132 ~ 4,
#         TRUE ~ 5
#       ),
#     growth_rate_cont=end_rate
#   ) |>  ungroup() |> 
#  |> 
#   #mutate(binned_growth_rate=factor(binned_growth_rate))  |> 
#   group_by(fips, county, state) |> 
#   mutate(
#     ,
#   ) |>  ungroup |>  mutate(binned_case_rate=factor(binned_case_rate))

county_boundaries <- st_read(here("../", "data", "ch8", "county_boundaries.geojson"))                             
state_boundaries <- st_read(here("../", "data", "ch8", "state_boundaries.geojson")) 
county_data <- read_csv(here("../", "data", "ch8", "county_data_covid.csv")) |> 
    # Cast binned_growth_rate to factor.
  mutate(binned_growth_rate=factor(binned_growth_rate))
#-----------------------------------------
# 3. Techniques graphics
#-----------------------------------------

plot <- 
county_data |> 
  ggplot()+
  geom_sf(data=state_boundaries, fill="#eeeeee", colour="#ffffff", linewidth=0.4)+ 
  coord_sf(crs = 5070, datum=NA, clip="off")+
  geom_point(data=.%>% filter(date=="2020-05-03"), aes(x=x, y=y, colour=binned_growth_rate, alpha=binned_growth_rate, size=case_rate))+
  # Plot case data.
  geom_path(
    aes(x=x+(day_num*6000)-6000, y=y+(growth_rate*50000)-50000, group=fips, colour=binned_growth_rate, size=case_rate, alpha=binned_growth_rate),
    lineend="round"
  ) +
  scale_colour_manual(values=c("#fa9fb5", "#dd3497", "#7a0177", "#49006a"))+
  scale_size(range=c(.1,2.5))+
  scale_alpha_ordinal(range=c(.2,1))+
  guides(colour=FALSE, size=FALSE, alpha=FALSE)+
  theme_void()

ggsave(filename=here("figs","08","wp-basic.png"), plot=plot,width=14, height=10, dpi=300)



county_data |> 
  ggplot()+
  geom_sf(data=state_boundaries, fill="#eeeeee", colour="#ffffff", linewidth=0.4)+ 
  geom_text(data=state_boundaries, aes(x=x,y=y,label=STUSPS), alpha=.8, family="Avenir Book")+
  coord_sf(crs = 5070, datum=NA, clip="off")+
  geom_point(data=.%>% filter(date=="2020-05-03"), aes(x=x, y=y, colour=binned_growth_rate, alpha=binned_growth_rate, size=case_rate))+
  # Plot case data.
  geom_path(
    aes(x=x+(day_num*6000)-6000, y=y+(growth_rate*50000)-50000, group=fips, colour=binned_growth_rate, size=case_rate, alpha=binned_growth_rate),
    lineend="round"
  ) +
  scale_colour_manual(values=c("#fa9fb5", "#dd3497", "#7a0177", "#49006a"))+
  scale_size(range=c(.1,2.5))+
  scale_alpha_ordinal(range=c(.2,1))+
  guides(colour=FALSE, size=FALSE, alpha=FALSE)+
  theme_void()

# Counties to annotate.
annotate <- county_data |>  
  filter(
    date=="2020-05-03",
    county==c("Huntingdon") & state=="Pennsylvania" |
    county==c("Lenawee") & state=="Michigan" |
    county==c("Crawford") & state=="Iowa" |
    county==c("Wapello") & state=="Iowa" |
    county==c("Lake") & state=="Tennessee" |
    county=="Texas" & state == c("Oklahoma") |
    county==c("Duplin") & state=="North Carolina" |
    county==c("Santa Cruz") & state=="Arizona"|
    county==c("Titus") & state=="Texas"|
    county==c("Yakima") & state=="Washington"
    ) |> 
  mutate(
    state_abbr=case_when(
      state=="Pennsylvania" ~ "Penn.",
      state=="Iowa" ~ "Iowa",
      state=="Tennessee" ~ "Tenn.",
      state=="Oklahoma" ~ "Okla.",
      state=="Texas" ~ "Texas",
      state=="North Carolina" ~ "N.C.",
      state=="Washington" ~ "Wash.",
      state=="Michigan" ~ "Mich.",
      state=="Arizona" ~ "Arizona",
      TRUE ~ ""),
    end_rate_round = round(end_rate,0)
  )

county_data |> 
  ggplot()+
  geom_sf(data=state_boundaries, fill="#eeeeee", colour="#ffffff", linewidth=0.4)+ 
  geom_text(data=state_boundaries, aes(x=x,y=y,label=STUSPS), alpha=.8, family="Avenir Book")+
  
  geom_text(data=annotate, aes(x=x,y=y-20000,label=paste0(county,", ",state_abbr)), alpha=1, size=3, family="Avenir Heavy")+
  geom_text(data=annotate, aes(x=x,y=y-65000,label=paste0(end_rate_round,"X more cases")), alpha=1, size=2.5, family="Avenir Book")+
  
  coord_sf(crs = 5070, datum=NA, clip="off")+
  geom_point(data=.%>% filter(date=="2020-05-03"), aes(x=x, y=y, colour=binned_growth_rate, alpha=binned_growth_rate, size=case_rate))+
  # Plot case data.
  geom_path(
    aes(x=x+(day_num*6000)-6000, y=y+(growth_rate*50000)-50000, group=fips, colour=binned_growth_rate, size=case_rate, alpha=binned_growth_rate),
    lineend="round"
  ) +
  scale_colour_manual(values=c("#fa9fb5", "#dd3497", "#7a0177", "#49006a"))+
  scale_size(range=c(.1,2.5))+
  scale_alpha_ordinal(range=c(.2,1))+
  guides(colour=FALSE, size=FALSE, alpha=FALSE)+
  theme_void()


# Bounding box for mainland US.
bbox <- st_bbox(state_boundaries)
width <- bbox$xmax-bbox$xmin
height <- bbox$ymax-bbox$ymin


# Legend : growth
legend_growth <- county_data %>%
  filter(
    county=="Dubois" & state=="Indiana" |
      county=="Androscoggin" & state=="Maine" |
      county=="Fairfax" & state=="Virginia" |
      county=="Bledsoe" & state=="Tennessee"
  ) %>%
  mutate(
    x=bbox$xmax-.25*width,y=bbox$ymax+.05*height,
    case_rate=.01,
    label=case_when(
      county == "Dubois" ~ "7x more cases than on May 3",
      county == "Androscoggin" ~ "4x",
      county == "Fairfax" ~ "2x",
      county == "Bledsoe" ~ "About the same as on May 3"
    )
  )
  

# Legend : case
legend_case <- county_data %>%
  filter(
    county == "Kings" & state=="California" ) %>%
  mutate(
    x=bbox$xmax-.88*width,y=bbox$ymax+.05*height,
    binned_growth_rate=factor(binned_growth_rate)
  ) %>%
  select(x, y, day_num, growth_rate, binned_growth_rate, fips) %>%
  mutate(
    low=.001, mid=.009, high=.015,
  ) %>%
  pivot_longer(cols=c(low, mid, high), names_to="offset", values_to="offset_rate") %>%
  mutate(
    offset_day= case_when(
      offset == "low" ~ 0,
      offset == "mid" ~ .04,
      offset == "high" ~ .08
    )
  )

plot <- county_data |> 
  ggplot()+
  geom_sf(data=state_boundaries, fill="#eeeeee", colour="#ffffff", linewidth=0.6)+ 
  geom_text(data=state_boundaries, aes(x=x,y=y,label=STUSPS), alpha=.8, size=5, family="Avenir Book")+
  
  geom_text(data=annotate, aes(x=x,y=y-20000,label=paste0(county,", ",state_abbr)), alpha=1, size=3.5, family="Avenir Heavy")+
  geom_text(data=annotate, aes(x=x,y=y-70000,label=paste0(end_rate_round,"X more cases")), alpha=1, size=3, family="Avenir Book")+
  
  # Plot growth legend lines.
  geom_path(
    data=legend_growth,
    aes(
      x=x+(day_num*6000)+80000, y=y+(growth_rate*50000)-150000,
      group=fips, colour=binned_growth_rate, size=case_rate,
      alpha=binned_growth_rate
    ),
    lineend="round"
  ) +
  
  # Text label for growth legend lines.
  geom_text(
    # For appropriate positioning, we manually edit the growth_rate values of
    # Bledsoe, no growth county.
    data=legend_growth %>% filter(day_num == max(county_data$day_num)) %>%
      mutate(growth_rate=if_else(county=="Bledsoe", -1,growth_rate)),
    aes(
      x=x+(day_num*6000)+100000,y=y+(growth_rate*50000)-150000,
      label=str_wrap(label, 15)
    ),
    alpha=1, size=3.5, hjust=0, vjust=0
  )+
  
  annotate("text",
           x=bbox$xmax-.24*width, y=bbox$ymax+.06*height,
           label=str_wrap("Line height and colour show change in reported cases
    relative to May 3",35), alpha=1, size=4, hjust=1)+
  
  
  # Plot case legend lines.
  geom_path(
    data=legend_case,
    aes(
      x=x+(day_num*6000)-6000+offset_day*(.9*width), y=y+(growth_rate*50000)-50000,
      group=paste0(fips,offset), colour=binned_growth_rate, size=offset_rate,
      alpha=binned_growth_rate
    ),
    lineend="round"
  ) +
  
  coord_sf(crs = 5070, datum=NA, clip="off")+
  geom_point(data=.%>% filter(date=="2020-05-03"), aes(x=x, y=y, colour=binned_growth_rate, alpha=binned_growth_rate, size=case_rate))+
  
  # Plot case data.
  geom_path(
    aes(x=x+(day_num*6000)-6000, y=y+(growth_rate*50000)-50000, group=fips, colour=binned_growth_rate, size=case_rate, alpha=binned_growth_rate),
    lineend="round"
  ) +
  
  # Text label for case legend lines.
  annotate("text", x=bbox$xmax-.88*width, y=bbox$ymax+.03*height, label="Less",
           alpha=1, hjust=0.5, size=3.5)+
  annotate("text", x=bbox$xmax-.8*width, y=bbox$ymax+.03*height, label="More",
           alpha=1, hjust=0.5, size=3.5)+
  annotate("text", x=bbox$xmax-.76*width, y=bbox$ymax+.06*height,
           label=str_wrap("Line thickness shows current number relative to county population",35),
           alpha=1, size=4, hjust=0)+
  # Title
  annotate("text", x=bbox$xmax-.5*width, y=bbox$ymax+.15*height,
           label="Change in reported cases since May 3", alpha=1, size=7, family="Avenir Medium")+
  
  scale_colour_manual(values=c("#fa9fb5", "#dd3497", "#7a0177", "#49006a"))+
  scale_size(range=c(.1,2.5))+
  scale_alpha_ordinal(range=c(.2,1))+
  guides(colour=FALSE, size=FALSE, alpha=FALSE)+
  theme_void()

ggsave(filename=here("figs","08","wp.png"), plot=plot,width=13.4, height=9, dpi=300)



plot <- county_data |> 
  ggplot()+
  geom_sf(data=state_boundaries, fill="#eeeeee", colour="#ffffff", linewidth=0.6)+ 
  geom_text(data=state_boundaries, aes(x=x,y=y,label=STUSPS), alpha=.8, size=5, family="Avenir Book")+
  
  geom_text(data=annotate, aes(x=x,y=y-20000,label=paste0(county,", ",state_abbr)), alpha=1, size=3.5, family="Avenir Heavy")+
  geom_text(data=annotate, aes(x=x,y=y-70000,label=paste0(end_rate_round,"X more cases")), alpha=1, size=3, family="Avenir Book")+
  
  # Plot growth legend lines.
  geom_path(
    data=legend_growth,
    aes(
      x=x+(day_num*6000)+80000, y=y+(growth_rate*50000)-150000,
      group=fips, colour=binned_growth_rate, size=case_rate,
      alpha=binned_growth_rate
    ),
    lineend="round"
  ) +
  
  # Text label for growth legend lines.
  geom_text(
    data=legend_growth %>% filter(day_num == max(county_data$day_num)),
    aes(
      x=x+(day_num*6000)+100000,y=y+(growth_rate*50000)-150000,
      label=str_wrap(label, 15)
    ),
    alpha=1, size=3.5, hjust=0, vjust=0
  )+
  
  annotate("text",
           x=bbox$xmax-.24*width, y=bbox$ymax+.06*height,
           label=str_wrap("Line height and colour show change in reported cases
    relative to May 3",35), alpha=1, size=4, hjust=1)+
  
  
  # Plot case legend lines.
  geom_path(
    data=legend_case,
    aes(
      x=x+(day_num*6000)-6000+offset_day*(.9*width), y=y+(growth_rate*50000)-50000,
      group=paste0(fips,offset), colour=binned_growth_rate, size=offset_rate,
      alpha=binned_growth_rate
    ),
    lineend="round"
  ) +
  
  coord_sf(crs = 5070, datum=NA, clip="off")+
  geom_point(data=.%>% filter(date=="2020-05-03"), aes(x=x, y=y, colour=binned_growth_rate, alpha=binned_growth_rate, size=case_rate))+
  
  # Plot case data.
  # geom_path(
  #   aes(x=x+(day_num*6000)-6000, y=y+(growth_rate*50000)-50000, group=fips, colour=binned_growth_rate, size=case_rate, alpha=binned_growth_rate),
  #   lineend="round"
  # ) +
  
  # Text label for case legend lines.
  annotate("text", x=bbox$xmax-.88*width, y=bbox$ymax+.03*height, label="Less",
           alpha=1, hjust=0.5, size=3.5)+
  annotate("text", x=bbox$xmax-.8*width, y=bbox$ymax+.03*height, label="More",
           alpha=1, hjust=0.5, size=3.5)+
  annotate("text", x=bbox$xmax-.76*width, y=bbox$ymax+.06*height,
           label=str_wrap("Line thickness shows current number relative to county population",35),
           alpha=1, size=4, hjust=0)+
  # Title
  # Change in reported cases since May 3
  annotate("text", x=bbox$xmax-.5*width, y=bbox$ymax+.15*height,
           label="", alpha=1, size=7, family="Avenir Medium")+
  
  scale_colour_manual(values=c("#fa9fb5", "#dd3497", "#7a0177", "#49006a"))+
  scale_size(range=c(.1,2.5))+
  scale_alpha_ordinal(range=c(.2,1))+
  guides(colour=FALSE, size=FALSE, alpha=FALSE)+
  theme_void()


ggsave(filename=here("figs","08","wp-legend-case.png"), plot=plot,width=13.4, height=9, dpi=300)


