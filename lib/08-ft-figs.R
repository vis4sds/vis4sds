# Filename: 08-ft-figs.R 
#
# Figures for Chapter 8 of vis4sds : reproducing FT's grapics
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
library(lubridate)
library(RcppRoll) # You may also need to install this package.

remotes::install_github("joachim-gassen/tidycovid19")
library(tidycovid19)

# 1.2 Data ---------------------------

# Download Johns Hopkins Data.
dat <- download_jhu_csse_covid19_data()
 dat_original <- dat

#-----------------------------------------
# 1. Concepts graphics
#-----------------------------------------


# 1.1 JBM death charts -------------------

countries <- c("Spain", "Italy", "France", "United Kingdom", "US", "Korea, South", "Japan", "China", "Iran", "Netherlands", "Switzerland", "Germany", "Belgium", "Indonesia", "Philippines", "Iraq", "Sweden")

spain <- "#578ad2" 
france <- "#578ad2" 
italy <- "#615d5e" 
china <- "#dd9e40" 
us <- "#d4608b" 
uk <- "#93395c" 
japan <- "#64a7b3" 
korea_s <- "#64a7b3"
iran <- "#869141"
nl <- "#807972"
switzerland <- "#807972"
germany <- "#807972"
belgium <- "#807972"
indonesia <- "#807972"
philippines <- "#807972"
iraq <- "#807972"
sweden <- "#807972"

country_colours <- c(spain, france, italy, china, us, uk, japan, korea_s, iran, nl,
                     switzerland, germany, belgium, indonesia, philippines, iraq, sweden)


dat <- dat |>  filter(country %in% countries) |> filter(date< "2020-03-25") |> 
  group_by(country) |> 
  mutate(
    deaths_cum=cumsum(deaths),
    start_count=as.numeric(deaths_cum>9),
    day_num=cumsum(start_count)
  ) |> 
  ungroup() |> 
  filter(day_num>0)

plot <- dat |> 
  ggplot() +
  geom_line(aes(x=day_num, y=deaths_cum, group=country) ) +
  scale_y_log10(breaks = c(10, 100, 1000, 10000)) +
  labs(x="# days since 10th death", y="# deaths")
# ggsave(here("cum_deaths1.png"), plot, width=5, height=3, dpi=300)

country_focus <- c("Spain", "France", "Italy", "China", "US", "UK", "Japan", "S Korea", "Iran")

country_levels <- 
  c("Spain", "France", "Italy", "China", "US", "UK", "Japan", "S Korea", "Iran",
    "Netherlands", "Switzerland", "Germany", "Belgium", "Indonesia", "Philippines", "Iraq", "Sweden")

growth_count <- function(start, rate, days) {start*(2^rate)^days}



china_40 <- dat |> filter(country=="China") |> filter(day_num=="40") 
china_text <- "         had 50,178\ndeaths at 40 days."

korea_40 <- dat |> mutate(max_date=max(date)) |> filter(day_num<41, country=="Korea, South", date==max_date)  
korea_text <- "         early and large-scale testing and\ntracing helped authorities get the\noutbreak under control."

spain_14 <- dat |> filter(country=="Spain", date=="2020-03-14")
france_14 <- dat |> filter(country=="France", date=="2020-03-18")
spain_text <- "Spain locked down    after c.540 deaths,\nFrance after c.410, Italy not until c.1780.\nWith 40 days since its 10th death,\nUK has yet to lockdown."

plot <- dat |> 
  filter(!country %in% c("Germany", "Belgium")) |> 
  mutate(
    country = case_when(
      country == "United Kingdom" ~ "UK",
      country == "Korea, South" ~ "S Korea",
      TRUE ~ country),
    country_focus=country %in% country_focus,
    country=factor(country, levels=country_levels),
    is_lockdown=
      case_when(
        (country=="Spain" & date == "2020-03-14") ~ TRUE,
        (country=="France" & date == "2020-03-14") ~ TRUE,
        (country=="UK" & date == "2020-03-23") ~ TRUE,
        (country=="Italy" & date == "2020-03-09") ~ TRUE,
        TRUE ~ FALSE
      )
  ) |>  
  filter(day_num<41) |> 
  group_by(country) |> 
  mutate(max_date=max(date)) |> 
  ungroup() |> 
  ggplot(aes(x=day_num, y=deaths_cum, colour=country)) +
  geom_line(aes(group=country), linewidth=.5 ) +
  geom_point(size=.5) +
  geom_point(data=. %>% filter(date==max_date), aes(fill=country), pch=21, colour="#000000") +
  geom_text(data=. %>% filter(date==max_date, country_focus), 
             aes(label=country), hjust=0, nudge_x = .7, family="Avenir Heavy", size=3) +
  
  geom_point(data=. %>% filter(is_lockdown), aes(fill=country), shape=8, size=1.8, stroke=.8) +
  
  geom_text(data=. %>% filter(!country_focus, date==max_date), 
            aes(label=country), hjust=0, nudge_x = .7, family="Avenir Light", size=2.6) +
  
  annotate("segment", x=0, xend=36, y=10, yend=growth_count(10,1/2,26), linetype="dashed",
           linewidth=.2) +
  
  annotate("text", x=35, y=growth_count(10,1/2,25)+1000, label="DOUBLES EVERY \n2 DAYS", size=2, 
           angle=40, hjust="left", vjust="centre") +
  
  annotate("text", x=41.7, y=china_40$deaths_cum+5000, label=china_text, size=2.5, 
          hjust="left", vjust="top", colour="#525252") +
  
  annotate("text", x=korea_40$day_num+2.8, y=korea_40$deaths_cum+190, label=korea_text, size=2.5, 
           hjust="left", vjust="top", colour="#525252") +
  
  # annotate("text", x=korea_40$day_num+2.8, y=korea_40$deaths_cum+250, label=korea_text, size=2.5, 
  #          hjust="left", vjust="top", colour="#525252") +
  # 
  annotate("text", x=0, y=spain_14$deaths_cum+10000, label=spain_text, size=2.5, 
           hjust="left", vjust="top", colour="#525252") +
  
  annotate("point", x=8.2, y=spain_14$deaths_cum+9300, shape=8, size=1.5, stroke=.5, colour="#525252") +
  
  scale_y_log10(
    breaks = c(10, 20, 60, 200, 1000, 10000, 80000), 
    expand = c(0, 0), label=scales::comma,
    limits=c(10,250000)
    ) +
  scale_x_continuous(breaks=seq(0, 40, 5), expand = c(0, 0), limits=c(0,50))+

  labs(x="# days since 10th death", y="") +
  scale_colour_manual(values=country_colours, guide="none") +
  scale_fill_manual(values=country_colours, guide="none") +
  
  labs(
    title="Coronavirus deaths in UK, France and Spain increasing more\nrapidly than they did in China",
    subtitle = "Cumulative number of deaths, by number of days since 10th death"
    ) +
  
  theme(
    plot.title = element_text(size=13),
    plot.subtitle = element_text(size=10, family="Avenir Book"),
    axis.line.y =element_blank(),
    panel.grid.major = element_line(colour="#e0e0e0", linewidth=0.15),
    axis.text.x=element_text(size=9), axis.text.y=element_text(size=9),
    axis.title.x=element_text(size=9)
    ) 

plot

ggsave(here("figs", "08", "cum_deaths_full.png"), plot, width=6.4, height=5, dpi=300)


# 1.2 Bhatia Covid Trends -------------------


countries_frame_1 <- c(
  "Taiwan", "Hong Kong", "Singapore", "India", "Japan", "Korea, South", 
  "Australia", "Canada", "Brazil", "US", "United Kingdom", "France",
  "Germany", "Italy")

countries_selected <- c("Singapore", "US", "Germany")
               

# sysfonts::font_add(family = "Font Awesome 6 Free",
#                    regular = "/Users/rogerbeecham/Library/Fonts/FontAwesome6Free-Regular-400.otf")
# showtext::showtext_auto()
# 
# sysfonts::font_add(family = "Avenir",
#                    regular = "/Users/rogerbeecham/Library/Fonts/Avenir.ttc")

# https://nrennie.rbind.io/blog/adding-social-media-icons-ggplot2/

github_icon <- "&#xf09b"
play_icon <- "&#xf01D"


social_caption <- glue::glue(
  "<span style='font-family:\"Font Awesome 6 Free\"; color: #db386c'>{play_icon};</span>
  <span style='color: #db386c; font-family: Avenir Black'>2020-09-01</span>"
)

#db386c


plot <- dat_original |>  
  group_by(country) |> 
  mutate(
    cases_cum=cumsum(confirmed),
    cases_week=(RcppRoll::roll_sum(confirmed, 7, align="right", fill=0)),
  ) |>
  filter(country %in% countries_frame_1, date< "2020-09-02", cases_cum>50, cases_week>10) |> 
  ggplot(aes(x=cases_cum, y=cases_week)) +
  
  geom_line(data=. %>% filter(!country %in% countries_selected), aes(group=country), alpha=.4, colour="#525252", linewidth=.6) +
  
  geom_line(data=. %>% filter(country %in% countries_selected), aes(group=country), alpha=.9, colour="#db386c", linewidth=.6) +
  
  geom_point(data=. %>% filter(country %in% countries_selected, date=="2020-09-01"), colour="#db386c", alpha=1, size=1.8) +
  
  geom_point(data=. %>% filter(!country %in% countries_selected, date=="2020-09-01"), colour="#525252", alpha=.4, size=1.8) +
  
  geom_abline(intercept=10, slope=1, linetype="dashed") +
  
  geom_text(data=. %>% filter(date=="2020-09-01", country %in% countries_selected), 
            aes(label=country, y=cases_week^0.96-7000), colour="#db386c", size=3.5, hjust="left") +
  
  annotate("text", x=1.5*(10^7), y=1.5*(10^7), label="2 day doubling\ntime of cases", size=2.5, 
           angle=35, hjust="left", vjust="centre") +
  annotate("segment", x=10, xend=10^8, y=10, yend=10^8, linetype="dashed",
           linewidth=.2) +
  scale_x_log10(limits=c(10,10^9+10^8), 
                breaks = c(10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7, 10^8, 10^9),
                labels=c("10","100","1000","10k","100k","1M","10M", "100M", "10B"),
                expand = c(0, 0)) +
  scale_y_log10(limits=c(1,10^8*2),
                breaks = c(1, 10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7),
                labels=c("1", "10","100","1000","10k","100k","1M","10M"),
                expand = c(0, 0)) +
  labs(
    title="Trajectory of World Covid-19 Confirmed Cases",
    subtitle=social_caption,
    y="new confirmed cases (past 7 days)", 
    x="total confirmed cases") +
  theme(
    panel.grid.major = element_line(colour="#e0e0e0", linewidth=0.15),
    plot.subtitle = element_markdown(size=14),
    plot.title = element_text(size=16)
    )

ggsave(here("figs", "08", "covid_logs.png"), plot, width=6.4, height=5, dpi=300)


# ggsave(here("cum_deaths1.png"), plot, width=5, height=3, dpi=300)

country_focus <- c("Spain", "France", "Italy", "China", "US", "UK", "Japan", "S Korea", "Iran")

country_levels <- 
  c("Spain", "France", "Italy", "China", "US", "UK", "Japan", "S Korea", "Iran",
    "Netherlands", "Switzerland", "Germany", "Belgium", "Indonesia", "Philippines", "Iraq", "Sweden")

growth_count <- function(start, rate, days) {start*(2^rate)^days}



china_40 <- dat |> filter(country=="China") |> filter(day_num=="40") 
china_text <- "         had 50,178\ndeaths at 40 days."

korea_40 <- dat |> mutate(max_date=max(date)) |> filter(day_num<41, country=="Korea, South", date==max_date)  
korea_text <- "         early and large-scale testing and\ntracing helped authorities get the\noutbreak under control."

spain_14 <- dat |> filter(country=="Spain", date=="2020-03-14")
france_14 <- dat |> filter(country=="France", date=="2020-03-18")
spain_text <- "Spain locked down    after c.540 deaths,\nFrance after c.410, Italy not until c.1780.\nWith 40 days since its 10th death,\nUK has yet to lockdown."

plot <- dat |> 
  filter(!country %in% c("Germany", "Belgium")) |> 
  mutate(
    country = case_when(
      country == "United Kingdom" ~ "UK",
      country == "Korea, South" ~ "S Korea",
      TRUE ~ country),
    country_focus=country %in% country_focus,
    country=factor(country, levels=country_levels),
    is_lockdown=
      case_when(
        (country=="Spain" & date == "2020-03-14") ~ TRUE,
        (country=="France" & date == "2020-03-14") ~ TRUE,
        (country=="UK" & date == "2020-03-23") ~ TRUE,
        (country=="Italy" & date == "2020-03-09") ~ TRUE,
        TRUE ~ FALSE
      )
  ) |>  
  filter(day_num<41) |> 
  group_by(country) |> 
  mutate(max_date=max(date)) |> 
  ungroup() |> 
  ggplot(aes(x=day_num, y=deaths_cum, colour=country)) +
  geom_line(aes(group=country), linewidth=.5 ) +
  geom_point(size=.5) +
  geom_point(data=. %>% filter(date==max_date), aes(fill=country), pch=21, colour="#000000") +
  geom_text(data=. %>% filter(date==max_date, country_focus), 
            aes(label=country), hjust=0, nudge_x = .7, family="Avenir Heavy", size=3) +
  
  geom_point(data=. %>% filter(is_lockdown), aes(fill=country), shape=8, size=1.8, stroke=.8) +
  
  geom_text(data=. %>% filter(!country_focus, date==max_date), 
            aes(label=country), hjust=0, nudge_x = .7, family="Avenir Light", size=2.6) +
  
  annotate("segment", x=0, xend=36, y=10, yend=growth_count(10,1/2,26), linetype="dashed",
           linewidth=.2) +
  
  annotate("text", x=35, y=growth_count(10,1/2,25)+1000, label="DOUBLES EVERY \n2 DAYS", size=2, 
           angle=40, hjust="left", vjust="centre") +
  
  annotate("text", x=41.7, y=china_40$deaths_cum+5000, label=china_text, size=2.5, 
           hjust="left", vjust="top", colour="#525252") +
  
  annotate("text", x=korea_40$day_num+2.8, y=korea_40$deaths_cum+190, label=korea_text, size=2.5, 
           hjust="left", vjust="top", colour="#525252") +
  
  # annotate("text", x=korea_40$day_num+2.8, y=korea_40$deaths_cum+250, label=korea_text, size=2.5, 
  #          hjust="left", vjust="top", colour="#525252") +
  # 
  annotate("text", x=0, y=spain_14$deaths_cum+10000, label=spain_text, size=2.5, 
           hjust="left", vjust="top", colour="#525252") +
  
  annotate("point", x=8.2, y=spain_14$deaths_cum+9300, shape=8, size=1.5, stroke=.5, colour="#525252") +
  
  scale_y_log10(
    breaks = c(10, 20, 60, 200, 1000, 10000, 80000), 
    expand = c(0, 0), label=scales::comma,
    limits=c(10,250000)
  ) +
  scale_x_continuous(breaks=seq(0, 40, 5), expand = c(0, 0), limits=c(0,50))+
  
  labs(x="# days since 10th death", y="") +
  scale_colour_manual(values=country_colours, guide="none") +
  scale_fill_manual(values=country_colours, guide="none") +
  
  labs(
    title="Coronavirus deaths in UK, France and Spain increasing more\nrapidly than they did in China",
    subtitle = "Cumulative number of deaths, by number of days since 10th death"
  ) +
  
  theme(
    plot.title = element_text(size=13),
    plot.subtitle = element_text(size=10, family="Avenir Book"),
    axis.line.y =element_blank(),
    panel.grid.major = element_line(colour="#e0e0e0", linewidth=0.15),
    axis.text.x=element_text(size=9), axis.text.y=element_text(size=9),
    axis.title.x=element_text(size=9)
  ) 


     