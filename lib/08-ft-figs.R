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


# Recode UK to 0 until first public announcement of death. 
# Recode Philipines to 0 also
dat <- dat_original |>  filter(country %in% countries) |> filter(date< "2020-03-25") |> 
  mutate(
    deaths=case_when(
      country == "United Kingdom" & date< "2020-03-06" ~ 0,
      country == "Philippines" & date< "2020-03-12" ~ 0,
      country == "Japan" & date< "2020-03-08" ~ 0,
      country == "France" & date< "2020-02-15" ~ 0,
      TRUE ~ deaths
    )
  ) |> 
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
spain_text <- "Spain locked down    after c.540 deaths,\nFrance after c.410, Italy not until c.1780.\nUK's lockdown even later than Italy."

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
           linewidth=.25) +
  
  annotate("text", x=35, y=growth_count(10,1/2,25)+1000, label="DOUBLES EVERY \n2 DAYS", size=2, 
           angle=40, hjust="left", vjust="centre") +
  
  annotate("text", x=41.7, y=china_40$deaths_cum+5000, label=china_text, size=2.5, 
          hjust="left", vjust="top", colour="#525252") +
  
  annotate("text", x=korea_40$day_num+2.8, y=korea_40$deaths_cum+190, label=korea_text, size=2.5, 
           hjust="left", vjust="top", colour="#525252") +
  
  # annotate("text", x=korea_40$day_num+2.8, y=korea_40$deaths_cum+250, label=korea_text, size=2.5, 
  #          hjust="left", vjust="top", colour="#525252") +
  # 
  annotate("text", x=0, y=spain_14$deaths_cum+20000, label=spain_text, size=2.5, 
           hjust="left", vjust="top", colour="#525252") +
  
  annotate("point", x=8.2, y=spain_14$deaths_cum+20300, shape=8, size=1.5, stroke=.5, colour="#525252") +
  
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

play_icon <- "&#xf01D"
play_caption <- glue::glue(
  "<span style='font-family:\"Font Awesome 6 Free\"; color: #db386c'>{play_icon};</span>
  <span style='color: #db386c; font-family:\"Avenir Black\"'>2020-09-01</span>"
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
    subtitle=play_caption,
    y="new confirmed cases (past 7 days)", 
    x="total confirmed cases") +
  theme(
    panel.grid.major = element_line(colour="#e0e0e0", linewidth=0.15),
    plot.subtitle = element_markdown(size=11.5),
    plot.title = element_text(size=13.5),
    axis.title.x = element_text(size=9),
    axis.title.y = element_text(size=9),
    axis.text =element_text(size=8)
    )


ggsave(here("figs", "08", "covid_logs.png"), plot, width=6, height=4.5, dpi=300)


# 1.4 JBM Animated hospitalisations -------------------

# https://webarchive.nationalarchives.gov.uk/ukgwa/20220402124218/https://www.gov.uk/government/collections/weekly-national-flu-reports
# data https://webarchive.nationalarchives.gov.uk/ukgwa/20220401234257/https://www.gov.uk/government/statistics/weekly-national-flu-reports-2019-to-2020-season
# https://www.england.nhs.uk/statistics/statistical-work-areas/hospital-activity/monthly-hospital-activity/mar-data/
# https://www.gov.uk/government/statistics/weekly-national-flu-reports-2019-to-2020-season
# https://webarchive.nationalarchives.gov.uk/ukgwa/20220401234439/https://www.gov.uk/government/statistics/national-flu-and-covid-19-surveillance-reports-2021-to-2022-season
# https://twitter.com/jburnmurdoch/status/1347200811303055364

# week 40 Oct -- Week 20 May
# file ODS
# file , sheet
# YEAR : 2021
# 2021w51_2020w51.xlsx : Figure 44. SARI Watch-ICUHDU

# YEAR : 2020
# 2021w26_2020w27.xlsx : Figure 41. SARI Watch-ICUHDU 


# 2020w51_2019w27.xlsx : Figure 39. SARI Watch-ICUHDU
# 2020w51_2019w27.xlsx : Figure 39. SARI Watch-ICUHDU



# w27y2019w52y2020.xlsx : Figure 39. SARI Watch-ICUHDU


months <- c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May")
counts <- c(4, 4, 5, 4, 5, 4, 4, 4, 5, 4) 
breaks <- tibble(month=months, count=counts) |>  uncount(count) |> 
  mutate(
    obs=row_number(),
    is_break=lag(month, 1) != month,
    is_break=if_else(is.na(is_break), TRUE, is_break),
    label=str_sub(month, 1,3)
  ) |> 
  filter(is_break)

icu <- read_csv(here("files", "csv", "icu_data_all.csv"))


grey <- "#CEC5BE"
red <- "#A62538"
blue <- "#5086BF"


title_text <- 
  "<span style='font-family:\"Avenir Black Oblique\"'>'No different to a bad flu season?'</span><br>
  How England's winter Covid surge compares to flu seasons"



subtitle_text <-
  "Weekly ICU admissions of 
  <span style= 'font-family:\"Avenir Black\"; color: #A62538;'>Covid-19</span> + flu
  patients per million people in winter 
  <span style= 'font-family:\"Avenir Black\"; color: #A62538;'>2020-2021</span>"

subtitle_text_comp <-
  "Weekly ICU admissions of flu
  patients per million people in winter 
  <span style= 'font-family:\"Avenir Black\"; color: #5086BF;'>2014-2015</span>"


p1 <- icu |> 
  filter(month %in% months, 
         !season %in% c("2021-2022", "2020-2021","2019-2020", "2018-2019", "2017-2018", "2016-2017", "2015-2016"), 
         !week %in% 23:31, include) |> 
  mutate(focus= season == "2014-2015") |> 
  group_by(season) |> 
  mutate(obs=row_number()) |> ungroup() |> 
  ggplot(aes(x=obs, y=rate, colour=focus)) +
  geom_line(aes(group=season), linewidth=1) +
  scale_colour_manual(values=c(grey, blue), guide="none") +
  scale_x_continuous(breaks=breaks$obs, labels=breaks$label, expand=c(0,0)) +
  scale_y_continuous(breaks=c(0:25), labels=0:25, expand=c(0,0)) +
  labs(x="", y="", title=title_text, subtitle = subtitle_text_comp) +
  theme(
    panel.grid.major.y = element_line(colour="#e0e0e0", linewidth=0.15),
    plot.title = element_markdown(size=13),
    plot.subtitle = element_markdown(size=10, family="Avenir Book"),
    axis.text.x = element_text(hjust=0, size=9),
    axis.text.y = element_text(hjust=0, size=8),
    axis.ticks.x = element_line(colour="#000000", linewidth=0.15)
  )


subtitle_text_comp <-
  "Weekly ICU admissions of flu
  patients per million people in winter 
  <span style= 'font-family:\"Avenir Black\"; color: #5086BF;'>2017-2018</span>"


p2 <- icu |> 
  filter(month %in% months, !season %in% c("2021-2022", "2020-2021", "2019-2020", "2018-2019"), !week %in% 23:31, include) |> 
  mutate(focus= season == "2017-2018") |> 
  group_by(season) |> 
  mutate(obs=row_number()) |> ungroup() |> 
  ggplot(aes(x=obs, y=rate, colour=focus)) +
  geom_line(aes(group=season), linewidth=1) +
  scale_colour_manual(values=c(grey, blue), guide="none") +
  scale_x_continuous(breaks=breaks$obs, labels=breaks$label, expand=c(0,0)) +
  scale_y_continuous(breaks=c(0:25), labels=0:25, expand=c(0,0)) +
  labs(x="", y="", title=title_text, subtitle = subtitle_text_comp) +
  theme(
    panel.grid.major.y = element_line(colour="#e0e0e0", linewidth=0.15),
    plot.title = element_markdown(size=13),
    plot.subtitle = element_markdown(size=10, family="Avenir Book"),
    axis.text.x = element_text(hjust=0, size=9),
    axis.text.y = element_text(hjust=0, size=8),
    axis.ticks.x = element_line(colour="#000000", linewidth=0.15)
  )


subtitle_text_comp <-
  "Weekly ICU admissions of flu
  patients per million people in winter 
  <span style= 'font-family:\"Avenir Black\"; color: #5086BF;'>2019-2020</span>"


p3 <- icu |> 
  filter(month %in% months, !season %in% c("2021-2022", "2020-2021"), !week %in% 23:31, include) |> 
  mutate(focus= season == "2019-2020") |> 
  group_by(season) |> 
  mutate(obs=row_number()) |> ungroup() |> 
  ggplot(aes(x=obs, y=rate, colour=focus)) +
  geom_line(aes(group=season), linewidth=1) +
  scale_colour_manual(values=c(grey, blue), guide="none") +
  scale_x_continuous(breaks=breaks$obs, labels=breaks$label, expand=c(0,0)) +
  scale_y_continuous(breaks=c(0:25), labels=0:25, expand=c(0,0)) +
  labs(x="", y="", title=title_text, subtitle = subtitle_text_comp) +
  theme(
    panel.grid.major.y = element_line(colour="#e0e0e0", linewidth=0.15),
    plot.title = element_markdown(size=13),
    plot.subtitle = element_markdown(size=10, family="Avenir Book"),
    axis.text.x = element_text(hjust=0, size=9),
    axis.text.y = element_text(hjust=0, size=8),
    axis.ticks.x = element_line(colour="#000000", linewidth=0.15)
  )




p4 <- icu |> 
  filter(month %in% months, season != "2021-2022", !week %in% 23:31, include) |> 
  mutate(focus= season == "2020-2021") |> 
  group_by(season) |> 
  mutate(obs=row_number()) |> ungroup() |> 
  ggplot(aes(x=obs, y=rate, colour=focus)) +
  geom_line(aes(group=season), linewidth=1) +
  scale_colour_manual(values=c(grey, red), guide="none") +
  scale_x_continuous(breaks=breaks$obs, labels=breaks$label, expand=c(0,0)) +
  scale_y_continuous(breaks=c(0:25), labels=0:25, expand=c(0,0)) +
  labs(x="", y="", title=title_text, subtitle = subtitle_text) +
  theme(
    panel.grid.major.y = element_line(colour="#e0e0e0", linewidth=0.15),
    plot.title = element_markdown(size=13),
    plot.subtitle = element_markdown(size=10, family="Avenir Book"),
    axis.text.x = element_text(hjust=0, size=9),
    axis.text.y = element_text(hjust=0, size=8),
    axis.ticks.x = element_line(colour="#000000", linewidth=0.15)
  )


plot <-  (p4+plot_spacer()) /(p1+p3) + 
  plot_annotation(tag_levels =list(c('Final\nframe', 'Context\nframes'), '1')) &
      theme(plot.tag = element_text(size = 13, hjust=1), plot.tag.position = "left")




ggsave(here("figs", "08", "flu_years.png"), plot, width=12, height=9, dpi=500)


# 1.5 Cases, hospitalisations, deaths -------------------
# Deaths by age
# https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-deaths/

# Dashboard data
# https://coronavirus.data.gov.uk/details/download

# cases : https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDateAgeDemographics&format=csv
# admissions : https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=cumAdmissionsByAge&format=csv
# deaths : https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newDeaths28DaysByDeathDateAgeDemographics&format=csv
# 2020-12-20



install.packages("tidyjson")
library(tidyjson)
library(jsonlite)

cases <- read_csv(here("files", "csv", "cases.csv"))
deaths <- read_csv(here("files", "csv", "deaths.csv"))
admissions <- read_csv(here("files", "csv", "admissions.csv"))

young_cases <- cases |> select(age) |> unique() |> slice(5:13,15,16) |> pull(age)
old_cases <-   cases |> select(age) |> unique() |> slice(19:21) |> pull(age)
young_admissions <- c("18_to_64")
old_admissions <- c("85+")
young_deaths <- deaths |> select(age) |> unique() |> slice(5:13,15,16)  |> pull(age)
old_deaths <- deaths |> select(age) |> unique() |> slice(19:21) |> pull(age)


cases <- cases |> 
  select(age, date, count=cases) |> mutate(type="cases") |> 
  mutate(age= case_when(
    age %in% young_cases ~ "young",
    age %in% old_cases ~ "old",
    TRUE ~ NA)) |> filter(!is.na(age))
deaths <- deaths |> 
  select(age, date, count=deaths) |> mutate(type="deaths") |> 
  mutate(age= case_when(
    age %in% young_deaths ~ "young",
    age %in% old_deaths ~ "old",
    TRUE ~ NA)) |> filter(!is.na(age))
admissions <- admissions |> 
  select(age, date, count=value) |> mutate(type="hospitalisations") |> 
  mutate(age= case_when(
    age %in% young_admissions ~ "young",
    age %in% old_admissions ~ "old",
    TRUE ~ NA)) |> filter(!is.na(age)) |> 
  group_by(age) |> 
  arrange(date) |> 
  mutate(count=count-lag(count,1))

effect_data <- bind_rows(cases, admissions, deaths)

 
effect_data <- effect_data |> 
  group_by(type, age, date) |> 
  summarise(count=sum(count)) |> 
  mutate(
    #count=log(count),
    rolling=slider::slide_mean(count, before=7)
    ) |> ungroup() |> 
  filter(date >= "2020-12-20" & date <= "2021-04-05") |>  
  group_by(type, age) |> 
  mutate(rolling_rescaled=rolling/max(rolling)) |> ungroup() 


start_cases <- "2021-01-28"
start_admissions <- "2021-01-18"

effect_data <- effect_data |> 
  mutate(type=factor(
    type, levels=c("cases", "hospitalisations", "deaths"),
    labels=c("Cases", "Hospital admissions", "Deaths"))) 


title_text <- 
  "<span style='font-family:\"Avenir Black\"'>The UK's vaccine effect:</span> cases, hospital admissions and deaths are<br>
  now falling much faster in older groups than younger ones"
subtitle_text <- "Cases, hospitalisations and deaths as a percentage of winter peak, by age group (log scale)"

plot <- effect_data |> 
  ggplot() +
  geom_line(aes(x=date, y=rolling_rescaled, colour=age, group=age)) +
  geom_ribbon(
    data =  . %>%
      pivot_wider(id_cols=c(type, date), names_from=age, values_from=rolling_rescaled) %>%
      filter(
        (type == "Cases" & date >= lubridate::as_date("2021-02-21")) |
        (type == "Hospital admissions" & date >= lubridate::as_date("2021-01-17")) |
          (type == "Deaths" & date >= lubridate::as_date("2021-01-25"))
      ),
    aes(x=date, ymin=old, ymax=young), fill="#5F6D28", alpha=.4
  ) +

  geom_vline(xintercept=lubridate::as_date("2021-01-06"), linewidth=.3) +
  annotate("text", x=lubridate::as_date("2021-01-05"), y=.05, hjust=1, vjust=1,
           label="Lockdown\nbegins", size=2) +
  
  geom_vline(xintercept=lubridate::as_date("2021-01-17"), linetype="dashed", 
             linewidth=.4, colour="#5F6D28") +
  annotate("text", x=lubridate::as_date("2021-01-18"), colour="#5F6D28", y=.05, hjust=0, vjust=1,
           label="First dose\ngiven to 50%\nof age 50+", size=2, alpha=1) +
  
  geom_text(
    data = . %>% filter(type=="Cases", date==as_date("2021-04-01")) %>% slice(1),
    aes(x=date, y=.06), label="Vaccine\neffect", hjust=1, vjust=1, family="Avenir Black", colour="#5F6D28", size=2.5
  ) +
  
  geom_text(
    data = effect_data %>% filter(date==as_date("2021-04-04")) %>% 
      mutate(label=case_when(
        (type=="Cases" & age == "old") ~ "Age 80+",
        (type=="Cases" & age == "young") ~ "Age 15-69",
        (type=="Deaths" & age == "old") ~ "80+",
        (type=="Deaths" & age == "young") ~ "15-69",
        (type=="Hospital admissions" & age == "old") ~ "85+",
        (type=="Hospital admissions" & age == "young") ~ "18-64",
        TRUE ~ NA),
      rolling_rescaled=if_else(age=="old", rolling_rescaled-.001, rolling_rescaled+.05)
      ),
    aes(x=date, y=rolling_rescaled, colour=age, label=label), hjust=1, vjust=1, family="Avenir Black", size=2.5
  ) +
  
  geom_text(
    data = . %>% filter(type=="Cases", date==as_date("2021-04-01")) %>% slice(1),
    aes(x=date, y=.06), label="Vaccine\neffect", hjust=1, vjust=1, family="Avenir Black", colour="#5F6D28", size=2.5
  ) +
  
  
  facet_wrap(~type) +
  
  labs(title=title_text, subtitle = subtitle_text) +
  
  scale_x_date(
    breaks=c(lubridate::as_date("2020-12-20"), lubridate::as_date("2021-04-05")), 
    labels=c("Dec 20", "Apr 5"), expand=c(0,0)) +
  # scale_y_continuous(
  #   limits=c(0,1),
  #   breaks=c(.05,.1,.2,.5,1), 
  #   labels=c("5","10","20","50","100"), 
  #   expand=c(0,0)) +
  scale_y_log10(limits=c(.01,1),
                  breaks=c(.05,.1,.2,.5,1),
                  labels=c("5","10","20","50","100"),
                  expand=c(0,0)) +
  
  scale_colour_manual(values=c("#355399", "#CE6882"), guide="none") +
  
  
  theme(
    axis.line.y = element_blank(),
    strip.text.x = element_text(hjust=0, size=9, family="Avenir Black"),
    panel.grid.major.y = element_line(colour="#e0e0e0", linewidth=0.15),
    plot.title = element_markdown(size=12),
    plot.subtitle = element_markdown(size=9.5, family="Avenir Book"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(hjust=c(0,1), size=8),
    axis.text.y = element_text(hjust=0, size=8),
    axis.ticks.x = element_line(colour="#000000", linewidth=0.15)
  )
   


ggsave(here("figs", "08", "vaccine_effect.png"), plot, width=7.8, height=3.5, dpi=300)


   
deaths <- read_csv(here("files", "csv", "deaths.csv"))
hospitalisations <- read_csv(here("files", "csv", "admissions.csv"))




