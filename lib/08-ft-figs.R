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
# dat_original <- dat

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
ggsave(here("cum_deaths1.png"), plot, width=5, height=3, dpi=300)

country_focus <- c("Spain", "France", "Italy", "China", "US", "UK", "Japan", "S Korea", "Iran")

country_levels <- 
  c("Spain", "France", "Italy", "China", "US", "UK", "Japan", "S Korea", "Iran",
    "Netherlands", "Switzerland", "Germany", "Belgium", "Indonesia", "Philippines", "Iraq", "Sweden")


yscalebreaks <- sapply(c(1,2,5), function(x) {x * 10^(0:10)}) %>% t %>% as.vector %>% .[.<max(dataclean$cumulcases)*2]

growth_count <- function(start, rate, days) {start*(2^rate)^days}

plot <- `dat |> 
  mutate(
    country = case_when(
      country == "United Kingdom" ~ "UK",
      country == "Korea, S" ~ "S Korea",
      TRUE ~ country),
    country_focus=country %in% country_focus,
    country=factor(country, levels=country_levels),
  ) |> 
  ggplot(aes(x=day_num, y=deaths_cum, colour=country)) +
  geom_line(aes(group=country), linewidth=.5 ) +

  #geom_abline(intercept = 0, slope = 1) +
  geom_point(size=.5) +
  geom_point(data=. %>% filter(date=="2020-03-24"), aes(fill=country), pch=21, colour="#000000") +
  geom_text(data=. %>% filter(date=="2020-03-24", country_focus), 
             aes(label=country), hjust=0, nudge_x = .5, family="Avenir Heavy", size=3.2) +
  
  annotate("segment", x=0, xend=40, y=10, yend=growth_count(10,1/2,25), linetype="dashed") +
  
  scale_y_log10(breaks = c(10, 100, 1000, 10000, 100000), expand = c(0, 0), label=scales::comma) +
  scale_x_continuous(breaks=seq(10, 60, 10), expand = c(0, 0))+

  labs(x="# days since 10th death", y="") +
  scale_colour_manual(values=country_colours, guide="none") +
  scale_fill_manual(values=country_colours, guide="none") +
  theme(
    axis.line.y =element_blank(),
    panel.grid.major = element_line(colour="#e0e0e0", size=0.2)
    )
`
ggsave(here("cum_deaths2.png"), plot, width=8.2, height=4, dpi=300)



rate_boots <- ped_veh |> 
  mutate(
    is_ksi=accident_severity!="Slight",
    year=lubridate::year(date)
  ) |> 
  filter(year==2019,
         local_authority_district %in% c("Bristol, City of",
                                         "Sheffield", "Bromsgrove", "Cotswold")
  ) |>
  select(local_authority_district, is_ksi) |> 
  nest(data=-local_authority_district) |> 
  mutate(la_boot=map(data, bootstraps, times=1000, apparent=TRUE)) |> 
  select(-data) |> 
  unnest(la_boot) |> 
  mutate(
    is_ksi=map(splits, ~ analysis(.) |>  pull(is_ksi)),
    ksi_rate=map_dbl(is_ksi, ~mean(.x)),
    sample_size=map_dbl(is_ksi, ~length(.x))
  ) |> 
  select(-c(splits, is_ksi))



rate_boots |> 
  ggplot(aes(ksi_rate)) +
  geom_histogram() +
  facet_wrap(~local_authority_district)


rate_boots |>
  group_by(local_authority_district) |>
  mutate(std.error=sd(ksi_rate)) |>
  filter(id=="Apparent")

