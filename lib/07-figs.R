###############################################################################
# Figures for vis4sds
# Chapter 7
# Author: Roger Beecham
###############################################################################


library(tidyverse)
library(tidymodels) # For bootstrapping
library(here)
library(sf)
library(lubridate)
library(gganimate)
#install.packages("ggdist")
library(ggdist)
#install.packages("distributional")
library(distributional)
library(patchwork)

ped_veh <- fst::read_fst(here("../", "data", "ch4", "ped_veh.fst"))


array_data <- tibble(
  row=rep(1:10, times=1, each=10),
  col=rep(1:10, times=10, each=1),
  Oxford=sample(c(FALSE,TRUE), size=100, replace=TRUE, prob=c(.83,.17)),
  Fareham=sample(c(FALSE,TRUE), size=100, replace=TRUE, prob=c(.59,.41))
)

plot <- array_data %>%
  pivot_longer(cols=c(Oxford,Fareham), names_to="la", values_to="is_ksi") %>%
  ggplot(aes(x=row,y=col, fill=is_ksi)) +
  geom_tile(colour="#ffffff", size=1) +
  scale_fill_manual(values=c("#fee0d2","#de2d26"), guide="none")+
  facet_wrap(~la) +
  theme(
    axis.line = element_blank(),
    axis.text.x=element_blank(),axis.title.x=element_blank(),
    axis.text.y=element_blank(),axis.title.y=element_blank(),
    panel.grid.major=element_blank(), panel.grid.minor = element_blank())


array_data <- tibble(
  row=rep(1:16, times=1, each=16),
  col=rep(1:16, times=16, each=1),
  Bristol=
    c(sample(
      c(rep(TRUE, times=1, each=35), rep(FALSE, times=1, each=193)),
      size=228, replace=FALSE), rep(3,28)),
  Sheffield=
    c(sample(
      c(rep(TRUE, times=1, each=124), rep(FALSE, times=1, each=124)),
      size=248, replace=FALSE), rep(3,8))
)


icon <- array_data %>%
  pivot_longer(cols=c(Bristol,Sheffield), names_to="la", values_to="is_ksi") %>%
  mutate(is_ksi=factor(is_ksi)) %>%
  ggplot(aes(x=row,y=col, fill=is_ksi, group=is_ksi)) +
  geom_tile(colour="#ffffff", size=.7) +
  scale_fill_manual(values=c("#fee0d2","#de2d26", "#f0f0f0"), guide=FALSE)+
  facet_wrap(~la) +
  coord_equal() +
  theme(
    axis.line = element_blank(),
    axis.text.x=element_blank(),axis.title.x=element_blank(),
    axis.text.y=element_blank(),axis.title.y=element_blank(),
    panel.grid.major=element_blank(), panel.grid.minor = element_blank())

array_data <- tibble(
  col=rep(1:25, times=1, each=10),
  row=rep(1:10, times=25, each=1),
  Bristol=
    c(rep(TRUE, times=1, each=35), rep(FALSE, times=1, each=193),rep(-1,22)),
  Sheffield=
      c(rep(TRUE, times=1, each=124), rep(FALSE, times=1, each=124),rep(-1,2))
)

waffle <- array_data %>%
  pivot_longer(cols=c(Bristol,Sheffield), names_to="la", values_to="is_ksi") %>%
  mutate(is_ksi=factor(is_ksi)) %>%
  ggplot(aes(x=col,y=row, fill=is_ksi, group=is_ksi)) +
  geom_tile(colour="#ffffff", size=.7) +
  scale_fill_manual(values=c("#f0f0f0","#fee0d2","#de2d26"), guide=FALSE)+
  facet_wrap(~la) +
  coord_equal() +
  theme(
    strip.text.x = element_blank(),
    axis.line = element_blank(),
    axis.text.x=element_blank(),axis.title.x=element_blank(),
    axis.text.y=element_blank(),axis.title.y=element_blank(),
    panel.grid.major=element_blank(), panel.grid.minor = element_blank())


plot <- icon /waffle + plot_layout(heights=c(1,.4))

ggsave(filename=here("figs", "07", "icon_arrays.png"), plot=plot,width=7, height=5, dpi=300)

n_row <- 43
n_col <- 46
grid_index <- map2_df(
  rep(1:n_row, each = n_col), rep(1:n_col, times = n_row),
  ~ tibble(col = .y, row = .x)
) |>
  mutate(x=col, y=row, id=row_number()) |>
  st_as_sf(coords = c("x", "y"))

grid <-
  st_sf(
    geom=st_make_grid(grid_index, n=c(n_col,n_row), what="polygons", cellsize=1)
  ) |>
  mutate(id=row_number())

# Add centroid locations to hex_grid object.
grid <- grid |>
  left_join(grid_index |> st_drop_geometry() )

# grid |>  ggplot() + geom_sf() +
#   geom_text(aes(x=col+.5, y=row+.7, label=paste("c",col)), size=2) +
#   geom_text( aes(x=col+.5, y=row+.3, label=paste("r", row_lookup)), size=2)

theatre_cells <- read_csv(here("lib", "theatre_cells.csv"))

grid |>
  ggplot() +
  geom_sf(fill="transparent", linewidth=.0) +
  geom_sf(
    data=. %>% inner_join(theatre_cells, by=c("row"="row", "col"="col"))
  ) +
  annotate("text", x=23, y=1, label="Stage") +
  annotate("text", x=23, y=21, label="Orchestra") +
  annotate("text", x=23, y=31, label="Front mezzanine") +
  annotate("text", x=23, y=42, label="Rear mezzanine") +
  theme_void()

poll_data <- bind_rows(
  theatre_cells |> slice_sample(n=286) |> add_column(poll="1. FiveThirtyEight\n286 cases in 1,000"),
  theatre_cells |> slice_sample(n=150) |> add_column(poll="2. NYT Upshot\n150 cases in 1,000"),
  theatre_cells |> slice_sample(n=20) |> add_column(poll="3. Huffpost Pollster\n20 cases in 1,000")
)

poll_theatres <- bind_rows(
  grid |> add_column(poll="1. FiveThirtyEight\n286 cases in 1,000"),
  grid |> add_column(poll="2. NYT Upshot\n150 cases in 1,000"),
  grid |> add_column(poll="3. Huffpost Pollster\n20 cases in 1,000")
)


plot <- poll_theatres |>
  ggplot() +
  geom_sf(fill="transparent", linewidth=.0) +
  geom_sf(
    data=. %>% inner_join(theatre_cells, by=c("row"="row", "col"="col"))
  ) +
  geom_sf(
    data=. %>% inner_join(poll_data, by=c("row"="row", "col"="col", "poll"="poll")),
    fill="#000000"
  ) +
  annotate("text", x=23, y=1, label="Stage", alpha=.5) +
  annotate("text", x=23, y=21, label="Orchestra", alpha=.5) +
  annotate("text", x=23, y=31, label="Front mezzanine", alpha=.5) +
  annotate("text", x=23, y=42, label="Rear mezzanine", alpha=.5) +
  facet_wrap(~poll) +
  theme_void()

ggsave(filename=here("figs", "07", "risk_theatre.png"), plot=plot,width=7.5, height=3.1, dpi=300)

rate_boots_selected <- ped_veh %>%
  mutate(
    is_ksi=accident_severity!="Slight",
    year=lubridate::year(date)
  ) %>%
  filter(year==2019,
         local_authority_district %in% c("Bristol, City of", "Sheffield", "Bromsgrove", "Cotswold")
  ) %>%
  select(local_authority_district, is_ksi) %>%
  nest(data=-local_authority_district) %>%
  mutate(la_boot=map(data, bootstraps, times=1000, apparent=TRUE)) %>%
  select(-data) %>%
  unnest(la_boot) %>%
  mutate(
    is_ksi=map(splits, ~ analysis(.) %>% pull(is_ksi)),
    ksi_rate=map_dbl(is_ksi, ~mean(.x)),
    sample_size=map_dbl(is_ksi, ~length(.x))
  ) %>%
  select(-c(splits, is_ksi))


plot <- rate_boots_selected %>%
  group_by(local_authority_district) %>%
  mutate(std.error=sd(ksi_rate)/sqrt(sample_size), lower=quantile(ksi_rate,probs=.025), upper=quantile(ksi_rate,probs=.975)) %>%
  filter(id=="Apparent") %>%
  ggplot(
    aes(x=reorder(local_authority_district, ksi_rate),
        y=ksi_rate,ymin=lower, ymax=upper)) +
  geom_pointrange(colour=site_colours$primary) +
  labs(y="estimated ksi rate", x="local authority") +
  coord_flip()

ggsave(filename=here("figs", "07", "bootstrap_selected.png"), plot=plot,width=5.5, height=3, dpi=300)

half_eye <- rate_boots_selected %>%
  group_by(local_authority_district) %>%
  mutate(std.error=sd(ksi_rate), lower=quantile(ksi_rate,probs=.025), upper=quantile(ksi_rate,probs=.975)) %>%
  filter(id=="Apparent") %>%
  ggplot(aes(x=reorder(local_authority_district, ksi_rate), y=ksi_rate)) +
  stat_dist_halfeye(
    aes(dist = dist_normal(mu = ksi_rate, sigma = std.error)),
    point_size = 1.5, colour=site_colours$primary, fill=site_colours$primary, alpha=.5) +
  labs(y="estimated ksi rate", x="local authority", subtitle="Half-eyes")+
  coord_flip()

gradient <- rate_boots_selected %>%
  group_by(local_authority_district) %>%
  mutate(
    std.error=sd(ksi_rate),
    lower=quantile(ksi_rate,probs=.025),
    upper=quantile(ksi_rate,probs=.975),
    std.error=(upper-lower)/2
  ) %>%
  filter(id=="Apparent") %>%
  ggplot(aes(x=reorder(local_authority_district, ksi_rate), y=ksi_rate)) +
  stat_dist_gradientinterval(
    aes(dist = dist_normal(mu=ksi_rate, sigma=std.error)),
    point_size = 1.5, colour=site_colours$primary, fill=site_colours$primary, alpha=.7) +
  labs(y="estimated ksi rate", x="local authority", subtitle="Gradient bars")+
  coord_flip() +
  theme(
    axis.text.y=element_blank(),
    axis.title.y=element_blank()
  )


plot <- half_eye + gradient + plot_layout(nrow=1)


ggsave(filename=here("figs", "07", "selected_uncertainty.png"), plot=plot,width=8, height=3.5, dpi=300)


rate_boots_temporal <- ped_veh %>%
  mutate(
    is_ksi=if_else(accident_severity=="Slight", FALSE, TRUE),
    year=lubridate::year(date)
  ) %>%
  filter(local_authority_district %in% c("Bristol, City of", "Sheffield", "Bromsgrove", "Cotswold")
  ) %>%
  select(local_authority_district, is_ksi, year) %>%
  nest(-c(local_authority_district, year)) %>%
  mutate(la_boot = map(data, bootstraps, times=50, apparent=TRUE)) %>%
  select(-data) %>%
  unnest(la_boot) %>%
  mutate(
    is_ksi=map(splits, ~ analysis(.) %>% pull(is_ksi)),
    ksi_rate=map_dbl(is_ksi, ~mean(.x)),
    sample_size=map_dbl(is_ksi, ~length(.x))
  ) %>%
  select(-c(splits, is_ksi))

plot <- rate_boots_temporal %>%
  mutate(
    year=as.character(year),
    local_authority_district=factor(
      local_authority_district, levels=c("Cotswold", "Sheffield", "Bromsgrove", "Bristol, City of")
    )
  ) %>%
  ggplot(aes(x=year, y=ksi_rate)) +
  geom_line(
    data=. %>%  filter(id=="Apparent"),
    aes(group=id), colour=site_colours$primary, size=.5
  ) +
  geom_line(
    data=. %>%  filter(id!="Apparent"),
    aes(group=id), colour=site_colours$primary, alpha=.2, size=.1
  ) +
  facet_wrap(~local_authority_district) +
  theme(axis.text = element_blank())

ggsave(filename=here("figs", "07", "temporal_uncertainty.png"), plot=plot,width=8, height=5, dpi=300)

sample_ids <- rate_boots_temporal |>  select(id) |> unique() |>  sample_n(size=20) |> pull(id)

rate_boots_temporal <- ped_veh %>%
  mutate(
    is_ksi=if_else(accident_severity=="Slight", FALSE, TRUE),
    year=lubridate::year(date)
  ) %>%
  filter(local_authority_district %in% c("Sheffield")
  ) %>%
  select(local_authority_district, is_ksi, year) %>%
  nest(data=-c(local_authority_district, year)) %>%
  mutate(la_boot = map(data, bootstraps, times=50, apparent=TRUE)) %>%
  select(-data) %>%
  unnest(la_boot) %>%
  mutate(
    is_ksi=map(splits, ~ analysis(.) %>% pull(is_ksi)),
    ksi_rate=map_dbl(is_ksi, ~mean(.x)),
    sample_size=map_dbl(is_ksi, ~length(.x))
  ) %>%
  select(-c(splits, is_ksi))

hop <- rate_boots_temporal |>
  filter(id %in% sample_ids) |>
  mutate(id=paste0("frame: ", as.numeric(factor(id))),
         id=factor(id, levels=map(1:20, ~paste0("frame: ", .x)) |> unlist())
         )|>
  ggplot(aes(x=year, y=ksi_rate)) +
  geom_line(aes(group=id), colour=site_colours$primary, size=.3, alpha=.7) +
  facet_wrap(~id, scales="free", nrow=4) +
  scale_y_continuous(limits=c(0,.6))+
  theme(
    axis.text.x=element_blank(), axis.text.y=element_blank(),
    strip.text.x = element_text(size=9),
    axis.title.x = element_blank(), axis.title.y = element_blank()
  )

ggsave(filename=here("figs", "07", "hop.png"), plot=hop,width=6, height=5, dpi=300)
