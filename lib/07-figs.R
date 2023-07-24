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

st_write(grid, here("../", "data", "ch7", "theatre_cells.geojson"))

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

rate_boots_selected <- ped_veh |> 
  mutate(
    is_ksi=accident_severity!="Slight",
    year=lubridate::year(date)
  ) |> 
  filter(year==2019,
         local_authority_district %in% c("Bristol, City of", "Sheffield", "Bromsgrove", "Cotswold")
  ) %>%
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
  labs(y="estimated ksi rate", x="local authority")+ #, subtitle="Half-eyes")+
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
  labs(y="estimated ksi rate", x="local authority")+ #, subtitle="Gradient bars")+
  coord_flip() +
  theme(
    axis.text.y=element_blank(),
    axis.title.y=element_blank()
  )


plot <- half_eye + gradient + plot_layout(nrow=1)


ggsave(filename=here("figs", "07", "selected_uncertainty.png"), plot=plot,width=8, height=3, dpi=300)


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
      local_authority_district, levels=c("Cotswold", "Bromsgrove", "Bristol, City of", "Sheffield")
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
  facet_wrap(~local_authority_district, nrow=1) +
  theme(axis.text = element_blank())

ggsave(filename=here("figs", "07", "temporal_uncertainty.png"), plot=plot,width=8, height=3, dpi=300)

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
  facet_wrap(~id, scales="free", nrow=3) +
  scale_y_continuous(limits=c(0,.6))+
  theme(
    axis.text.x=element_blank(), axis.text.y=element_blank(),
    strip.text.x = element_text(size=9),
    axis.title.x = element_blank(), axis.title.y = element_blank()
  )

ggsave(filename=here("figs", "07", "hop.png"), plot=hop,width=6, height=3.2, dpi=300)

# Oxford 2019: 9  KSI, 43 Slight 17%
# Fareham 2019: 12 KSI, 17 Slight 41%

array_data <- tibble(
  row=rep(1:10, times=1, each=10),
  col=rep(1:10, times=10, each=1),
  Oxford=
    sample(
      c(rep(TRUE, times=1, each=17), rep(FALSE, times=1, each=83)),
      size=100, replace=FALSE),
  Fareham=
    sample(
      c(rep(TRUE, times=1, each=41), rep(FALSE, times=1, each=59)),
      size=100, replace=FALSE)
)

array_data %>% 
  pivot_longer(cols=c(Oxford,Fareham), names_to="la", values_to="is_ksi") %>%
  ggplot(aes(x=row,y=col, fill=is_ksi)) +
  geom_tile(colour="#ffffff", size=1) +
  scale_fill_manual(values=c("#fee0d2","#de2d26"), guide="none")+
  facet_wrap(~la)

theatre_cells <- read_csv(here("lib", "theatre_cells.csv"))

n_row <- 43
n_col <- 46
# Generate dataset of candidate seat locations (row, col) for theatre.
grid_index <- map2_df(
  rep(1:n_row, each = n_col), rep(1:n_col, times = n_row),
  ~ tibble(col = .y, row = .x) 
) |>
  mutate(x=col, y=row, id=row_number()) |> 
  st_as_sf(coords = c("x", "y")) 
# Create shapefile on seat locations. 
grid <-
  st_sf(
    geom=st_make_grid(grid_index, n=c(n_col,n_row), what="polygons", cellsize=1)
  ) |> 
  mutate(id=row_number()) |> 
  left_join(grid_index |> st_drop_geometry() ) 

grid |>
  ggplot() +
  geom_sf(fill="transparent", linewidth=.0) +
  geom_sf(
    # inner join on theatre cells to create gallery
    data=. %>% inner_join(theatre_cells, by=c("row"="row", "col"="col"))
  ) +
  annotate("text", x=23, y=1, label="Stage") +
  annotate("text", x=23, y=21, label="Orchestra") +
  annotate("text", x=23, y=31, label="Front mezzanine") +
  annotate("text", x=23, y=42, label="Rear mezzanine") +
  theme_void()

# 1,000 seat gallery -- so use this for encoding KSI rates
poll_data <- bind_rows(
  theatre_cells |> slice_sample(n=170) |> add_column(poll="Oxford\n17 crashes in 1,000 KSI"),
  theatre_cells |> slice_sample(n=410) |> add_column(poll="Fareham\n41 crashes in 1,000 KSI")
)

# Oxford 2019: 9  KSI, 43 Slight 17%
# Fareham 2019: 12 KSI, 17 Slight 41%

poll_theatres <- bind_rows(
  grid |> add_column(poll="Oxford\n17 crashes in 1,000 KSI"),
  grid |> add_column(poll="Fareham\n41 crashes in 1,000 KSI")
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


lad_boundary <- st_read(here("../", "data", "ch7", "lad.geojson"))  |>  
  mutate(pop_density=(res_2019+workday)/st_areashape)


boots_lad <- ped_veh |> 
  # LAD changes
  mutate(local_authority_district=case_when(
    local_authority_district == "Stratford-upon-Avon" ~ "Stratford-on-Avon",
    local_authority_district == "East Dorset" ~ "Dorset",
    local_authority_district == "North Dorset" ~ "Dorset",
    local_authority_district == "West Dorset" ~ "Dorset",
    local_authority_district == "Forest Heath" ~ "West Suffolk",
    local_authority_district == "St. Edmundsbury" ~ "West Suffolk",
    local_authority_district == "Suffolk Coastal" ~ "East Suffolk",
    local_authority_district == "Waveney" ~ "East Suffolk",
    local_authority_district == "St. Albans" ~ "St Albans",
    local_authority_district =="Shepway" ~ "Folkestone and Hythe",
    local_authority_district =="Taunton Deane" ~ "Somerset West and Taunton",
    local_authority_district =="West Somerset" ~ "Somerset West and Taunton",
    local_authority_district == "Bournemouth" ~ "Bournemouth, Christchurch and Poole",
    local_authority_district == "Christchurch" ~ "Bournemouth, Christchurch and Poole",
    local_authority_district == "Poole" ~ "Bournemouth, Christchurch and Poole",
    local_authority_district == "Purbeck" ~ "Dorset",
    local_authority_district == "Weymouth and Portland" ~ "Dorset",
    TRUE ~ local_authority_district)
    ) |> 
  mutate(is_ksi=!accident_severity=="Slight") |> 
  # Select out LAD code as no other crash context required.
  select(local_authority_district, is_ksi) |> 
  # Nesting to collapse data to a list-col.
  nest(data=c(local_authority_district, is_ksi)) |> 
  # Resample observations from this data frame with replacement, keep original data.
  mutate(la_boot = map(data, rsample::bootstraps, times=100, apparent=TRUE)) |> 
  # Unnest to generate data frame of bootsrap IDs and associated data, stored in splits.
  select(-data) %>% unnest(la_boot) %>%
  # Map over splits and extract LAD code.
  mutate(
    la=map(splits, ~rsample::analysis(.) %>% pull(local_authority_district)),
    is_ksi=map(splits, ~rsample::analysis(.) %>% pull(is_ksi))
  ) |> 
  # Unnest to data frame where each observation is a bootstrap ID and sampled LAD.
  select(-splits) |>  unnest(c(la, is_ksi)) |> 
  group_by(id, la) |> 
  summarise(crash_count=n(), ksi=mean(is_ksi)) |>  ungroup() 
  

model2 <- boots_lad |> 
  inner_join(
    lad_boundary |> st_drop_geometry() |> select(lad19cd, lad19nm, pop_density) |> 
    # censor City of London
    mutate(pop_density=pmin(pop_density, .05)) |> 
    inner_join(lad_region_lookup |> select(LAD19CD, RGN19NM), by=c("lad19cd"="LAD19CD")) , 
    by=c("la"="lad19nm")) |> 
  mutate(ksi_count = ksi*crash_count, pop_density=sqrt(pop_density), is_london = RGN19NM == "London") |> 
  nest(data=-id) |> 
  mutate(
    model = map(data, ~lme4::glmer(ksi_count ~  (1|la) + pop_density + offset(log(crash_count)), # (1|is_london)
                                   data=.x,
                                   family = poisson)),
    bayesian_rr = map(model, ~lme4::ranef(.x) %>% as_tibble() %>%  dplyr::select(grp, condval))
  ) |> 
  ungroup() |> 
  dplyr::select(-model) |> 
  unnest(c(bayesian_rr)) |> 
  filter(!grp %in% c(TRUE, FALSE)) |> 
  #unnest(data) |> 
  mutate(
    bayes_rr=exp(condval),
    rr_estimate=if_else(id=="Apparent", bayes_rr,0)
  ) %>% mutate(la=grp) |>  select(-c(data, grp)) |> 
  group_by(la) %>%
  summarise(
    rr_estimate=max(rr_estimate),
    lower=quantile(bayes_rr,probs=.025),
    upper=quantile(bayes_rr,probs=.975),
  )

hexmap <- st_read(here("../", "data", "ch7", "lad_hex.geojson"))

map_data <- hexmap %>%
    inner_join(lad_region_lookup,
               by=c("lad_code"="LAD19CD")) %>% dplyr::select(-id) %>%
    left_join(model1_data_lon, by=c("LAD19NM"="la")) |> 
    group_by(LAD19NM) |> 
    mutate(
      is_sig= !between(1,lower, upper),
      sig_type = case_when(
        !is_sig ~ "none", 
        rr_estimate > 1 ~ "greater", 
        rr_estimate < 1 ~ "less")
    ) |> ungroup() 

map_data <- t %>% select(lad19cd, east, north) |> 
  inner_join(lad_region_lookup,
             by=c("lad19cd"="LAD19CD")) %>% 
  left_join(model2, by=c("LAD19NM"="la")) |> 
  group_by(LAD19NM) |> 
  mutate(
    is_sig= !between(1,lower, upper),
    sig_type = case_when(
      !is_sig ~ "none", 
      rr_estimate > 1 ~ "greater", 
      rr_estimate < 1 ~ "less")
  ) |> ungroup() 


# Annotate high/low values outside of London. 
bristol <- map_data |> filter(LAD19NM == "Bristol, City of")
bath <-   map_data |> filter(LAD19NM == "Bath and North East Somerset")
canterbury <-   map_data |> filter(LAD19NM == "Canterbury")
york <-   map_data |> filter(LAD19NM == "York")
leicester <-   map_data |> filter(LAD19NM == "Leicester")
exeter <-   map_data |> filter(LAD19NM == "Exeter")
ipswich <-   map_data |> filter(LAD19NM == "Ipswich")
derby <-   map_data |> filter(LAD19NM == "Derby")

cotswold <-   map_data |> filter(LAD19NM == "Cotswold")
corby <-   map_data |> filter(LAD19NM == "Corby")
chelmsford <-   map_data |> filter(LAD19NM == "Chelmsford")
hartlepool <-   map_data |> filter(LAD19NM == "Hartlepool")
liverpool <-   map_data |> filter(LAD19NM == "Liverpool")
burnley <-   map_data |> filter(LAD19NM == "Burnley")



# Bounds to pin oriented lines to.
max_rr <- max(map_data %>% pull(rr_estimate), na.rm = TRUE)
min_rr <- min(map_data %>% pull(rr_estimate), na.rm = TRUE)
# Define colours for encoding by sig type and direction.
colours_type <- c("#b2182b","#2166ac", "#878787")

eng_bounds <- st_bbox(
  lad_boundary |> inner_join(lad_region_lookup, by=c("lad19cd"="LAD19CD"))
  )
eng_width <- eng_bounds$xmax-eng_bounds$xmin
eng_height <- eng_bounds$ymax-eng_bounds$ymin

p1_lon <- map_data |> 
  ggplot()+
  geom_sf(aes(fill=sig_type, alpha=is_sig, colour=sig_type), linewidth=0.1, alpha=.3)+
  geom_sf(data=. %>% group_by(RGN19CD) %>% summarise(), fill="transparent", linewidth=0.3)+
  coord_sf(datum=NA)+
  # Greater.
  geom_spoke(
    data=. %>% filter(rr_estimate>1),
    aes(x=east, y=north, angle=get_radians(map_scale(rr_estimate,1,max_rr,90,45)), colour=sig_type),
    linewidth=.3, radius=.4, position="center_spoke"
  )+
  # Less.
  geom_spoke(
    data=. %>% filter(rr_estimate<1),
    aes(x=east, y=north, angle=get_radians(map_scale(rr_estimate,min_rr,1,135,90)), colour=sig_type),
    linewidth=.3, radius=.4, position="center_spoke" 
  )+
  scale_fill_manual(values=colours_type) +
  scale_colour_manual(values=colours_type) +
  guides(fill=FALSE, colour=FALSE, size=FALSE, alpha=FALSE) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())

model1_map_real <- map_data |> 
  ggplot()+
  geom_sf(aes(fill=sig_type, alpha=is_sig, colour=sig_type), linewidth=0.03, alpha=.3)+
  geom_sf(data=. %>% group_by(RGN19CD) %>% summarise(), fill="transparent", linewidth=0.15)+
  coord_sf(datum=NA)+
  # Greater.
  geom_spoke(
    data=. %>% filter(rr_estimate>1),
    aes(x=east, y=north, angle=get_radians(map_scale(rr_estimate,1,max_rr,90,45)), colour=sig_type),
    linewidth=.3, radius=6000, position="center_spoke" #.4
  )+
  # Less.
  geom_spoke(
    data=. %>% filter(rr_estimate<1),
    aes(x=east, y=north, angle=get_radians(map_scale(rr_estimate,min_rr,1,135,90)), colour=sig_type),
    linewidth=.3, radius=6000, position="center_spoke" #.4
  )+
  
  annotate(geom="text", x=bristol$east-0.18*eng_width, y=bristol$north, hjust="centre", label=paste0("Bristol\n", round(bristol$rr_estimate,2)), family="Avenir Next", size=2.5, colour="#2166ac")+
  annotate(geom="segment", xend=bristol$east-0.12*eng_width, yend=bristol$north, x=bristol$east-.01*eng_width, y=bristol$north, size=.1)+
  
  annotate(geom="text", x=york$east+0.17*eng_width, y=york$north, hjust="centre", label=paste0(york$LAD19NM, "\n", round(york$rr_estimate,2)), family="Avenir Next", size=2.5, colour="#2166ac")+
  annotate(geom="segment", xend=york$east+0.13*eng_width, yend=york$north, x=york$east+.01*eng_width, y=york$north, size=.1)+
  
  annotate(geom="text", x=canterbury$east+0.03*eng_width, y=canterbury$north-0.1*eng_height, hjust="centre", label=paste0(canterbury$LAD19NM, "\n", round(canterbury$rr_estimate,2)), family="Avenir Next", size=2.5, colour="#2166ac")+
  annotate(geom="segment", xend=canterbury$east, yend=canterbury$north-0.01*eng_height, x=canterbury$east+.03*eng_width, y=canterbury$north-0.07*eng_height, size=.1)+

  annotate(geom="text", x=leicester$east-0.3*eng_width, y=leicester$north, hjust="centre", label=paste0(leicester$LAD19NM, "\n", round(leicester$rr_estimate,2)), family="Avenir Next", size=2.5, colour="#2166ac")+
  annotate(geom="segment", xend=leicester$east-.01*eng_width, yend=leicester$north, x=leicester$east-.25*eng_width, y=leicester$north, size=.1)+
  
  annotate(geom="text", x=ipswich$east+0.05*eng_width, y=ipswich$north-0.08*eng_height, hjust="centre", label=paste0(ipswich$LAD19NM, "\n", round(ipswich$rr_estimate,2)), family="Avenir Next", size=2.5, colour="#2166ac")+
  annotate(geom="segment", xend=ipswich$east, yend=ipswich$north-0.01*eng_height, x=ipswich$east+.03*eng_width, y=ipswich$north-0.05*eng_height, size=.1)+
  
  annotate(geom="text", x=cotswold$east-0.25*eng_width, y=cotswold$north+0.04*eng_height, hjust="centre", label=paste0(cotswold$LAD19NM, "\n", round(cotswold$rr_estimate,2)), family="Avenir Next", size=2.5, colour="#b2182b")+
  annotate(geom="segment", xend=cotswold$east-0.18*eng_width, yend=cotswold$north+0.04*eng_height, x=cotswold$east-.01*eng_width, y=cotswold$north, size=.1)+
  
  annotate(geom="text", x=liverpool$east-0.15*eng_width, y=liverpool$north, hjust="centre", label=paste0(liverpool$LAD19NM, "\n", round(liverpool$rr_estimate,2)), family="Avenir Next", size=2.5, colour="#b2182b")+
  annotate(geom="segment", xend=liverpool$east-0.08*eng_width, yend=liverpool$north, x=liverpool$east-.01*eng_width, y=liverpool$north, size=.1)+
  
  annotate(geom="text", x=hartlepool$east+0.15*eng_width, y=hartlepool$north, hjust="centre", label=paste0(hartlepool$LAD19NM, "\n", round(hartlepool$rr_estimate,2)), family="Avenir Next", size=2.5, colour="#b2182b")+
  annotate(geom="segment", xend=hartlepool$east+0.08*eng_width, yend=hartlepool$north, x=hartlepool$east+.01*eng_width, y=hartlepool$north, size=.1)+
  
  
  scale_fill_manual(values=colours_type) +
  scale_colour_manual(values=colours_type) +
  guides(fill=FALSE, colour=FALSE, size=FALSE, alpha=FALSE) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())


model2_map_real_dens <- map_data |> 
  ggplot()+
  geom_sf(aes(fill=sig_type, alpha=is_sig, colour=sig_type), linewidth=0.03, alpha=.3)+
  geom_sf(data=. %>% group_by(RGN19CD) %>% summarise(), fill="transparent", linewidth=0.15)+
  coord_sf(datum=NA)+
  # Greater.
  geom_spoke(
    data=. %>% filter(rr_estimate>1),
    aes(x=east, y=north, angle=get_radians(map_scale(rr_estimate,1,max_rr,90,45)), colour=sig_type),
    linewidth=.3, radius=6000, position="center_spoke" #.4
  )+
  # Less.
  geom_spoke(
    data=. %>% filter(rr_estimate<1),
    aes(x=east, y=north, angle=get_radians(map_scale(rr_estimate,min_rr,1,135,90)), colour=sig_type),
    linewidth=.3, radius=6000, position="center_spoke" #.4
  )+
  
  # annotate(geom="text", x=havering$east+0.14*eng_width, y=havering$north, hjust="centre", label=paste0("Havering\n", round(havering$rr_estimate,2)), family="Avenir Next", size=2.5, colour="#b2182b")+
  # annotate(geom="segment", xend=havering$east+0.1*eng_width, yend=havering$north, x=havering$east+.01*eng_width, y=havering$north, size=.1)+
  # 
  # annotate(geom="text", x=hillingdon$east-0.02*eng_width, y=hillingdon$north-0.18*eng_height, hjust="centre", label=paste0("Hillingdon\n", round(havering$rr_estimate,2)), family="Avenir Next", size=2.5, colour="#b2182b")+
  # annotate(geom="segment", xend=hillingdon$east-0.02*eng_width, yend=hillingdon$north-0.15*eng_height, x=hillingdon$east, y=hillingdon$north, size=.1)+
  # 
  annotate(geom="text", x=tower$east+0.1*eng_width, y=tower$north-0.16*eng_height, hjust="centre", label=paste0("City | Tower H\n", round(1.27,2), " | ", round(tower$rr_estimate,2)), family="Avenir Next", size=2.5, colour="#b2182b")+
  annotate(geom="segment", xend=tower$east+0.07*eng_width, yend=tower$north-0.14*eng_height, x=tower$east, y=tower$north-.01*eng_height, size=.1)+
  
  annotate(geom="text", x=pendle$east-0.13*eng_width, y=pendle$north-0.06*eng_height, hjust="right", label="High in North West\n Liverpool KSI is\n 1.4x > than expected", family="Avenir Next", size=2.5) +
  annotate(geom="text", x=wakefield$east+0.18*eng_width, y=wakefield$north+0.06*eng_height, hjust="left", label="High rates in\n South Yorkshire\n York remains locally\n exceptional", family="Avenir Next", size=2.5) +
  annotate(geom="text", x=birmingham$east-0.17*eng_width, y=birmingham$north, hjust="right", label="Group of high rates in\n Birmingham and neighbours\n Walsall, Wolverhampton etc.", family="Avenir Next", size=2.5) +
  annotate(geom="text", x=southampton$east, y=southampton$north-0.08*eng_height, hjust="centre", label="South coast has\n consistantly high rates", family="Avenir Next", size=2.5) +
  
  
  scale_fill_manual(values=colours_type) +
  scale_colour_manual(values=colours_type) +
  guides(fill=FALSE, colour=FALSE, size=FALSE, alpha=FALSE) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())


pendle <-   map_data |> filter(LAD19NM == "Pendle")
wakefield <-   map_data |> filter(LAD19NM == "Wakefield")
birmingham <-   map_data |> filter(LAD19NM == "Birmingham")
southampton <-   map_data |> filter(LAD19NM == "Southampton")


Liverpool, Manchester, Wigan 

model2_map_real <- map_data |> 
  ggplot()+
  geom_sf(aes(fill=sig_type, alpha=is_sig, colour=sig_type), linewidth=0.03, alpha=.3)+
  geom_sf(data=. %>% group_by(RGN19CD) %>% summarise(), fill="transparent", linewidth=0.15)+
  coord_sf(datum=NA)+
  # Greater.
  geom_spoke(
    data=. %>% filter(rr_estimate>1),
    aes(x=east, y=north, angle=get_radians(map_scale(rr_estimate,1,max_rr,90,45)), colour=sig_type),
    linewidth=.3, radius=6000, position="center_spoke" #.4
  )+
  # Less.
  geom_spoke(
    data=. %>% filter(rr_estimate<1),
    aes(x=east, y=north, angle=get_radians(map_scale(rr_estimate,min_rr,1,135,90)), colour=sig_type),
    linewidth=.3, radius=6000, position="center_spoke" #.4
  )+
  
  annotate(geom="text", x=havering$east+0.14*eng_width, y=havering$north, hjust="centre", label=paste0("Havering\n", round(havering$rr_estimate,2)), family="Avenir Next", size=2.5, colour="#b2182b")+
  annotate(geom="segment", xend=havering$east+0.1*eng_width, yend=havering$north, x=havering$east+.01*eng_width, y=havering$north, size=.1)+
  
  annotate(geom="text", x=hillingdon$east-0.02*eng_width, y=hillingdon$north-0.18*eng_height, hjust="centre", label=paste0("Hillingdon\n", round(havering$rr_estimate,2)), family="Avenir Next", size=2.5, colour="#b2182b")+
  annotate(geom="segment", xend=hillingdon$east-0.02*eng_width, yend=hillingdon$north-0.15*eng_height, x=hillingdon$east, y=hillingdon$north, size=.1)+
  
  annotate(geom="text", x=tower$east+0.1*eng_width, y=tower$north-0.16*eng_height, hjust="centre", label=paste0("Tower Hamlets\n", round(tower$rr_estimate,2)), family="Avenir Next", size=2.5, colour="#b2182b")+
  annotate(geom="segment", xend=tower$east+0.07*eng_width, yend=tower$north-0.14*eng_height, x=tower$east, y=tower$north-.01*eng_height, size=.1)+
  
  scale_fill_manual(values=colours_type) +
  scale_colour_manual(values=colours_type) +
  guides(fill=FALSE, colour=FALSE, size=FALSE, alpha=FALSE) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())

havering <-   map_data |> filter(LAD19NM == "Havering")
hillingdon <-   map_data |> filter(LAD19NM == "Hillingdon")
tower <-   map_data |> filter(LAD19NM == "Tower Hamlets")

plot <- model1_map_real + model2_map_real_dens

ggsave(filename=here("figs", "07", "geog-severity-2.png"), plot=plot,width=10, height=6, dpi=300)



# Generate permutation data.
permuted_data <-
  #model1_data_lon |> 
  model2 |> 
  group_by(la) |> 
  mutate(
    is_sig= !between(1,lower, upper),
    sig_type = case_when(
      !is_sig ~ "none", 
      rr_estimate > 1 ~ "greater", 
      rr_estimate < 1 ~ "less")
    ) |> ungroup() |> 
  select(la, rr_estimate, sig_type, is_sig) %>%
  permutations(permute=c(la), times=8, apparent=TRUE) %>%
  mutate(data=map(splits, ~rsample::analysis(.))) %>%
  select(id, data) %>%
  unnest(cols=data)

lad_region_lookup <- read_csv(here("../", "data", "ch7", "lad_region_lookup.csv"))

# Join on hex cartogram file.
permuted_data_geo <- hexmap %>%
  inner_join(lad_region_lookup,
             by=c("lad_code"="LAD19CD")) %>% dplyr::select(-id) %>%
  left_join(permuted_data, by=c("LAD19NM"="la"))

permuted_data_geo <- t %>%
  inner_join(lad_region_lookup,
             by=c("lad19cd"="LAD19CD")) |> 
  left_join(permuted_data, by=c("LAD19NM"="la"))


# Position subclass for centring geom_spoke.
# As in --
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

# Convert degrees to radians.
get_radians <- function(degrees) {
  (degrees * pi) / (180)
}

# Rescaling function.
map_scale <- function(value, min1, max1, min2, max2) {
  return  (min2+(max2-min2)*((value-min1)/(max1-min1)))
}

# Bounds to pin oriented lines to.
max_rr <- max(permuted_data %>% pull(rr_estimate), na.rm = TRUE)
min_rr <- min(permuted_data %>% pull(rr_estimate), na.rm = TRUE)
# Define colours for encoding by sig type and direction.
colours_type <- c("#b2182b","#2166ac", "#878787")

permuted_data_geo %>%
  filter(!is.na(id)) %>%
  ggplot()+
  geom_sf(aes(fill=sig_type, alpha=is_sig, colour=sig_type), size=0.05, alpha=.3)+
  geom_sf(data=. %>% group_by(RGN19CD) %>% summarise(), fill="transparent", linewidth=0.3)+
  coord_sf(datum=NA)+
  # Greater.
  geom_spoke(
    data=. %>% filter(rr_estimate>1),
    aes(x=east, y=north, angle=get_radians(map_scale(rr_estimate,1,max_rr,90,45)), colour=sig_type),
    size=.3, radius=.4, position="center_spoke"
  )+
  # Less.
  geom_spoke(
    data=. %>% filter(rr_estimate<1),
    aes(x=east, y=north, angle=get_radians(map_scale(rr_estimate,min_rr,1,135,90)), colour=sig_type),
    size=.3, radius=.4, position="center_spoke"
  )+
  facet_wrap(~id, nrow=2)+
  scale_fill_manual(values=colours_type) +
  scale_colour_manual(values=colours_type) +
  guides(fill=FALSE, colour=FALSE, size=FALSE, alpha=FALSE)


ids <- permuted_data %>% pull(id) %>% unique()

plot <- permuted_data_geo %>%
  filter(!is.na(id)) %>%
  mutate(id=factor(id, sample(ids)), id=paste0("p", as.numeric(id))) |>
  ggplot()+
  geom_sf(aes(fill=sig_type, alpha=is_sig, colour=sig_type), linewidth=0.02, alpha=.3)+
  geom_sf(data=. %>% group_by(RGN19CD) %>% summarise(), fill="transparent", linewidth=0.08)+
  #geom_sf(aes(fill=sig_type, alpha=is_sig, colour=sig_type), size=0.05, alpha=.3)+
  # Greater.
  geom_spoke(
    data=. %>% filter(rr_estimate>1),
    aes(x=east, y=north, angle=get_radians(map_scale(rr_estimate,1,max_rr,90,45)), colour=sig_type),
    linewidth=.3, radius=6000, position="center_spoke" #.4
  )+
  # Less.
  geom_spoke(
    data=. %>% filter(rr_estimate<1),
    aes(x=east, y=north, angle=get_radians(map_scale(rr_estimate,min_rr,1,135,90)), colour=sig_type),
    linewidth=.3, radius=6000, position="center_spoke" #.4
  )+
  
  coord_sf(datum=NA)+
  facet_wrap(~id, nrow=3)+
  scale_fill_manual(values=colours_type) +
  scale_colour_manual(values=colours_type) +
  guides(fill=FALSE, colour=FALSE, size=FALSE, alpha=FALSE) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())


ggsave(filename=here("figs", "07", "geog-severity-lineup.png"), plot=plot,width=10, height=12, dpi=300)


array_data <- tibble(
  row=rep(1:10, times=1, each=10),
  col=rep(1:10, times=10, each=1),
  Oxford=
    sample(
      c(rep(TRUE, times=1, each=17), rep(FALSE, times=1, each=83)),
      size=100, replace=FALSE),
  Fareham=
    sample(
      c(rep(TRUE, times=1, each=41), rep(FALSE, times=1, each=59)),
      size=100, replace=FALSE)
)

plot <- array_data |> 
  pivot_longer(cols=c(Oxford,Fareham), names_to="la", values_to="is_ksi") |> 
  ggplot(aes(x=row,y=col, fill=is_ksi)) +
  geom_tile(colour="#ffffff", size=1) +
  scale_fill_manual(values=c("#fee0d2","#de2d26"), guide=FALSE)+
  facet_wrap(~la) +
  theme(
    axis.line = element_blank(), axis.text = element_blank(), 
    axis.title.x = element_blank(), axis.title.y = element_blank()
    )

ggsave(filename=here("figs", "07", "icon-technical.png"), plot=plot,width=5, height=2.8, dpi=500)


theatre_cells <- st_read(here("../", "data", "ch7", "theatre_cells.geojson"))

ksi_data <- bind_rows(
  theatre_cells |> slice_sample(n=170) |> add_column(la="Oxford\n170 KSI in 1,000 crashes"),
  theatre_cells |> slice_sample(n=410) |> add_column(la="Fareham\n410 KSI in 1,000 crashes")
)


plot <- theatre_cells |>
  ggplot() +
  geom_sf() +
  geom_sf(
    data=ksi_data,
    fill="#000000"
  ) +
  annotate("text", x=23, y=1, label="Stage", alpha=.5) +
  annotate("text", x=23, y=21, label="Orchestra", alpha=.5) +
  annotate("text", x=23, y=31, label="Front mezzanine", alpha=.5) +
  annotate("text", x=23, y=42, label="Rear mezzanine", alpha=.5) +
  facet_wrap(~la) +
  theme(
    axis.line = element_blank(), axis.text = element_blank(), 
    axis.title.x = element_blank(), axis.title.y = element_blank()
  )

ggsave(filename=here("figs", "07", "theatre-technical.png"), plot=plot,width=8, height=5, dpi=500)


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
  group_by(local_authority_district) |> 
  mutate(std.error=sd(ksi_rate)) |> 
  filter(id=="Apparent") |> 
  ggplot(aes(x=reorder(local_authority_district, ksi_rate), y=ksi_rate)) +
  stat_dist_gradientinterval(
    aes(dist = dist_normal(mu=ksi_rate, sigma=std.error)),
    point_size = 1.5
  ) +
  coord_flip()


rate_boots_temporal |> 
  ggplot(aes(x=year, y=ksi_rate)) +
  geom_line(data=. %>%  filter(id=="Apparent"), aes(group=id), size=.5) +
  geom_line(data=. %>%  filter(id!="Apparent"), aes(group=id), alpha=.1, size=.2) +
  facet_wrap(~local_authority_district)


