###############################################################################
# Figures for vis4sds 
# Chapter 8
# Author: Roger Beecham
###############################################################################

library(tidyverse)
library(here)
###############################################################################
# T H E M E S
###############################################################################


# Blur? 
# https://github.com/coolbutuseless/ggblur
# https://ggfx.data-imaginist.com/

# Sketchy 
# https://github.com/schochastics/roughsf
remotes::install_github("schochastics/roughsf")

library(sf)
pts  <- tibble(
  pt = c(1:15), y = rep(1:3, each=4), x = rep(1:4, times=3))

pts |> 
  ggplot(aes(x,y)) + 
  geom_point(size=20, pch=21) +
  scale_x_continuous(limits=c(0,4), expand=c(0,0)) +
  scale_y_continuous(limits=c(0,4), expand=c(0,0)) 
                       


pts_sf   <- st_as_sf(pts, coords = c("x", "y")) 
circles_sf <- pts_sf |>  st_buffer(dist = .45) 



roughsf::roughsf(circles_sf, width = 100, height = 200) 
devtools::install_github("xvrdm/ggrough")


plot <- pts |> ggplot(aes(x,y)) + geom_point(size=20, pch=21)

options <- list(GeomArea=list(fill_style="hachure", 
                              angle_noise=0.5,
                              roughness=2,
                              gap_noise=0.2,
                              gap=1.5,
                              fill_weight=1))

get_rough_chart(plot, options)


parse_polygons <- function (svg) {
  shape <- "polygon" # was "polyline" in ggrough:::parse_areas
  keys <- NULL
  ggrough:::parse_shape(svg, shape, keys) %>% {
    purrr::map(., 
               ~purrr::list_modify(.x, 
                                   points = stringr::str_squish(.x$points) %>% 
                                     {stringr::str_glue("M{.}Z")}, 
                                   shape = "path"))
  }
}


trace(ggrough:::parse_rough, edit = TRUE)

uspopage <- gcookbook::uspopage
p <- ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + 
  geom_area(alpha=0.8) +
  scale_x_continuous(expand=c(0,0)); p

options <- list(GeomArea=list(fill_style="hachure", 
                               angle_noise=0.5,
                               gap_noise=0.2,
                               roughness=5,
                               gap=1.5,
                               fill_weight=1))
get_rough_chart(p, options)

plot <- pts |> 
  ggplot(aes(x,y)) + 
  geom_point(size=20) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)); plot

options <- list(GeomPoint=list(fill_style="hachure", 
                              color_style="hachure",
                              angle_noise=0.5,
                              gap_noise=0.2,
                              roughness=2,
                              #gap=1.5,
                              fill_weight=1))
get_rough_chart(plot, options)


remotes::install_github("schochastics/roughnet")

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

grid |>  ggplot() + geom_sf() +
  geom_text(aes(x=col+.5, y=row+.7, label=paste("c",col)), size=2) +
  geom_text( aes(x=col+.5, y=row+.3, label=paste("r", row_lookup)), size=2)

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


poll_theatres |> 
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
  
poll_data |> right_join(grid)




  geom_text(aes(x=col+.5, y=row+.7, label=paste("c",col)), size=2) +
  geom_text( aes(x=col+.5, y=row+.3, label=paste("r", row_lookup)), size=2)

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
# C H    8
###############################################################################

