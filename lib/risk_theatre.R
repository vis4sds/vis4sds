###############################################################################
# R script for creating risk theatre shapefile
# Author: Roger Beecham
###############################################################################

library(tidyverse)
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
  mutate(id=row_number()) |> 
  left_join(grid_index |> st_drop_geometry() ) 

