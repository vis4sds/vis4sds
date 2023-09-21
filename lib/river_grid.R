n_row <- 8
n_col <- 8
pts <- london_boroughs |>
  st_drop_geometry() |>
  select(area_name, x = easting, y = northing)
solution <- points_to_grid(pts, n_row, n_col, compactness = .6)
grid <- make_grid(london_boroughs, n_row, n_col)
grid <- grid |>
  inner_join(solution)

x = st_as_binary(st_sfc(st_point(0:1), st_point(5:6)))
st_as_sfc(x)


river_grid <- grid |> filter(area_name == "Havering") |> st_coordinates() |> as_tibble() |> 
  summarise(x=max(X)+500, y=min(Y), id=1) |> 
  add_row(
    grid |> filter(area_name == "Havering") |> st_coordinates() |> as_tibble() |> 
  summarise(x=min(X), y=min(Y), id=2)) |> 
  add_row(
    grid |> filter(area_name == "Barking and Dagenham") |> st_coordinates() |> as_tibble() |> 
  summarise(x=min(X), y=min(Y), id=3)) |> 
  add_row(
    grid |> filter(area_name == "Tower Hamlets") |> st_coordinates() |> as_tibble() |> 
  summarise(x=max(X), y=min(Y), id=4)) |> 
  add_row(
    grid |> filter(area_name == "Tower Hamlets") |> st_coordinates() |> as_tibble() |> 
  summarise(x=min(X), y=min(Y), id=5)) |> 
  add_row(
    grid |> filter(area_name == "City of London") |> st_coordinates() |> as_tibble() |> 
  summarise(x=min(X), y=min(Y), id=6)) |> 
  add_row(
    grid |> filter(area_name == "Kensington and Chelsea") |> st_coordinates() |> as_tibble() |> 
  summarise(x=max(X), y=min(Y), id=7)) |> 
  add_row(
    grid |> filter(area_name == "Kensington and Chelsea") |> st_coordinates() |> as_tibble() |> 
  summarise(x=min(X), y=min(Y), id=8)) |> 
  add_row(
    grid |> filter(area_name == "Hammersmith and Fulham") |> st_coordinates() |> as_tibble() |> 
  summarise(x=min(X), y=min(Y), id=9)) |> 
  add_row(
    grid |> filter(area_name == "Hounslow") |> st_coordinates() |> as_tibble() |> 
      summarise(x=min(X)-500, y=min(Y), id=10)) |> 
  st_as_sf(coords = c("x", "y"), crs = "epsg:27700") |>
  st_combine() %>% st_cast("LINESTRING")
  
grid |> 
  ggplot() +
  geom_sf() +
  geom_text(aes(x=x, y=y, label=area_name)) +
  geom_sf(data=river_grid)



