# Follow tutorial at : https://github.com/gicentre/litvis/blob/master/documents/tutorials/geoTutorials/openstreetmap.md

# Corners of London
# left : 522177.4 -0.23910754
# right : 539057.8 0.00021658491
# top : 185468.9 51.550980
# bottom : 174191.1 51.449638

# ogr2ogr -f GeoJSON point_london_outline.geojson london_outline.osm points
# ogr2ogr -f GeoJSON line_london_outline.geojson london_outline.osm lines
# ogr2ogr -f GeoJSON poly_london_outline.geojson london_outline.osm multipolygons

# load in via mapshapr
# clip bbox=-0.23910754,51.449638,0.00021658491,51.550980
# filter 'building != undefined || man_made == "bridge"'
# o 'buildings.json' format=geojson drop-table

# filter 'leisure == "park"'
# o 'parks.json' format=geojson drop-table

# filter 'waterway == "river" && name == "River Thames"' 
# o 'rivers.json' format=geojson drop-table

# simplify using mapshaper interface

library(sf)
library(rmapshaper)
library(tidyverse)

#### Avoid : inefficient ########
# Read in and cast to Easting/Northing
polys <- st_read("./data/poly_london_outline.geojson") %>%
  st_transform(crs=27700) 
# Clip according to bounding box - takes a while to execute. 
# Speed up by using system mapshaper library.
check_sys_mapshaper()
# Need mapshaper on system : npm install -g mapshaper
# polys <- ms_clip(polys, bbox=c(520878,172239,540356,187420), remove_slivers=TRUE, sys=TRUE)
st_write(polys, "./data/polys.geojson")

#### Load in as sf objects, cast ton OSB 

buildings <- st_read("./data/buildings.json") %>%
  st_transform(crs=27700) 

parks <- st_read("./data/parks.json") %>%
  st_transform(crs=27700) 

rivers <- st_read("./data/rivers.json") %>%
  st_transform(crs=27700) 

### Plot 

# buildings "#eee"
# parks #EFF4E1
# rivers #E2EDF6

left <- min(stations_lon$easting)
top <- max(stations_lon$northing)
right <- max(stations_lon$easting)
bottom <- min(stations_lon$northing)
# border
border_width <- 0.02*(right-left)
border_height <- 0.02*(top-bottom)
# aspect ratio
aspect <- ((top-bottom)+2*border_height) / ((right-left)+2*border_width)
# Buildings is large and takes some drawing time.
# Could exclude as layer or write tp .png file.
london_outline <- ggplot() +
  geom_sf(data=buildings, fill="#EEEEEE",  colour="#EEEEEE")+
  geom_sf(data=parks, fill="#EFF4E1",  colour="#EFF4E1")+
  geom_sf(data=rivers, fill="#E2EDF6",  colour="#E2EDF6", size=5.5)+
  coord_sf(crs=st_crs(parks), datum=NA)+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

# Save output as a .png to your local directory : this takes some time to draw -- for this reason it is worth perhaps drawing pairs > stated freq.
ggsave("./figures/london_outline.png",plot = london_outline, width=20, height=20*aspect)
  