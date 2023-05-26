# Washington Post map 
url <- "https://github.com/rogerbeecham/update-ecological-analyses/blob/3a30b8e8ea5add81e28e83793bdb3ba69d827533/data/trump.geojson"
trump_data <- st_read(here("../", "update-ecological-analyses", "data", "trump.geojson"), crs=2163)
st_write(trump_data, here("../", "data", "ch3", "trump_data.geojson"))
trump_data <- st_read(here("../", "data", "ch3", "trump_data.geojson"), crs=2163)

trump_data_raw <- trump_data

# rotate function (see here: https://r-spatial.github.io/sf/articles/sf3.html#affine-transformations
rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

trump_data <- trump_data |>
  mutate(geom_rot = st_geometry(trump_data)*rot(pi*1.5)) |> 
  st_drop_geometry() |> 
  rename(geometry = geom_rot) |> 
  st_set_geometry("geometry") 


# Vary width (votes cast) and height (margin in net votes) of triangle.
triangle <-  ggplot()+
  geom_segment(aes(x=-.4, xend=0, y=-.35, yend=0.35), linewidth=0.15, lineend="round")+
  geom_segment(aes(x=.4, xend=0, y=-.35, yend=0.35), linewidth=0.15, lineend="round")+
  geom_segment(aes(x=0, y=-.32, xend=0, yend=.32), linewidth=0.15, arrow=arrow(type="closed", ends="both", length = unit(.025, "inches")), linetype = "dashed", colour="#636363")+
  geom_segment(aes(x=-.34, y=-.35, xend=.34, yend=-.35), linewidth=0.15, arrow=arrow(type="closed", ends="both", length = unit(.025, "inches")), linetype = "dashed", colour="#636363")+
  geom_text(aes(label="Height is \ntotal votes cast",x=0, y=-.1),hjust="centre", size=2)+
  geom_text(aes(label="Width is margin \nin net votes",x=0, y=-.5),hjust="centre", size=2)+
  xlim(-0.5,0.5)+
  ylim(-0.6,0.35) +
  theme_void()
# Use of colour to encode party.
temp_dat <-tibble(
  elected=c("CLINTON", "TRUMP"),
  y=c(1,1),
  x=2:1
)
party <- temp_dat |>
  ggplot()+
  geom_segment(aes(x=x-.2, xend=x, y=y, yend=y+.4, colour=elected), size=0.25, lineend="round")+
  geom_segment(aes(x=x+.2, xend=x, y=y, yend=y+.4, colour=elected), size=0.25, lineend="round")+
  scale_colour_manual(values=c("#46779F", "#CD5B4F"))+
  geom_text(
    aes(label=elected,x=x, y=y-0.2, colour=elected),hjust="centre",vjust="top", size=2, family="Avenir Next Medium")+
  guides(colour=FALSE)+
  xlim(.5,2.5)+
  ylim(0.5,1.5) +
  theme_void()

# Use of thickness to flips.
line <-  ggplot()+
  geom_segment(aes(x=-0.3-.2, xend=-0.3, y=-.35, yend=.2), size=0.2)+
  geom_segment(aes(x=-0.3+.2, xend=-0.3, y=-.35, yend=.2), size=0.2)+
  geom_segment(aes(x=0.35-.2, xend=0.35, y=-.35, yend=.2), size=0.45)+
  geom_segment(aes(x=0.35+.2, xend=0.35, y=-.35, yend=.2), size=0.45)+
  xlim(-0.6,0.7)+
  ylim(-0.35,0.35) +
  theme_void()


legend <- ggplot()+
  geom_text(aes(label="Each county is a triangle",x=3, y=7), hjust="middle", vjust="top", family="Avenir Next Medium", size=3)+
  geom_text(aes(label=str_wrap("Colour hue is winner",15),x=0.5, y=4.5), hjust="right", vjust="middle", size=2.3)+
  geom_text(aes(label=str_wrap("Thick stroke is county landslide",20),x=2, y=1.5), hjust="right", vjust="middle", size=2.3)+
  annotation_custom(ggplotGrob(party),xmin=0,xmax=6,ymin=3,ymax=5.5)+
  annotation_custom(ggplotGrob(line),xmin=2,xmax=6,ymin=0.5,ymax=2.5)+
  annotation_custom(grob=ggplotGrob(triangle),xmin=6.5,xmax=12,ymin=0,ymax=6)+
  xlim(-2.5,12)+
  ylim(0,7) +
  theme_void()

us_bbox <- st_bbox(trump_data)
us_width <- unname(us_bbox$xmax)-unname(us_bbox$xmin) 
us_height <- unname(us_bbox$ymax)-unname(us_bbox$ymin) 

us_bbox_raw <- st_bbox(trump_data_raw)
us_width_raw <- unname(us_bbox_raw$xmax)-unname(us_bbox_raw$xmin) 
us_height_raw <- unname(us_bbox_raw$ymax)-unname(us_bbox_raw$ymin) 


min_width <- .002*us_width
max_width <- 50*min_width
min_height <- .002*us_height
max_height <- 80*min_height

# Rescaling function.
map_scale <- function(value, min1, max1, min2, max2) {
  return  (min2+(max2-min2)*((value-min1)/(max1-min1)))
}

centroids <- trump_data |> st_centroid() |> st_coordinates() |> as_tibble() |> 
  rename_all(~tolower(.x))

state_data <- trump_data |>  
  mutate(votes_trump=share_trump*total_pop) |> 
  group_by(state_abbr) |> 
  summarise(
    is_trump=(sum(votes_trump)/sum(total_pop))>.5, 
    division=first(division)
  ) |> 
  mutate(is_trump=if_else(state_abbr %in% c("MI", "FL"), TRUE, is_trump))

state_centroids <- state_data |> st_centroid() |> st_coordinates() |> as_tibble() |> 
  rename_all(~tolower(.x))

state_data_raw <- trump_data_raw |> mutate(votes_trump=share_trump*total_pop) |> 
  group_by(state_abbr) |> 
  summarise(
    is_trump=(sum(votes_trump)/sum(total_pop))>.5, 
    division=first(division)
  ) |> 
  mutate(is_trump=if_else(state_abbr %in% c("MI", "FL"), TRUE, is_trump))

state_centroids <- state_data_raw |> st_centroid() |> st_coordinates() |> as_tibble() |> 
  rename_all(~tolower(.x))

state_centroids_rot <- state_data |> st_centroid() |> st_coordinates() |> as_tibble() |> 
  rename_all(~tolower(.x))


# state_data_raw <- state_data_raw |> bind_cols(state_centroids)
# state_data <- state_data |> bind_cols(state_centroids_rot)
# trump_data <- trump_data |> bind_cols(centroids)
# 
# ggsave(filename=here("figs", "03", "wp.png"), plot=wp,width=6.5, height=10, dpi=500)
# 




wp_coords <- trump_data |> st_centroid() |>
  st_coordinates() |> as_tibble() |> rename_all(tolower)

wp_map <- trump_data |>
  mutate(
    votes_trump=share_trump*total_pop, net_margin=abs(votes_trump-(.5*total_pop)),
    width=map_scale(net_margin, min(net_margin), max(net_margin), min_width, max_width),
    height=map_scale(total_pop, min(total_pop), max(total_pop), min_height, max_height),
    is_trump=share_trump>.5, is_landslide=abs(share_trump-.5)>.2
  ) |>
  ggplot() +

  geom_sf(
    data=state_data,
    aes(fill=is_trump), colour="#ffffff", linewidth=.2
  ) +

  geom_text(
    data=state_data,
    aes(label=state_abbr, colour=is_trump, x=x,y=y), size=3, family="Avenir Next Demi Bold", alpha=.6
  ) +

  geom_segment(
    aes(x=x-width, xend=x, y=y, yend=y+height, colour=is_trump, size=is_landslide), alpha=0.8, lineend="round"
  )+
  geom_segment(
    aes(x=x+width, xend=x, y=y, yend=y+height, colour=is_trump, size=is_landslide), alpha=0.8, lineend="round"
  )+
  coord_sf(
    datum=NA,
    xlim = c(unname(us_bbox$xmin), unname(us_bbox$xmax)),
    ylim = c(unname(us_bbox$ymin), unname(us_bbox$ymax))
  )+

  scale_colour_manual(values=c("#46779F", "#CD5B4F"), guide="none") +
  scale_fill_manual(values=c("#EEF4F9", "#FDF1F1"), guide="none") +
  scale_size_ordinal(range=c(.15,.4), guide="none")+

  theme(
    plot.background=element_rect(fill="#ffffff"),
    axis.text=element_blank(), panel.grid.major=element_blank(),
    axis.title.x=element_blank(), axis.title.y=element_blank()
  )
# 
# 
# 
# 
# 
great_lakes <- "Clinton’s large wins in Midwestern cities like Cleveland and Detroit weren’t enough to offset the Trump margins from many more smaller cities and counties. For example, Clinton won seven of Ohio’s 88 counties. She lost the area around Dayton, a medium-sized city that voted for Obama in 2012"
north_east <- "To no one’s surprise, Clinton won decisively in the Northeast Corridor. Those cities provided huge margins for her from Boston to Washington. Trump’s most notable big-city win was in Suffolk County on Long Island. While Trump didn’t win in the most urban counties, he held a significant edge in suburban counties."
florida <- "Clinton held her own in Democratic strongholds in South Florida and Orlando, but Trump flipped St. Petersburg by a slim margin. Trump pulled away with large wins up and down both coasts in areas growing with retirees."
south_west <- "Maricopa County bucks the trend of urban areas voting for Democrats. Like Romney in 2012, Trump narrowly carried the county, netting him by far his largest single county win. The county includes the urban voters in Phoenix but even more conservative suburban voters."
texas <- "Compared to Trump’s wins in the South, his margins in rural counties in the Great Plains were much higher, consistently winning by more than 50 percentage points. These counties are tiny, but combined, they handed him easy wins through the region."
urban_rural <- "Nationwide, Clinton won the urban core overwhelmingly, but Trump won 75 percent or more of everything else from suburbs to rural counties."



ggsave(filename=here("figs", "03", "wp.png"), plot=wp,width=10, height=7.8, dpi=500)


choropleth <- trump_data_raw |> 
  mutate(is_trump=share_trump>.5) |> 
  ggplot() +
  coord_sf(
    datum=NA,
    xlim = c(unname(us_bbox_raw$xmin), unname(us_bbox_raw$xmax)), 
    ylim = c(unname(us_bbox_raw$ymin), unname(us_bbox_raw$ymax)+0.08*us_height)
  )+
  geom_sf(aes(fill=is_trump, colour=is_trump), linewidth=.03)+
  geom_sf(
    data=state_data_raw, 
    fill="transparent", aes(colour=is_trump), linewidth=.07
    ) +
  geom_text(
    data=state_data_raw |> st_drop_geometry(),
    aes(label=state_abbr, colour=is_trump, x=x,y=y), size=2, family="Avenir Next Demi Bold"
  )+
  annotate("text", x=us_bbox_raw$xmin+.09*us_width_raw, y=us_bbox_raw$ymax+.08*us_height_raw, label="Choropleth of county party majority", family="Avenir Next Medium", hjust="left", size=3) +
  scale_colour_manual(values=c("#46779F", "#CD5B4F"), guide="none") +
  scale_fill_manual(values=c("#EEF4F9", "#FDF1F1"), guide="none") +
  theme_void()
  


wp <- 
  ggplot() +
  
  coord_sf(
    datum=NA,
    xlim = c(unname(us_bbox$xmin)-0.05*us_width, unname(us_bbox$xmin)+1.6*us_width), 
    ylim = c(unname(us_bbox$ymin), unname(us_bbox$ymax)+0.1*us_height)
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.75*us_width,y=unname(us_bbox$ymax)+0.13*us_height,
           label="The peaks and valleys of Trump and Clinton’s support", 
           family="Cinzel", hjust="middle", vjust="top", size=5.5
  ) +
  annotate("text", 
           x=unname(us_bbox$xmin)+0.75*us_width,y=unname(us_bbox$ymax)+0.09*us_height,
           label="Clinton won in urban counties, while Trump won everywhere else.", 
           family="Cinzel", hjust="middle", vjust="top", size=4
  )+
  
  annotation_custom(
    grob=ggplotGrob(wp_map),
    xmin=unname(us_bbox$xmin),
    xmax=unname(us_bbox$xmax),
    ymin=unname(us_bbox$ymin),
    ymax=unname(us_bbox$ymax)
  )+
  annotation_custom(
    grob=ggplotGrob(legend),
    xmin=unname(us_bbox$xmax) + .11*us_width,
    xmax=unname(us_bbox$xmax) + 0.72*us_width,
    ymin=unname(us_bbox$ymax) - 0.01*us_height,
    ymax=unname(us_bbox$ymax) - .17*us_height
  ) +
  annotation_custom(
    grob=ggplotGrob(choropleth),
    xmin=unname(us_bbox$xmax)+.15*us_width,
    xmax=unname(us_bbox$xmax)+.70*us_width,
    ymin=unname(us_bbox$ymax)-.19*us_height,
    ymax=unname(us_bbox$ymax)-.48*us_height
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin-.08*us_width),y=unname(us_bbox$ymin)+0.825*us_height,
           label="THE GREAT LAKES", 
           hjust="left", vjust="top", size=2.5, family="Avenir Next Demi Bold"
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin-.08*us_width),y=unname(us_bbox$ymin)+0.81*us_height,
           label=str_wrap(great_lakes,25), 
           hjust="left", vjust="top", size=2.5
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.35*us_width,y=unname(us_bbox$ymax)+0.015*us_height,
           label="THE NORTH EAST", 
           hjust="left", vjust="top", size=2.5, family="Avenir Next Demi Bold"
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.35*us_width,y=unname(us_bbox$ymax)+0.00*us_height,
           label=str_wrap(north_east,60), 
           hjust="left", vjust="top", size=2.5
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.78*us_width,y=unname(us_bbox$ymax)-.24*us_height,
           label="THE URBAN-RURAL DIVIDE", 
           hjust="left", vjust="top", size=2.5, family="Avenir Next Demi Bold"
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.78*us_width,y=unname(us_bbox$ymax)-0.255*us_height,
           label=str_wrap(urban_rural,30), 
           hjust="left", vjust="top", size=2.5
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmax)-0.15*us_width,y=unname(us_bbox$ymin)+.435*us_height,
           label="TEXAS AND THE PLAINS", 
           hjust="left", vjust="top", size=2.5, family="Avenir Next Demi Bold"
  )+
  annotate("text", 
           x=unname(us_bbox$xmax)-0.15*us_width,y=unname(us_bbox$ymin)+0.42*us_height,
           label=str_wrap(texas,40), 
           hjust="left", vjust="top", size=2.5
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmax)-.25*us_width,y=unname(us_bbox$ymin)+.165*us_height,
           label="THE SOUTH WEST", 
           hjust="left", vjust="top", size=2.5, family="Avenir Next Demi Bold"
  )+
  annotate("text", 
           x=unname(us_bbox$xmax)-.25*us_width,y=unname(us_bbox$ymin)+0.15*us_height,
           label=str_wrap(south_west,40), 
           hjust="left", vjust="top", size=2.5
  )+
  
  annotate("text",
           x=unname(us_bbox$xmin)+.1*us_width,y=unname(us_bbox$ymax)-.01*us_height,
           label="EAST\n COAST", hjust="middle", vjust="top", size=3.5
  ) +
  annotate("text", 
           x=unname(us_bbox$xmin)+.2*us_width,y=unname(us_bbox$ymin)+.025*us_height,
           label="WEST\n COAST", hjust="middle", vjust="bottom", size=3.5
  )+
  annotate("segment", 
           x=unname(us_bbox$xmax)+.12*us_width, xend=unname(us_bbox$xmax)+.12*us_width,
           y=unname(us_bbox$ymax), yend=unname(us_bbox$ymax)-.4*us_height, size=.05
  )+
  
  theme(
    plot.background=element_rect(fill="#ffffff"),
    axis.text=element_blank(), panel.grid.major=element_blank(),  
    axis.title.x=element_blank(), axis.title.y=element_blank()
  )

ggsave(filename=here("figs", "03", "wp_long.png"), plot=wp,width=8.5, height=8.5, dpi=500)

wp <- 
  ggplot() +
  
  coord_sf(
    datum=NA,
    xlim = c(unname(us_bbox$xmin)-0.1*us_width, unname(us_bbox$xmax)+0.4*us_width),
    ylim = c(unname(us_bbox$ymin)-.3*us_height, unname(us_bbox$ymax)+0.1*us_height)
  )+

  annotate("text", 
           x=unname(us_bbox$xmin)+0.57*us_width,y=unname(us_bbox$ymax)+0.13*us_height,
           label="The peaks and valleys of Trump and Clinton’s support", 
           family="Cinzel", hjust="middle", vjust="top", size=5
  ) +
  annotate("text", 
           x=unname(us_bbox$xmin)+0.57*us_width,y=unname(us_bbox$ymax)+0.09*us_height,
           label="Clinton won in urban counties, while Trump won everywhere else.", 
           family="Cinzel", hjust="middle", vjust="top", size=3.5
  )+
  
  annotation_custom(
    grob=ggplotGrob(wp_map),
    xmin=unname(us_bbox$xmin),
    xmax=unname(us_bbox$xmax),
    ymin=unname(us_bbox$ymin),
    ymax=unname(us_bbox$ymax)
  )+
  annotation_custom(
    grob=ggplotGrob(legend),
    xmin=unname(us_bbox$xmin),
    xmax=unname(us_bbox$xmin) + 0.62*us_width,
    ymin=unname(us_bbox$ymin) - 0.04*us_height,
    ymax=unname(us_bbox$ymin) - .21*us_height
  ) +
  annotation_custom(
    grob=ggplotGrob(choropleth),
    xmin=unname(us_bbox$xmin) + 0.63*us_width,
    xmax=unname(us_bbox$xmin) + 1.2*us_width,
    ymin=unname(us_bbox$ymin) - .01*us_height,
    ymax=unname(us_bbox$ymin) - .32*us_height
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin-.08*us_width),y=unname(us_bbox$ymin)+0.825*us_height,
           label="THE GREAT LAKES", 
           hjust="left", vjust="top", size=2.5, family="Avenir Next Demi Bold"
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin-.08*us_width),y=unname(us_bbox$ymin)+0.81*us_height,
           label=str_wrap(great_lakes,25), 
           hjust="left", vjust="top", size=2.5
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.39*us_width,y=unname(us_bbox$ymax)+0.015*us_height,
           label="THE NORTH EAST", 
           hjust="left", vjust="top", size=2.5, family="Avenir Next Demi Bold"
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.39*us_width,y=unname(us_bbox$ymax)+0.00*us_height,
           label=str_wrap(north_east,60), 
           hjust="left", vjust="top", size=2.5
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.82*us_width,y=unname(us_bbox$ymax)-.24*us_height,
           label="THE URBAN-RURAL DIVIDE", 
           hjust="left", vjust="top", size=2.5, family="Avenir Next Demi Bold"
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.82*us_width,y=unname(us_bbox$ymax)-0.255*us_height,
           label=str_wrap(urban_rural,30), 
           hjust="left", vjust="top", size=2.5
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmax)-0.12*us_width,y=unname(us_bbox$ymin)+.455*us_height,
           label="TEXAS AND THE PLAINS", 
           hjust="left", vjust="top", size=2.5, family="Avenir Next Demi Bold"
  )+
  annotate("text", 
           x=unname(us_bbox$xmax)-0.12*us_width,y=unname(us_bbox$ymin)+0.44*us_height,
           label=str_wrap(texas,30), 
           hjust="left", vjust="top", size=2.5
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmax)-.26*us_width,y=unname(us_bbox$ymin)+.185*us_height,
           label="THE SOUTH WEST", 
           hjust="left", vjust="top", size=2.5, family="Avenir Next Demi Bold"
  )+
  annotate("text", 
           x=unname(us_bbox$xmax)-.26*us_width,y=unname(us_bbox$ymin)+0.17*us_height,
           label=str_wrap(south_west,40), 
           hjust="left", vjust="top", size=2.5
  )+
  
  annotate("text",
           x=unname(us_bbox$xmin)+.1*us_width,y=unname(us_bbox$ymax)-.01*us_height,
           label="EAST\n COAST", hjust="middle", vjust="top", size=3.5
  ) +
  annotate("text", 
           x=unname(us_bbox$xmin)+.2*us_width,y=unname(us_bbox$ymin)+.025*us_height,
           label="WEST\n COAST", hjust="middle", vjust="bottom", size=3.5
  )+
  annotate("segment", 
           x=unname(us_bbox$xmin) +.04*us_width, xend=unname(us_bbox$xmax) +.08*us_width,
           y=unname(us_bbox$ymin) -.02*us_height, yend=unname(us_bbox$ymin)-.02*us_height, size=.05
  )+
  
  theme(
    plot.background=element_rect(fill="#ffffff"),
    axis.text=element_blank(), panel.grid.major=element_blank(),  
    axis.title.x=element_blank(), axis.title.y=element_blank()
  )


ggsave(filename=here("figs", "03", "wp_long.png"), plot=wp,width=7.5, height=9.8, dpi=500)


####################################### SPOKE 


max(abs(trump_data$shift_trump))



# Convert degrees to radians.
get_radians <- function(degrees) {
  (degrees * pi) / (180)
}
# Rescaling function.
map_scale <- function(value, min1, max1, min2, max2) {
  return  (min2+(max2-min2)*((value-min1)/(max1-min1)))
}
# Position subclass for centred geom_spoke as per --
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


swing <-  ggplot()+
  geom_spoke(aes(x=0, y=-.35,angle=get_radians(90)),radius=0.55, size=0.2, colour="#636363", lineend="round")+
  geom_spoke(aes(x=0, y=-.35,angle=get_radians(135)),radius=0.55, size=0.2,colour="#636363", linetype = "dashed", lineend="round")+
  geom_spoke(aes(x=0, y=-.35,angle=get_radians(45)),radius=0.55,size=0.2,colour="#636363",linetype = "dashed", lineend="round")+
  geom_text(aes(label="+24% R",x=.5, y=0), angle=45,hjust="right", family="Avenir Next", size=2, colour="#636363")+
  geom_text(aes(label="+24% D",x=-.5, y=0), angle=315,hjust="left", family="Avenir Next", size=2, colour="#636363")+
  geom_text(aes(label="+",x=.3, y=.2),hjust="left", family="Avenir Heavy", size=2.5, colour=winner_cols[1])+
  geom_text(aes(label="+",x=-.3, y=.2),hjust="left", family="Avenir Heavy", size=2.5, colour=winner_cols[2])+
  geom_curve(aes(x=-.04, y=.2, xend=-.3, yend=.08), size=0.2, curvature = 0.2, arrow=arrow(type="closed", length = unit(.04, "inches")), colour="#636363")+
  geom_curve(aes(x=.04, y=.2, xend=.3, yend=.08), size=0.2, curvature = -0.2, arrow=arrow(type="closed", length = unit(.04, "inches")), colour="#636363")+
  xlim(-0.5,0.5)+
  ylim(-0.35,0.35)+
  coord_equal() +
  theme_void() +
  theme(axis.title.x= element_blank(), axis.title.y= element_blank(), axis.text = element_blank())

winner_cols <- c("#CD5B4F", "#46779F")
names(winner_cols) <- c("TRUMP", "CLINTON")

# Use colour to encode party.
temp_dat <-tibble(
  elected=names(winner_cols),
  y=c(1,1),
  x=2:1
)

line <- temp_dat |>
  ggplot()+
  geom_spoke(data=. %>%  filter(elected=="TRUMP"), aes(x=x, y=y-.04, radius=.3, angle=get_radians(55), colour=elected), size=0.65, lineend="round")+
  geom_spoke(data=. %>%  filter(elected=="CLINTON"), aes(x=x, y=y-.04, radius=.3, angle=get_radians(120), colour=elected), size=0.65, lineend="round")+
  scale_colour_manual(values=winner_cols)+
  guides(colour=FALSE)+
  xlim(0,5)+
  ylim(0.8,1.3) +
  theme_void()


party <- temp_dat |>
  ggplot()+
  geom_spoke(data=. %>%  filter(elected=="TRUMP"), aes(x=x, y=y+.05, radius=.1, angle=get_radians(90), colour=elected), size=0.4, lineend="round")+
  geom_spoke(data=. %>%  filter(elected=="CLINTON"), aes(x=x, y=y+.05, radius=.1, angle=get_radians(90), colour=elected), size=0.4, lineend="round")+
  scale_colour_manual(values=winner_cols)+
  geom_text(
    aes(label=elected,x=x, y=y, colour=elected),hjust="centre",vjust="top", size=1.7, family="Avenir Next Medium")+
  guides(colour=FALSE)+
  xlim(0,5)+
  ylim(.9,1.2) +
  theme_void()



# Use annotation_custom to organise grobs in legend.
legend <- ggplot()+
  geom_text(aes(label="Each county is a line",x=0, y=6.2), hjust="left", vjust="top", family="Avenir Next Medium", size=2.5)+
  geom_text(aes(label="Colour hue is \n winning party",x=0.02, y=5.2), hjust="left", vjust="top", family="Avenir Next", size=2)+
  geom_text(aes(label="Thick stroke \ncounty flipped \n from 2012",x=0.02, y=4), hjust="left", vjust="top", family="Avenir Next", size=2)+
  geom_text(aes(label="Swing -- \n % change votes \n from 2012 Rep-Dem",x=0.02, y=2.2), hjust="left", vjust="top", family="Avenir Next", size=2)+
  annotation_custom(ggplotGrob(party),xmin=0.1,xmax=1.2,ymin=4.3,ymax=5.3)+
  annotation_custom(ggplotGrob(line),xmin=0.1,xmax=1.2,ymin=2.8,ymax=3.8)+
  annotation_custom(grob=ggplotGrob(swing),xmin=0.1,xmax=1.2,ymin=.5,ymax=2.9)+
  xlim(0,2)+
  ylim(0, 6.2) +
  theme_void() 

legend <- ggplot()+
  geom_text(aes(label="Each county is a line",y=1.95, x=0), hjust="left", vjust="top", family="Avenir Next Medium", size=3)+
  geom_text(aes(label="Colour hue \nwinning \nparty",y=1.7, x=6.4), hjust="left", vjust="top", family="Avenir Next", size=2.5)+
  geom_text(aes(label="Thick stroke \ncounty flipped \nfrom 2012", y=1.7, x=4.0), hjust="left", vjust="top", family="Avenir Next", size=2.5)+
  geom_text(aes(label="Swing -- \n% change votes \n rom 2012 Rep-Dem", y=1.7, x=0), hjust="left", vjust="top", family="Avenir Next", size=2.5)+
  annotation_custom(ggplotGrob(party), ymin=1.35, ymax=1.8, xmin=6.8,xmax=10.5)+
  annotation_custom(ggplotGrob(line), ymin=1.45, ymax=1.75, xmin=5.1, xmax=7.1)+
  annotation_custom(grob=ggplotGrob(swing), ymin=1.3, ymax=1.9, xmin=1.6, xmax=3.8)+
  xlim(0,10)+
  ylim(0,2) +
  theme_void() 


min_shift <- -max(abs(trump_data$shift_trump))
max_shift <- max(abs(trump_data$shift_trump))

# wp_map <- trump_data |> 
#   mutate(
#     votes_trump=share_trump*total_pop, net_margin=abs(votes_trump-(.5*total_pop)),
#     width=map_scale(net_margin, min(net_margin), max(net_margin), min_width, max_width),
#     height=map_scale(total_pop, min(total_pop), max(total_pop), min_height, max_height), 
#     is_trump=share_trump>.5, is_landslide=abs(share_trump-.5)>.2
#   ) 
# 
# 
# rural_counties <- trump_data |> bind_cols(wp_coords) |> 
#   mutate(is_flip= flip %in% c("Obama Trump", "Romney Clinton")) |> 
#   filter(is_flip, state_abbr %in% c("ME","NH", "VT"))
# 
# rural_counties <- trump_data |> bind_cols(wp_coords) |> 
#   mutate(is_flip= flip %in% c("Obama Trump", "Romney Clinton")) |> 
#   filter(is_flip, state_abbr %in% c("ME","NH", "VT"))
# 
# 
rural_counties <- trump_data |> 
  #bind_cols(wp_coords) |>
  mutate(is_flip= flip %in% c("Obama Trump", "Romney Clinton")) |>
  filter(is_flip, county_name == "Aroostook County")

georgia_counties <- trump_data |> 
  #bind_cols(wp_coords) |>
  mutate(is_flip= flip %in% c("Romney Clinton")) |>
  filter(is_flip, state_abbr == "GA")

lakes_counties <- trump_data |> 
  #bind_cols(wp_coords) |>
  mutate(is_flip= flip %in% c("Obama Trump")) |>
  filter(is_flip, state_abbr %in% c("WI","IA"))
  


wp_map <- trump_data |> 
 # bind_cols(wp_coords) |> 
  mutate(
    is_flip= flip %in% c("Obama Trump", "Romney Clinton"),
    is_trump=share_trump>.5
    ) |> 

  ggplot() +
  
  geom_sf(
    data=state_data,
    aes(fill=is_trump), colour="#ffffff", linewidth=.2
  ) +
  geom_text(
    data=state_data,
    aes(label=state_abbr, colour=is_trump, x=x,y=y), size=3, alpha=.6, family="Avenir Next Demi Bold"
  ) +
  geom_spoke(
    aes(x=x, y=y, 
        angle=get_radians(map_scale(shift_trump,min_shift,max_shift,135,45)), 
        colour=is_trump, size=is_flip, radius=.009*us_height), 
    alpha=0.9, lineend="round") +
  
  # geom_circle(data=rural_counties, aes(x0=x, y0=y, r=50000), size=.1) +
  geom_mark_circle(data=rural_counties, aes(x=x, y=y), size=.1) +
  geom_mark_circle(data=georgia_counties, aes(x=x, y=y), size=.1) +
  geom_mark_circle(data=lakes_counties, aes(x=x, y=y), size=.1) +
  
  coord_sf(
    datum=NA,
    xlim = c(unname(us_bbox$xmin), unname(us_bbox$xmax)), 
    ylim = c(unname(us_bbox$ymin), unname(us_bbox$ymax))
  )+
  
  scale_colour_manual(values=c("#46779F", "#CD5B4F"), guide="none") +
  scale_fill_manual(values=c("#EEF4F9", "#FDF1F1"), guide="none") +
  scale_size_ordinal(range=c(.15,.5), guide="none")+
  
  theme(
    plot.background=element_rect(fill="#ffffff"),
    axis.text=element_blank(), panel.grid.major=element_blank(),  
    axis.title.x=element_blank(), axis.title.y=element_blank()
  )



great_lakes <- "The Midwest is where Trump redrew the electoral map. States like Michigan and Wisconsin were considered favorable to Clinton, but instead swung to Trump mostly due to voters in mid-sized counties outside the major cities. The most striking change occurred in counties along the junction of Illinois, Wisconsin and Iowa. In this farm country, Trump’s message to people left behind helped him seize a significant advantage."
north_east <- "Those bold red swings stretching from inland Maine through New Hampshire and into upstate New York are counties that flipped in Trump’s favor from 2012. Away from the large cities on the coast, these counties resemble the pattern seen widely, where cities voted slightly more Democratic, but suburbs and beyond swung way to the right."
border <- "People closest to where Trump said he would build a wall consistently voted against him, all the way from the Gulf of Mexico to the Pacific Ocean."
west <- "Even though early voting suggested a historic Hispanic turnout in Nevada, Clinton won the two largest counties in the state by a slightly slimmer margin than Obama did in 2012. California became even more Democratic: Clinton won Orange County, which hasn’t gone for a Democrat since Franklin Roosevelt. In the Pacific Northwest, a pocket of rural counties between Seattle and Portland swung toward Trump."
deep_south <- "Voters across Alabama, Mississippi and Georgia predictably voted Republican, but in no dramatic fashion. Rapidly urbanizing counties around Atlanta swung hard to the left for Clinton. She flipped three counties in this area that Obama lost in 2012."
utah <- "The reason you’re seeing counties in Utah swinging has a simple answer: Evan McMullin. The three-way contest with the independent conservative candidate in this state reduced the Republican margin, even though Clinton wasn’t competitive."


library(ggforce)
rural_counties <- trump_data |> bind_cols(wp_coords) |> 
  mutate(is_flip= flip %in% c("Obama Trump", "Romney Clinton")) |> 
  filter(is_flip, state_abbr %in% c("ME","NH", "VT"))

rural_counties <- trump_data |> bind_cols(wp_coords) |> 
  mutate(is_flip= flip %in% c("Obama Trump", "Romney Clinton")) |> 
  filter(is_flip, state_abbr %in% c("ME","NH", "VT"))


rural_counties <- trump_data |> bind_cols(wp_coords) |> 
  mutate(is_flip= flip %in% c("Obama Trump", "Romney Clinton")) |> 
  filter(is_flip, county_name == "Aroostook County")

wp <- 
  ggplot() +
  
  coord_sf(
    datum=NA,
    xlim = c(unname(us_bbox$xmin)-.1*us_width, unname(us_bbox$xmax)+.1*us_width),
    ylim = c(unname(us_bbox$ymax)+.05*us_height, unname(us_bbox$ymin)-0.1*us_height)
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.55*us_width,y=unname(us_bbox$ymax)+0.1*us_height,
           label="How the country swung to the right", 
           family="Cinzel", hjust="middle", vjust="top", size=5.5
  ) +
  annotate("text", 
           x=unname(us_bbox$xmin)+0.55*us_width,y=unname(us_bbox$ymax)+0.07*us_height,
           label="Vast swaths of the nation voted more Republican than in 2012", 
           family="Cinzel", hjust="middle", vjust="top", size=3.5
  )+
  
  annotation_custom(
    grob=ggplotGrob(wp_map),
    xmin=unname(us_bbox$xmin),
    xmax=unname(us_bbox$xmax),
    ymin=unname(us_bbox$ymin),
    ymax=unname(us_bbox$ymax)
  )+
  
  annotation_custom(
    grob=ggplotGrob(legend),
    xmin=unname(us_bbox$xmin)-.05*us_width,
    xmax=unname(us_bbox$xmax)+.05*us_width,
    ymin=unname(us_bbox$ymin)- .03*us_height,
    ymax=unname(us_bbox$ymin) - .36*us_height
  ) +
  
  annotate("text", 
           x=unname(us_bbox$xmin)-.05*us_width,y=unname(us_bbox$ymin)+0.815*us_height,
           label="THE GREAT LAKES", 
           hjust="left", vjust="top", size=2.5, family="Avenir Next Demi Bold"
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)-.05*us_width,y=unname(us_bbox$ymin)+0.80*us_height,
           label=str_wrap(great_lakes,25), 
           hjust="left", vjust="top", size=2.5
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.35*us_width,y=unname(us_bbox$ymax)+0.015*us_height,
           label="THE NORTH EAST", 
           hjust="left", vjust="top", size=2.5, family="Avenir Next Demi Bold"
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.35*us_width,y=unname(us_bbox$ymax)+0.00*us_height,
           label=str_wrap(north_east,60), 
           hjust="left", vjust="top", size=2.5
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.8*us_width,y=unname(us_bbox$ymax)-.23*us_height,
           label="THE DEEP SOUTH", 
           hjust="left", vjust="top", size=2.5, family="Avenir Next Demi Bold"
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.8*us_width,y=unname(us_bbox$ymax)-0.245*us_height,
           label=str_wrap(deep_south,30), 
           hjust="left", vjust="top", size=2.5
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmax)-0.15*us_width,y=unname(us_bbox$ymin)+.44*us_height,
           label="ALONG THE BORDER", 
           hjust="left", vjust="top", size=2.5, family="Avenir Next Demi Bold"
  )+
  annotate("text", 
           x=unname(us_bbox$xmax)-0.15*us_width,y=unname(us_bbox$ymin)+0.425*us_height,
           label=str_wrap(border,30), 
           hjust="left", vjust="top", size=2.5
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)-.05*us_width,y=unname(us_bbox$ymin)+.06*us_height,
           label="THE WEST", 
           hjust="left", vjust="top", size=2.5, family="Avenir Next Demi Bold"
  )+
  annotate("text", 
           x=unname(us_bbox$xmin)-.05*us_width,y=unname(us_bbox$ymin)+.045*us_height,
           label=str_wrap(west,105), 
           hjust="left", vjust="top", size=2.5
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmax)-.25*us_width,y=unname(us_bbox$ymin)+.27*us_height,
           label="UTAH", 
           hjust="left", vjust="top", size=2.5, family="Avenir Next Demi Bold"
  )+
  annotate("text", 
           x=unname(us_bbox$xmax)-.25*us_width,y=unname(us_bbox$ymin)+0.255*us_height,
           label=str_wrap(utah,30), 
           hjust="left", vjust="top", size=2.5
  )+
  
  annotate("text",
           x=unname(us_bbox$xmin)+.1*us_width,y=unname(us_bbox$ymax)-.01*us_height,
           label="EAST\n COAST", hjust="middle", vjust="top", size=3.5
  ) +
  annotate("text", 
           x=unname(us_bbox$xmin)+.0*us_width,y=unname(us_bbox$ymin)+.15*us_height,
           label="WEST\n COAST", hjust="middle", vjust="bottom", size=3.5
  )+
  annotate("segment", 
           x=unname(us_bbox$xmin) -.05*us_width, xend=unname(us_bbox$xmax),
           y=unname(us_bbox$ymin) -.04*us_height, yend=unname(us_bbox$ymin)-.04*us_height, size=.05
  )+
  
  theme(
    plot.background=element_rect(fill="#ffffff"),
    axis.text=element_blank(), panel.grid.major=element_blank(),  
    axis.title.x=element_blank(), axis.title.y=element_blank()
  )

ggsave(filename=here("figs", "03", "wp_spoke.png"), plot=wp,width=6.5, height=10, dpi=500)

