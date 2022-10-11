# Washington Post map 
url <- "https://github.com/rogerbeecham/update-ecological-analyses/blob/3a30b8e8ea5add81e28e83793bdb3ba69d827533/data/trump.geojson"
trump_data <- st_read(here("data", "trump.geojson"), crs=2163)

trump_data_raw <- trump_data

# rotate function (see here: https://r-spatial.github.io/sf/articles/sf3.html#affine-transformations
rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

trump_data <- trump_data %>%
  mutate(geom_rot = st_geometry(.)*rot(pi*1.5)) |> 
  st_drop_geometry() |> 
  rename(geometry = geom_rot) |> 
  st_set_geometry("geometry") 



# Vary width (votes cast) and height (margin in net votes) of triangle.
triangle <-  ggplot()+
  geom_segment(aes(x=-.4, xend=0, y=-.35, yend=0.35), size=0.1, lineend="round")+
  geom_segment(aes(x=.4, xend=0, y=-.35, yend=0.35), size=0.1, lineend="round")+
  geom_segment(aes(x=0, y=-.32, xend=0, yend=.32), size=0.1, arrow=arrow(type="closed", ends="both", length = unit(.015, "inches")), linetype = "dashed", colour="#636363")+
  geom_segment(aes(x=-.34, y=-.35, xend=.34, yend=-.35), size=0.1, arrow=arrow(type="closed", ends="both", length = unit(.015, "inches")), linetype = "dashed", colour="#636363")+
  geom_text(aes(label="Height is \ntotal votes cast",x=0, y=-.1),hjust="centre", size=1.5)+
  geom_text(aes(label="Width is margin in net votes",x=0, y=-.45),hjust="centre", size=1.5)+
  xlim(-0.5,0.5)+
  ylim(-0.5,0.35) +
  theme_void()
# Use of colour to encode party.
temp_dat <-tibble(
  elected=c("CLINTON", "TRUMP"),
  y=c(1,1),
  x=2:1
)
party <- temp_dat %>%
  ggplot()+
  geom_segment(aes(x=x-.2, xend=x, y=y, yend=y+.4, colour=elected), size=0.15, lineend="round")+
  geom_segment(aes(x=x+.2, xend=x, y=y, yend=y+.4, colour=elected), size=0.15, lineend="round")+
  scale_colour_manual(values=c("#46779F", "#CD5B4F"))+
  geom_text(
    aes(label=elected,x=x, y=y-0.2, colour=elected),hjust="centre",vjust="top", size=1.5, family="Avenir Next Medium")+
  guides(colour=FALSE)+
  xlim(.5,2.5)+
  ylim(0.5,1.5) +
  theme_void()

# Use of thickness to flips.
line <-  ggplot()+
  geom_segment(aes(x=-0.3-.2, xend=-0.3, y=-.35, yend=.2), size=0.15)+
  geom_segment(aes(x=-0.3+.2, xend=-0.3, y=-.35, yend=.2), size=0.15)+
  geom_segment(aes(x=0.35-.2, xend=0.35, y=-.35, yend=.2), size=0.4)+
  geom_segment(aes(x=0.35+.2, xend=0.35, y=-.35, yend=.2), size=0.4)+
  xlim(-0.6,0.7)+
  ylim(-0.35,0.35) +
  theme_void()


legend <- ggplot()+
  geom_text(aes(label="Each county is a triangle",x=3, y=7), hjust="middle", vjust="top", family="Avenir Next Medium", size=2)+
  geom_text(aes(label=str_wrap("Colour hue is winner",15),x=0.5, y=4.5), hjust="right", vjust="middle", size=1.5)+
  geom_text(aes(label=str_wrap("Thick stroke is county landslide",20),x=2, y=1.5), hjust="right", vjust="middle", size=1.5)+
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

# state_data_raw <- state_data_raw |> bind_cols(state_centroids)
# state_data <- state_data |> bind_cols(state_centroids)
# trump_data <- trump_data |> bind_cols(centroids)

wp <- trump_data |> 
  mutate(
    votes_trump=share_trump*total_pop, net_margin=abs(votes_trump-(.5*total_pop)),
    width=map_scale(net_margin, min(net_margin), max(net_margin), min_width, max_width),
    height=map_scale(total_pop, min(total_pop), max(total_pop), min_height, max_height), 
    is_trump=share_trump>.5, is_landslide=abs(share_trump-.5)>.2
  ) |> 
  ggplot() +
  geom_sf(
    data=state_data,
    aes(fill=is_trump), colour="#ffffff", size=.2
  ) +
  geom_segment(aes(x=x-width, xend=x, y=y, yend=y+height, colour=is_trump, size=is_landslide), alpha=0.8, lineend="round")+
  geom_segment(aes(x=x+width, xend=x, y=y, yend=y+height, colour=is_trump, size=is_landslide), alpha=0.8, lineend="round")+
  
  coord_sf(
    datum=NA,
    xlim = c(unname(us_bbox$xmin)-.1*us_width, unname(us_bbox$xmax)+.1*us_width), 
    ylim = c(unname(us_bbox$ymin), unname(us_bbox$ymax)+0.22*us_height)
  )+
  
  
  scale_colour_manual(values=c("#46779F", "#CD5B4F"), guide="none") +
  scale_fill_manual(values=c("#EEF4F9", "#FDF1F1"), guide="none") +
  scale_size_ordinal(range=c(.2,.5), guide="none")

annotate("text", 
         x=unname(us_bbox$xmin)+.5*us_width,y=unname(us_bbox$ymax)+0.22*us_height,
         label="The peaks and valleys of Trump and Clinton’s support", 
         family="Cinzel", hjust="middle", vjust="top", size=4.5
) +
  annotate("text", 
           x=unname(us_bbox$xmin)+.5*us_width,y=unname(us_bbox$ymax)+0.19*us_height,
           label="Clinton won in urban counties, while Trump won everywhere else.", 
           family="Cinzel", hjust="middle", vjust="top", size=3
  )+
  annotate("text", 
           x=unname(us_bbox$xmin)+.2*us_width,y=unname(us_bbox$ymax),
           label="EAST COAST", hjust="middle", vjust="top", size=2
  ) +
  annotate("text", 
           x=unname(us_bbox$xmax)-.35*us_width,y=unname(us_bbox$ymin)+0.015*us_height,
           label="WEST COAST", hjust="middle", vjust="top", size=2
  ) +
  
  annotation_custom(
    grob=ggplotGrob(legend),
    xmin=unname(us_bbox$xmin) + .3*us_width,
    xmax=unname(us_bbox$xmin) + .8*us_width,
    ymin=unname(us_bbox$ymax) + .04*us_height,
    ymax=unname(us_bbox$ymax) + .15*us_height
  )+
  
  theme(
    plot.background=element_rect(fill="#ffffff"),
    axis.text=element_blank(), panel.grid.major=element_blank(),  
    axis.title.x=element_blank(), axis.title.y=element_blank()
  )

ggsave(filename=here("figs", "03", "wp.png"), plot=wp,width=6.5, height=10, dpi=500)


wp_north <- trump_data |> 
  mutate(
    votes_trump=share_trump*total_pop, net_margin=abs(votes_trump-(.5*total_pop)),
    width=map_scale(net_margin, min(net_margin), max(net_margin), min_width, max_width),
    height=map_scale(total_pop, min(total_pop), max(total_pop), min_height, max_height), 
    is_trump=share_trump>.5, is_landslide=abs(share_trump-.5)>.2
  ) |> 
  ggplot() +
  
  geom_sf(
    data=state_data,
    aes(fill=is_trump), colour="#ffffff", size=.2
  ) +
  
  geom_text(
    data=state_data,
    aes(label=state_abbr, colour=is_trump, x=x,y=y), size=2, family="Avenir Next Demi Bold"
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
    ylim = c(unname(us_bbox$ymin)+.5*us_height, unname(us_bbox$ymax))
  )+
  
  scale_colour_manual(values=c("#46779F", "#CD5B4F"), guide="none") +
  scale_fill_manual(values=c("#EEF4F9", "#FDF1F1"), guide="none") +
  scale_size_ordinal(range=c(.15,.4), guide="none")+
  
  theme(
    plot.background=element_rect(fill="#ffffff"),
    axis.text=element_blank(), panel.grid.major=element_blank(),  
    axis.title.x=element_blank(), axis.title.y=element_blank()
  )

wp_south <- trump_data |> 
  mutate(
    votes_trump=share_trump*total_pop, net_margin=abs(votes_trump-(.5*total_pop)),
    width=map_scale(net_margin, min(net_margin), max(net_margin), min_width, max_width),
    height=map_scale(total_pop, min(total_pop), max(total_pop), min_height, max_height), 
    is_trump=share_trump>.5, is_landslide=abs(share_trump-.5)>.2
  ) |> 
  ggplot() +
  
  geom_sf(
    data=state_data,
    aes(fill=is_trump), colour="#ffffff", size=.2
  ) +
  
  geom_text(
    data=state_data,
    aes(label=state_abbr, colour=is_trump, x=x,y=y), size=2, family="Avenir Next Demi Bold"
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
    ylim = c(unname(us_bbox$ymax)-.5*us_height, unname(us_bbox$ymin))
  )+
  
  scale_colour_manual(values=c("#46779F", "#CD5B4F"), guide="none") +
  scale_fill_manual(values=c("#EEF4F9", "#FDF1F1"), guide="none") +
  scale_size_ordinal(range=c(.15,.4), guide="none")+
  
  theme(
    plot.background=element_rect(fill="#ffffff"),
    axis.text=element_blank(), panel.grid.major=element_blank(),  
    axis.title.x=element_blank(), axis.title.y=element_blank()
  )


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
    aes(fill=is_trump), colour="#ffffff", size=.2
  ) +
  
  geom_text(
    data=state_data,
    aes(label=state_abbr, colour=is_trump, x=x,y=y), size=2, family="Avenir Next Demi Bold"
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





great_lakes <- "Clinton’s large wins in Midwestern cities like Cleveland and Detroit weren’t enough to offset the Trump margins from many more smaller cities and counties. For example, Clinton won seven of Ohio’s 88 counties. She lost the area around Dayton, a medium-sized city that voted for Obama in 2012"
north_east <- "To no one’s surprise, Clinton won decisively in the Northeast Corridor. Those cities provided huge margins for her from Boston to Washington. Trump’s most notable big-city win was in Suffolk County on Long Island. While Trump didn’t win in the most urban counties, he held a significant edge in suburban counties."
florida <- "Clinton held her own in Democratic strongholds in South Florida and Orlando, but Trump flipped St. Petersburg by a slim margin. Trump pulled away with large wins up and down both coasts in areas growing with retirees."
south_west <- "Maricopa County bucks the trend of urban areas voting for Democrats. Like Romney in 2012, Trump narrowly carried the county, netting him by far his largest single county win. The county includes the urban voters in Phoenix but even more conservative suburban voters."
texas <- "Compared to Trump’s wins in the South, his margins in rural counties in the Great Plains were much higher, consistently winning by more than 50 percentage points. These counties are tiny, but combined, they handed him easy wins through the region."
urban_rural <- "Nationwide, Clinton won the urban core overwhelmingly, but Trump won 75 percent or more of everything else from suburbs to rural counties."

wp <- 
  ggplot() +
  
  coord_sf(
    datum=NA,
    xlim = c(unname(us_bbox$xmin)-0*us_width, unname(us_bbox$xmin)+2*us_width), 
    ylim = c(unname(us_bbox$ymin)+.5*us_height, unname(us_bbox$ymax)+0.2*us_height)
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmax)+0*us_width,y=unname(us_bbox$ymax)+0.2*us_height,
           label="The peaks and valleys of Trump and Clinton’s support", 
           family="Cinzel", hjust="middle", vjust="top", size=4.5
  ) +
  annotate("text", 
           x=unname(us_bbox$xmax)+0*us_width,y=unname(us_bbox$ymax)+0.17*us_height,
           label="Clinton won in urban counties, while Trump won everywhere else.", 
           family="Cinzel", hjust="middle", vjust="top", size=3
  )+
  
  annotation_custom(
    grob=ggplotGrob(legend),
    xmin=unname(us_bbox$xmax) - .3*us_width,
    xmax=unname(us_bbox$xmax) + .3*us_width,
    ymin=unname(us_bbox$ymax) + .02*us_height,
    ymax=unname(us_bbox$ymax) + .13*us_height
  ) +
  annotation_custom(
    grob=ggplotGrob(wp_north),
    xmin=unname(us_bbox$xmin),
    xmax=unname(us_bbox$xmax),
    ymin=unname(us_bbox$ymin) + .5*us_height,
    ymax=unname(us_bbox$ymax)
  )+
  annotation_custom(
    grob=ggplotGrob(wp_south),
    xmin=unname(us_bbox$xmax),
    xmax=unname(us_bbox$xmax)+us_width,
    ymin=unname(us_bbox$ymin) + .48*us_height,
    ymax=unname(us_bbox$ymax) - .02*us_height
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0*us_width,y=unname(us_bbox$ymin)+0.772*us_height,
           label="THE GREAT LAKES", 
           hjust="left", vjust="top", size=1.5, family="Avenir Next Demi Bold"
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0*us_width,y=unname(us_bbox$ymin)+0.76*us_height,
           label=str_wrap(great_lakes,35), 
           hjust="left", vjust="top", size=1.5
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.38*us_width,y=unname(us_bbox$ymax)+0.012*us_height,
           label="THE NORTH EAST", 
           hjust="left", vjust="top", size=1.5, family="Avenir Next Demi Bold"
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.38*us_width,y=unname(us_bbox$ymax)+0.00*us_height,
           label=str_wrap(north_east,50), 
           hjust="left", vjust="top", size=1.5
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.75*us_width,y=unname(us_bbox$ymax)-.02*us_height,
           label="THE URBAN-RURAL DIVIDE", 
           hjust="left", vjust="top", size=1.5, family="Avenir Next Demi Bold"
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.75*us_width,y=unname(us_bbox$ymax)-0.032*us_height,
           label=str_wrap(urban_rural,45), 
           hjust="left", vjust="top", size=1.5
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+1.8*us_width,y=unname(us_bbox$ymax)-.15*us_height,
           label="TEXAS AND THE PLAINS", 
           hjust="left", vjust="top", size=1.5, family="Avenir Next Demi Bold"
  )+
  annotate("text", 
           x=unname(us_bbox$xmin)+1.8*us_width,y=unname(us_bbox$ymax)-0.162*us_height,
           label=str_wrap(texas,40), 
           hjust="left", vjust="top", size=1.5
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+1.7*us_width,y=unname(us_bbox$ymax)-.36*us_height,
           label="THE SOUTH WEST", 
           hjust="left", vjust="top", size=1.5, family="Avenir Next Demi Bold"
  )+
  annotate("text", 
           x=unname(us_bbox$xmin)+1.7*us_width,y=unname(us_bbox$ymax)-0.372*us_height,
           label=str_wrap(south_west,40), 
           hjust="left", vjust="top", size=1.5
  )+
  
  annotate("text",
           x=unname(us_bbox$xmin)+.1*us_width,y=unname(us_bbox$ymax),
           label="EAST\n COAST", hjust="middle", vjust="top", size=2.5
  ) +
  annotate("text", 
           x=unname(us_bbox$xmax)+.2*us_width,y=unname(us_bbox$ymax)-.5*us_height,
           label="WEST\n COAST", hjust="middle", vjust="bottom", size=2.5
  )+
  
  theme(
    plot.background=element_rect(fill="#ffffff"),
    axis.text=element_blank(), panel.grid.major=element_blank(),  
    axis.title.x=element_blank(), axis.title.y=element_blank()
  )


ggsave(filename=here("figs", "03", "wp.png"), plot=wp,width=10, height=7.8, dpi=500)


choropleth <- trump_data_raw |> 
  mutate(is_trump=share_trump>.5) |> 
  ggplot() +
  coord_sf(
    datum=NA,
    xlim = c(unname(us_bbox_raw$xmin), unname(us_bbox_raw$xmax)), 
    ylim = c(unname(us_bbox_raw$ymin), unname(us_bbox_raw$ymax)+0.08*us_height)
  )+
  geom_sf(aes(fill=is_trump, colour=is_trump), size=.01)+
  geom_sf(
    data=state_data_raw, 
    fill="transparent", aes(colour=is_trump), size=.025
    ) +
  geom_text(
    data=state_data_raw %>% st_drop_geometry(),
    aes(label=state_abbr, colour=is_trump, x=x,y=y), size=1.2, family="Avenir Next Demi Bold"
  )+
  annotate("text", x=us_bbox_raw$xmin+.09*us_width_raw, y=us_bbox_raw$ymax+.08*us_height_raw, label="Choropleth of county party majority", family="Avenir Next Medium", hjust="left", size=2) +
  scale_colour_manual(values=c("#46779F", "#CD5B4F"), guide="none") +
  scale_fill_manual(values=c("#EEF4F9", "#FDF1F1"), guide="none") +
  theme_void()
  


wp <- 
  ggplot() +
  
  coord_sf(
    datum=NA,
    xlim = c(unname(us_bbox$xmin)-0*us_width, unname(us_bbox$xmin)+1.4*us_width), 
    ylim = c(unname(us_bbox$ymin), unname(us_bbox$ymax)+0.1*us_height)
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.75*us_width,y=unname(us_bbox$ymax)+0.1*us_height,
           label="The peaks and valleys of Trump and Clinton’s support", 
           family="Cinzel", hjust="middle", vjust="top", size=4.5
  ) +
  annotate("text", 
           x=unname(us_bbox$xmin)+0.75*us_width,y=unname(us_bbox$ymax)+0.07*us_height,
           label="Clinton won in urban counties, while Trump won everywhere else.", 
           family="Cinzel", hjust="middle", vjust="top", size=3
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
    xmax=unname(us_bbox$xmax) + 0.62*us_width,
    ymin=unname(us_bbox$ymax) - 0.01*us_height,
    ymax=unname(us_bbox$ymax) - .13*us_height
  ) +
  annotation_custom(
    grob=ggplotGrob(choropleth),
    xmin=unname(us_bbox$xmax)+.11*us_width,
    xmax=unname(us_bbox$xmax)+.62*us_width,
    ymin=unname(us_bbox$ymax)-.19*us_height,
    ymax=unname(us_bbox$ymax)-.38*us_height
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin),y=unname(us_bbox$ymin)+0.772*us_height,
           label="THE GREAT LAKES", 
           hjust="left", vjust="top", size=1.5, family="Avenir Next Demi Bold"
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin),y=unname(us_bbox$ymin)+0.76*us_height,
           label=str_wrap(great_lakes,30), 
           hjust="left", vjust="top", size=1.5
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.35*us_width,y=unname(us_bbox$ymax)+0.012*us_height,
           label="THE NORTH EAST", 
           hjust="left", vjust="top", size=1.5, family="Avenir Next Demi Bold"
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.35*us_width,y=unname(us_bbox$ymax)+0.00*us_height,
           label=str_wrap(north_east,50), 
           hjust="left", vjust="top", size=1.5
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.78*us_width,y=unname(us_bbox$ymax)-.25*us_height,
           label="THE URBAN-RURAL DIVIDE", 
           hjust="left", vjust="top", size=1.5, family="Avenir Next Demi Bold"
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmin)+0.78*us_width,y=unname(us_bbox$ymax)-0.262*us_height,
           label=str_wrap(urban_rural,40), 
           hjust="left", vjust="top", size=1.5
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmax)-0.15*us_width,y=unname(us_bbox$ymin)+.4*us_height,
           label="TEXAS AND THE PLAINS", 
           hjust="left", vjust="top", size=1.5, family="Avenir Next Demi Bold"
  )+
  annotate("text", 
           x=unname(us_bbox$xmax)-0.15*us_width,y=unname(us_bbox$ymin)+0.388*us_height,
           label=str_wrap(texas,40), 
           hjust="left", vjust="top", size=1.5
  )+
  
  annotate("text", 
           x=unname(us_bbox$xmax)-.25*us_width,y=unname(us_bbox$ymin)+.15*us_height,
           label="THE SOUTH WEST", 
           hjust="left", vjust="top", size=1.5, family="Avenir Next Demi Bold"
  )+
  annotate("text", 
           x=unname(us_bbox$xmax)-.25*us_width,y=unname(us_bbox$ymin)+0.138*us_height,
           label=str_wrap(south_west,40), 
           hjust="left", vjust="top", size=1.5
  )+
  
  annotate("text",
           x=unname(us_bbox$xmin)+.1*us_width,y=unname(us_bbox$ymax)-.01*us_height,
           label="EAST\n COAST", hjust="middle", vjust="top", size=2.5
  ) +
  annotate("text", 
           x=unname(us_bbox$xmin)+.2*us_width,y=unname(us_bbox$ymin)+.025*us_height,
           label="WEST\n COAST", hjust="middle", vjust="bottom", size=2.5
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
