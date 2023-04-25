###############################################################################
# Figures for vis4sds 
# Chapter 5
# Author: Roger Beecham
###############################################################################


remotes::install_github('thomasp85/tidygraph')
# remotes::install_github("ITSLeeds/pct")
# library(pct)
remotes::install_github('thomasp85/ggraph')
library(tidygraph)
library(tidyverse)
library(ggraph)
library(ggforce)
library(here)

od_pairs <- read_csv(here("../", "data", "ch5", "london_ttw.csv"))


edges <- od_pairs |> 
  group_by(o_bor, d_bor)  |>  
  summarise(
    count = sum(count),
    is_prof = mean(as.numeric(is_prof))
    ) |>  
  ungroup() |> 
  select(x=o_bor, y=d_bor, count, focus=o_bor)
  

nodes_d <- od_pairs |> 
  group_by(d_bor) |> 
  summarise(
    count = sum(count),
    is_prof = mean(as.numeric(is_prof))
  ) |> 
  ungroup() |>  
  rename(la = d_bor) |> 
  mutate(type="jobs")

nodes_o <- od_pairs |> 
  group_by(o_bor) |> 
  summarise(
    count = sum(count),
    is_prof = mean(as.numeric(is_prof))
  ) |> 
  ungroup() |>  
  rename(la = o_bor) |> 
  mutate(type="workers")

nodes  <- nodes_o %>% rbind(nodes_d) %>% 
  left_join(london_squared, by=c("la"="authority")) %>% 
  select(la, all, type, BOR) %>% 
  pivot_wider(names_from="type", values_from="all")

edges <- edges_o %>% rbind(edges_d) %>% 
  left_join(london_squared, by=c("focus"="authority")) %>% 
  select(x,y,type,all, focus, BOR) 

bor_orders <- nodes %>% arrange(-`in`) %>% pull(BOR)

plot <- nodes %>% 
  mutate(BOR=factor(BOR, levels=bor_orders)) %>% 
  ggplot() + 
  geom_col(aes(x=BOR, y=`in`), alpha=1, fill="#377eb8") +
  geom_col(aes(x=BOR, y=-out), alpha=1, fill="#e41a1c") +
  annotate("text", x="BAR", y=150000, 
           label="flows in", family="Roboto Condensed", hjust=1, size=3.5) +
  annotate("text", x="BAR", y=-150000, 
           label="flows out", family="Roboto Condensed", hjust=1, size=3.5) +
  #scale_fill_manual(values=c(site_colours$primary, site_colours$secondary))+
  labs(x="Borough", y="Count", 
       title="Flows in- and out- of London Boroughs for work",
       subtitle="-- 2011 Cenus",
       caption="2011 Census data accessed via `pct` package")+
  theme(axis.text.x = element_text(angle=-90, hjust=0), axis.text.y = element_blank())

ggsave(filename="./static/class/05-class_files/flows-bor.png", plot=plot,width=7, height=5, dpi=300)


edges <- edges %>% select(x,y,type,all, focus, BOR) %>% 
   pivot_wider(names_from="type", values_from="all") %>% 
  mutate(all=out+`in`)
 
 
#nodes <- nodes %>% select(la, all, type, BOR) %>% 
   #pivot_wider(names_from="type", values_from="all") %>% 
   #mutate(all=out+`in`)

nodes <- nodes %>% 
  #pivot_wider(names_from="type", values_from="all") %>% 
  mutate(all=out+`in`)


graph <- 
  tbl_graph(
    nodes=nodes, 
    edges=edges 
    )
  
# Not specifying the layout - defaults to "auto"
plot <- ggraph(graph, weights = log(all)) + 
  #geom_edge_fan(aes(edge_width = all, edge_alpha=all, edge_colour=type)) +
  geom_edge_link(aes(edge_width = all, edge_alpha=all), colour="#08306b") +
  geom_node_label(aes(label=BOR), size=3, 
                  label.padding = unit(0.1, "lines"),
                  label.size = 0, alpha=.8)+
  scale_edge_width(range = c(0.1,3))+
#  scale_edge_colour_manual(values=c(site_colours$primary, site_colours$secondary))+
  guides(edge_alpha=FALSE, size=FALSE, edge_width=FALSE) +
  theme_v_gds() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(title="Node-link diagram with edges showing frequencies between London Boroughs for work",
     subtitle="-- 2011 Cenus",
     caption="2011 Census data accessed via `pct` package")

ggsave(filename="./static/class/05-class_files/node-link-bor.png", plot=plot,width=9, height=6, dpi=300)

grid_real_sf <- read_sf("./lib/grid_real.geojson") %>% 
  left_join(london_squared %>% select(authority, BOR))

# Find smwg cell size.
cell_height <- grid_real_sf %>% st_drop_geometry() %>% filter(type=="grid") %>% 
  filter(case_when(x==5 & y==6 ~ TRUE,
                   x==6 & y==6 ~ TRUE,
                   TRUE ~ FALSE)) %>% 
  transmute(diff=east-lag(east,1)) %>% filter(!is.na(diff), diff>0) %>% pull

real <- grid_real_sf %>% 
  filter(type=="real") %>% 
  ggplot()+
  geom_sf(fill="#cfcfcf", colour="#9e9e9e", size=0.1)+
  coord_sf(crs=27700, datum=NA)+
  geom_text(aes(x=east, y=north, label=BOR), size=3, alpha=.6, show.legend=FALSE, family="Roboto Condensed")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

grid <- grid_real_sf %>% 
  filter(type=="grid") %>% 
  ggplot()+
  geom_sf(fill="#cfcfcf", colour="#9e9e9e", size=0.1)+
  coord_sf(crs=27700, datum=NA)+
  geom_text(aes(x=east, y=north+.2*cell_height, label=BOR), size=3, alpha=.6, show.legend=FALSE, family="Roboto Condensed")+
  geom_text(aes(x=east, y=north-.05*cell_height, label=str_wrap(authority,15)), size=1.6, alpha=.9, show.legend=FALSE, family="Roboto Condensed", vjust=1)+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

plot <- real + grid +
  plot_annotation(
    title="Relaxed spatial layout of London Boroughs",
     subtitle="--LondonSquared's After the Flood layout",
     caption="See: github.com/aftertheflood/londonsquared")

ggsave(filename="./static/class/05-class_files/geogs.png", plot=plot,width=9, height=4.35, dpi=300)


# Load helper functions for defining Bezier.
source("./lib/bezier_path.R")
# Load helper functions for defining straight line.
source("./lib/straight_path.R")

edges <- edges %>% 
  transmute(od_pair=paste(x,"-",y), o_bor=x, d_bor=y, count=out, combined=out+`in`) %>% 
  left_join(grid_real_sf %>% st_drop_geometry %>% filter(type=="real") %>% 
              select(authority, east, north), by=c("o_bor"="authority")) %>% 
  rename("o_x"="east", "o_y"="north") %>% 
  left_join(grid_real_sf %>% st_drop_geometry %>% filter(type=="real") %>% 
              select(authority, east, north), by=c("d_bor"="authority")) %>% 
  rename("d_x"="east", "d_y"="north")
  
# Build data frame of asymmetric trajectories (OD pair with controls) : all bike journeys
od_trajectories_bezier <- bind_rows(edges %>% filter(o_bor!=d_bor) %>% pull(od_pair) 
                                    %>% unique %>% purrr::map_df(
                                      ~get_trajectory(edges %>% mutate(count=count) %>% filter(od_pair==.x))
                                    )
)
# Build data frame of trajectories (OD pair with controls) : all bike journeys
od_trajectories_line <- bind_rows(edges %>% filter(o_bor!=d_bor) %>% pull(od_pair) %>% 
                                    unique %>% purrr::map_df(
                                      ~get_trajectory_line(edges %>% mutate(count=combined) %>%
                                                             filter(od_pair==.x))
                                    )
)


od_trajectories <- od_trajectories_line %>% mutate(f_od=((count/max(count))^0.9))
plot_line <- ggplot()+
  geom_sf(data=grid_real_sf %>% filter(type=="real"),  fill="#cfcfcf", colour="#9e9e9e", size=0.1)+
  coord_sf(crs=st_crs(grid_real_sf %>% filter(type=="real")), datum=NA)+
  geom_path(aes(x=x, y=y, group=od_pair, alpha=f_od, size=f_od), data=od_trajectories, colour="#08306b")+
  # geom_label(data=grid_real_sf %>% filter(type=="real"),
  #            aes(x=east, y=north, label=BOR), size=3, label.padding = unit(0.1, "lines"),label.size = 0, alpha=.8)+
  annotate("text", x=558297.5+.5*38642.8, y=197713.7, label="")+
  geom_text(data=grid_real_sf %>% filter(type=="real"),
            aes(x=east, y=north, label=BOR), size=3, colour="#FFFFFF", family="Roboto Condensed Regular")+ #colour="#FFFFFF"
  scale_alpha_continuous(range=c(0.01,1))+
  scale_size_continuous(range=c(0.01,2))+
  guides(alpha=FALSE, size=FALSE)+
  theme(axis.title=element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

od_trajectories <- od_trajectories_bezier %>% mutate(f_od=((count/max(count))^0.9))
plot_bezier <- ggplot()+
  geom_sf(data=grid_real_sf %>% filter(type=="real"),  fill="#cfcfcf", colour="#9e9e9e", size=0.1)+
  coord_sf(crs=st_crs(grid_real_sf %>% filter(type=="real")), datum=NA)+
  #geom_label(data=grid_real_sf %>% filter(type=="real"),
  #           aes(x=east, y=north, label=BOR), size=3, label.padding = unit(0.1, "lines"),label.size = 0, alpha=.8)+
  geom_bezier0(aes(x=x, y=y, group=od_pair, alpha=f_od, size=f_od), data=od_trajectories, colour="#08306b")+
  geom_text(data=grid_real_sf %>% filter(type=="real"),
            aes(x=east, y=north, label=BOR), size=3, colour="#FFFFFF", family="Roboto Condensed Regular")+
  annotate("text", x=558297.5+.5*38642.8, y=197713.7, label="")+
  scale_alpha_continuous(range=c(0.3,1))+
  scale_size_continuous(range=c(0.01,2))+
  guides(alpha=FALSE, size=FALSE)+
  theme(axis.title=element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())


plot <- plot_line / plot_bezier +
  plot_annotation(
    title="Lines with edges showing frequencies between London Boroughs for work",
    subtitle="--Left straight lines showing total flows between OD pairs; Right asymmetric Bezier curves encoding direction",
    caption="2011 Census data accessed via `pct` package",
    theme=theme(plot.title = element_text(size = 12),
                plot.subtitle = element_text(size = 9),
                plot.caption = element_text(size = 7)))

ggsave(filename="./static/class/05-class_files/lines-geog.png", plot=plot,width=7, height=9.4, dpi=300)

matrix_1 <- edges %>% 
  left_join(grid_real_sf %>% st_drop_geometry() %>% select(authority, BOR), by=c("o_bor"="authority")) %>% 
  rename(o_bor_abbr=BOR) %>% 
  left_join(grid_real_sf %>% st_drop_geometry() %>% select(authority, BOR), by=c("d_bor"="authority")) %>% 
  rename(d_bor_abbr=BOR) %>% 
  mutate(o_bor_abbr=factor(o_bor_abbr, levels=bor_orders),
         d_bor_abbr=factor(d_bor_abbr, levels=bor_orders)) %>% 
  ggplot(aes(y=fct_rev(o_bor_abbr), x=d_bor_abbr, fill=count)) +
  geom_tile(colour="#707070", size=.2)+
  scale_x_discrete(position = "top") +
  scale_fill_distiller(palette="Blues", direction=1) +
  labs(x="destination borough", y="origin borough", subtitle="global scaling: same OD included", fill="count") +
  theme(axis.text.x = element_text(angle=90))

matrix_2 <- edges %>% 
  left_join(grid_real_sf %>% st_drop_geometry() %>% select(authority, BOR), by=c("o_bor"="authority")) %>% 
  rename(o_bor_abbr=BOR) %>% 
  left_join(grid_real_sf %>% st_drop_geometry() %>% select(authority, BOR), by=c("d_bor"="authority")) %>% 
  rename(d_bor_abbr=BOR) %>% 
  mutate(o_bor_abbr=factor(o_bor_abbr, levels=bor_orders),
         d_bor_abbr=factor(d_bor_abbr, levels=bor_orders),
         count=if_else(o_bor==d_bor,0,count)) %>% 
  ggplot(aes(y=fct_rev(o_bor_abbr), x=d_bor_abbr, fill=count)) +
  geom_tile(colour="#707070", size=.2)+
  geom_tile(data=. %>% filter(o_bor==d_bor),
         aes(y=fct_rev(o_bor_abbr), x=d_bor_abbr), fill="#e0e0e0", colour="#707070", size=.2) +
  scale_x_discrete(position = "top") +
  scale_fill_distiller(palette="Blues", direction=1) +
  labs(x="destination borough", y="origin borough", subtitle="global scaling: same OD removed", fill="count") +
  theme(axis.text.x = element_text(angle=90))


matrix_3 <- edges %>% 
  left_join(grid_real_sf %>% st_drop_geometry() %>% select(authority, BOR), by=c("o_bor"="authority")) %>% 
  rename(o_bor_abbr=BOR) %>% 
  left_join(grid_real_sf %>% st_drop_geometry() %>% select(authority, BOR), by=c("d_bor"="authority")) %>% 
  rename(d_bor_abbr=BOR) %>% 
  mutate(o_bor_abbr=factor(o_bor_abbr, levels=bor_orders),
         d_bor_abbr=factor(d_bor_abbr, levels=bor_orders)) %>% 
  group_by(d_bor) %>% 
  mutate(
    count_rescaled=(count-min(count))/(max(count)-min(count))
  ) %>% 
  ggplot(aes(y=fct_rev(o_bor_abbr), x=d_bor_abbr, fill=count_rescaled)) +
  geom_tile(colour="#707070", size=.2)+
  scale_x_discrete(position = "top") +
  scale_fill_distiller(palette="Blues", direction=1) +
  labs(x="destination borough", y="origin borough", subtitle="local scaling by destination borough: same OD included", fill="count (local scaling)") +
  theme(axis.text.x = element_text(angle=90))

matrix_4 <- edges %>% 
  left_join(grid_real_sf %>% st_drop_geometry() %>% select(authority, BOR), by=c("o_bor"="authority")) %>% 
  rename(o_bor_abbr=BOR) %>% 
  left_join(grid_real_sf %>% st_drop_geometry() %>% select(authority, BOR), by=c("d_bor"="authority")) %>% 
  rename(d_bor_abbr=BOR) %>% 
  mutate(o_bor_abbr=factor(o_bor_abbr, levels=bor_orders),
         d_bor_abbr=factor(d_bor_abbr, levels=bor_orders)) %>% 
  group_by(d_bor) %>% 
  mutate(
    count_rescaled=(count-min(count))/(max(count)-min(count))
  ) %>% 
  ggplot(aes(y=fct_rev(o_bor_abbr), x=d_bor_abbr, fill=count_rescaled)) +
  geom_tile(fill="transparent", size=0)+
  scale_x_discrete(position = "top") +
  scale_fill_distiller(palette="Blues", direction=1) +
  labs(x="destination borough", y="origin borough", subtitle="local scaling by destination borough: same OD included", fill="count (local scaling)") +
  theme(
    axis.text.x = element_text(angle=90, colour="#eeeeee"),
    axis.title.x=element_text(colour="#eeeeee"),
    axis.title.y=element_text(colour="#eeeeee"),
    axis.text.y=element_text(colour="#eeeeee"),
    plot.subtitle=element_text(colour="#eeeeee"),
    panel.grid = element_blank())


plot <- (matrix_1 + matrix_2) / (matrix_3 + matrix_4)+
  plot_annotation(
    title="Origin-destination matrix of flow freuqneices between London boroughs",
    subtitle="--Different colour scalings are explored between charts",
    caption="2011 Census data accessed via `pct` package")

ggsave(filename="./static/class/05-class_files/matrices.png", plot=plot,width=12, height=13, dpi=300)


# Generate trjectories based on semi-spatial layout.
edges <- edges %>% select(-c(o_x,o_y,d_x,d_y)) %>% 
  left_join(grid_real_sf %>% st_drop_geometry %>% filter(type=="grid") %>% 
              select(authority, east, north), by=c("o_bor"="authority")) %>% 
  rename("o_x"="east", "o_y"="north") %>% 
  left_join(grid_real_sf %>% st_drop_geometry %>% filter(type=="grid") %>% 
              select(authority, east, north), by=c("d_bor"="authority")) %>% 
  rename("d_x"="east", "d_y"="north")


od_trajectories_temp <- bind_rows(edges %>% filter(o_bor!=d_bor) %>% pull(od_pair) 
                                  %>% unique %>% purrr::map_df(
                                    ~get_trajectory(edges %>% filter(od_pair==.x))
                                  )
)

# Join with temp_data for d_fX and d_fY for faceting.
od_trajectories_temp <- od_trajectories_temp %>% left_join(edges %>% select(od_pair, o_x, o_y, d_x, d_x, o_bor, d_bor), by=c("od_pair"="od_pair"))
# Temporary plot object of data joined to geom_sf geometries. DO map so geometries join on origin.
plot_data_temp <- grid_real_sf %>% filter(type=="grid") %>% right_join(edges %>% select(-c(o_x, o_y, d_x, d_y)), by=c("authority"="o_bor")) %>% 
  mutate(o_bor=authority) %>% 
  rename(o_fx=x, o_fy=y) %>% 
  left_join(grid_real_sf %>% filter(type=="grid") %>% st_drop_geometry() %>% select(authority,x,y), by=c("d_bor"="authority")) %>%
  rename(d_fx=x, d_fy=y) 
  
# Identify borough in focus (edit this to switch between D-OD and O-DO matrix).
plot_data_temp <- plot_data_temp %>% mutate(bor_label=if_else(o_bor==d_bor,d_bor,""),
                                            bor_focus=if_else(o_bor==d_bor,1,0))

# Rescale f_od for plotting.
od_trajectories_temp <- od_trajectories_temp %>% mutate(f_od=((count/max(count))^0.5)) 
# Plot grid-within-grid (for demonstration).
# width
width <- plot_data_temp %>% summarise(width=max(east)-min(east))  %>% pull(width)
# height
height <- plot_data_temp %>% summarise(height=max(north)-min(north)) %>% pull(height)

grid_within_grid <- ggplot()+
  geom_sf(data=plot_data_temp,  fill="#ffffff", colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#252525", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  theme(
    panel.spacing=unit(-0.2, "lines"),
    legend.position="bottom",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank()
    )

# Grid-bezier.
wst_grid_bezier_do <- ggplot()+
  geom_sf(data=plot_data_temp %>% filter(d_bor=="Westminster"),  fill="#fafafa", colour="#616161", size=0.1, alpha=0.7)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1, d_bor=="Westminster"), fill="transparent",  colour="#373737", size=0.3)+
  geom_bezier0(data=od_trajectories_temp  %>% filter(d_bor=="Westminster"), aes(x=x, y=y, group=od_pair, alpha=f_od, colour=f_od, size=f_od^2))+
  #geom_text(data=plot_data_temp %>% filter(bor_focus==1, d_bor=="Westminster"), aes(x=east, y=north, label=BOR), colour="#252525", alpha=0.8, show.legend=FALSE, size=3.5, hjust="left", family="Avenir Book")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  #scale_alpha_continuous(range=c(0.4,0.99))+
  scale_size_continuous(range=c(.1,.7))+
  scale_colour_distiller(palette="Blues", direction=1.4, limits=c(0,1))+
  guides(colour=FALSE, alpha=FALSE, size=FALSE)+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  labs(caption="Commutes into Westminster")+
  theme(
    legend.position="bottom",
    axis.title.x =element_blank(),axis.title.y =element_blank(),
    strip.text.x=element_blank(),strip.text.y = element_blank()
    )

# Plot grid-fill.
wst_grid_fill_do <- ggplot()+
  geom_sf(data=plot_data_temp %>% filter(d_bor=="Westminster", o_bor!=d_bor), aes(fill=count), colour="#616161", size=0.1)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1, d_bor=="Westminster"), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1, d_bor=="Westminster"), 
            aes(x=east, y=north, label=BOR), 
            colour="#252525", alpha=0.8, show.legend=FALSE, size=2.5, hjust="centre", family="Roboto Condensed")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  scale_fill_distiller(palette="Blues", direction=1)+
  guides(fill=FALSE)+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  labs(caption="Commutes into Westminster")+
  theme(
    legend.position="bottom",
    axis.title.x =element_blank(),axis.title.y =element_blank(),
    strip.text.x=element_blank(),strip.text.y = element_blank()
  )

# Remove geometries and rejoin for 0-DO map so geometries join on destination.
st_geometry(plot_data_temp) <- NULL
plot_data_temp <-  grid_real_sf %>% filter(type=="grid") %>% right_join(edges %>% select(-c(o_x, o_y, d_x, d_y)), by=c("authority"="d_bor")) %>% 
  mutate(d_bor=authority) %>% 
  rename(d_fx=x, d_fy=y) %>% 
  left_join(grid_real_sf %>% filter(type=="grid") %>% st_drop_geometry() %>% select(authority,x,y), by=c("o_bor"="authority")) %>%
  rename(o_fx=x, o_fy=y) 
  
# Edit borough in focus (switch between D-OD and O-DO matrix).
plot_data_temp <- plot_data_temp %>% mutate(bor_label=if_else(o_bor==d_bor,o_bor,""),
                                            bor_focus=if_else(o_bor==d_bor,1,0))

# Plot grid-bezier.
hck_grid_bezier_od <- ggplot()+
  geom_sf(data=plot_data_temp %>% filter(o_bor=="Hackney"),  fill="#fafafa", colour="#616161", size=0.1, alpha=0.7)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1, o_bor=="Hackney"), fill="transparent",  colour="#373737", size=0.3)+
  geom_bezier0(data=od_trajectories_temp  %>% filter(o_bor=="Hackney"), aes(x=x, y=y, group=od_pair, alpha=f_od, colour=f_od, size=f_od))+
  #geom_text(data=plot_data_temp %>% filter(bor_focus==1, o_bor=="Hackney"), aes(x=lon_east_min, y=lon_north_min, label=BOR), colour="#252525", alpha=0.8, show.legend=FALSE, size=3.5, hjust="left", family="Avenir Book")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  #scale_alpha_continuous(range=c(0.4,0.99))+
  scale_size_continuous(range=c(.1,.7))+
  scale_colour_distiller(palette="Blues", direction=1, limits=c(0,1))+
  guides(colour=FALSE, alpha=FALSE, size=FALSE)+
  facet_grid(o_fy~o_fx, shrink=FALSE)+
  labs(caption="Commutes out of Hackney")+
  theme(
    legend.position="bottom",
    axis.title.x =element_blank(),axis.title.y =element_blank(),
    strip.text.x=element_blank(),strip.text.y = element_blank()
  )

# Plot grid-fill.
hck_grid_fill_od <- ggplot()+
  geom_sf(data=plot_data_temp %>% filter(o_bor=="Hackney", o_bor!=d_bor), aes(fill=count), colour="#616161", size=0.1)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1, o_bor=="Hackney"), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1, o_bor=="Hackney"), 
            aes(x=east, y=north, label=BOR), 
            colour="#252525", alpha=0.8, show.legend=FALSE, size=2.5, hjust="centre", family="Roboto Condensed")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  scale_fill_distiller(palette="Blues", direction=1)+
  guides(fill=FALSE)+
  facet_grid(o_fy~o_fx, shrink=FALSE)+
  labs(caption="Commutes out of Hackney")+
  theme(
    legend.position="bottom",
    axis.title.x =element_blank(),axis.title.y =element_blank(),
    strip.text.x=element_blank(),strip.text.y = element_blank()
  )

plot <- grid_within_grid /
  (wst_grid_bezier_do| wst_grid_fill_do | hck_grid_bezier_od | hck_grid_fill_od) +
  #plot_layout(widths=c(.69,.005,.29)) +
  plot_layout(heights=c(.82,.18)) +
  plot_annotation(
    title="OD Map layout (map-within-map) ",
    subtitle="--Map cells can be reorganised to make desitinations / origins the focus",
    caption="See Wood et al. (2010)"
)


ggsave(filename="./static/class/05-class_files/od_map.png", plot=plot,width=6.5, height=7, dpi=300)


matrix <- edges %>% 
  left_join(grid_real_sf %>% filter(type=="grid") %>%  st_drop_geometry() %>% select(authority, BOR), by=c("o_bor"="authority")) %>% 
  rename(o_bor_abbr=BOR) %>% 
  left_join(grid_real_sf %>% filter(type=="grid") %>% st_drop_geometry() %>% select(authority, BOR), by=c("d_bor"="authority")) %>% 
  rename(d_bor_abbr=BOR) %>% 
  mutate(o_bor_abbr=factor(o_bor_abbr, levels=bor_orders),
         d_bor_abbr=factor(d_bor_abbr, levels=bor_orders)) %>% 
  group_by(d_bor) %>% 
  mutate(
    count=if_else(o_bor==d_bor,0,count),
    count_rescaled=(count-min(count))/(max(count)-min(count))
  ) %>% 
  ggplot(aes(y=fct_rev(o_bor_abbr), x=d_bor_abbr, fill=count_rescaled)) +
  geom_tile(colour="#cfcfcf", size=.1, alpha=.2)+
  geom_tile(data=. %>% filter(d_bor == "Westminster" | o_bor=="Hackney"), colour="#707070", size=.1)+
  geom_tile(data=. %>% filter(o_bor==d_bor,  o_bor %in% c("Hackney", "Westminster")),
            aes(y=fct_rev(o_bor_abbr), x=d_bor_abbr), fill="#e0e0e0", colour="#707070", size=.1) +
  geom_text(data=. %>% filter(o_bor==d_bor, o_bor %in% c("Hackney", "Westminster")), 
            aes(label=o_bor_abbr), 
            colour="#252525", alpha=1, show.legend=FALSE, size=1.5, hjust="centre", family="Roboto Condensed")+
  scale_x_discrete(position = "top") +
  scale_fill_distiller(palette="Blues", direction=1) +
  guides(fill=FALSE)+
  labs(x="destination borough", y="origin borough", fill="count (local scaling)")+
  theme(
    axis.text.x = element_text(angle=90,colour="#8e8e8e", size=5),
    axis.text.y = element_text(colour="#8e8e8e", size=5),
    axis.title.x = element_text(size=6),
    axis.title.y = element_text(size=6),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


plot <- matrix +
  ((wst_grid_bezier_do+ wst_grid_fill_do) / (hck_grid_bezier_od + hck_grid_fill_od)) +
  plot_annotation(
    title="Highlighted destination (WST) and origin (HCK) with spatial arrangement",
    subtitle="--Allows us to show the geography of workers commuting into WST and out of HCK",
    theme=theme(plot.title = element_text(size = 13),
                plot.subtitle = element_text(size = 10)))


ggsave(filename="./static/class/05-class_files/reordered-matrix.png", plot=plot,width=9.5, height=6, dpi=300)


grid_within_grid <- ggplot()+
  geom_sf(data=plot_data_temp,  fill="#ffffff", colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#252525", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  theme(
    panel.spacing=unit(-0.2, "lines"),
    legend.position="bottom",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank()
  )



od_trajectories_temp <- bind_rows(edges %>% filter(o_bor!=d_bor) %>% pull(od_pair) 
                                  %>% unique %>% purrr::map_df(
                                    ~get_trajectory(edges %>% filter(od_pair==.x))
                                  )
)

# Join with temp_data for d_fX and d_fY for faceting.
od_trajectories_temp <- od_trajectories_temp %>% left_join(edges %>% select(od_pair, o_x, o_y, d_x, d_x, o_bor, d_bor), by=c("od_pair"="od_pair"))
# Temporary plot object of data joined to geom_sf geometries. DO map so geometries join on origin.
plot_data_temp <- grid_real_sf %>% filter(type=="grid") %>% right_join(edges %>% select(-c(o_x, o_y, d_x, d_y)), by=c("authority"="o_bor")) %>% 
  mutate(o_bor=authority) %>% 
  rename(o_fx=x, o_fy=y) %>% 
  left_join(grid_real_sf %>% filter(type=="grid") %>% st_drop_geometry() %>% select(authority,x,y), by=c("d_bor"="authority")) %>%
  rename(d_fx=x, d_fy=y) 

# Identify borough in focus (edit this to switch between D-OD and O-DO matrix).
plot_data_temp <- plot_data_temp %>% mutate(bor_label=if_else(o_bor==d_bor,d_bor,""),
                                            bor_focus=if_else(o_bor==d_bor,1,0))

# Rescale f_od for plotting.
od_trajectories_temp <- od_trajectories_temp %>% mutate(f_od=((count/max(count))^0.5)) 
# Plot grid-within-grid (for demonstration).
# width
width <- plot_data_temp %>% summarise(width=max(east)-min(east))  %>% pull(width)
# height
height <- plot_data_temp %>% summarise(height=max(north)-min(north)) %>% pull(height)

#373737
#ffffff
bbox_grid <- st_bbox(grid_real_sf %>% filter(type=="grid"))


d_od_global <- ggplot()+
  geom_sf(data=plot_data_temp, 
          #%>% mutate(count=if_else(o_bor==d_bor,0,count)), 
          aes(fill=count), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#252525", alpha=0.9, size=1.6, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=1.8, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  #guides(fill=FALSE)+
  labs(caption="global scaling by destination borough: same OD included")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Blues", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    plot.caption = element_text(size = 7),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff"),
    legend.key = element_rect(size=1.4),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 5),
    legend.key.size = unit(.8,"line"),
  )

d_od_local <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            #mutate(count=if_else(o_bor==d_bor,0,count)) %>% 
            group_by(d_bor) %>% 
            mutate(count_rescaled=(count-min(count))/(max(count)-min(count))), aes(fill=count_rescaled), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", alpha=0.9, size=1.6, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=1.8, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  #guides(fill=FALSE)+
  labs(caption="local scaling by destination borough: same OD included")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Blues", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    plot.caption = element_text(size = 7),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff"),
    legend.key = element_rect(size=1.4),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 5),
    legend.key.size = unit(.8,"line")
  )


plot <- d_od_global + d_od_local + plot_layout(nrow=2)+
    plot_annotation(
    title="OD Map layout (map-within-map)",
    subtitle="--Large cell: destinations; small cell: origins",
    caption="See Wood et al (2010)",
    theme=theme(plot.title = element_text(size = 9),
                plot.subtitle = element_text(size = 7),
                plot.caption = element_text(size = 5)))


ggsave(filename="./static/class/05-class_files/d-od-map.png", plot=plot,width=5.5, height=8, dpi=300)



edges <- od_pairs %>% 
  # Filter only *within* London.
  filter(la_1 %in% london_las, la_2 %in% london_las) %>% 
  group_by(la_1, la_2) %>% 
  summarise(
    across(c(all:other), sum)
  ) %>% 
  ungroup %>% 
  mutate(o_bor=la_1, d_bor=la_2, 
         public_transport=train+bus+light_rail, 
         car=car_driver+car_passenger,
         active=bicycle+foot
         ) %>% 
  select(-c(la_1, la_2, 
            other, car_driver, car_passenger, train, bus, light_rail, 
            taxi, motorbike, other, from_home, bicycle, foot)
         ) 

nodes_d <- od_pairs_borough %>% 
  group_by(la_2) %>% 
  summarise(
    across(c(all:other), sum)
  ) %>% 
  ungroup %>% 
  rename(bor = la_2) %>% 
  mutate(
    type="destination", 
    public_transport=train+bus+light_rail, 
    car=car_driver+car_passenger,
    active=bicycle+foot
  ) %>% 
  select(-c(other, car_driver, car_passenger, train, bus, light_rail, 
           taxi, motorbike, other, from_home,  bicycle, foot))

nodes_o <- od_pairs_borough %>% 
  group_by(la_1) %>% 
  summarise(
    across(c(all:other), sum)
  ) %>% 
  ungroup %>% 
  rename(bor = la_1) %>% 
  mutate(
    type="origin", 
    public_transport=train+bus+light_rail, 
    car=car_driver+car_passenger,
    active=bicycle+foot
  ) %>% 
  select(-c(other, car_driver, car_passenger, train, bus, light_rail, 
           taxi, motorbike, other, from_home,  bicycle, foot)
         )

nodes  <- nodes_o %>% rbind(nodes_d) 

bor_orders <- nodes %>% 
  filter(type=="destination") %>%  
  arrange(-all) %>% pull(bor)

# pt #4daf4a - green, active #377eb8 - blue, car #e41a1c - red
# green - #006d2c, red - #a50f15, blue - #08519c

bars <- nodes %>% 
  left_join(grid_real_sf %>% st_drop_geometry() %>% select(authority, BOR), by=c("bor"="authority")) %>% 
  pivot_longer(cols=c(active, public_transport, car), names_to="mode", values_to="count") %>% 
  mutate(bor=factor(bor, levels=bor_orders)) %>% 
  ggplot() +
  geom_col(aes(x=bor, y=count, fill=mode)) +
  #scale_fill_manual(values=c("#377eb8","#e41a1c", "#4daf4a"))+
  scale_fill_manual(values=c("#2171b5","#cb181d", "#238b45"))+
  guides(fill=FALSE)+
  labs(x="", y="count") +
  facet_wrap(~type, nrow=2)+
  coord_flip() 


bars <- nodes %>% 
  left_join(grid_real_sf %>% st_drop_geometry() %>% select(authority, BOR), by=c("bor"="authority")) %>% 
  pivot_longer(cols=c(active, public_transport, car), names_to="mode", values_to="count") %>% 
  mutate(bor=factor(bor, levels=bor_orders)) %>% 
  ggplot() +
  geom_col(aes(x=bor, y=count, fill=mode)) +
  #scale_fill_manual(values=c("#377eb8","#e41a1c", "#4daf4a"))+
  scale_fill_manual(values=c("#2171b5","#cb181d", "#238b45"))+
  guides(fill=FALSE)+
  labs(x="", y="count") +
  facet_wrap(~type, ncol=2)+
  theme(axis.text.x = element_text(angle=90))

bars_filled <- nodes %>% 
  pivot_longer(cols=c(active, public_transport, car), names_to="mode", values_to="count") %>% 
  mutate(bor=factor(bor, levels=bor_orders)) %>% 
  ggplot() +
  geom_col(aes(x=bor, y=count, fill=mode), position="fill") +
  scale_fill_manual(values=c("#2171b5","#cb181d", "#238b45"))+
  labs(x="", y="count") +
  facet_wrap(~type, nrow=2)+
  theme(legend.position="right",
        axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.title.y = element_blank(), axis.title.x = element_blank()) +
  coord_flip()

bars_filled <- nodes %>% 
  pivot_longer(cols=c(active, public_transport, car), names_to="mode", values_to="count") %>% 
  mutate(bor=factor(bor, levels=bor_orders)) %>% 
  ggplot() +
  geom_col(aes(x=bor, y=count, fill=mode), position="fill") +
  scale_fill_manual(values=c("#2171b5","#cb181d", "#238b45"))+
  labs(x="", y="count") +
  facet_wrap(~type, ncol=2)+
  theme(
        axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.title.y = element_blank(), axis.title.x = element_blank()) 

plot <- bars + bars_filled + (dest_plot/origin_plot)+ plot_layout(widths=c(.3,.25,.45)) +
  plot_annotation(
    title="Frequencies of commutes into- (destination) and out of- (origin) London boroughs by travel mode",
    subtitle="--Bars, filled bars (showing proportion by mode) and relaxed spatial arrangement",
    caption="2011 Census data accessed via `pct` package"
  )


library(ggmosaic)

dest_origin_plot <- nodes %>% 
  left_join(grid_real_sf %>% st_drop_geometry() %>% filter(type=="grid") %>% 
                      select(authority, BOR,x,y),by=c("bor"="authority")) %>% 
  pivot_longer(cols=c(active, public_transport, car), names_to="mode", values_to="count") %>% 
  mutate(type_abbr=str_sub(type,1,1)) %>% 
  group_by(bor) %>%
  mutate(bor_total=sum(count)) %>% 
  group_by(bor,type) %>% 
  mutate(type_total=sum(count)) %>% 
  ggplot() +
  geom_col(aes(x=type_abbr, y=count/bor_total, fill=mode))+
  geom_text(data=. %>% filter(mode=="active", type=="destination"), aes(x=1.5, y=1, label=BOR), vjust="top", hjust="centre")+
  geom_text(data=. %>% filter(mode=="active", type=="destination"), aes(x=1.4, y=.85, label="dest"), vjust="top", hjust="right", size=3, family="Roboto Light")+
  geom_text(data=. %>% filter(mode=="active", type=="origin"), aes(x=1.6, y=.85, label="origin"), vjust="top", hjust="left", size=3, family="Roboto Light")+
  scale_fill_manual(values=c("#2171b5","#cb181d", "#238b45"))+
  scale_alpha_continuous(range=c(.4,1))+
  facet_grid(y~x)+
  guides(fill=FALSE, alpha=FALSE)+
  theme(
    panel.spacing.y=unit(.2, "lines"), panel.spacing.x=unit(.2, "lines"), 
    panel.background = element_rect(fill="#ffffff", colour="#ffffff"),
    axis.title.x = element_blank(),axis.title.y = element_blank(),
    axis.text.x=element_blank(), axis.text.y = element_blank(),
    strip.text.x=element_blank(), strip.text.y = element_blank(),
    panel.grid=element_blank(),
  )
  


dest_plot <- nodes %>% 
  filter(type=="destination") %>% 
  left_join(grid_real_sf %>% st_drop_geometry() %>% filter(type=="grid") %>% 
              select(authority, BOR,x,y),by=c("bor"="authority")) %>% 
  pivot_longer(cols=c(active, public_transport, car), names_to="mode", values_to="count") %>% 
  group_by(bor,type) %>% 
  mutate(total_commutes=sum(count)) %>% 
  ggplot() +
  geom_col(aes(x=0, y=count, fill=mode, alpha=total_commutes), position="fill")+
  geom_text(data=. %>% filter(mode=="active"), aes(x=0, y=.5, label=BOR))+
  scale_fill_manual(values=c("#2171b5","#cb181d", "#238b45"))+
  scale_alpha_continuous(range=c(.4,1))+
  facet_grid(y~x)+
  labs(subtitle="destination")+
  guides(fill=FALSE, alpha=FALSE)+
  theme(
    panel.spacing.y=unit(-.1, "lines"), panel.spacing.x=unit(-.2, "lines"), 
    axis.title.x = element_blank(),axis.title.y = element_blank(),
    axis.text.x=element_blank(), axis.text.y = element_blank(),
    strip.text.x=element_blank(), strip.text.y = element_blank(),
    panel.grid=element_blank()
  )

origin_plot <- nodes %>% 
  filter(type=="origin") %>% 
  left_join(grid_real_sf %>% st_drop_geometry() %>% filter(type=="grid") %>% 
              select(authority, BOR,x,y),by=c("bor"="authority")) %>% 
  pivot_longer(cols=c(active, public_transport, car), names_to="mode", values_to="count") %>% 
  group_by(bor,type) %>% 
  mutate(total_commutes=sum(count)) %>% 
  ggplot() +
  geom_col(aes(x=0, y=count, fill=mode, alpha=total_commutes), position="fill")+
  geom_text(data=. %>% filter(mode=="active"), aes(x=0, y=.5, label=BOR))+
  scale_fill_manual(values=c("#2171b5","#cb181d", "#238b45"))+
  scale_alpha_continuous(range=c(.4,1))+
  guides(fill=FALSE, alpha=FALSE)+
  labs(subtitle="origin")+
  facet_grid(y~x)+
  theme(
    panel.spacing.y=unit(-.1, "lines"), panel.spacing.x=unit(-.2, "lines"), 
    axis.title.x = element_blank(),axis.title.y = element_blank(),
    axis.text.x=element_blank(), axis.text.y = element_blank(),
    strip.text.x=element_blank(), strip.text.y = element_blank(),
    panel.grid=element_blank()
  )

ggsave(filename="./static/class/05-class_files/mode-plots.png", plot=plot,width=16, height=12, dpi=300)

plot <- bars / bars_filled / (dest_plot|origin_plot)+ plot_layout(heights=c(.35,.15,.5)) +
  plot_annotation(
    title="Frequencies of commutes into- (destination) and out of- (origin) London boroughs by travel mode",
    subtitle="--Bars, filled bars (showing proportion by mode) and relaxed spatial arrangement",
    caption="2011 Census data accessed via `pct` package"
  )

ggsave(filename="./static/class/05-class_files/mode-plots.png", plot=plot,width=12, height=13, dpi=300)




layout <- "
AAAAAAAA
BBBBBBBB
#CCCCCC#"


plot <- bars + bars_filled + dest_origin_plot + plot_layout(design = layout, heights=c(.2,.1,.7)) +
  plot_annotation(
    title="Frequencies of commutes into- (destination) and out of- (origin) London boroughs by travel mode",
    subtitle="--Bars, filled bars (showing proportion by mode) and relaxed spatial arrangement",
    caption="2011 Census data accessed via `pct` package"
  )

ggsave(filename="./static/class/05-class_files/mode-plots.png", plot=plot,width=12, height=13, dpi=300)



# Temporary plot object of data joined to geom_sf geometries. DO map so geometries join on origin.
plot_data_temp <- grid_real_sf %>% filter(type=="grid") %>% right_join(edges, by=c("authority"="o_bor")) %>% 
  mutate(o_bor=authority) %>% 
  rename(o_fx=x, o_fy=y) %>% 
  left_join(grid_real_sf %>% filter(type=="grid") %>% st_drop_geometry() %>% select(authority,x,y), by=c("d_bor"="authority")) %>%
  rename(d_fx=x, d_fy=y) 

# Identify borough in focus (edit this to switch between D-OD and O-DO matrix).
plot_data_temp <- plot_data_temp %>% mutate(bor_label=if_else(o_bor==d_bor,d_bor,""),
                                            bor_focus=if_else(o_bor==d_bor,1,0))

# width
width <- plot_data_temp %>% summarise(width=max(east)-min(east))  %>% pull(width)
# height
height <- plot_data_temp %>% summarise(height=max(north)-min(north)) %>% pull(height)

#373737
#ffffff
bbox_grid <- st_bbox(grid_real_sf %>% filter(type=="grid"))



d_od_global_pt <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=public_transport),# count=if_else(o_bor==d_bor,0,count)), 
aes(fill=count), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#252525", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="public transport: large cells: destinations; small cells; origins")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Greens", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )


d_od_global_active <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=active), #count=if_else(o_bor==d_bor,0,count)), 
          aes(fill=count), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="foot+bike: large cells: destinations; small cells; origins")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Blues", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )

d_od_global_car <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=car),# count=if_else(o_bor==d_bor,0,count)), 
          aes(fill=count), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="Car: large cells: destinations; small cells; origins")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Reds", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )

d_od_local_pt <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            group_by(d_bor) %>% 
            mutate(count=public_transport,# count=if_else(o_bor==d_bor,0,count),
                   count_rescaled=(count-min(count))/(max(count)-min(count))), 
          aes(fill=count_rescaled), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="public transport: large cells: destinations; small cells; origins")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Greens", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )


d_od_global_active <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=active), #count=if_else(o_bor==d_bor,0,count)), 
                   aes(fill=count), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#252525", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="foot+bike: large cells: destinations; small cells; origins")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Blues", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )

d_od_local_active <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=active) %>% # count=if_else(o_bor==d_bor,0,count)) %>% 
            group_by(d_bor) %>% 
            mutate(count_rescaled=(count-min(count))/(max(count)-min(count))), aes(fill=count_rescaled), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="foot+bike: large cells: destinations; small cells; origins")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Blues", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )

d_od_global_car <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=car),# count=if_else(o_bor==d_bor,0,count)), 
          aes(fill=count), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#252525", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="Car: large cells: destinations; small cells; origins")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Reds", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )



d_od_local_car <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=car)#, count=if_else(o_bor==d_bor,0,count)) 
          %>%  group_by(d_bor) %>% 
            group_by(d_bor) %>% 
            mutate(count_rescaled=(count-min(count))/(max(count)-min(count))), 
          aes(fill=count_rescaled), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="Car: large cells: destinations; small cells; origins")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Reds", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )

# Temporary plot object of data joined to geom_sf geometries. DO map so geometries join on origin.
plot_data_temp <- grid_real_sf %>% filter(type=="grid") %>% right_join(edges, by=c("authority"="d_bor")) %>% 
  mutate(d_bor=authority) %>% 
  rename(d_fx=x, d_fy=y) %>% 
  left_join(grid_real_sf %>% filter(type=="grid") %>% st_drop_geometry() %>% select(authority,x,y), by=c("o_bor"="authority")) %>%
  rename(o_fx=x, o_fy=y) 

# Identify borough in focus (edit this to switch between D-OD and O-DO matrix).
plot_data_temp <- plot_data_temp %>% mutate(bor_label=if_else(o_bor==d_bor,o_bor,""),
                                            bor_focus=if_else(o_bor==d_bor,1,0))



o_od_global_pt <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=public_transport),#, count=if_else(o_bor==d_bor,0,count)), 
                   aes(fill=count), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#252525", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="public transport: large cells: origins; small cells; destinations")+
  facet_grid(o_fy~o_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Greens", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )


o_od_local_pt <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=public_transport, count=if_else(o_bor==d_bor,0,count)) %>% 
            group_by(o_bor) %>% 
            mutate(count_rescaled=(count-min(count))/(max(count)-min(count))), 
          aes(fill=count_rescaled), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#252525", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="public transport: large cells: origins; small cells; destinations")+
  facet_grid(o_fy~o_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Greens", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )


o_od_global_active <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=active), # count=if_else(o_bor==d_bor,0,count)), 
          aes(fill=count), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="foot+bike: large cells: origins; small cells; destinations")+
  facet_grid(o_fy~o_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Blues", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )

o_od_local_active <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=active) %>%  #, count=if_else(o_bor==d_bor,0,count)) %>% 
            group_by(o_bor) %>% 
            mutate(count_rescaled=(count-min(count))/(max(count)-min(count))), 
          aes(fill=count_rescaled), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="foot+bike: large cells: origins; small cells; destinations")+
  facet_grid(o_fy~o_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Blues", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )



o_od_global_car <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=car), #count=if_else(o_bor==d_bor,0,count)), 
          aes(fill=count), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="Car: large cells: origins; small cells; destinations")+
  facet_grid(o_fy~o_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Reds", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )

o_od_local_car <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=car) %>%  #, count=if_else(o_bor==d_bor,0,count)) %>% 
          group_by(o_bor) %>% 
            mutate(count_rescaled=(count-min(count))/(max(count)-min(count))),
          aes(fill=count_rescaled), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="Car: large cells: origins; small cells; destinations")+
  facet_grid(o_fy~o_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Reds", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )

plot <- 
(d_od_global_pt | o_od_global_pt)/
(d_od_global_active | o_od_global_active)/
(d_od_global_car | o_od_global_car) +
  plot_annotation(
    title="OD maps of commutes into- and out of- London boroughs separately by travel mode: Global colour scaling is used",
    subtitle="--The left column focuses on destinations (D-OD map); the right on origins (O-OD map)",
    caption="2011 Census data accessed via `pct` package"
  )

ggsave(filename="./static/class/05-class_files/od-maps-mode.png", plot=plot,width=10, height=13, dpi=300)



plot <- 
  (d_od_local_pt | o_od_local_pt)/
  (d_od_local_active | o_od_local_active)/
  (d_od_local_car | o_od_local_car) +
  plot_annotation(
    title="OD maps of commutes into- and out of- London boroughs separately by travel mode: Local colour scaling is used",
    subtitle="--The left column focuses on destinations (D-OD map); the right on origins (O-OD map)",
    caption="2011 Census data accessed via `pct` package"
  )


ggsave(filename="./static/class/05-class_files/od-maps-mode-local.png", plot=plot,width=10, height=13, dpi=300)

ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=car, count=if_else(o_bor==d_bor,0,count)), aes(fill=count), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#252525", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(caption="global scaling by destination borough: same OD excluded")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Reds", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    legend.position="bottom",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    plot.caption = element_text(size = 7),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )


ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=active, count=if_else(o_bor==d_bor,0,count)), aes(fill=count), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#252525", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(caption="global scaling by destination borough: same OD excluded")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Blues", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    legend.position="bottom",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    plot.caption = element_text(size = 7),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )

d_od_global_local <- plot_data_temp %>%
  group_by(d_bor) %>% 
  mutate(
    count=public_transport, count=if_else(o_bor==d_bor,0,count), 
    count_rescaled=(count-min(count))/(max(count)-min(count))) %>% ungroup %>% 
  ggplot()+
  geom_sf(aes(fill=count_rescaled), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#252525", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(caption="local scaling by destination borough: same OD excluded")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Greens", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    legend.position="bottom",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    plot.caption = element_text(size = 7),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )


plot_data_temp %>%
  group_by(d_bor) %>% 
  mutate(
    count=car, count=if_else(o_bor==d_bor,0,count), 
    count_rescaled=(count-min(count))/(max(count)-min(count))) %>% ungroup %>% 
  ggplot()+
  geom_sf(aes(fill=count_rescaled), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#252525", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(caption="local scaling by destination borough: same OD excluded")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Reds", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    legend.position="bottom",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    plot.caption = element_text(size = 7),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )


plot_data_temp %>%
  group_by(d_bor) %>% 
  mutate(
    count=active, count=if_else(o_bor==d_bor,0,count), 
    count_rescaled=(count-min(count))/(max(count)-min(count))) %>% ungroup %>% 
  ggplot()+
  geom_sf(aes(fill=count_rescaled), colour="#616161", size=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#252525", alpha=0.9, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(caption="local scaling by destination borough: same OD excluded")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Blues", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    legend.position="bottom",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    plot.caption = element_text(size = 7),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )


########### HOMEWORK ################

# Temporary plot object of data joined to geom_sf geometries. 
# DO map so geometries join on origin.
plot_data_temp <- grid_real_sf %>% filter(type=="grid") %>% right_join(edges, by=c("authority"="o_bor")) %>% 
  mutate(o_bor=authority) %>% 
  rename(o_fx=x, o_fy=y) %>% 
  left_join(grid_real_sf %>% filter(type=="grid") %>% st_drop_geometry() %>% select(authority,x,y), by=c("d_bor"="authority")) %>%
  rename(d_fx=x, d_fy=y) %>% 
  # Identify borough in focus (edit this to switch between D-OD and O-DO matrix).
  mutate(bor_label=if_else(o_bor==d_bor,d_bor,""),
         bor_focus=if_else(o_bor==d_bor,1,0))

# width
width <- plot_data_temp %>% summarise(width=max(east)-min(east))  %>% pull(width)
# height
height <- plot_data_temp %>% summarise(height=max(north)-min(north)) %>% pull(height)

bbox_grid <- st_bbox(grid_real_sf %>% filter(type=="grid"))

d_od_global_pt <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=public_transport), 
          aes(fill=count), colour="#616161", size=0.15)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", alpha=1.0, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="public transport: large cells: destinations; small cells; origins",
    subtitle="global scaling")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Greens", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    plot.subtitle = element_text(size = 7),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )

d_od_local_pt <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            group_by(d_bor) %>% 
            mutate(count=public_transport,# count=if_else(o_bor==d_bor,0,count),
                   count_rescaled=(count-min(count))/(max(count)-min(count))), 
          aes(fill=count_rescaled), colour="#616161", size=0.15)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="public transport: large cells: destinations; small cells; origins",
    subtitle="local scaling")+
  facet_grid(d_fy~d_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Greens", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    plot.subtitle = element_text(size = 7),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )



# Temporary plot object of data joined to geom_sf geometries. DO map so geometries join on origin.
plot_data_temp <- grid_real_sf %>% filter(type=="grid") %>% right_join(edges, by=c("authority"="d_bor")) %>% 
  mutate(d_bor=authority) %>% 
  rename(d_fx=x, d_fy=y) %>% 
  left_join(grid_real_sf %>% filter(type=="grid") %>% st_drop_geometry() %>% select(authority,x,y), by=c("o_bor"="authority")) %>%
  rename(o_fx=x, o_fy=y) 

# Identify borough in focus (edit this to switch between D-OD and O-DO matrix).
plot_data_temp <- plot_data_temp %>% mutate(bor_label=if_else(o_bor==d_bor,o_bor,""),
                                            bor_focus=if_else(o_bor==d_bor,1,0))



o_od_global_pt <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=public_transport),#, count=if_else(o_bor==d_bor,0,count)), 
          aes(fill=count), colour="#616161", size=0.15)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="public transport: large cells: origins; small cells; destinations",
    subtitle="global scaling")+
  facet_grid(o_fy~o_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Greens", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    plot.subtitle = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )


o_od_local_pt <- ggplot()+
  geom_sf(data=plot_data_temp %>% 
            mutate(count=public_transport, count=if_else(o_bor==d_bor,0,count)) %>% 
            group_by(o_bor) %>% 
            mutate(count_rescaled=(count-min(count))/(max(count)-min(count))), 
          aes(fill=count_rescaled), colour="#616161", size=0.15)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="transparent",  colour="#373737", size=0.3)+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=east, y=north, label=str_sub(BOR,1,1)), 
            colour="#ffffff", size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="centre", vjust="middle")+
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=BOR), 
            colour="#252525", alpha=0.6, size=2, show.legend=FALSE, family="Roboto Condensed", 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  labs(
    title="public transport: large cells: origins; small cells; destinations",
  subtitle="local scaling")+
  facet_grid(o_fy~o_fx, shrink=FALSE)+
  scale_fill_distiller(palette="Greens", direction=1)+
  theme(
    panel.spacing=unit(0.1, "lines"),
    #legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    legend.key = element_rect(size=1),
    plot.title = element_text(size = 8),
    plot.subtitle = element_text(size = 7),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 0),
    legend.key.size = unit(.5,"line"),
    panel.background = element_rect(fill="#ffffff", colour="#ffffff")
  )

plot <- 
  d_od_global_pt + o_od_global_pt +
  d_od_local_pt + o_od_local_pt +
  plot_annotation(
    subtitle="OD maps of commutes into- and out of- London boroughs using public transport",
    caption="2011 Census data accessed via `pct` package"
  )+plot_layout(ncol=1)

ggsave(filename="./static/class/05-class_files/od-maps-mode-homework.png", plot=plot,width=5, height=17, dpi=300)



