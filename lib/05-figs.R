# Filename: 05-figs.R 
#
# Figures for Chapter 5 of vis4sds 
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
#
#-----------------------------------------


#-----------------------------------------
# 1. Packages and Data
#-----------------------------------------

# 1.1 Packages ---------------------------

install.packages('tidygraph')
install.packages('ggraph')
# devtools::install_github('thomasp85/tidygraph')
# devtools::install_github('thomasp85/ggraph')
library(tidygraph)
library(tidyverse)
library(ggraph)
library(ggforce)
library(here)
library(ggtext)
# install.packages("devtools")
devtools::install_github("rogerbeecham/gridmappr")
library(gridmappr)
devtools::install_github("rogerbeecham/odvis")
library(odvis)


# 1.2 Data ---------------------------

# Generate gridmap of London boroughs.
n_row <- 8
n_col <- 8
pts <- london_boroughs |>
  st_drop_geometry() |>
  select(area_name, x = easting, y = northing)
solution <- points_to_grid(pts, n_row, n_col, compactness = .6)
grid <- make_grid(london_boroughs, n_row, n_col)
grid <- grid |>
  inner_join(solution)

od_pairs <- read_csv(here("../", "data", "ch5", "london_ttw.csv"))
rivers_real <- st_read(here("../", "data", "ch5", "rivers_real.geojson"))

edges <- od_pairs |> 
  group_by(o_bor, d_bor)  |>  
  summarise(
    commutes = sum(count),
    is_prof = sum(count[is_prof]),
    prop_prof= is_prof/commutes
    ) |>  
  ungroup() 
nodes_d <- od_pairs |> 
  group_by(d_bor) |> 
  summarise(
    commutes = sum(count),
    is_prof = sum(count[is_prof]),
  ) |> 
  ungroup() |>  
  rename(la = d_bor) |> 
  mutate(type="jobs")
nodes_o <- od_pairs |> 
  group_by(o_bor) |> 
  summarise(
    commutes = sum(count),
    is_prof = sum(count[is_prof]),
  ) |> 
  ungroup() |>  
  rename(la = o_bor) |> 
  mutate(type="workers")

nodes  <- nodes_o |>  rbind(nodes_d) |> 
  left_join(grid, by=c("la"="area_name")) |> select(-is_prof) |>  
  pivot_wider(names_from="type", values_from="commutes")

edges <- edges |> 
  left_join(grid, by=c("o_bor"="area_name")) |> st_drop_geometry() |>  select(-geom) |> 
  rename(o_x=x, o_y=y, o_col=col, o_row=row) |> 
  left_join(grid, by=c("d_bor"="area_name")) |> st_drop_geometry() |>  select(-geom) |> 
  rename(d_x=x, d_y=y, d_col=col, d_row=row)


#-----------------------------------------
# 2. Concepts graphics
#-----------------------------------------

# 2.1 Workers/jobs  ----------------------
   
# bor_orders <- nodes |> mutate(jobs=prof_workers+prof_jobs)  |>  arrange(-jobs) |>  pull(la)
bor_orders <- nodes |>  arrange(-jobs) |>  pull(la)

plot <- nodes |>  
  mutate(la=factor(la, levels=bor_orders)) |> 
  ggplot() + 
  geom_col(aes(x=la, y=jobs), fill="#377eb8", alpha=.7) +
  geom_col(aes(x=la, y=workers), fill="#e41a1c", alpha=.7) +
  annotate("segment", x=10.5, xend=10.5, y =0, yend=500000, linewidth=.1) +
  annotate("text", x=1.8, y=400000, 
           label=str_wrap("job-rich boroughs", 10), family="Avenir Book", hjust=0, size=4.5) +
  annotate("text", x=11.8, y=400000, 
           label=str_wrap("worker-rich boroughs", 10), family="Avenir Book", hjust=0, size=4.5) +
  
  # scale_y_continuous(
  #   breaks=c(0:4*10000), 
  #   labels = scales::comma_format(scale = .001)) +
  
  labs(x="", y="",  subtitle="<span style = 'color: #377eb8;'>jobs</span> | <span style = 'color: #e41a1c;'>workers</span> ")+
  theme(
    axis.text.x = element_text(angle=-90, hjust=0), 
    axis.text.y = element_blank(), 
    plot.subtitle = element_markdown(size=15, family="Avenir Book"))

ggsave(filename=here("figs", "05", "flows-bor.png"), plot=plot,width=7, height=5, dpi=500)


# 2.1 Node-link  ----------------------

nodes <- nodes |> mutate(commutes=jobs+workers) 
graph <- tbl_graph(nodes=nodes, edges=edges)

wst <- nodes_plot_labels |> filter(la=="Westminster")
col <- nodes_plot_labels |> filter(la=="City of London")
lam <- nodes_plot_labels |> filter(la=="Lambeth")
wnd <- nodes_plot_labels |> filter(la=="Wandsworth")
eln <- nodes_plot_labels |> filter(la=="Ealing")
sth <- nodes_plot_labels |> filter(la=="Southwark")
twr <- nodes_plot_labels |> filter(la=="Tower Hamlets")
hns <- nodes_plot_labels |> filter(la=="Hounslow")
hil <- nodes_plot_labels |> filter(la=="Hilingdon")

geom_text(data=nodes_plot_labels, aes(x,y+.00001, label=abbreviate(la,3)), size=2) +
  
# Not specifying the layout - defaults to "auto"
node_link <- ggraph(graph, weights = commutes) + 
  geom_edge_link(aes(edge_width = commutes), colour="#003c8f", alpha=.3) +

  geom_node_point(aes(size = commutes), fill="#003c8f", colour="#003c8f", alpha=.5) +
  
  annotate("text", wst$x-.00025, wst$y+.000045, label=wst$la, size=3, hjust='left') +
  annotate(
    geom = "curve",
    xend = wst$x-.000095, x = wst$x-.000045,
    yend = wst$y+.00004, y = wst$y+.00002,
    curvature = -.5,
    angle = 135, size=.2
  ) + 
  
  annotate("text", lam$x-.0001, lam$y-.00005, label=lam$la, size=3, hjust='left') +
  annotate(
    geom = "curve",
    xend = lam$x-.00002, x = lam$x,
    yend = lam$y-.000045, y = lam$y-.00001,
    curvature = -.5,
    angle = 135, size=.2
  ) +
  annotate("text", wnd$x-.0003, wnd$y+.000035, label=wnd$la, size=3, hjust='left') +
  annotate(
    geom = "curve",
    xend = wnd$x-.00008, x = wnd$x,
    yend = wnd$y+.00003, y = wnd$y+.00001,
    curvature = -.5,
    angle = 135, size=.2
  ) +
  
  annotate("text", hns$x-.000075, hns$y, label=hns$la, size=3, hjust='right') +
  annotate(
    geom = "curve",
    xend = hns$x-.000015, x = hns$x-.00007,
    yend = hns$y, y = hns$y,
    curvature = -.5,
    angle = 135, size=.2
  ) +
  
  annotate("text", col$x+.00004, col$y+.00003, label=col$la, size=3, hjust='left') +
  annotate(
    geom = "curve",
    xend = col$x+.000035, x = col$x,
    y = col$y+.00001, yend = col$y+.000025,
    curvature = -.5,
    angle = 135, size=.2
  ) +
  
  scale_edge_width(range = c(0.1,3), guide="none")+
  #scale_edge_colour_distiller(palette="Blues", direction=1, guide="none") +
  # annotate("text", x=0, y=-.19, label="") +
  # annotate("text", x=0, y=.19, label="") +
  scale_size(labels = scales::comma_format(scale = .001), range=c(.5, 15), breaks=c(0:4*100000))+
  labs(subtitle = "layout : force-directed", size="# commutes\nthousands") +
  theme(
    legend.position = "right",
    legend.text=element_text(size=9),
    legend.title = element_text(size=9.5),
    axis.title.x = element_blank(), axis.title.y = element_blank(),
    plot.subtitle = element_text(size=13, colour="#000000", family = "Avenir Book") 
) 



node_plot_data <- ggplot_build(node_link)$data[[2]] 
nodes_plot_labels <- nodes |> mutate(commutes=jobs+workers)  |> st_drop_geometry() |> select(la, commutes) |> arrange(commutes)
nodes_plot_labels <-  nodes_plot_labels |> add_column(node_plot_data |> arrange(size) |>  select(x,y))


# Build data frame of asymmetric trajectories (OD pair with controls)
edges_trajectories <- edges |> mutate(od_pair=paste(o_bor,"-",d_bor)) |> 
  select(od_pair, o_bor, d_bor) |> 
  left_join(
    london_boroughs |> st_drop_geometry() |> select(area_name, o_x=easting, o_y=northing),
    by=c("o_bor"="area_name")
  ) |> 
  left_join(
    london_boroughs |> st_drop_geometry() |>select(area_name, d_x=easting, d_y=northing),
    by=c("d_bor"="area_name")
  ) |> 
  filter(o_bor!=d_bor) |> nest(data=c(od_pair, o_x, o_y, d_x, d_y)) |> 
  mutate(
    trajectory=map(data, ~get_trajectory(
      .x$o_x, .x$o_y, .x$d_x, .x$d_y, .x$od_pair)
    )) |> 
  select(trajectory) |> unnest(cols=trajectory)

  
# Build data frame of trajectories (OD pair with controls) : straight
edges_trajectories_line <- edges |> mutate(od_pair=paste(o_bor,"-",d_bor)) |> 
  select(od_pair, o_bor, d_bor) |> 
  left_join(
    london_boroughs |> st_drop_geometry() |> select(area_name, o_x=easting, o_y=northing),
    by=c("o_bor"="area_name")
    ) |> 
  left_join(
    london_boroughs |> st_drop_geometry() |>select(area_name, d_x=easting, d_y=northing),
    by=c("d_bor"="area_name")
  ) |> 
  
  filter(o_bor!=d_bor) |> nest(data=c(od_pair, o_x, o_y, d_x, d_y)) |> 
  
  mutate(
    trajectory=map(data, ~get_trajectory(
      .x$o_x, .x$o_y, .x$d_x, .x$d_y, .x$od_pair, 0)
    )) |> 
  select(trajectory) |> unnest(cols=trajectory)

  
trajs <- edges_trajectories_line |> 
  left_join(edges |> mutate(od_pair=paste0(o_bor," - ",d_bor)) |> 
              select(od_pair, count=commutes)) |>  mutate(f_od=((count/max(count))^0.9)) 

nodes_count <- nodes |> select(la, count=jobs)

wst <- london_boroughs |> filter(area_name=="Westminster")
wnd <- london_boroughs |> filter(area_name=="Wandsworth")
hns <- london_boroughs |> filter(area_name=="Hounslow")
lam <- london_boroughs |> filter(area_name=="Lambeth")
col <- london_boroughs |> filter(area_name=="City of London")
cmd <- london_boroughs |> filter(area_name=="Camden")

london_bbox <- st_bbox(london_boroughs)
london_width <- london_bbox$xmax-london_bbox$xmin
  
flowline <- ggplot()+
  geom_sf(data=london_boroughs,  fill="#eeeeee", colour="#bdbdbd", linewidth=0.1, alpha=.7)+
  coord_sf(crs=st_crs(london_boroughs), datum=NA)+
  geom_point(
    data=london_boroughs %>% inner_join(nodes_count, by=c("area_name"="la")),
    aes(x=easting, y=northing, size=count),colour="#003c8f", fill="#003c8f", pch=21, alpha=.3, stroke=.7) +
  geom_path(aes(x=x, y=y, group=od_pair, linewidth=f_od, size=f_od^.3), alpha=.5, data=trajs, colour="#003c8f")+
  # geom_text(data=london_boroughs,
  #           aes(x=easting, y=northing, label=abbreviate(area_name,3)), size=3.5, family="Avenir Next")+ 
  annotate("text", wst$easting+.29*london_width, wst$northing, label=wst$area_name, size=3, hjust='left') +
  annotate(
    geom = "curve",
    xend = wst$easting+.28*london_width, x = wst$easting,
    yend = wst$northing, y = wst$northing,
    curvature = -.5,
    angle = 135, size=.2
  ) + 
  
  annotate("text", wnd$easting-.16*london_width, wnd$northing-.1*london_width, label=wnd$area_name, size=3, hjust='right') +
  annotate(
    geom = "curve",
    xend = wnd$easting-.15*london_width, x = wnd$easting,
    y = wnd$northing, yend = wnd$northing-.1*london_width,
    curvature = -.5,
    angle = 135, size=.2
  ) +
  
  annotate("text", hns$easting-.06*london_width, hns$northing, label=hns$area_name, size=3, hjust='right') +
  annotate(
    geom = "curve",
    xend = hns$easting-.05*london_width, x = hns$easting,
    y = hns$northing, yend = hns$northing,
    curvature = -.5,
    angle = 135, size=.2
  ) +
  
  
  scale_linewidth_continuous(range=c(0.05,2))+
  # scale_colour_distiller(palette="Blues", direction=1, guide="none") +
  #scale_size(range=c(.1,8)) +
  scale_linewidth_continuous(range=c(0.02,2))+
  scale_size(labels = scales::comma_format(scale = .001), range=c(.1, 15))+
  guides(linewidth="none", colour="none", fill="none", size="none")+
  labs(subtitle = "layout : geospatial", size="# commutes\nthousands") +
  theme(
    plot.subtitle = element_text(size=13, colour="#000000", family = "Avenir Book"),
    axis.title=element_blank(), axis.title.x = element_blank(), 
    axis.title.y = element_blank(), plot.caption = element_text(size=12, colour="#000000")
    )
flowline

# Data for drawing legend.
dat <- get_trajectory(0,1,1,1,"demo") |>  
  mutate(
    row=row_number(),
    type=if_else(row==1,"origin", if_else(row==2,"mid", "dest"))
  )
# Plot legend
legend <- ggplot() +
  geom_point(data=dat %>% filter(row!=2), aes(x=x, y=y), size=1, colour="#737373", alpha=0.5)+
  ggforce::geom_bezier0(data=dat, aes(x=x, y=y, group=od_pair), colour="#737373", alpha=.8, linewidth=.5)+
  coord_equal()+
  geom_text(data=dat %>% filter(row!=2),
            aes(x=x, y=y-0.13, label=type), 
            colour="#252525", size=3, show.legend=FALSE, hjust="Middle", vjust="Top")+
  scale_x_continuous(limits=c(-0.14,1.4))+
  scale_y_continuous(limits=c(0.8,1.19)) +
  theme_void()
# Plot extents in geographic space.
bbox <- st_bbox(london_boroughs)
width <- unname(bbox$xmax)-unname(bbox$xmin) 
height <- unname(bbox$ymax)-unname(bbox$ymin) 
aspect <- width/height

trajs <- edges_trajectories |> 
  left_join(edges |> mutate(od_pair=paste0(o_bor," - ",d_bor)) |> select(od_pair, count=commutes)) |>  mutate(f_od=(count/max(count))^0.9)   

rivers_real_simp <- rmapshaper::ms_simplify(rivers_real, keep=.9)

bezierline <- ggplot()+
  geom_sf(data=london_boroughs,  fill="#eeeeee", colour="#bdbdbd", linewidth=0.1, alpha=.7)+
  geom_sf(data=rivers_real, colour="#bdbdbd") +
  coord_sf(crs=st_crs(london_boroughs), datum=NA)+
  geom_point(data=
               nodes |> select(la, jobs) |> st_drop_geometry() |> 
               left_join(london_boroughs |> st_drop_geometry(), by=c("la"="area_name")),   
             aes(x=easting, y=northing, size=jobs), shape=21, fill="#003c8f", colour="#003c8f", stroke=.7, alpha=.4) +
  geom_bezier0(aes(x=x, y=y, group=od_pair, linewidth=f_od), colour="#003c8f", alpha=.4, data=trajs)+
  
  # geom_text(data=london_boroughs,
  #           aes(x=easting, y=northing, label=abbreviate(area_name,3)), size=3.5, family="Avenir Next")+ 
  scale_linewidth_continuous(range=c(0.02,2))+
  #scale_colour_distiller(palette="Blues", direction=1, guide="none") +
  scale_size(range=c(.1,15), guide = "none") +
  guides(linewidth="none", colour="none")+
  labs(subtitle = "layout : geospatial") +
  
  annotate("text", wst$easting+.25*london_width, wst$northing-.05*london_width, label=wst$area_name, size=3, hjust='left') +
  annotate(
    geom = "curve",
    xend = wst$easting+.24*london_width, x = wst$easting,
    yend = wst$northing-.05*london_width, y = wst$northing,
    curvature = -.5,
    angle = 135, size=.2
  ) + 
  
  annotate("text", col$easting+.26*london_width, col$northing, label=col$area_name, size=3, hjust='left') +
  annotate(
    geom = "curve",
    xend = col$easting+.25*london_width, x = col$easting,
    yend = col$northing, y = col$northing,
    curvature = -.5,
    angle = 135, size=.2
  ) + 
  
  annotate("text", wnd$easting-.16*london_width, wnd$northing-.1*london_width, label=wnd$area_name, size=3, hjust='right') +
  annotate(
    geom = "curve",
    xend = wnd$easting-.15*london_width, x = wnd$easting,
    y = wnd$northing, yend = wnd$northing-.1*london_width,
    curvature = -.5,
    angle = 135, size=.2
  ) +
  
  annotate("text", hns$easting-.06*london_width, hns$northing, label=hns$area_name, size=3, hjust='right') +
  annotate(
    geom = "curve",
    xend = hns$easting-.05*london_width, x = hns$easting,
    y = hns$northing, yend = hns$northing,
    curvature = .5,
    angle = 45, size=.2
  ) +
  
  annotate("text", cmd$easting, cmd$northing+.08*london_width, label=cmd$area_name, size=3, hjust='right') +
  annotate(
    geom = "curve",
    xend = cmd$easting, x = cmd$easting,
    yend = cmd$northing, y = cmd$northing+.07*london_width,
    curvature = -.5,
    angle = 135, size=.2
  ) +
  
  
  
  annotation_custom(
    grob=ggplotGrob(legend),
    xmax=unname(bbox$xmax + +0.05*width),
    xmin=unname(bbox$xmax-0.23*width),
    ymax=unname(bbox$ymax)+.1*height,
    ymin=unname(bbox$ymax)-0.25*height
  ) +
  theme(
    plot.subtitle = element_text(size=13, colour="#000000", family = "Avenir Book"),
    axis.title=element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
    plot.caption = element_text(size=12, colour="#000000")
    )

bezierline

plot <- node_link + bezierline + plot_layout(widths=c(.55,1))

ggsave(filename=here("figs", "05", "flowlines_test.png"), plot=plot,width=10, height=5, dpi=300)
# ggsave(filename=here("figs", "05", "flowlines.png"), plot=plot,width=7, height=8, dpi=300)
# ggsave(filename=here("figs", "05", "flowlines.png"), plot=plot,width=10, height=3.2, dpi=300)
# ggsave(filename="./static/class/05-class_files/geogs.png", plot=plot,width=9, height=4.35, dpi=300)

bor_orders <- nodes |>  arrange(-jobs) |>  pull(la)

# 2.2 Matrix  ----------------------
matrix_1 <- edges |> 
  mutate(o_bor=factor(o_bor, levels=bor_orders),
         d_bor=factor(d_bor, levels=bor_orders)) |>  
  ggplot(aes(y=fct_rev(o_bor), x=d_bor, fill=commutes)) +
  #geom_tile(colour="#707070", size=.2)+
  geom_tile(colour="#ffffff", size=.2)+
  scale_x_discrete(position = "top", labels=map(bor_orders, ~abbreviate(.x, 3))) +
  scale_fill_distiller(palette="Blues", direction=1, guide="none") +
  labs(x="destination borough", y="origin borough", caption="global scaling", fill="count") +
  theme(
    axis.text.x = element_text(angle=90, hjust=0, size=8),
    axis.text.y = element_text(size=8, hjust=1),
    #axis.title = element_text(size=9), axis.title.y = element_text(size=9, angle=90), 
    axis.title.x=element_blank(), axis.title.y=element_blank(),
    axis.line = element_blank(),
    plot.subtitle = element_text(size=13), plot.caption = element_text(size=11, colour="#000000"))

matrix_2 <- edges |> 
  mutate(o_bor=factor(o_bor, levels=bor_orders),
         d_bor=factor(d_bor, levels=bor_orders))|>  
  group_by(d_bor) |> 
  mutate(
    commutes_rescaled=(commutes-min(commutes))/(max(commutes)-min(commutes))
  ) |> 
  ggplot(aes(y=fct_rev(o_bor), x=d_bor, fill=commutes_rescaled)) +
  geom_tile(colour="#707070", size=.2)+
  geom_tile(colour="#ffffff", size=.2)+
  geom_tile(data=. %>% filter(o_bor==d_bor),
            aes(y=fct_rev(o_bor), x=d_bor), fill="#bdbdbd", colour="#ffffff", size=.2) +
  geom_text(data=. %>% filter(o_bor==d_bor), 
            aes(label=stringr::str_extract(o_bor, "^.{1}")), 
            colour="#ffffff", size=2, hjust="centre", family="Avenir Black")+
  scale_x_discrete(position = "top", labels=map(bor_orders, ~abbreviate(.x, 3))) +
  scale_fill_distiller(palette="Blues", direction=1, guide="none") +
  labs(x="destination borough", y="origin borough", caption="local scaling", fill="count") +
  # theme(
  #   #axis.text.x = element_text(angle=90, hjust=0, size=10),
  #   #axis.text.y = element_text(size=10),
  #   #axis.title = element_text(size=11), 
  #   axis.text = element_blank(), axis.line = element_blank(),
  #   axis.title.x = element_blank(),
  #   plot.subtitle = element_text(size=13),
  #   axis.title.y = element_blank(), plot.caption = element_text(size=12, colour="#000000")
  #   )
  theme(
    # axis.text.x = element_text(angle=90, hjust=0, size=8),
    # axis.text.y = element_text(size=8, hjust=1),
    # axis.title = element_text(size=9), 
    # axis.title.y = element_text(size=9, angle=90), 
    axis.line = element_blank(),
    plot.subtitle = element_text(size=13), plot.caption = element_text(size=12, colour="#000000"),
    axis.title.x = element_blank(), axis.title.y=element_blank(),
    axis.text=element_blank()
    )


plot <- matrix_1 + matrix_2

ggsave(filename=here("figs", "05", "matrices.png"), plot=plot,width=8.8, height=4.5, dpi=800)


img <- image_read(here("figs", "05", "matrices.png")) |> 
  image_fill('none') |> 
  as.raster()

aspect <- 3340/6756

plot <- ggplot() +
  annotation_raster(img, 0, 1, 0, 1) +
  annotate("text", 
           label="destination boroughs",
           x=.01, y=1.02, size=2.5, hjust="left", family="Avenir Book") +
  annotate(
    geom = "curve",
    x = .165, xend = .25,
    y = 1.015, yend = .98,
    curvature = -.3,
    angle = 135, size=.1
  ) +
  
  annotate("text", 
           label=str_wrap("central job-rich boroughs draw workers from across London", 30),
           x=.7, y=.96, size=2.5, hjust="left", family="Avenir Book") +
  annotate(
    geom = "curve",
    x = .62, xend = .69,
    yend = .96, y = .9,
    curvature = -.3,
    angle = 135, size=.1
  ) +
  
  annotate("text", 
           label=str_wrap("outer London boroughs have more self-contained labour markets",30),
           x=.38, y=-.08, size=2.5, hjust="left", family="Avenir Book") +
  annotate(
    geom = "curve",
    x = .565, xend = .565,
    y = -.02, yend = .4,
    curvature = .3,
    angle = 45, size=.1
  ) +
  
  scale_x_continuous(limits=c(0, 1)) +
  scale_y_continuous(limits=c(-.1, 1.1))+
  theme(axis.text = element_blank(), 
        axis.title.x=element_blank(),
        axis.title.y = element_blank(), axis.line = element_blank())
  
ggsave(filename=here("figs", "05", "matrices.png"), plot=plot,width=7.3, height=4.5, dpi=600)


# 2.3 Spatial matrix  ----------------------

# Build data frame of asymmetric trajectories (OD pair with controls)
edges_trajectories <- edges |> mutate(od_pair=paste0(o_bor," - ", d_bor)) |> 
  filter(o_bor!=d_bor) |> nest(data=c(od_pair, o_x, o_y, d_x, d_y)) |> 
  mutate(
    trajectory=map(data, ~get_trajectory(
      .x$o_x, .x$o_y, .x$d_x, .x$d_y, .x$od_pair)
    )) |> 
  select(trajectory) |> unnest(cols=trajectory)

# Join with temp_data for d_fX and d_fY for faceting.
edges_trajectories <- edges_trajectories |>  left_join(edges |> mutate(od_pair=paste0(o_bor," - ", d_bor)) |>   select(od_pair, o_x, o_y, d_x, d_x, o_col, o_row, d_col, d_row, o_bor, d_bor, commutes), by=c("od_pair"="od_pair"))
# Temporary plot object of data joined to geom_sf geometries. DO map so geometries join on origin.
plot_data_temp <- grid |> select(area_name, geom) |>  right_join(edges, by=c("area_name"="o_bor")) |> 
  mutate(o_bor=area_name) 
  
# Identify borough in focus (edit this to switch between D-OD and O-DO matrix).
plot_data_temp <- plot_data_temp |>  
  mutate(bor_label=if_else(o_bor==d_bor,d_bor,""),
         bor_focus=if_else(o_bor==d_bor,1,0))

# Rescale f_od for plotting.
od_trajectories_temp <- edges_trajectories %>% mutate(f_od=((commutes/max(commutes))^0.5)) 
# Plot grid-within-grid (for demonstration).
# width
width <- plot_data_temp %>% summarise(width=max(o_x)-min(o_x))  %>% pull(width)
# height
height <- plot_data_temp %>% summarise(height=max(o_y)-min(o_y)) %>% pull(height)

# Grid-bezier.
wst_grid_bezier_do <- ggplot()+
  geom_sf(data=plot_data_temp %>% filter(d_bor=="Westminster"),  fill="#f0f0f0", colour="#ffffff", linewidth=0.5)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1, d_bor=="Westminster"), fill="#d9d9d9",  colour="#ffffff", linewidth=0.9)+
  geom_sf(data=river_grid, linewidth=.35, colour="#525252") +
  geom_bezier0(data=od_trajectories_temp  %>% filter(d_bor=="Westminster"), aes(x=x, y=y, group=od_pair, alpha=f_od, colour=f_od, size=f_od^2))+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  scale_size_continuous(range=c(.1,.9))+
  scale_colour_distiller(palette="Blues", direction=1.4, limits=c(0,1))+
  guides(colour=FALSE, alpha=FALSE, size=FALSE)+
  #labs(subtitle ="Commutes into Westminster")+
  theme(
    legend.position="bottom",
    axis.title.x =element_blank(),axis.title.y =element_blank(),
    strip.text.x=element_blank(),strip.text.y = element_blank(),
    plot.subtitle = element_text(size=12), plot.caption = element_text(size=12, colour="#000000")
    )

# Plot grid-fill.
wst_grid_fill_do <- ggplot()+
  geom_sf(data=plot_data_temp %>% filter(d_bor=="Westminster", o_bor!=d_bor), aes(fill=commutes), colour="#ffffff", linewidth = .5)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1, d_bor=="Westminster"), fill="#bdbdbd",  colour="#ffffff", linewidth=0.9)+
  # geom_text(data=plot_data_temp %>% filter(bor_focus==1, d_bor=="Westminster"), 
  #           aes(x=o_x, y=o_y, label=abbreviate(d_bor,3)), 
  #           colour="#ffffff", show.legend=FALSE, size=2.5, hjust="centre", family="Avenir Black")+
  geom_text(data=plot_data_temp %>% filter(d_bor=="Westminster"),
            aes(x=o_x, y=o_y, label=abbreviate(o_bor,3)),
            colour="#ffffff", show.legend=FALSE, size=2.5, hjust="centre", family="Avenir Black")+
  #geom_sf(data=river_grid, linewidth=.65, colour="#252525") +
  geom_sf(data=river_grid, linewidth=.35, colour="#525252") +
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  scale_fill_distiller(palette="Blues", direction=1)+
  guides(fill=FALSE)+
  labs(caption="Commutes into Westminster")+
  theme(
    legend.position="bottom",
    axis.title.x =element_blank(),axis.title.y =element_blank(),
    strip.text.x=element_blank(),strip.text.y = element_blank(), plot.subtitle = element_text(size=12), plot.caption = element_text(size=12, colour="#000000")
  )

# Remove geometries and rejoin for 0-DO map so geometries join on destination.
st_geometry(plot_data_temp) <- NULL
plot_data_temp <-  grid |>  right_join(edges, by=c("area_name"="d_bor")) |> 
  rename(d_bor=area_name)
  
# Edit borough in focus (switch between D-OD and O-DO matrix).
plot_data_temp <- plot_data_temp |> 
  mutate(bor_label=if_else(o_bor==d_bor,o_bor,""),
         bor_focus=if_else(o_bor==d_bor,1,0)
         )


geom_sf(data=plot_data_temp %>% filter(d_bor=="Westminster", o_bor!=d_bor), aes(fill=commutes), colour="#ffffff", linewidth = .5)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1, d_bor=="Westminster"), fill="#bdbdbd",  colour="#ffffff", linewidth=0.9)+
# Plot grid-bezier.
hck_grid_bezier_od <- ggplot()+
  geom_sf(data=plot_data_temp |>  filter(o_bor=="Hackney"),  fill="#f0f0f0", colour="#ffffff", linewidth=0.5)+
  geom_sf(data=plot_data_temp  |>  filter(bor_focus==1, o_bor=="Hackney"), fill="#d9d9d9",  colour="#ffffff", linewidth=0.9)+
  geom_bezier0(data=od_trajectories_temp  %>% filter(o_bor=="Hackney"), aes(x=x, y=y, group=od_pair, alpha=f_od, colour=f_od, size=f_od))+
  geom_sf(data=river_grid, linewidth=.35, colour="#525252") +
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  scale_size_continuous(range=c(.1,.9))+
  scale_colour_distiller(palette="Blues", direction=1, limits=c(0,1))+
  guides(colour=FALSE, alpha=FALSE, size=FALSE)+
  #labs(subtitle="Commutes out of Hackney")+
  theme(
    legend.position="bottom",
    axis.title.x =element_blank(),axis.title.y =element_blank(),
    strip.text.x=element_blank(),strip.text.y = element_blank(), 
    plot.subtitle=element_text(size=12), plot.caption = element_text(size=12, colour="#000000")
  )

# Plot grid-fill.
hck_grid_fill_od <- ggplot()+
  geom_sf(data=plot_data_temp |>  filter(o_bor=="Hackney", o_bor!=d_bor), aes(fill=commutes), colour="#ffffff", linewidth=0.5)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1, o_bor=="Hackney"), fill="#d9d9d9",  colour="#ffffff", linewidth=0.9)+
  # geom_text(data=plot_data_temp %>% filter(bor_focus==1, o_bor=="Hackney"), 
  #           aes(x=o_x, y=o_y, label=abbreviate(o_bor, 3)), 
  #           colour="#ffffff", show.legend=FALSE, size=2.5, hjust="centre", family="Avenir Black")+
  
  geom_text(data=plot_data_temp %>% filter(d_bor=="Hackney"), 
            aes(x=o_x, y=o_y, label=abbreviate(o_bor, 3)), 
            colour="#ffffff", show.legend=FALSE, size=2.5, hjust="centre", family="Avenir Black")+
  geom_sf(data=river_grid, linewidth=.35, colour="#525252") +
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  scale_fill_distiller(palette="Blues", direction=1)+
  guides(fill=FALSE)+
  labs(caption = "Commutes out of Hackney")+
  theme(
    legend.position="bottom",
    axis.title.x =element_blank(),axis.title.y =element_blank(), plot.caption = element_text(size=12, colour="#000000"),
    strip.text.x=element_blank(),strip.text.y = element_blank(), plot.subtitle=element_text(size=12)
  )

matrix <- plot_data_temp |>  st_drop_geometry() |> 
  mutate(o_bor=factor(o_bor, levels=bor_orders),
         d_bor=factor(d_bor, levels=bor_orders))|>  
  group_by(d_bor) |> 
  mutate(
    commutes=if_else(o_bor==d_bor,0,commutes),
    commutes_rescaled_dest=(commutes-min(commutes))/(max(commutes)-min(commutes)),
  ) |> ungroup() |> 
  group_by(o_bor) |> 
  mutate(
    commutes=if_else(o_bor==d_bor,0,commutes),
    commutes_rescaled_origin=(commutes-min(commutes))/(max(commutes)-min(commutes)),
  ) |> ungroup() |> 
  mutate(o_bor=fct_rev(o_bor)) |> 
  ggplot(aes(y=o_bor, x=d_bor)) +
  geom_tile(aes(fill=commutes_rescaled_dest), colour="#ffffff", size=.2, alpha=.2) +
  # geom_tile(data=. %>% filter(d_bor == "Westminster" | o_bor=="Hackney"), colour="#ffffff", size=.2)+
  # geom_tile(data=. %>% filter(d_bor == "Westminster" | o_bor=="Hackney"), colour="#ffffff", size=.2)+
  
  geom_tile(data=. %>% filter(d_bor == "Westminster"), aes(fill=commutes_rescaled_dest), colour="#ffffff", size=.2)+
  geom_tile(data=. %>% filter(o_bor=="Hackney"), aes(fill=commutes_rescaled_origin), colour="#ffffff", size=.2)+
  
  # geom_text(data=. %>% filter(o_bor==d_bor, o_bor %in% c("Hackney", "Westminster")), 
  #           aes(label=stringr::str_extract(o_bor, "^.{1}")), 
  #           colour="#ffffff", alpha=1, show.legend=FALSE, size=2, hjust="centre", family="Avenir Black")+
  
  geom_text(data=. %>% filter(d_bor == "Westminster", !(o_bor=="Hackney" & d_bor=="Westminster") ), 
            aes(label=stringr::str_extract(o_bor, "^.{1}")), 
            colour="#ffffff", alpha=1, show.legend=FALSE, size=2, hjust="centre", family="Avenir Black")+
  
  geom_text(data=. %>% filter(o_bor == "Hackney", !(o_bor=="Hackney" & d_bor=="Westminster") ), 
aes(label=stringr::str_extract(d_bor, "^.{1}")), 
colour="#ffffff", alpha=1, show.legend=FALSE, size=2, hjust="centre", family="Avenir Black")+
  
  geom_text(data=. %>% filter(o_bor=="Hackney" & d_bor=="Westminster"), label="W|H", 
            colour="#ffffff", alpha=1, show.legend=FALSE, size=1.5, hjust="centre", family="Avenir Black")+
  
  geom_richtext(
    data=. %>% slice(1), 
    aes(x=6.5, y=24, label="large numbers into Westminster<br>from Wandsworth and Lambeth"), 
    hjust=0, size=3.5, fill="transparent", label.colour = NA, colour="#252525", family="Avenir Book"
    ) +
  geom_bezier0(data=tibble(x=c(2,3,6.5), y=c(22,23,24), commutes=1),
               aes(x=x, y=y), colour="#252525", size=.3) +
  geom_bezier0(data=tibble(x=c(2,3,6.5), y=c(27,27,24), commutes=1),
               aes(x=x, y=y), colour="#252525", size=.3) +
  
  geom_richtext(
    data=. %>% slice(1), 
    aes(x=11.5, y=12, label="large numbers out of Hackney<br>to Islington and Westminster"), 
    hjust=0, size=3.5, fill="transparent", label.colour = NA, colour="#252525", family="Avenir Book"
  ) +
  geom_bezier0(data=tibble(x=c(6,7,11.5), y=c(15,13,12), commutes=1),
               aes(x=x, y=y), colour="#252525", size=.3) +
  geom_bezier0(data=tibble(x=c(1,2,11.5), y=c(15,13,12), commutes=1),
               aes(x=x, y=y), colour="#252525", size=.3) +
  scale_x_discrete(position = "top", labels=map(bor_orders, ~abbreviate(.x, 3))) +
  scale_fill_distiller(palette="Blues", direction=1) +
  guides(fill=FALSE)+
  labs(x="destination borough", y="origin borough", fill="count (local scaling)")+
  theme(
    #axis.text.x = element_text(angle=90, hjust=0), 
    plot.subtitle = element_text(size=13), axis.line = element_blank(),
    axis.title.y = element_blank(), axis.text = element_blank(), axis.title.x = element_blank()
  )

plot <- 
  #(matrix_1 + matrix_2) /
  (matrix + (wst_grid_bezier_do+ wst_grid_fill_do) / (hck_grid_bezier_od + hck_grid_fill_od)) +
  plot_layout(widths=c(.8,1))

ggsave(filename=here("figs", "05", "reordered_matrix.png"), plot=plot,width=10, height=5.5, dpi=300)


# 2.4 OD map  ----------------------

# Temporary plot object of data joined to geom_sf geometries. DO map so geometries join on origin.
plot_data_temp <- grid |>  select(area_name, geom) |> right_join(edges, by=c("area_name"="o_bor")) |> 
  rename(o_bor=area_name) 

# Identify borough in focus (edit this to switch between D-OD and O-DO matrix).
plot_data_temp <- plot_data_temp %>% mutate(bor_label=if_else(o_bor==d_bor,d_bor,""),
                                            bor_focus=if_else(o_bor==d_bor,1,0))

width <- plot_data_temp %>% summarise(width=max(o_x)-min(o_x))  %>% pull(width)
# height
height <- plot_data_temp %>% summarise(height=max(o_y)-min(o_y)) %>% pull(height)

#373737
#ffffff
bbox_grid <- st_bbox(grid)


rivers_facet <- plot_data_temp |> st_drop_geometry() |> 
  filter(o_bor==d_bor) |> 
  mutate(od_pair=paste0(o_bor," - ", d_bor)) |> 
  select(od_pair, d_row, d_col) |> 
  add_column(river_grid |> as_tibble()) |> st_as_sf()

spatial_dependency <- "Spatial dependency across London."
containment <- "Some containment north/south of river."
job_rich <- "Less spatial dependency for job-rich boroughs."

d_od_local <- ggplot() +
  geom_tile(
    data =  plot_data_temp |>  filter(o_bor==d_bor),
    aes(x = bbox_grid$xmin + .6 * width, y = bbox_grid$ymin + .6 * height),
    height = height * 1.2, width = width * 1.2, fill="#d9d9d9", alpha=.4
    ) +
  
  geom_sf(data=plot_data_temp %>% filter(o_bor!=d_bor) %>% group_by(d_bor) %>% 
            mutate(commutes_rescaled=(commutes-min(commutes))/(max(commutes)-min(commutes))), 
          aes(fill=commutes_rescaled), colour="#ffffff", linewidth=0.15, alpha=0.9)+
  geom_sf(data=plot_data_temp  %>% filter(bor_focus==1), fill="#bdbdbd",  colour="#ffffff", linewidth=0.3)+
  geom_sf(data=rivers_facet, colour="#525252", alpha=.6, linewidth=0.3) +
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=o_x, y=o_y, label=str_sub(o_bor,1,1)), 
            colour="#ffffff", alpha=0.9, size=1.6, show.legend=FALSE,
            hjust="centre", vjust="middle", family="Avenir Black")+

  
  # geom_text(
  #   data=tibble(d_row=5, d_col=7), 
  #   aes(x=bbox_grid$xmin, bbox_grid$ymax-.52*height),
  #   label=str_wrap(job_rich,15), 
  #   vjust="middle", hjust="left", size=2.2) +
  # 
  # geom_text(
  #   data=tibble(d_row=7, d_col=2), 
  #   aes(x=bbox_grid$xmax, bbox_grid$ymax-.52*height),
  #   label=str_wrap(spatial_dependency,10), 
  #   vjust="middle", hjust="right", size=2.2) +
  # 
  # geom_text(
  #   data=tibble(d_row=2, d_col=2), 
  #   aes(x=bbox_grid$xmax, bbox_grid$ymax-.52*height),
  #   label=str_wrap(containment,10), 
  #   vjust="middle", hjust="right", size=2.2) +
  
  geom_text(data=plot_data_temp %>% filter(bor_focus==1), 
            aes(x=bbox_grid$xmax, y=bbox_grid$ymin, label=abbreviate(o_bor,3)), 
            colour="#252525", alpha=0.9, size=2.5, show.legend=FALSE, 
            hjust="right", vjust="bottom")+
  coord_sf(crs=st_crs(plot_data_temp), datum=NA)+
  guides(fill=FALSE)+
  facet_grid(-d_row~d_col, shrink=FALSE)+
  scale_fill_distiller(palette="Blues", direction=1)+
  theme(
    panel.spacing=unit(-0.1, "lines"),
    legend.position="right",
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    plot.caption = element_text(size = 7),
  #  panel.background = element_rect(fill="#ffffff", colour="#ffffff"),
   # plot.background = element_rect(fill="#f0f0f0f0"),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
    legend.key = element_rect(size=1.4),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 5),
    legend.key.size = unit(.8,"line"),
  )



# Find smwg cell size.
cell_height <- grid |>  st_drop_geometry() |>  
  filter(case_when(col==5 & row==6 ~ TRUE,
                   col==6 & row==6 ~ TRUE,
                   TRUE ~ FALSE)) |> 
  transmute(diff=x-lag(x,1)) |>  filter(!is.na(diff), diff>0) %>% pull()

bbox_real <- st_bbox(london_boroughs)

real <- london_boroughs |>  
  ggplot()+
  geom_sf(colour="#ffffff", fill="#d9d9d9", linewidth=.3)+
  geom_sf(data=rivers_real, colour="#525252", linewidth=.45) +
  geom_richtext(
    data=. %>% slice(1), 
    aes(x=bbox_real$xmin-2000, y=bbox_real$ymax-1000, label="London borough<br>outlines"), 
    hjust=0, size=2.5, fill="transparent", label.colour = NA, colour="#252525", family="Avenir Book"
  ) +
  coord_sf(crs=27700, datum=NA)+
  geom_text(aes(x=easting, y=northing, label=abbreviate(area_name,3)), size=2.2, colour="#252525", show.legend=FALSE, family="Avenir Book")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())


grid_positions <- make_grid(london_boroughs, n_row, n_col)

t <- grid |> st_drop_geometry() |> select(c(-x,-y)) |> 
  nest(data=c(col, row)) |> 
  mutate(geometry=map(data, ~add_column(grid_positions))) |> select(-data) |> 
  unnest(cols=geometry) |> 
  st_as_sf()


width <- plot_data_temp %>% summarise(width=max(o_x)-min(o_x))  %>% pull(width)
# height
height <- plot_data_temp %>% summarise(height=max(o_y)-min(o_y)) %>% pull(height)

generate_positions <- function(geom, dim_x, dim_y) {
  start_x <- st_bbox(geom)$xmin
  start_y <-st_bbox(geom)$ymin
  w <- (st_bbox(geom)$xmax-start_x) / dim_x
  h <- (st_bbox(geom)$ymax-start_y) / dim_y
  
  return(map2_df( 
    rep(1:n_row, each = n_col),  rep(1:n_col, times = n_row), 
    ~tibble(col= .y, x = ((.y-1)+.5)*w + start_x, row=.x, y = ((.x-1)+.5)*h + start_y )
    )) 
   #|>  st_as_sf(coords = c("x", "y"), crs = "epsg:27700")
  }
  

grid_coords <- grid |> select(area_name, geom) |> 
  nest(data=geom) |> 
  mutate(coords=map(data, ~generate_positions(.x, 8, 8))) |> select(-data) |> 
  unnest(cols=coords)


grid_coords <- grid |> select(area_name, geom) |> 
  nest(data=geom) |> 
  mutate(coords=map(data, ~generate_positions(.x, 8, 8))) |> select(-data) |> 
  unnest(cols=coords)


gridmap <- grid |> 
  ggplot()+
  geom_sf(fill="#d9d9d9", colour="#ffffff", linewidth=0.3, alpha=.5)+
  geom_tile(
    data=grid_coords %>% inner_join(grid |> st_drop_geometry() |> select(row, col), by=c("col"="col", "row"="row")), 
    aes(x,y), colour="#525252", fill="#d9d9d9", linewidth=0
    ) +
  geom_sf(data=river_grid, colour="#525252", linewidth=0.35) +
  geom_richtext(
    data=. %>% slice(1), 
    aes(x=bbox_grid$xmin-2000, y=bbox_grid$ymax-1000, label="map-within-map<br>layout"), 
    hjust=0, size=2.5, fill="transparent", label.colour = NA, colour="#252525", family="Avenir Book"
  ) +
  coord_sf(crs=27700, datum=NA)+
  geom_text(aes(x=x, y=y, label=abbreviate(area_name, 3)), size=2.2, colour="#252525", show.legend=FALSE, family="Avenir Book")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())





plot <- (real + gridmap) / d_od_local  + plot_layout(heights=c(.5,1))

ggsave(filename=here("figs", "05", "map_map.png"), plot=plot,width=6, height=2.4, dpi=600)

#ggsave(filename=here("figs", "05", "od_map.png"), plot=d_od_local,width=6, height=6.7, dpi=600)
ggsave(filename=here("figs", "05", "od_map.png"), plot=d_od_local,width=6, height=4.6, dpi=600)


#################################################
#       D A T A.  A N A L Y S I S
#################################################
nodes_d <- od_pairs |> 
  group_by(d_bor, occ_type) |> 
  summarise(
    count = sum(count),
    is_prof = first(is_prof)
  ) |> 
  ungroup() |>  
  rename(la = d_bor) |> 
  mutate(type="jobs")

nodes_o <- od_pairs |> 
  group_by(o_bor, occ_type) |> 
  summarise(
    count = sum(count),
    is_prof = first(is_prof)
  ) |> 
  ungroup() |>  
  rename(la = o_bor) |> 
  mutate(type="workers")

nodes  <- bind_rows(nodes_o, nodes_d)


grid <- make_grid(london_boroughs, n_row, n_col) |> 
  inner_join(solution)

lon_geogs <- bind_rows(
  london_boroughs |> mutate(type = "real") |> 
    select(area_name, x = easting, y = northing, type),
  grid |>  mutate(type = "grid") |> 
    select(area_name, x, y, type, geometry = geom)
)

library(odvis)
trajectories <- lon_geogs |>
  st_drop_geometry() |>
  pivot_wider(names_from = type, values_from = c(x, y)) |>
  mutate(id = row_number()) |>
  nest(data = c(area_name, x_real, y_real, x_grid, y_grid)) |>
  mutate(
    trajectory = 
      map(data, 
          ~get_trajectory(
            .x$x_real, .x$y_real, .x$x_grid, .x$y_grid, .x$area_name, curve_position = 5
            )
          )
  ) |>
  select(trajectory) |>
  unnest(cols = trajectory)

solution1 <- 
  
ggplot() +
  geom_sf(
    data = lon_geogs |> mutate(type = factor(type, levels = c("real", "grid"))), 
    aes(fill = type, colour = type), linewidth = .2
    ) +
  ggforce::geom_bezier(
    data = trajectories, 
    aes(x = x, y = y, group = od_pair), 
    colour = "#08306b", linewidth = .3
    ) +
  scale_fill_manual(values = c("#f0f0f0", "transparent"), guide = "none") +
  scale_colour_manual(values = c("#FFFFFF", "#525252"), guide = "none") +
    
    theme(axis.text = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.line = element_blank())


lon_geogs <- bind_rows(
  london_boroughs |> mutate(type = "real") |> 
    select(area_name, x = easting, y = northing, type),
  grid |>  mutate(type = "grid") |> 
    select(area_name, x, y, type, geometry = geom)
)

n_row <- 7
n_col <- 8
spacers <- list(
  c(1, 3), c(1, 5), c(1, 6),
  c(2, 2), c(2, 7),
  c(3, 1),
  c(6, 1), c(6, 2), c(6, 7), c(6, 8),
  c(7, 2), c(7, 3), c(7, 4), c(7, 6), c(7, 7)
)
pts <- london_boroughs |>
  st_drop_geometry() |>
  select(area_name, x = easting, y = northing)
solution <- points_to_grid(pts, n_row, n_col, 1, spacers)

trajectories <- lon_geogs |>
  st_drop_geometry() |>
  pivot_wider(names_from = type, values_from = c(x, y)) |>
  mutate(id = row_number()) |>
  nest(data = c(area_name, x_real, y_real, x_grid, y_grid)) |>
  mutate(
    trajectory = 
      map(data, 
          ~get_trajectory(
            .x$x_real, .x$y_real, .x$x_grid, .x$y_grid, .x$area_name, curve_position = 5
          )
      )
  ) |>
  select(trajectory) |>
  unnest(cols = trajectory)


solution2 <-
  ggplot() +
  geom_sf(
    data = lon_geogs |> mutate(type = factor(type, levels = c("real", "grid"))), 
    aes(fill = type, colour = type), linewidth = .2
  ) +
  ggforce::geom_bezier(
    data = trajectories, 
    aes(x = x, y = y, group = od_pair), 
    colour = "#08306b", linewidth = .3
  ) +
  scale_fill_manual(values = c("#f0f0f0", "transparent"), guide = "none") +
  scale_colour_manual(values = c("#FFFFFF", "#525252"), guide = "none") +
  
  theme(axis.text = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.line = element_blank())


plot <- solution1 + solution2

ggsave(filename=here("figs", "05", "displacements.png"), plot=plot,width=8, height=3.4, dpi=300)



nodes  <- bind_rows(nodes_o, nodes_d) |> 
  mutate(prof=is_prof, non_prof=commutes-is_prof) |> 
  select(-c(commutes, is_prof)) |> 
  pivot_longer(cols=c(prof, non_prof), names_to="is_prof", values_to="count") |> 
  mutate(is_prof = is_prof == "prof")

 
t <- nodes |> 
  mutate(
    is_prof = factor(if_else(is_prof, "professional", "non-professional"), levels = c("professional", "non-professional")),
    type = factor(type, levels = c("workers", "jobs")),
  ) |> 
  select(type, is_prof) |> 
  unique() 

rivers_facet <- t |> 
  add_column(river_grid |> as_tibble()) |> st_as_sf()  


nodes_summary <- grid |>
  inner_join(
    nodes |> 
      group_by(la, is_prof, type) |> 
      summarise(count=sum(count)), 
    by = c("area_name" = "la")
    ) |> 
  mutate(
    is_prof = factor(if_else(is_prof, "professional", "non-professional"), levels = c("professional", "non-professional")),
    type = factor(type, levels = c("workers", "jobs")),
  ) |> 
  ggplot() +
  geom_sf(aes(x = x, y = y), fill = "#ffffff") +
  geom_sf(data=rivers_facet, colour="#252525", alpha=.9, linewidth=0.55) +
  geom_point(aes(x = x, y = y, size = count, colour = is_prof), alpha = .3) +
  geom_point(aes(x = x, y = y, size = count, colour = is_prof), fill = "transparent", pch = 21, stroke = .5) +
  facet_grid(is_prof ~ type) +
  scale_fill_manual(values = c("#67000d", "#08306b"), guide = "none") +
  scale_colour_manual(values = c("#67000d", "#08306b"), guide = "none") +
  scale_size(range=c(1,9)) +
  theme(
    axis.text = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()
    )

gridmap <- grid |> 
  ggplot()+
  geom_sf(fill="#ffffff", colour="#252525", linewidth=0.2)+
  coord_sf(crs=27700, datum=NA)+
  geom_text(aes(x=x, y=y, label=abbreviate(area_name, 3)), size=3, alpha=.9, show.legend=FALSE)+
  geom_sf(data=river_grid, colour="#252525", alpha=.9, linewidth=0.6) +
  theme_void()
  
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())


plot <- nodes_summary + gridmap + plot_layout(widths=c(1,.6))

ggsave(filename=here("figs", "05", "nodes_summary.png"), plot=plot,width=8.5, height=4.5, dpi=300)



# Full summary by occupation

plot_data <- solution |>
  inner_join(nodes, by = c("area_name" = "la")) |>
  group_by(area_name) |>
  mutate(count = count / max(count)) |>
  ungroup() |>
  group_by(type, occ_type) |>
  mutate(mean = mean(count)) |>
  ungroup() |>
  mutate(
    count = if_else(type == "jobs", count, -count),
    mean = if_else(type == "jobs", mean, -mean),
    occ_name = factor(occ_type),
    occ_type = as.numeric(fct_rev(factor(occ_type)))
  )

bars <- plot_data |>
  filter(area_name %in% c("Wandsworth", "Westminster", "Bexley", "Hillingdon")) |>
  ggplot(aes(x = occ_type, y = count)) +
  geom_col(aes(fill = is_prof), alpha = .3, width = 1) +
  # Compator : avg relative size per borough.
  geom_step(
    data = . %>%
      group_by(area_name, type) %>%
      arrange(occ_type) %>%
      group_modify(~ add_row(., .after = 9)) %>%
      mutate(
        across(row:mean, ~ if_else(is.na(.x), nth(.x, 2), .x)),
        occ_type = row_number() - .5,
        is_prof = if_else(occ_type == 9.5, TRUE, is_prof)
      ),
    aes(x = occ_type, y = count, group = type, colour = is_prof), alpha = 1, linewidth = .25,
  ) +
  geom_hline(yintercept = 0, linewidth = .4, colour = "#ffffff") +
  # Annotate occ types.
  geom_text(
    data = . %>%
      filter(type == "jobs", area_name == "Westminster"),
    aes(x = occ_type, y = -1, label = word(str_replace_all(occ_name, "_", " "), 2)), fontface = "plain",
    size = 3, hjust = 0, alpha = 1
  ) +
  geom_text(
    data = . %>%
      filter(type == "jobs", area_name == "Bexley") %>%
      mutate(is_admin = occ_type == 4),
    aes(x = occ_type, y = 1, label = word(str_replace_all(occ_name, "_", " "), 2)), alpha = 1, fontface = "plain",
    size = 3, hjust = 1
  ) +
  facet_wrap(~area_name, scales = "free", nrow = 1) +
  # Annotate explanation
  geom_text(
    data = . %>% filter(type == "jobs", area_name == "Bexley", occ_type == 6),
    aes(x = 9.6, y = -1), fontface = "plain", label = "workers living \nin borough",
    size = 2.5, hjust = 0, alpha = 1
  ) +
  geom_segment(
    data = . %>% filter(type == "jobs", area_name == "Bexley", occ_type == 6),
    aes(x = 10.5, y = -.4, xend = 10.5, yend = -1), linewidth = .2, arrow = arrow(length = unit(0.02, "npc"))
  ) +
  geom_text(
    data = . %>% filter(type == "jobs", area_name == "Wandsworth", occ_type == 2),
    aes(x = occ_type, y = -1), fontface = "italic", label = "worker-rich \nborough \nprof",
    size = 2.5, hjust = 0, alpha = 1
  ) +
  geom_text(
    data = . %>% filter(type == "jobs", area_name == "Hillingdon", occ_type == 2),
    aes(x = 4.8, y = -1), fontface = "italic", label = "balanced \nworkers",
    size = 2.5, hjust = 0, alpha = 1
  ) +
  geom_text(
    data = . %>% filter(type == "jobs", area_name == "Hillingdon", occ_type == 2),
    aes(x = 4.8, y = 1), fontface = "italic", label = "balanced \njobs",
    size = 2.5, hjust = 1, alpha = 1
  ) +
  geom_text(
    data = . %>% filter(type == "jobs", area_name == "Westminster", occ_type == 2),
    aes(x = occ_type, y = 1), fontface = "italic", label = "job-rich \nborough \nprof+admin",
    size = 2.5, hjust = 1, alpha = 1
  ) +
  geom_text(
    data = . %>% filter(type == "jobs", area_name == "Wandsworth", occ_type == 6),
    aes(x = 9.6, y = 1), fontface = "plain", label = "jobs available \nin borough",
    size = 2.5, hjust = 1, alpha = 1
  ) +
  annotate("text", x=10, y=1, label="") +
  geom_segment(
    data = . %>% filter(type == "jobs", area_name == "Wandsworth", occ_type == 6),
    aes(x = 10.5, y = .4, xend = 10.5, yend = 1), linewidth = .2, arrow = arrow(length = unit(0.02, "npc"))
  ) +
  labs(caption = "bars: occupation classes | filled: bor-scaled counts") +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_fill_manual(values = c("#08306b", "#67000d"), guide = "none") +
  scale_colour_manual(values = c("#08306b", "#67000d"), guide = "none") +
  coord_flip() +
  theme(
    axis.text = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
    strip.text = element_text(size = 11), panel.spacing = unit(0.5, "lines"),
    plot.caption = element_text(size = 9, colour = "#000000", hjust = 1)
  )

bars

ggsave(filename=here("figs", "05", "nodes_bars.png"), plot=bars,width=6.5, height=2.4, dpi=300)

u <- t |>  inner_join(plot_data |> select(area_name, row, col) |> unique(), by=c("area_name"="area_name"))

real <- plot_data |>
  ggplot(aes(x = occ_type, y = count)) +
  geom_tile(aes(x = 6, y = 0), height = 2.1, width = 11.5, linewidth = .08, fill = "transparent", colour="#616161") +
  geom_col(aes(fill = is_prof), alpha = .3, width = 1) +
  
  geom_col(data = . %>% filter(area_name %in% c("Westminster", "City of London", "Camden", "Tower Hamlets"), type=="jobs"),
           aes(fill = is_prof), alpha = .3, width = 1) +
  
  geom_col(data = . %>% filter(area_name %in% c("Richmond upon Thames", "Wandsworth", "Barnet", "Haringey", "Merton", "Ealing"), type=="workers"),
           aes(fill = is_prof), alpha = .3, width = 1) +
  
  # Compator : avg relative size per borough.
  geom_step(
    data = . %>%
      group_by(area_name, type) %>%
      arrange(occ_type) %>%
      group_modify(~ add_row(., .after = 9)) %>%
      mutate(
        across(row:mean, ~ if_else(is.na(.x), nth(.x, 2), .x)),
        occ_type = row_number() - .5,
        is_prof = if_else(occ_type == 9.5, TRUE, is_prof)
      ),
    aes(x = occ_type, y = count, group = type, colour = is_prof), alpha = 1, linewidth = .25,
  ) +
  geom_step(
    data = . %>%
      group_by(area_name, type) %>%
      arrange(occ_type) %>%
      group_modify(~ add_row(., .after = 9)) %>%
      mutate(
        across(row:mean, ~ if_else(is.na(.x), nth(.x, 2), .x)),
        occ_type = row_number() - .5,
        is_prof = if_else(occ_type == 9.5, TRUE, is_prof)
      ) %>% 
      filter(area_name %in% c("Westminster", "City of London", "Camden", "Tower Hamlets"), type=="jobs"),
    aes(x = occ_type, y = count, group = type, colour = is_prof), alpha = 1, linewidth = .4,
  ) +
  
  
  geom_step(
    data = . %>%
      group_by(area_name, type) %>%
      arrange(occ_type) %>%
      group_modify(~ add_row(., .after = 9)) %>%
      mutate(
        across(row:mean, ~ if_else(is.na(.x), nth(.x, 2), .x)),
        occ_type = row_number() - .5,
        is_prof = if_else(occ_type == 9.5, TRUE, is_prof)
      ) %>% 
      filter(area_name %in% c("Richmond upon Thames", "Wandsworth", "Barnet", "Haringey", "Merton", "Ealing"), type=="workers"),
    aes(x = occ_type, y = count, group = type, colour = is_prof), alpha = 1, linewidth = .4,
  ) +
  
  geom_hline(yintercept = 0, linewidth = .5, colour = "#ffffff") +
  
  
  geom_text(
    data = solution,
    aes(x = 11.3, y = 0, label = abbreviate(area_name, 3)),
    size = 2.5, alpha = .9, hjust = .5, vjust=1,
  ) +
  
  
  # geom_tile(
  #   data =  tibble(row=c(3,5), col=c(2,7)),
  #   aes(x = 6, y = 0),
  #   height = 2, width = 11.5, linewidth = .1, fill = "#eeeeee", colour = "#eeeeee", 
  # ) +
  
  
  geom_text(
    data=tibble(row=5, col=7), 
    aes(x = 6, y = -1),
    label=str_wrap("Job-rich: right-pointing bars (outlined)",15), 
    vjust="middle", hjust="left", size=2.2) +
  
  geom_text(
    data=tibble(row=3, col=2), 
    aes(x = 6, y = -1),
    label=str_wrap("Worker-rich: left-pointing bars (outlined)",15), 
    vjust="middle", hjust="left", size=2.2) +
  facet_grid(-row ~ col) +
  
  scale_y_continuous(limits = c(-1.05, 1.05)) +
  scale_fill_manual(values = c("#08306b", "#67000d"), guide = "none") +
  scale_colour_manual(values = c("#08306b", "#67000d"), guide = "none") +
  coord_flip() +
  theme(
    panel.spacing = -unit(0.1, "lines"), strip.text.x = element_blank(),
    strip.text = element_blank(), axis.line = element_blank(),
    axis.text = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
    plot.caption = element_text(size = 9, colour = "#636363", face = "italic", hjust = 1), 
  )



plot_data |>
  ggplot(aes(x = occ_type, y = count)) +
  geom_col(aes(fill = is_prof), alpha = .5, width = 1) +
  geom_hline(yintercept = 0, linewidth = .4, colour = "#ffffff") +
  facet_grid(-row ~ col) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_fill_manual(values = c("#08306b", "#67000d"), guide = "none") +
  coord_flip()



# Line-up
do_lineup <- function(data, col_offset) {
  real <- sample(1:9,1)
  for(i in 1:9) {
    if(i==real) {
      data <- cbind(data, data$value)
      colnames(data)[i+col_offset] <- paste0("permutation", i)
    }
    else {
      permutation <- sample(data$value,nrow(data))
      data <- cbind(data, permutation)
      colnames(data)[i+col_offset] <- paste0("permutation", i)
    }
  }
  return(data %>% select(-value) %>% mutate(real=paste0("permutation", real)))
}

# Create the lineup data : swaps for boroughs
lineup_data <- do_lineup(plot_data |>  select(value=area_name) |>  unique(), 1) %>% 
  pivot_longer(cols=(permutation1:permutation9),names_to="perm", values_to="area_name") %>% arrange(perm) 

t <- map_df(
  lineup_data |> filter(perm=="permutation1") |> pull(area_name),
  ~tibble(area_name=rep(.x, times=18))) |>
  bind_cols(plot_data |>   select(-c(area_name, row, col)))



library(rsample)
permuted_data <- plot_data |> 
  select() |> 
  permutations(permute=c(area_name), times=2, apparent=TRUE) |> 
  mutate(data=map(splits, ~rsample::analysis(.))) |> 
  select(id, data) %>%
  unnest(cols=data)


plot <- real + random

ggsave(filename=here("figs", "05", "nodes_gridmap.png"), plot=real,width=5.5, height=4.5, dpi=500)





plot_data <- edges |> mutate(non_prof = commutes-is_prof) |> 
  rename(prof = is_prof) |>
  ungroup() |>
  mutate(global_prof = sum(prof) / sum(prof + non_prof)) |>
  group_by(d_bor) |>
  mutate(
    count = prof + non_prof, 
    obs = prof, 
    exp = (global_prof * count), 
    resid = (obs - exp) / (exp^.7)
    ) |>
  ungroup() |>
  left_join(grid |> select(area_name), by = c("o_bor" = "area_name")) |>
  mutate(
    bor_label = if_else(o_bor == d_bor, d_bor, ""),
    bor_focus = o_bor == d_bor
  ) |>
  st_as_sf()

bbox_grid <- st_bbox(grid)
width <- bbox_grid$xmax - bbox_grid$xmin
height <- bbox_grid$ymax - bbox_grid$ymin

range_resid <- max(abs(plot_data$resid))

plot <- plot_data |> 
  ggplot() +
  geom_tile(
    data = . %>% filter(bor_focus),
    aes(x = bbox_grid$xmin + .5 * width, y = bbox_grid$ymin + .5 * height),
    height = height * 1.02, width = width * 1.02, linewidth = .1, fill = "transparent", colour = "#616161"
    ) +
  geom_sf(aes(fill = resid), colour = "#616161", size = 0.15, alpha = 0.9) +
  geom_sf(data = . %>% filter(bor_focus), fill = "transparent", colour = "#373737", size = 0.3) +
  geom_text(
    data = plot_data %>% filter(bor_focus),
    aes(x = o_x, y = o_y, label = str_extract(o_bor, "^.{1}")),
    colour = "#252525", alpha = 0.9, size = 2.1,
    hjust = "centre", vjust = "middle"
  ) +
  geom_text(
    data = plot_data %>% filter(bor_focus),
    aes(x = bbox_grid$xmax, y = bbox_grid$ymin, label = abbreviate(o_bor, 3)),
    colour = "#252525", alpha = 0.9, size = 3.5,
    hjust = "right", vjust = "bottom"
  ) +
  coord_sf(crs = st_crs(plot_data), datum = NA) +
  labs(x="", y="") +
  facet_grid(-d_row ~ d_col, shrink = FALSE) +
  labs(
    fill = "",
    caption = "exp: prof vs. non-prof job flows distribute uniformly within borough<br> <span style = 'color: #67001f;'> more professional  </span> | <span style = 'color: #053061;'> more non-professional </span> <br> dark colour: large number commutes + difference from exp"
  ) +
  scale_fill_distiller(
    palette = "RdBu", 
    direction = -1, 
    breaks = c(-range_resid, 0, range_resid), 
    labels = c("non-prof", "avg", "prof"), 
    limits = c(-range_resid, range_resid),
    guide="none"
    ) +
  theme(
    panel.spacing = unit(-0.1, "lines"),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    plot.caption = element_markdown(hjust = 1, margin = margin(1, 1, 10, 1), size=10, colour="#000000"), 
    plot.title = element_text(hjust = .5, margin = margin(1, 1, 2, 1), size=14),
    legend.position = "right", legend.margin = margin(5, 0, 0, 0),
    legend.key.size = unit(.6, "cm")
  )



bbox_grid <- st_bbox(grid)
width <- bbox_grid$xmax - bbox_grid$xmin
height <- bbox_grid$ymax - bbox_grid$ymin

range_resid <- max(abs(plot_data$resid))



plot_data <- edges |> mutate(non_prof = commutes-is_prof) |> 
  rename(prof = is_prof) |>
  ungroup() |>
  mutate(local_prof = sum(prof) / sum(prof + non_prof)) |> 
  group_by(d_bor) |>
  mutate(
    #local_prof = sum(prof) / sum(prof + non_prof),
    count = prof + non_prof, 
    obs = prof, 
    exp = (local_prof * count), 
    resid = (obs - exp) / (exp^.6)
  ) |>
  ungroup() |>
  left_join(grid |> select(area_name), by = c("o_bor" = "area_name")) |>
  mutate(
    bor_label = if_else(o_bor == d_bor, d_bor, ""),
    bor_focus = o_bor == d_bor
  ) |>
  st_as_sf()

range_resid <- max(abs(plot_data$resid))

plot <- plot_data |> 
  ggplot() +
  geom_tile(
    data = . %>% filter(bor_focus),
    aes(x = bbox_grid$xmin + .52 * width, y = bbox_grid$ymin + .57 * height),
    height = height * 1.2, width = width * 1.1, linewidth = .1, fill = "transparent", colour = "#616161"
  ) +
  geom_sf(aes(fill = resid), colour = "#616161", size = 0.15, alpha = 0.9) +
  geom_sf(data = . %>% filter(bor_focus), fill = "transparent", colour = "#373737", size = 0.3) +
  geom_text(
    data = plot_data %>% filter(bor_focus),
    aes(x = o_x, y = o_y, label = str_extract(o_bor, "^.{1}")),
    colour = "#252525", alpha = 1, size = 2.1,
    hjust = "centre", vjust = "middle"
  ) +
  geom_text(
    data = plot_data %>% filter(bor_focus),
    aes(x = bbox_grid$xmax + .05*width, y = bbox_grid$ymin, label = abbreviate(o_bor, 3)),
    colour = "#252525", alpha = 1, size = 3.2,
    hjust = "right", vjust = "bottom"
  ) +
  geom_sf(data=rivers_facet, colour="#252525", alpha=.6, linewidth=0.4) +
  coord_sf(crs = st_crs(plot_data), datum = NA) +
  labs(x="", y="") +
  
  # geom_tile(
  #   data =  tibble(d_row=c(8,8), d_col=c(1,2)),
  #   aes(x = bbox_grid$xmin + .52 * width, y = bbox_grid$ymin + .57 * height),
  #   height = height * 1.2, width = width * 1.1, linewidth = .8, fill = "#eeeeee", colour = "#eeeeee", 
  # ) +
  
  # geom_text(
  #   data=tibble(d_row=8, d_col=1), 
  #   aes(x=bbox_grid$xmax-.49*width, bbox_grid$ymax-.55*height),
  #   label=str_wrap(
  #     "exp: prof vs. non-prof job flows distribute uniformly within borough"
  #     ,20), 
  #   vjust="middle", hjust="centre", size=1.8) +
geom_richtext(
  data=tibble(d_row=8, d_col=1), 
  aes(x=bbox_grid$xmax+.0*width, bbox_grid$ymax),
  label= "exp is that<br>prof vs. non-prof <br>uniformly dist<br>across London", 
  vjust="top", hjust="right", size=2.2, fill="transparent", label.colour = NA, family="Avenir Next") +  

  geom_richtext(
    data=tibble(d_row=8, d_col=2),
    aes(x=bbox_grid$xmin-.0*width, bbox_grid$ymax),
    label=
      "origin has more<br> <span style = 'color: #67001f;'>professional  </span> <br> <span style = 'color: #053061;'>non-professional</span><br>than exp", 
    hjust="left", vjust="top", size=2.2, fill="transparent", label.colour = NA, family="Avenir Next") +
  
  # geom_richtext(
  #   data=tibble(d_row=5, d_col=7),
  #   aes(x=bbox_grid$xmin, bbox_grid$ymax-.55*height),
  #   label=
  #     "job-rich boroughs <br> <span style = 'color: #67001f;'> > prof</span>: south west <br> <span style = 'color: #053061;'> > non-prof</span>: east",
  #   hjust="left", size=2, fill="transparent", label.colour = NA, family="Avenir Next") +

  # geom_text(
  #   data=tibble(d_row=7, d_col=1), 
  #   aes(x=bbox_grid$xmax-.52*width, bbox_grid$ymax-.55*height),
  #   label=str_wrap(
  #     "dark colour: large number commutes + difference from exp"
  #     ,20), 
  #   vjust="middle", hjust="centre", size=1.5) +
  
  facet_grid(-d_row ~ d_col, shrink = FALSE) +
  labs(
    fill = "",
    #caption = "exp: prof vs. non-prof job flows distribute uniformly within borough<br> <span style = 'color: #67001f;'> more professional  </span> | <span style = 'color: #053061;'> more non-professional</span> <br> dark colour: large number commutes + difference from exp"
  ) +
  scale_fill_distiller(
    palette = "RdBu", 
    direction = -1, 
    breaks = c(-range_resid, 0, range_resid), 
    labels = c("non-prof", "avg", "prof"), 
    limits = c(-range_resid, range_resid),
    guide="none"
  ) +
  theme(
    panel.spacing = unit(-0.1, "lines"),
    strip.text.x = element_blank(), strip.text.y = element_blank(),
    plot.caption = element_markdown(hjust = 1, margin = margin(1, 1, 10, 1), size=10, colour="#000000"), 
    plot.title = element_text(hjust = .5, margin = margin(1, 1, 2, 1), size=14),
    legend.position = "right", legend.margin = margin(5, 0, 0, 0),
    legend.key.size = unit(.6, "cm")
  )


ggsave(filename=here("figs", "05", "edges_odmap.png"), plot=plot,width=7, height=6, dpi=500)
ggsave(filename=here("figs", "05", "edges_odmap_within.png"), plot=plot,width=7, height=6, dpi=500)



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



