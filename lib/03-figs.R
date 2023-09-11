# Filename: 03-figs.R 
#
# Figures for Chapter 3 of vis4sds 
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
library(parlitools)
library(here)
library(sf)
library(tidyverse)
library(magick)

# 1.2 Data -----------------------------

# Constituency boundaries, simplified using mapshapr. 
# From -- https://geoportal.statistics.gov.uk/
constituency_boundaries <-
  st_read(here("../", "data", "ch3", "constituency_boundaries.geojson"), crs=27700)

# Swing.
data <- bes_2019 |>
  filter(region != "Northern Ireland") |>
  mutate(
    swing_con_lab=0.5*((con_19-con_17)-(lab_19-lab_17)),
    # Recode to 0 Chorley incoming speaker,Buckingham outgoing speaker --  uncontested seat.
    swing_con_lab=if_else(constituency_name %in% c("Chorley", "Buckingham"),0,swing_con_lab)
  )

# Party colours.
# Con :
con <- "#0575c9"
# Lab :
lab <- "#ed1e0e"
# Other :
other <- "#bdbdbd"
# Lib dem :
lib_dem <- "#fe8300"
# SNP :
snp <- "#ebc31c"
# Greens :
greens <- "#78c31e"
# plaid
plaid <- "#4e9f2f"
# sinn fein
sinn_fein <- "#0a6240"
# dup
dup <- "#be1a40"
# Other :
other <- "#bdbdbd"

# Store as vector.
elected_parties <- c("Conservative", "Labour", "Other")



#-----------------------------------------
# 2. Concepts graphics
#-----------------------------------------

# Assemble Munzner visual channels.
position_common <- image_read(here("figs", "03", "munzner", "position_common.png"))
position_common_image <- image_fill(position_common, 'none')
position_common_raster <- as.raster(position_common_image)

position_unaligned <- image_read(here("figs", "03", "munzner", "position_unaligned.png"))
position_unaligned_image <- image_fill(position_unaligned, 'none')
position_unaligned_raster <- as.raster(position_unaligned_image)

position_unaligned <- image_read(here("figs", "03", "munzner", "position_unaligned.png"))
position_unaligned_image <- image_fill(position_unaligned, 'none')
position_unaligned_raster <- as.raster(position_unaligned_image)

length <- image_read(here("figs", "03", "munzner", "length.png"))
length_image <- image_fill(length, 'none')
length_raster <- as.raster(length_image)

tilt <- image_read(here("figs", "03", "munzner", "tilt.png"))
tilt_image <- image_fill(tilt, 'none')
tilt_raster <- as.raster(tilt_image)

area <- image_read(here("figs", "03", "munzner", "area.png"))
area_image <- image_fill(area, 'none')
area_raster <- as.raster(area_image)

depth <- image_read(here("figs", "03", "munzner", "depth.png"))
depth_image <- image_fill(depth, 'none')
depth_raster <- as.raster(depth_image)

colour <- image_read(here("figs", "03", "munzner", "colour.png"))
colour_image <- image_fill(colour, 'none')
colour_raster <- as.raster(colour_image)

curve <- image_read(here("figs", "03", "munzner", "curvature.png"))
curve_image <- image_fill(curve, 'none')
curve_raster <- as.raster(curve_image)

spatial <- image_read(here("figs", "03", "munzner", "spatial_region.png"))
spatial_image <- image_fill(spatial, 'none')
spatial_raster <- as.raster(spatial_image)

volume <- image_read(here("figs", "03", "munzner", "volume.png"))
volume_image <- image_fill(volume, 'none')
volume_raster <- as.raster(volume_image)

hue <- image_read(here("figs", "03", "munzner", "hue.png"))
hue_image <- image_fill(hue, 'none')
hue_raster <- as.raster(hue_image)

motion <- image_read(here("figs", "03", "munzner", "motion.png"))
motion_image <- image_fill(motion, 'none')
motion_raster <- as.raster(motion_image)

shape <- image_read(here("figs", "03", "munzner", "shape.png"))
shape_image <- image_fill(shape, 'none')
shape_raster <- as.raster(shape_image)


munzner_effect <- ggplot() + 
  annotate("text", x=1.45, y=1.25, label="effectiveness", size=2.8, hjust="centre") +
  annotate("text", x=0.15, y=1.13, label="magnitude: order", size=2.8, hjust="left") +
  annotate("segment", x=.22, xend=2.9, y=1.22, yend=1.22, size=.2, 
           arrow = arrow(ends="last", length = unit(.15,"cm"))) +
  annotation_raster(position_common_raster, .19, .48, .895, .99) +
  annotate("text", x=.32, y=1.04, label="position:\n common scale", size=2.5, hjust="centre") +
  annotation_raster(position_unaligned_raster, .50, .78, .90, .985) +
  annotate("text", x=.6, y=1.04, label="position:\n unaligned scale", size=2.5, hjust="centre") +
  annotation_raster(length_raster, .81, 1.1, .89, .99) +
  annotate("text", x=.91, y=1.04, label="length:\n 1D size", size=2.5, hjust="centre") +
  annotation_raster(tilt_raster, 1.12, 1.42, .89, .99) +
  annotate("text", x=1.25, y=1.04, label="tilt/angle", size=2.5, hjust="centre") +
  annotation_raster(area_raster, 1.41, 1.71, .91, .99) +
  annotate("text", x=1.53, y=1.04, label="area:\n 2D size", size=2.5, hjust="centre") +
  annotation_raster(depth_raster, 1.72, 2.01, .91, .97) +
  annotate("text", x=1.86, y=1.04, label="depth:\n 3D position", size=2.5, hjust="centre") +
  annotation_raster(colour_raster, 2.05, 2.29, .91, .98) +
  annotate("text", x=2.15, y=1.04, label="colour luminance\nsaturation", size=2.5, hjust="centre") +
  annotation_raster(curve_raster, 2.31, 2.61, .91, .98) +
  annotate("text", x=2.45, y=1.04, label="curvature", size=2.5, hjust="centre") +
  annotation_raster(volume_raster, 2.62, 2.92, .9, .98) +
  annotate("text", x=2.75, y=1.04, label="volume", size=2.5, hjust="centre") +
  
  annotate("text", x=0.15, y=.82, label="identity: category", size=2.8, hjust="left") +
  #annotate("text", x=.15, y=.77, label="identity:\ncategory", size=3, hjust="right") +
  annotate("text", x=.32, y=.75, label="spatial region", size=2.5, hjust="centre") +
  annotation_raster(spatial_raster, .22, .51, .65, .715) +
  
  annotation_raster(hue_raster, .50, .765, .66, .72) +
  annotate("text", x=.6, y=.75, label="hue", size=2.5, hjust="centre") +
  
  annotation_raster(motion_raster, .81, 1.08, .65, .73) +
  annotate("text", x=.91, y=.75, label="motion", size=2.5, hjust="centre") +
  
  annotation_raster(shape_raster, 1.15, 1.4, .66, .72) +
  annotate("text", x=1.25, y=.75, label="shape", size=2.5, hjust="centre") +
  
  scale_x_continuous(limits=c(0, 3)) +
  scale_y_continuous(limits=c(0.65, 1.3)) +
  
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )


munzner_effect <- ggplot() + 
  annotate("text", x=-1, y=5.5, label="effectiveness", size=3.5, hjust="centre", angle=270) +
  annotate("text", x=-.3, y=8.4, label="magnitude:order", size=3.5, hjust="left", family="Avenir Medium") +
  annotate("segment", x=-.8, xend=-.8, y=2.8, yend=8, size=.2, 
           arrow = arrow(ends="first", length = unit(.15,"cm"))) +
  annotation_raster(position_common_raster, 1, 2, 8, 7.5) +
  annotate("text", x=.9, y=7.8, label="position:\n common scale", size=3, hjust="right") +
  annotation_raster(position_unaligned_raster, 1, 1.98, 7.35, 6.9) +
  annotate("text", x=.9, y=7.2, label="position:\n unaligned scale", size=3, hjust="right") +
  annotation_raster(length_raster, 1, 1.95, 6.25, 6.75) +
  annotate("text", x=.9, y=6.5, label="length:\n 1D size", size=3, hjust="right") +
  annotation_raster(tilt_raster, 1, 2, 6.2, 5.7) +
  annotate("text", x=.9, y=5.95, label="tilt/angle", size=3, hjust="right") +
  annotation_raster(area_raster, 1, 2.1, 5.15, 5.55) +
  annotate("text", x=.9, y=5.4, label="area:\n 2D size", size=3, hjust="right") +
  annotation_raster(depth_raster, 1, 2, 5, 4.65) +
  annotate("text", x=.9, y=4.75, label="depth:\n 3D position", size=3, hjust="right") +
  annotation_raster(colour_raster, 1, 2, 3.87, 4.35) +
  annotate("text", x=.9, y=4.1, label="colour luminance\nsaturation", size=3, hjust="right") +
  annotation_raster(curve_raster, 1, 2, 3.2, 3.7) +
   annotate("text", x=.9, y=3.5, label="curvature", size=3, hjust="right") +
  annotation_raster(volume_raster, 1, 2, 3.1, 2.6) +
  annotate("text", x=.9, y=3.0, label="volume", size=3, hjust="right") +
  # 
  annotate("text", x=2.5, y=8.4, label="identity:category", size=3.5, hjust="left", family="Avenir Medium") +
  # #annotate("text", x=.15, y=.77, label="identity:\ncategory", size=3, hjust="right") +
  annotate("text", x=3.5, y=7.8, label="spatial region", size=3, hjust="right") +
  annotation_raster(spatial_raster, 3.6, 4.8, 8, 7.6) +
  # 
  annotation_raster(hue_raster, 3.6, 4.7, 7.35, 7) +
  annotate("text", x=3.5, y=7.2, label="colour hue", size=3, hjust="right") +
  # 
  annotation_raster(motion_raster, 3.6, 4.7, 6.8, 6.3) +
  annotate("text", x=3.5, y=6.55, label="motion", size=3, hjust="right") +
  # 
  annotation_raster(shape_raster,  3.6, 4.65, 6.25, 5.85) +
  annotate("text", x=3.5, y=6, label="shape", size=3, hjust="right") +
  
  scale_y_continuous(limits=c(0, 9)) +
  scale_x_continuous(limits=c(-1.2, 6)) +
  
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

ggsave(here("figs", "03", "munzner_effect_trans.png"), munzner_effect, dpi=700, width = 5.7, height=6)


# And recode elected variable as factor for use in scale_colour_manual.
t <- bes_2019 |>
  mutate(
    elected=if_else(!winner_19 %in% elected_parties, "Other", winner_19),
    elected=factor(elected,
                   levels=c("Conservative","Labour", "Other"))
  )
colours <- c(con, lab, other)
names(colours) <- levels(t$elected)


plot <- bes_2019 |>
  filter(region != "Northern Ireland", constituency_name != "Chorley") |>
  ggplot(mapping=aes(x=leave_hanretty, y=con_1719)) +
  geom_point(alpha=.8) +
  labs(x="% Leave", y="% gain in Conservative vote share")

plot2 <- t |>
  filter(constituency_name != "Chorley", region != "Northern Ireland") |>
  ggplot(mapping=aes(x=leave_hanretty, y=con_1719)) +
  geom_point(mapping=aes(colour=elected), alpha=.8)+
  scale_colour_manual(values=colours)+
  labs(x="% Leave", y="% gain in Conservative vote share")+
  guides(colour=guide_legend(title="Winning party"))

# shape -- on flipped
t |>
  filter(constituency_name != "Chorley", region != "Northern Ireland") |>
  mutate(is_flipped=seat_change_1719=="Conservative gain from Labour",
         is_flipped=if_else(is.na(is_flipped), FALSE, is_flipped)) |>
  ggplot(mapping=aes(x=leave_hanretty, y=con_1719)) +
  geom_point(mapping=aes(colour=elected, pch=is_flipped))+
  scale_colour_manual(values=colours)+
  labs(
    #title="Conservative gain in vote shares by Leave vote (estimated)",
    #subtitle="-- Constituencies in Great Britain. Gain is diff in vote shares between 2017-2019 GE.",
    #caption="Data: Published by House of Commons Library, accessed via parlitools package",
    x="% Leave", y="% gain in Conservative vote share"
  )+
  theme_v_gds()

t |>
  filter(constituency_name != "Chorley", region != "Northern Ireland") |>
  mutate(is_flipped=seat_change_1719=="Conservative gain from Labour",
         is_flipped=if_else(is.na(is_flipped), FALSE, is_flipped)) |>
  ggplot(mapping=aes(x=leave_hanretty, y=con_1719)) +
  geom_point(mapping=aes(colour=elected, pch=is_flipped))+
  scale_colour_manual(values=colours)+
  labs(
    #title="Conservative gain in vote shares by Leave vote (estimated)",
    #subtitle="-- Constituencies in Great Britain. Gain is diff in vote shares between 2017-2019 GE.",
    #caption="Data: Published by House of Commons Library, accessed via parlitools package",
    x="% Leave", y="% gain in Conservative vote share"
  )

plot3 <- t |>
  filter(constituency_name != "Chorley", region != "Northern Ireland") |>
  mutate(is_flipped=seat_change_1719=="Conservative gain from Labour",
         is_flipped=if_else(is.na(is_flipped), FALSE, is_flipped)) |>
  ggplot(mapping=aes(x=leave_hanretty, y=con_1719)) +
  geom_point(mapping=aes(colour=elected, alpha=is_flipped, shape=is_flipped))+
  geom_vline(xintercept=50, size=.2)+
  scale_colour_manual(values=colours)+
  scale_fill_manual(values=colours)+
  scale_alpha_ordinal(range=c(.4,1))+
  scale_shape_manual(values=c(21,19)) +
  guides(colour=FALSE, alpha=FALSE,
         shape=guide_legend(title="Flipped Lab-to-Con"))+
  labs(
    #title="Conservative gain in vote shares by Leave vote (estimated)",
    #subtitle="-- Constituencies in Great Britain. Gain is diff in vote shares between 2017-2019 GE.",
    #caption="Data: Published by House of Commons Library, accessed via parlitools package",
    x="% Leave", y="% gain in Conservative vote share"
  )

t |>
  filter(constituency_name != "Chorley", region != "Northern Ireland") |>
  ggplot(data=.) +
  geom_point(
    mapping=
      aes(x=leave_hanretty, y=con_1719)
  )

plot_export <-plot + plot2 + plot3 +  plot_layout(nrow=3) +
  plot_annotation(#title="Conservative gain in vote shares by Leave vote (estimated)",subtitle="-- Constituencies in Great Britain. Gain is vote share shift between 2017-2019 GE",
                  #caption="Data: Published by House of Commons Library, accessed via parlitools package", theme=theme_v_gds() &
  theme(plot.subtitle=element_text(size=10)))

ggsave(filename=here("figs", "03", "gog-demo-label.png"), plot=plot_export,width=5, height=12, dpi=300)
ggsave(filename=here("figs", "03", "gog-demo-label.svg"), plot=plot_export,width=5, height=12)

# Preattentive
temp <- tibble(
  x = rep(0:39,5),
  y = c(rep(3, 40),rep(6, 40),rep(9, 40),rep(12, 40), rep(15, 40)),
  numbers = sample(0:9,200, replace=TRUE, prob=c(0.11,0.11,0.11,0.025, 0.11, 0.11,0.11,0.11,0.11,0.11))
)

none <- temp |>
  ggplot(aes(x, y))+
  geom_text(aes(label=numbers), colour="#636363", angle=0, size=8,hjust = 0.5, vjust=-0.5, family="Avenir Next")+
  scale_y_continuous(limits = c(1, 20))+
  theme_v_gds() +
  theme(axis.text=element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(filename=here("figs","03","no-encoding.png"), plot=none,width=8, height=3.2, dpi=300)
ggsave(filename=here("figs","03","no-encoding.svg"), plot=none,width=8, height=3.2)


temp <- tibble(
  x = rep(0:39,5),
  y = c(rep(3, 40),rep(6, 40),rep(9, 40),rep(12, 40), rep(15, 40)),
  numbers = sample(0:9,200, replace=TRUE, prob=c(0.11,0.11,0.11,0.025, 0.11, 0.11,0.11,0.11,0.11,0.11))
)

area <- temp |>
  ggplot(aes(x, y))+
  geom_text(data=subset(temp, numbers!=3),
            aes(label=numbers), colour="#636363", angle=0, size=8,hjust = 0.5, vjust=-0.5, family="Avenir Next")+
  geom_text(data=subset(temp, numbers==3),
            aes(label=numbers), colour="#636363", angle=0, size=9.5,hjust = 0.5, vjust=-0.5, family="Avenir Next")+
  scale_y_continuous(limits = c(1, 20))+
  theme_v_gds() +
  theme(axis.text=element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(filename=here("figs","03","area-encoding.png"), plot=area,width=8, height=3.2, dpi=300)
ggsave(filename=here("figs","03","area-encoding.svg"), plot=area,width=8, height=3.2)



temp <- tibble(
  x = rep(0:39,5),
  y = c(rep(3, 40),rep(6, 40),rep(9, 40),rep(12, 40), rep(15, 40)),
  numbers = sample(0:9,200, replace=TRUE, prob=c(0.11,0.11,0.11,0.025, 0.11, 0.11,0.11,0.11,0.11,0.11))
)

angle <- temp |>
  ggplot(aes(x, y))+
  geom_text(data=subset(temp, numbers!=3),
            aes(label=numbers), colour="#636363", angle=0, size=8,hjust = 0.5, vjust=-0.5, family="Avenir Next")+
  geom_text(data=subset(temp, numbers==3 & y %% 2==0),
            aes(label=numbers), colour="#636363", angle=-15, size=8,hjust = 0.5, vjust=-0.5, family="Avenir Next")+
  geom_text(data=subset(temp, numbers==3 & y %% 2!=0),
            aes(label=numbers), colour="#636363", angle=15, size=8,hjust = 0.5, vjust=-0.5, family="Avenir Next")+
  labs(x="x",y="y")+
  scale_y_continuous(limits = c(1, 20))+
  theme_v_gds() +
  theme(axis.text=element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(filename=here("figs","03","angle-encoding.png"), plot=angle,width=8, height=3.2, dpi=300)
ggsave(filename=here("figs","03","angle-encoding.svg"), plot=angle,width=8, height=3.2)

temp <- tibble(
  x = rep(0:39,5),
  y = c(rep(3, 40),rep(6, 40),rep(9, 40),rep(12, 40), rep(15, 40)),
  numbers = sample(0:9,200, replace=TRUE, prob=c(0.11,0.11,0.11,0.025, 0.11, 0.11,0.11,0.11,0.11,0.11))
)


hue <- temp |>
  ggplot(aes(x, y))+
  geom_text(data=subset(temp, numbers!=3),
            aes(label=numbers), colour="#636363", angle=0, size=8,hjust = 0.5, vjust=-0.5, family="Avenir Next")+
  geom_text(data=subset(temp, numbers==3),
            aes(label=numbers), colour="#de2d26", angle=0, size=8,hjust = 0.5, vjust=-0.5, family="Avenir Next")+
  scale_y_continuous(limits = c(1, 20))+
  theme_v_gds() +
  theme(axis.text=element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(filename=ggsave(filename=here("figs","03","hue-encoding.png"), plot=hue,width=8, height=3.2, dpi=300), plot=hue,width=8, height=3.2, dpi=300)
ggsave(filename=ggsave(filename=here("figs","03","hue-encoding.svg"), plot=hue,width=8, height=3.2, dpi=300), plot=hue,width=8, height=3.2)

temp <- tibble(
  x = rep(0:39,5),
  y = c(rep(3, 40),rep(6, 40),rep(9, 40),rep(12, 40), rep(15, 40)),
  numbers = sample(0:9,200, replace=TRUE, prob=c(0.11,0.11,0.11,0.025, 0.11, 0.11,0.11,0.11,0.11,0.11))
)

temp_region <- temp |> filter(!(y==3 & x>34))

temp_region <- temp_region |>
  add_row(x=0,y=21,numbers=3) |>
  add_row(x=1,y=21,numbers=3) |>
  add_row(x=2,y=21,numbers=3) |>
  add_row(x=3,y=21,numbers=3) |>
  add_row(x=4,y=21,numbers=3)

spatial <- temp_region |>
  ggplot(aes(x, y))+
  geom_text(data=subset(temp_region, numbers!=3),
            aes(label=numbers), colour="#636363", angle=0, size=8,hjust = 0.5, vjust=-0.5, family="Avenir Next")+
  geom_text(data=subset(temp_region, numbers==3),
            aes(label=numbers), colour="#636363", angle=0, size=8,hjust = 0.5, vjust=-0.5, family="Avenir Next")+
  labs(x="x",y="y")+
  scale_y_continuous(limits = c(1, 25))+
  theme(axis.text=element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())


ggsave(filename=here("figs","03","spatial-encoding.png"), plot=spatial, width=8, height=3.5, dpi=300)
ggsave(filename=here("figs","03","spatial-encoding.svg"), plot=spatial, width=8, height=3.5)

hue <- temp |>
  ggplot(aes(x, y))+
  geom_text(data=subset(temp, numbers!=3),
            aes(label=numbers), colour="#636363", angle=0, size=8,hjust = 0.5, vjust=-0.5, family="Avenir Next")+
  geom_text(data=subset(temp, numbers==3),
            aes(label=numbers), colour="#de2d26", angle=0, size=8,hjust = 0.5, vjust=-0.5, family="Avenir Next")+
  scale_y_continuous(limits = c(1, 20))+
  theme_v_gds() +
  theme(axis.text=element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(filename=here("figs","03","hue-encoding.png"), plot=hue,width=8, height=3.2, dpi=300)
ggsave(filename=here("figs","03","hue-encoding.svg"), plot=hue,width=8, height=3.2)

temp <- tibble(
  x = seq(0,5,.01),
  length = x,
  flannery = x^.87,
  stevens = x^.7
)

power_laws <- temp |>
  pivot_longer(cols=c(length,stevens, flannery), names_to="stimulus", values_to="perception") |>
  ggplot() +
  geom_path(aes(x=x,y=perception, group=stimulus), colour=site_colours$primary, size=0.8, alpha=.8) +
  scale_x_continuous(limits=c(-.3,9.5))+
  scale_y_continuous(limits=c(-.5,5.5))+
  annotate("segment", x=0, xend=5, y=0, yend=0, arrow=arrow(ends="last", type="closed", length = unit(.15, "cm")), size=.35)+
  annotate("segment", x=0, xend=0, y=0, yend=5, arrow=arrow(ends="last", type="closed", length = unit(.15, "cm")), size=.35)+
  annotate("text", x=-.25, y=5, label="perceived size", hjust=1, angle=90)+
  annotate("text", x=5, y=-.3, label="graphical size", hjust=1)+
  annotate("text", x=5.1, y=max(temp$flannery)+.1, label="0.87 Flannery", hjust=0, size=3.4)+
  annotate("text", x=5.1, y=max(temp$flannery)-.2, label="circle area", hjust=0, size=3)+
  annotate("text", x=5.1, y=max(temp$x)+.1, label="1.0 Linear", hjust=0, size=3.4)+
  annotate("text", x=5.1, y=max(temp$x)-.2, label="line/bar length", hjust=0, size=3)+
  annotate("text", x=5.1, y=max(temp$stevens)+.1, label="0.7 Stevens", hjust=0, size=3.4)+
  annotate("text", x=5.1, y=max(temp$stevens)-.2, label="rectangle area", hjust=0, size=3)+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(filename=here("figs","03","power-laws.png"), plot=power_laws,width=5, height=3.5, dpi=900)
ggsave(filename=here("figs","03","power-laws.svg"), plot=power_laws,width=5, height=3.5)




temp <- tibble(
  r=c(10,4.5),
  x=c(0,20),
  y=c(0,-5),
  area=pi*r^2,
  width=sqrt(area)
)

library(ggforce)
circles <- temp |>
  ggplot()+
  geom_circle(aes(x0=x,y0=y, r=r), fill=site_colours$primary, colour=site_colours$primary, alpha=.7)+
  coord_equal()+
  theme(axis.text = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
        panel.grid = element_blank())

ggsave(filename=here("figs","03","circles.png"), plot=circles,width=5, height=3.5, dpi=300)
ggsave(filename=here("figs","03","circles.svg"), plot=circles,width=5, height=3.5)


rectangles <- ggplot()+
  geom_rect(aes(xmin=0,ymin=0, xmax=17.7, ymax=17.7), fill=site_colours$primary, colour=site_colours$primary, alpha=.7)+
  geom_rect(aes(xmin=23,ymin=0, xmax=30.98, ymax=7.98), fill=site_colours$primary, colour=site_colours$primary, alpha=.7)+
  coord_equal() +
  theme(axis.text = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.grid = element_blank())

ggsave(filename=here("figs","03","rectangles.png"), plot=rectangles,width=5, height=3.5, dpi=300)
ggsave(filename=here("figs","03","rectangles.svg"), plot=rectangles,width=5, height=3.5)




bars <- ggplot()+
  geom_rect(aes(xmin=0,ymin=0, xmax=6.36, ymax=4), fill=site_colours$primary, colour=site_colours$primary, alpha=.7)+
  geom_rect(aes(xmin=0,ymin=7, xmax=31.4, ymax=11), fill=site_colours$primary, colour=site_colours$primary, alpha=.7)+
  scale_x_continuous(limits=c(0,31.4))+
  coord_equal() +
  theme(axis.text = element_blank(), panel.grid = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) 

ggsave(filename=here("figs","03","bars.png"), plot=bars,width=5, height=3.5, dpi=300)
ggsave(filename=here("figs","03","bars.svg"), plot=bars,width=5, height=3.5)

  
power_laws | (bars / circles / rectangles / plot_spacer())  + plot_layout(widths=c(5,1))

ggsave(filename=here("figs","03","perception.svg"), plot=plot,width=7, height=3.5)

plot <- power_laws +
  annotation_custom(ggplotGrob(bars), xmin=6.5, xmax=9.2, ymin=4.1, ymax=6.0) +
  annotation_custom(ggplotGrob(circles), xmin=6.0, xmax=9.5, ymin=2.8, ymax=4.5) +
  annotation_custom(ggplotGrob(rectangles), xmin=6.0, xmax=9.5, ymin=1.3, ymax=3)

gb +
  annotation_custom(
    grob=ggplotGrob(london),
    xmin=unname(uk_bbox$xmax +1*london_width),
    xmax=unname(uk_bbox$xmax) + 6*london_width,
    ymin=unname(uk_bbox$ymin) +1*london_height,
    ymax=unname(uk_bbox$ymin) + 6*london_height
  )

# Number of constituencies won by party.
bes_2019 |>
  group_by(winner_19) |>
  summarise(count=n()) |>
  arrange(desc(count))

# Share of vote by party.
bes_2019 |>
  select(constituency_name, total_vote_19, con_vote_19:alliance_vote_19) |>
  pivot_longer(cols=con_vote_19:alliance_vote_19, names_to="party", values_to="votes") |>
  mutate(party=str_extract(party, "[^_]+")) |>
  group_by(party) |>
  summarise(vote_share=sum(votes, na.rm=TRUE)/sum(total_vote_19)) |>
  arrange(desc(vote_share))


#-----------------------------------------
# 3. Techniques graphics
#-----------------------------------------


data_gb <- bes_2019 |>
  filter(region!="Northern Ireland") |>
  mutate(
    swing_con_lab=0.5*((con_19-con_17)-(lab_19-lab_17)),
    # Recode to 0 Chorley incoming speaker,Buckingham outgoing speaker --  uncontested seat.
    swing_con_lab=if_else(constituency_name %in% c("Chorley", "Buckingham"),0,swing_con_lab)
  )

data_gb  |> 
  summarise(
    min_swing=min(swing_con_lab),
    max_swing=max(swing_con_lab),
    median_swing=median(swing_con_lab),
    num_swing=sum(swing_con_lab>0),
    num_landslide_con=sum(con_19>50, na.rm=TRUE),
    num_landslide_lab=sum(lab_19>50, na.rm=TRUE)
  )


hist_1 <- data_gb |>
  ggplot() +
  geom_histogram(mapping=aes(swing_con_lab)) +
  labs() 

hist_2 <- data_gb |>
  ggplot(mapping=aes(swing_con_lab)) +
  geom_histogram(fill="#003c8f") +
  labs(x="Swing", y="count")

hist_3 <- data_gb |>
  mutate(
    region=case_when(
      region == "Yorkshire and The Humber" ~ "Yorks and Humber",
      TRUE ~ region)
    ) |> 
  ggplot(mapping=aes(swing_con_lab)) +
  geom_histogram(fill="#003c8f") +
  geom_vline(xintercept=4.44, size=.3)+
  labs(x="Swing", y="count")+
  facet_wrap(~region)

plot <- (hist_1 + hist_2)
ggsave(filename=here("figs", "03", "hist.png"), plot=plot,width=7, height=3.2, dpi=300)
ggsave(filename=here("figs", "03", "hist.svg"), plot=plot,width=8, height=3.5)
ggsave(filename=here("figs", "03", "hist-region.png"), plot=hist_3,width=7.5, height=5, dpi=300)
ggsave(filename=here("figs", "03", "hist-region.svg"), plot=hist_3,width=9.5, height=6)


# Share of vote by party.
party_shares <- data_gb |>
  select(constituency_name, region, total_vote_19, con_vote_19:alliance_vote_19) |>
  pivot_longer(cols=con_vote_19:alliance_vote_19, names_to="party", values_to="votes") |>
  mutate(party=str_extract(party, "[^_]+")) |>
  group_by(party) |>
  summarise(vote_share=sum(votes, na.rm=TRUE)/sum(total_vote_19)) |>
  filter(vote_share>0) |>
  ggplot(aes(x=reorder(party, -vote_share), y=vote_share)) +
  geom_col(fill="#003c8f") +
  labs(
    #title="Vote share by party in GB",
    #subtitle="-- 2019 UK General Election",
    #caption="Data published by House of Commons Library, accessed via `parlitools`",
    x="party", y="vote share"
  )


party_shares_rotate <- data_gb |>
  select(constituency_name, region, total_vote_19, con_vote_19:alliance_vote_19) |>
  pivot_longer(cols=con_vote_19:alliance_vote_19, names_to="party", values_to="votes") |>
  mutate(party=str_extract(party, "[^_]+")) |>
  group_by(party) |>
  summarise(vote_share=sum(votes, na.rm=TRUE)/sum(total_vote_19)) |>
  filter(vote_share>0) |>
  ggplot(aes(x=reorder(party, vote_share), y=vote_share)) +
  geom_col(fill="#003c8f") +
  coord_flip() +
  labs(
    # title="Vote share by party in GB",
    # subtitle="-- 2019 UK General Election",
    # caption="Data published by House of Commons Library, accessed via `parlitools`",
    x="party", y="vote share"
  )

party_shares_colour <- temp_party_shares |>
  ggplot(aes(x=reorder(party, vote_share), y=vote_share)) +
  geom_col(aes(fill=party)) +
  coord_flip() +
  labs(
    # title="Vote share by party in GB",
    # subtitle="-- 2019 UK General Election",
    # caption="Data published by House of Commons Library, accessed via `parlitools`",
    x="party", y="vote share"
  )+
  scale_fill_manual(values=party_colours)

temp_party_shares <- data_gb |>
  select(constituency_name, region, total_vote_19, con_vote_19:alliance_vote_19) |>
  pivot_longer(cols=con_vote_19:alliance_vote_19, names_to="party", values_to="votes") |>
  mutate(party=str_extract(party, "[^_]+")) |>
  group_by(party) |>
  summarise(vote_share=sum(votes, na.rm=TRUE)/sum(total_vote_19)) |>
  filter(vote_share>0) |>
  mutate(party=factor(party, levels=c("con", "lab", "ld", "snp", "green", "brexit", "pc")))

plot <- party_shares + party_shares_rotate
ggsave(filename=here("figs", "03", "bars.png"), plot=plot,width=7, height=3, dpi=300)
ggsave(filename=here("figs", "03", "bars.svg"), plot=plot,width=7, height=3)

temp_party_shares_region <- data_gb |>
  select(constituency_name, region, total_vote_19, con_vote_19:alliance_vote_19) |>
  pivot_longer(cols=con_vote_19:alliance_vote_19, names_to="party", values_to="votes") |>
  mutate(party=str_extract(party, "[^_]+")) |>
  group_by(party, region) |>
  summarise(vote_share=sum(votes, na.rm=TRUE)/sum(total_vote_19)) |>
  filter(party %in% c("con", "lab", "ld", "snp", "green", "brexit", "pc")) |>
  mutate(party=factor(party, levels=c("con", "lab", "ld", "snp", "green", "brexit", "pc")))

# Con :
con <- "#0575c9"
# Lab :
lab <- "#ed1e0e"
# Lib dem :
ld <- "#fe8300"
# SNP :
snp <- "#ebc31c"
# Greens :
green <- "#78c31e"
# Plaid
pc <- "#4e9f2f"
# Brexit
brexit <- "#25b6ce"
# Other : recode brexit to other as they didn't win a seat.
other <- "#bdbdbd"

party_colours <- c(con, lab, ld, snp, green, brexit, pc)
names(party_colours) <- levels(temp_party_shares$party)

party_shares_region <- temp_party_shares_region |>
  mutate(
    region=case_when(
      region == "Yorkshire and The Humber" ~ "Yorks and Humber",
      TRUE ~ region)
  ) |> 
  ggplot(aes(x=reorder(party, vote_share), y=vote_share)) +
  geom_col(aes(fill=party)) +
  scale_fill_manual(values=party_colours) +
  coord_flip() +
  labs(
   # title="Vote share by party in GB, grouped by Region",
  #  subtitle="-- 2019 UK General Election",
   # caption="Data published by House of Commons Library, accessed via `parlitools`",
    x="party", y="vote share"
  )+
  facet_wrap(~region) +
  #guides(fill = guide_legend(nrow = 1)) +
  theme(
    axis.text.x=element_blank(), 
    axis.line.x = element_blank(), 
    legend.position = "right")

ggsave(filename=here("figs", "03", "bars-region.png"), plot=party_shares_region,width=8.2, height=5.5, dpi=300)
ggsave(filename=here("figs", "03", "bars-region.svg"), plot=party_shares_region,width=10, height=8)

plot_scatters <- data_gb |>
  mutate(winner_19=case_when(
    winner_19 == "Conservative" ~ "Conservative",
    winner_19 == "Labour" ~ "Labour",
    TRUE ~ "Other"
  )) |>
  ggplot(aes(x=con_17, y=con_19)) +
  geom_point(aes(colour=winner_19), alpha=.8) +
  geom_abline(intercept = 0, slope = 1) +
  scale_colour_manual(values=c(con,lab,other)) 


plot_scatters_con <- data_gb |>
  mutate(is_flipped=seat_change_1719=="Conservative gain from Labour",
         is_flipped=if_else(is.na(is_flipped), FALSE, is_flipped),
         winner_19=case_when(
           winner_19 == "Conservative" ~ "Conservative",
           winner_19 == "Labour" ~ "Labour",
           TRUE ~ "Other"
         )) |>
  ggplot(aes(x=con_17, y=con_19)) +
  geom_point(aes(colour=winner_19, alpha=is_flipped, shape=is_flipped)) +
  geom_abline(intercept = 0, slope = 1, size=.3) +
  scale_colour_manual(values=c(con,lab,other)) +
  scale_alpha_ordinal(range=c(.5,1)) +
  scale_shape_manual(values=c(21,19)) +
  scale_x_continuous(limits=c(0,90)) +
  annotate("text", x=20, y=60, label="Con increase on\n 2017 vote share", hjust=0.5, size=3.8, colour="#525252") +
  annotate("text", x=76, y=35, label="Con decrease on\n 2019 vote share", hjust=0.5, size=3.8, colour="#525252") +
  labs(x="vote share 2017 ", y="vote share 2019") +
  guides(fill=FALSE, alpha=FALSE, shape=FALSE, colour=FALSE) 

# plot_scatters_lab <- data_gb |>
#   mutate(is_flipped=seat_change_1719=="Conservative gain from Labour",
#          is_flipped=if_else(is.na(is_flipped), FALSE, is_flipped),
#          winner_19=case_when(
#            winner_19 == "Conservative" ~ "Conservative",
#            winner_19 == "Labour" ~ "Labour",
#            TRUE ~ "Other"
#          )) |>
#   ggplot(aes(x=lab_17, y=lab_19)) +
#   geom_point(aes(colour=winner_19, alpha=is_flipped, shape=is_flipped)) +
#   geom_abline(intercept=0, slope=1, size=.3) +
#   annotate("text", x=40, y=90, label="Labour", hjust=0.5, size=4.5) +
#   annotate("text", x=20, y=60, label="vote share > than 2019", hjust=0.5, size=3.5) +
#   annotate("text", x=80, y=20, label="vote share < than 2019", hjust=0.5, size=3.5) +
#   scale_colour_manual(values=c(con,lab,other)) +
#   scale_alpha_ordinal(range=c(.5,1)) +
#   scale_x_continuous(limits=c(0,90)) +
#   guides(fill=FALSE, alpha=FALSE, shape=FALSE, colour=FALSE) +
#   labs(x="vote share 2017 ", y="vote share 2019")

plot <- plot_scatters_con  

ggsave(filename=here("figs", "03", "scatters-con.png"), plot=plot,width=6, height=4.5, dpi=300)



plot <- data_gb |>
  mutate(is_flipped=seat_change_1719=="Conservative gain from Labour",
         is_flipped=if_else(is.na(is_flipped), FALSE, is_flipped),
         winner_19=case_when(
           winner_19 == "Conservative" ~ "Conservative",
           winner_19 == "Labour" ~ "Labour",
           TRUE ~ "Other"
         )) |>
  ggplot(aes(x=lab_17, y=lab_19)) +
  geom_point(aes(colour=winner_19, alpha=is_flipped, shape=is_flipped)) +
  geom_abline(intercept=0, slope=1, size=.3) +
  #annotate("text", x=40, y=90, label="Labour", hjust=0.5, size=4.5) +
  annotate("text", x=20, y=60, label="vote share > than 2019", hjust=0.5, size=3.5) +
  annotate("text", x=80, y=20, label="vote share < than 2019", hjust=0.5, size=3.5) +
  scale_colour_manual(values=c(con,lab,other)) +
  scale_alpha_ordinal(range=c(.5,1)) +
  scale_x_continuous(limits=c(0,90)) +
  guides(fill=FALSE, alpha=FALSE, shape=FALSE, colour=FALSE) +
  labs(x="vote share 2017 ", y="vote share 2019", title="Labour vote share in 2019 against Labour vote share in 2017",
       subtitle="-- Constituencies in Great Britain.",
       caption="Data published by House of Commons Library, accessed via `parlitools`")

ggsave(filename=here("figs", "03", "scatters-lab.png"), plot=plot,width=9, height=5, dpi=300)
ggsave(filename=here("figs", "03", "scatters-lab.svg"), plot=plot,width=9, height=5)

plot_scatters_region <- data_gb |>
  mutate(is_flipped=seat_change_1719=="Conservative gain from Labour",
         is_flipped=if_else(is.na(is_flipped), FALSE, is_flipped),
         winner_19=case_when(
           winner_19 == "Conservative" ~ "Conservative",
           winner_19 == "Labour" ~ "Labour",
           TRUE ~ "Other"
         )) |>
  ggplot(aes(x=con_17, y=con_19)) +
  geom_point(aes(colour=winner_19, fill=winner_19, alpha=is_flipped, shape=is_flipped)) +
  geom_abline(intercept=0, slope=1, size=.2) +
  scale_colour_manual(values=c(con,lab,other)) +
  scale_fill_manual(values=c(con,lab,other)) +
  scale_alpha_ordinal(range=c(.5,1)) +
  facet_wrap(~region) +
  labs(
    title="Conservative vote share in 2019 against Conservative vote share in 2017, grouped by Region",
    subtitle="-- Constituencies in Great Britain.",
    caption="Data published by House of Commons Library, accessed via `parlitools`",
    x="Conservative vote share 2017 ", y="Conservative vote share 2019"
  )

plot <- plot_scatters_emph

ggsave(filename=here("figs", "03", "scatters-region.png"), plot=plot_scatters_region,width=11, height=7, dpi=300)
ggsave(filename=here("figs", "03", "scatters-region.svg"), plot=plot_scatters_region,width=11, height=7)


constituency_boundaries <- constituency_boundaries |>  filter(pcon17nm!="Orkney and Shetland") |> 
  rmapshaper::ms_simplify(keep=.9)

data_gb <- constituency_boundaries |>
  inner_join(data_gb, by=c("pcon17cd"="ons_const_id"))

class(data_gb)

data_gb <- data_gb |>
  mutate(
    winner_19=if_else(winner_19=="Speaker", "Other", winner_19),
    winner_19=as_factor(winner_19))


party_colours <- c(con, lab, ld, green, other, snp, pc)
names(party_colours) <- levels(data_gb$winner_19)


map_1 <- data_gb |> 
  mutate(
    winner_19=factor(winner_19, 
                     levels=c("Conservative", "Labour", "Liberal Democrat","Green", "Scottish National Party", 
                              "Plaid Cymru", "Other")
                     )
    ) |> 
  ggplot() +
  geom_sf(aes(fill=winner_19), colour="#eeeeee", alpha=.7, linewidth=0.001)+
  coord_sf(crs=27700, datum=NA) +
  guides(fill="none") +
  scale_fill_manual(values=party_colours)


party_colours <- c(con, lab, ld, green, snp, pc, other)
#names(party_colours) <- levels(temp_party_shares$party)

map_2 <- data_gb |> 
  mutate(
    winner_19=case_when(
      winner_19== "Conservative" ~ "Conservative",
      winner_19== "Labour" ~ "Labour",
      winner_19== "Liberal Democrat" ~  "Lib Dems",
      winner_19== "Green" ~  "Green",
      winner_19== "Scottish National Party" ~  "SNP",
      winner_19== "Plaid Cymru" ~  "Plaid",
      winner_19== "Other" ~  "Other"
      ),
    winner_19=factor(winner_19, 
                     levels=c("Conservative", "Labour", "Lib Dems","Green", "SNP", 
                              "Plaid", "Other")
    )
  ) |> 
  ggplot() +
  geom_sf(aes(fill=winner_19), alpha=.7, colour="#eeeeee", linewidth=0.001)+
  geom_sf(data=. %>% group_by(region) %>% summarise(), colour="#eeeeee", fill="transparent", linewidth=0.1)+
  coord_sf(crs=27700, datum=NA) +
  labs(fill="") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values=party_colours) +
  guides(fill="none") +
  guides(fill=guide_legend(nrow = 1)) +
  theme(
    legend.title = element_text(size=8), 
    legend.text = element_text(size=6.8), 
    legend.key.size = unit(.2, 'cm'))

legend <- ggpubr::get_legend(map_2)

ggpubr::as_ggplot(legend)

map_3 <- data_gb |> 
  mutate(
    winner_19=factor(winner_19, 
                     levels=c("Conservative", "Labour", "Liberal Democrat","Green", "Scottish National Party", 
                              "Plaid Cymru", "Other")
    )
  ) |> 
  ggplot() +
  geom_sf(aes(fill=winner_19, alpha=swing_con_lab), colour="#eeeeee", linewidth=0.001)+
  geom_sf(data=. %>% group_by(region) %>% summarise(), colour="#eeeeee", fill="transparent", linewidth=0.1)+
  coord_sf(crs=27700, datum=NA) +
  labs(fill="") +
  theme(legend.position = "right") +
  guides(alpha="none", fill="none") +
  scale_fill_manual(values=party_colours)

plot <- (map_1 + map_2 + map_3) / ggpubr::as_ggplot(legend) +
  plot_layout(heights=c(1,.05))


ggsave(here("figs", "03", "map-winners.png"), plot=plot,width=6, height=3.5, dpi=300)
ggsave(here("figs", "03", "map-winners.svg"), plot=plot,width=8, height=6)


max_shift <- max(abs(data_gb$swing_con_lab))
min_shift <- -max_shift
# Calculate bounding boxes for use in annotation_custom().
london_bbox <- st_bbox(data_gb |> filter(region=="London"))
london_width <- unname(london_bbox$xmax)-unname(london_bbox$xmin)
london_height <- unname(london_bbox$ymax)-unname(london_bbox$ymin)
london_aspect <- london_width/london_height
uk_bbox <- st_bbox(data_gb)
uk_width <- unname(uk_bbox$xmax)-unname(uk_bbox$xmin)
uk_height <- unname(uk_bbox$ymax)-unname(uk_bbox$ymin)

# Annotate constituencies that *really* defied expectation (discussed in the EPA paper).
bassetlaw <- data_gb |> filter(constituency_name == "Bassetlaw")
redcar <-  data_gb |> filter(constituency_name == "Redcar")
sedgefield <- data_gb |> filter(constituency_name == "Sedgefield")
stoke <- data_gb |> filter(constituency_name == "Stoke-On-Trent Central")

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
position_centre_spoke <- function() PositionCentreSpoke
PositionCentreSpoke <-
  ggplot2::ggproto('PositionCentreSpoke', ggplot2::Position,
                   compute_panel = function(self, data, params, scales)
                   {
                     data$x <- 2*data$x - data$xend
                     data$y <- 2*data$y - data$yend
                     data$radius <- 2*data$radius
                     data
                   }
  )


party_colours <- c(con, lab, other)
names(party_colours) <- c("Conservative", "Labour", "Other")

gb <- data_gb |>
  filter(region!="London") |>
  mutate(is_flipped=seat_change_1719 %in% c("Conservative gain from Labour","Labour gain from Conservative"),
         is_flipped=if_else(is.na(is_flipped), FALSE, is_flipped),
         elected=if_else(!winner_19 %in% c("Conservative", "Labour"), "Other", as.character(winner_19))
  ) |>
  ggplot()+
  geom_sf(aes(fill=elected), colour="#ffffff", alpha=0.2, linewidth=0.01)+
  geom_sf(data=. %>% group_by(region) %>% summarise(), colour="#ffffff", fill="transparent", linewidth=0.2)+
  geom_sf(data=. %>% summarise(), colour="#636363", fill="transparent", linewidth=0.05)+
  coord_sf(crs=27700, datum=NA,
           xlim = c(unname(uk_bbox$xmin)-.9*uk_width, unname(uk_bbox$xmax)+0*london_width),
           ylim = c(unname(uk_bbox$ymin), unname(uk_bbox$ymax)-0*uk_height)
  )+
  geom_spoke(
    aes(x=bng_e, y=bng_n, angle=get_radians(map_scale(swing_con_lab,min_shift,max_shift,135,45)), colour=elected, size=is_flipped),
    radius=7000, position="centre_spoke", lineend="round"
  )+
  scale_size_ordinal(range=c(.3,.9))+
  scale_colour_manual(values=party_colours)+
  scale_fill_manual(values=party_colours)+
  annotate(geom="segment", xend=bassetlaw$bng_e, yend=bassetlaw$bng_n, x=bassetlaw$bng_e+0.15*uk_width, y=bassetlaw$bng_n, size=.2)+
  annotate(geom="text", x=bassetlaw$bng_e+0.16*uk_width, y=bassetlaw$bng_n, hjust="left", label=paste0(bassetlaw$constituency_name), family="Avenir Next", size=3.5)+
  annotate(geom="segment", xend=sedgefield$bng_e, yend=sedgefield$bng_n, x=sedgefield$bng_e+0.06*uk_width, y=sedgefield$bng_n+0.02*uk_height, size=.2)+
  annotate(geom="text", x=sedgefield$bng_e+0.07*uk_width, y=sedgefield$bng_n+0.02*uk_height, hjust="left", label=paste0(sedgefield$constituency_name), family="Avenir Next", size=3.5)+
  annotate(geom="segment", xend=redcar$bng_e, yend=redcar$bng_n, x=redcar$bng_e+0.05*uk_width, y=redcar$bng_n, size=.2)+
  annotate(geom="text", x=redcar$bng_e+0.06*uk_width, y=redcar$bng_n, hjust="left", label=paste0(redcar$constituency_name), family="Avenir Next", size=3.5)+
  annotate(geom="segment", xend=stoke$bng_e, yend=stoke$bng_n, x=stoke$bng_e-.15*uk_width, y=stoke$bng_n+0.05*uk_height, size=.2)+
  annotate(geom="text", x=stoke$bng_e-0.16*uk_width, y=stoke$bng_n+0.05*uk_height, hjust="right", label=paste0(stoke$constituency_name), family="Avenir Next", size=3.5)+
  guides(colour=FALSE, fill=FALSE, size=FALSE)+
  #theme_v_gds() +
  theme(axis.title.x= element_blank(), axis.title.y= element_blank())


data_gb |>
  mutate(
    is_flipped=seat_change_1719 %in% c("Conservative gain from Labour","Labour gain from Conservative"),
    elected=if_else(!winner_19 %in% c("Conservative", "Labour"), "Other", as.character(winner_19))
  ) |>
  ggplot()+
  geom_sf(aes(fill=elected), colour="#636363", alpha=.2, linewidth=.01)+
  geom_sf(data=. %>% group_by(region) %>% summarise(), colour="#636363", fill="transparent", linewidth=0.08)+
  geom_spoke(
    aes(x=bng_e, y=bng_n, angle=get_radians(map_scale(swing_con_lab,min_shift,max_shift,135,45)), colour=elected, size=is_flipped),
    radius=7000, position="centre_spoke", lineend="round"
  ) +
  coord_sf(crs=27700, datum=NA)+
  scale_size_ordinal(range=c(.3,.9))+
  scale_colour_manual(values=party_colours)+
  scale_fill_manual(values=party_colours)

london <- data_gb |>
  filter(region=="London") |>
  mutate(is_flipped=seat_change_1719 %in% c("Conservative gain from Labour","Labour gain from Conservative"),
         is_flipped=if_else(is.na(is_flipped), FALSE, is_flipped),
         elected=if_else(!winner_19 %in% c("Conservative", "Labour"), "Other", as.character(winner_19))
  ) |>
  ggplot()+
  geom_sf(aes(fill=elected), colour="#ffffff", alpha=0.2, linewidth=0.01)+
  geom_sf(data=. %>% group_by(region) %>% summarise(), colour="#ffffff", fill="transparent", linewidth=0.2)+
  coord_sf(datum=NA)+
  geom_spoke(
    aes(x=bng_e, y=bng_n, angle=get_radians(map_scale(swing_con_lab,min_shift,max_shift,135,45)), colour=elected, size=is_flipped),
    radius=7000/5, position="centre_spoke", lineend="round"
  )+
  scale_size_ordinal(range=c(.3,.9))+
  scale_colour_manual(values=party_colours)+
  scale_fill_manual(values=party_colours)+
  guides(colour=FALSE, fill=FALSE, size=FALSE)+
  #theme_v_gds() +
  theme(axis.title.x= element_blank(), axis.title.y= element_blank())



# Use of angle to encode swing.
swing <-  ggplot()+
  geom_spoke(aes(x=0, y=-.35,angle=get_radians(90)),radius=0.55, size=0.2, colour="#636363", lineend="round")+
  geom_spoke(aes(x=0, y=-.35,angle=get_radians(135)),radius=0.55, size=0.2,colour="#636363", linetype = "dashed", lineend="round")+
  geom_spoke(aes(x=0, y=-.35,angle=get_radians(45)),radius=0.55,size=0.2,colour="#636363",linetype = "dashed", lineend="round")+
  geom_text(aes(label="+18% to \n Con",x=.5, y=0), angle=45,hjust="right", family="Avenir Next", size=2.5, colour="#636363")+
  geom_text(aes(label="+18% to \n Lab",x=-.5, y=0), angle=315,hjust="left", family="Avenir Next", size=2.5, colour="#636363")+
  geom_curve(aes(x=-.04, y=.2, xend=-.3, yend=.08), size=0.3, curvature = 0.2, arrow=arrow(type="closed", length = unit(.03, "inches")), colour="#636363")+
  geom_curve(aes(x=.04, y=.2, xend=.3, yend=.08), size=0.3, curvature = -0.2, arrow=arrow(type="closed", length = unit(.03, "inches")), colour="#636363")+
  xlim(-0.5,0.5)+
  ylim(-0.35,0.35)+
  coord_equal() +
  theme_void() +
#theme_v_gds() +
  theme(axis.title.x= element_blank(), axis.title.y= element_blank(), axis.text = element_blank())
# Use colour to encode party.
temp_dat <-tibble(
  elected=names(party_colours),
  y=c(3,2,1),
  x=c(1,1,1)
)

# Use thickness to show flips.
line <-  ggplot()+
  geom_spoke(aes(x=-0.2, y=-.35,angle=get_radians(90)),radius=0.55, size=0.2, lineend="round")+
  geom_spoke(aes(x=0.2, y=-.35,angle=get_radians(90)),radius=0.55, size=0.8, lineend="round")+
  xlim(-0.5,0.5)+
  ylim(-0.35,0.35)+
  theme_void()

# Party colours for legend
party <- temp_dat |>
  mutate(elected=if_else(elected=="Conservative", "Con", elected)) |> 
  mutate(y=map_scale(y, 1,3,3,1)) |> 
  ggplot()+
  geom_spoke(aes(x=y, y=x,angle=get_radians(90), colour=elected),radius=0.7, size=1, lineend="round")+
  scale_colour_manual(values=party_colours)+
  geom_text(aes(label=elected,x=y+0.08, y=x+0.2),hjust="left",vjust="middle", family="Avenir Book", size=3, colour="#636363")+
  guides(colour=FALSE)+
  xlim(1,3.5)+
  ylim(1,1.7)+
  theme_void()


legend <- ggplot()+
  #geom_text(aes(label="Each constituency is a line",x=0, y=6.2), hjust="left", vjust="top", family="Avenir Medium", size=4.5)+
  geom_text(aes(label="Colour hue -- \n winning party",x=0.1, y=5), hjust="left", vjust="top", family="Avenir Next", size=3.5)+
  geom_text(aes(label="Thick line -- \n constituency flipped \n winning party",x=.1, y=3.3), hjust="left", vjust="top", family="Avenir Next", size=3.5)+
  geom_text(aes(label="Line angle -- \n Butler % swing \n in vote share",x=.1, y=.5), hjust="left", vjust="top", family="Avenir Next", size=3.5)+
  annotation_custom(grob=ggplotGrob(swing),xmin=1.5,xmax=3.6,ymin=-1.2,ymax=1.5)+
  annotation_custom(ggplotGrob(line),xmin=1.5,xmax=3.6,ymin=2,ymax=3.5)+
  annotation_custom(ggplotGrob(party),xmin=1.8,xmax=4,ymin=4.3,ymax=5.3)+
  xlim(0,4)+
  ylim(-1,6.25) +
  theme_void() 




# Assemble with annotation_custom.
map <- gb +
  annotation_custom(
    grob=ggplotGrob(london),
    xmin=unname(uk_bbox$xmin + 2*london_width),
    xmax=unname(uk_bbox$xmin) - 3*london_width,
    ymin=unname(uk_bbox$ymin) +1*london_height,
    ymax=unname(uk_bbox$ymin) + 6*london_height
  ) +
  annotation_custom(
    grob=ggplotGrob(legend),
    xmin=unname(uk_bbox$xmin -1*uk_width),
    xmax=unname(uk_bbox$xmin -.1*uk_width),
    ymin=unname(uk_bbox$ymax) -.4*uk_height,
    ymax=unname(uk_bbox$ymax) -0*uk_height
  ) 
  
ggsave(filename=here("figs", "03", "spoke-map.png"), plot=map,width=8, height=7, dpi=300)

ggsave(filename="./static/class/03-class_files/spoke-map.png", plot=map,width=11, height=7, dpi=300)



# Map Swing

plot <- data_gb |>
  mutate(margin_leave=leave_hanretty-50) |>
  ggplot() +
  geom_sf(aes(fill=margin_leave), colour="#eeeeee", size=0.01)+
  geom_sf(data=. |> group_by(region) |> summarise(), colour="#636363", fill="transparent", size=0.04)+
  geom_sf(data=. |> summarise(), colour="#636363", fill="transparent", size=0.08)+
  coord_sf(crs=27700, datum=NA) +
  guides(fill=guide_legend(title="Majority Leave:Remain")) +
  theme_v_gds() +
  theme(legend.position = "right") +
  scale_fill_distiller(palette="BrBG", limits=c(-30,30), labels=c("Heavy Remain", "", "", "No majority", "","",  "Heavy Leave")) +
  labs(
    title="Vote margin for Leave:Remain in Great Britain",
    subtitle="-- Estimated by Constituency via Hanretty 2017.",
    caption="Data published by House of Commons Library, accessed via `parlitools`"
  )

ggsave(filename=here("figs","03","referendum-map.png"), plot=plot,width=6.5, height=7, dpi=300)
ggsave(filename=here("figs","03","referendum-map.svg"), plot=plot,width=6.5, height=7)
