###############################################################################
# Figures for vis4sds 
# Chapter 1
# Author: Roger Beecham
###############################################################################

library(tidyverse) 
library(here) 
library(patchwork)

# Plot Anscombe's quartet.
plot <- anscombe |> 
  pivot_longer(cols=everything(), names_to="var", values_to="value") |> arrange(var) |>  
  add_column(var_type=c(rep("x",44),rep("y",44)), row_index=rep(1:44,2)) |> 
  mutate(dataset=paste("dataset",str_sub(var,2,2))) |> 
  select(-var) |> 
  pivot_wider(names_from=var_type, values_from=value) |> 
  ggplot(aes(x, y))+
  geom_point(colour=site_colours$primary, fill=site_colours$primary, pch=21) +
  stat_smooth(method=lm, se=FALSE, size=0.6, colour="#636363")+
  annotate("segment", x=9, xend=9, y=2.5, yend=7.5, colour=site_colours$secondary, alpha=.5, size=.5)+
  annotate("segment", x=5, xend=9, y=7.5, yend=7.5, colour=site_colours$secondary, alpha=.5, size=.5)+
  annotate("text", label="mean - 9.00 ",
           vjust="centre", hjust="centre", family="Avenir Next",size=2.5,
           x=9, y=2, colour=site_colours$secondary)+
  annotate("text", label="variance  - 11.00 ",
           vjust="centre", hjust="centre", family="Avenir Next",size=2.5,
           x=9, y=1)+
  annotate("text", label="r. 0.82",
           vjust="centre", hjust="right", family="Avenir Next",size=2.5,
           x=18, y=7.5)+
  annotate("text", label="mean - 7.50 ",
           vjust="centre", hjust="right", family="Avenir Next",size=2.5,
           x=5, y=8, colour=site_colours$secondary)+
  annotate("text", label="variance  - 4.12 ",
           vjust="centre", hjust="right", family="Avenir Next",size=2.5,
           x=5, y=7)+
  facet_wrap(~dataset, nrow=2)+
  coord_equal(xlim = c(5, 20), ylim=c(3,13), # This focuses the x-axis on the range of interest
              clip = 'off')+
  theme(axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.spacing = unit(3, "lines"))

ggsave(filename=here("figs", "01", "anscombe_plot.svg"), plot=plot,width=7, height=5.5)

plot <- anscombe %>%
  gather(var, value) %>%
  add_column(var_type=c(rep("x",44),rep("y",44)), row_index=rep(1:44,2)) %>%
  mutate(dataset=paste("dataset",str_sub(var,2,2))) %>%
  select(-var) %>%
  spread(key=var_type, value=value) %>%
  ggplot(aes(x, y))+
  geom_point(colour="#003c8f", fill="#003c8f", pch=21) +
  stat_smooth(method=lm, se=FALSE, size=0.6, colour="#636363")+
  annotate("segment", x=9, xend=9, y=2.5, yend=7.5, colour="#003c8f", alpha=.5, size=.5)+
  annotate("segment", x=4.2, xend=9, y=7.5, yend=7.5, colour="#003c8f", alpha=.5, size=.5)+
  annotate("text", label="mean - 9.00 ",
           vjust="centre", hjust="centre", family="Avenir Book",size=2.5,
           x=9, y=2, colour="#d03231")+
  annotate("text", label="variance  - 11.00 ",
           vjust="centre", hjust="centre", family="Avenir Book",size=2.5,
           x=9, y=1)+
  annotate("text", label="correlation r.0.82",
           vjust="top", hjust="right", family="Avenir Book",size=2.5,
           x=18, y=4)+
  annotate("text", label="mean - 7.50 ",
           vjust="centre", hjust="right", family="Avenir Book",size=2.5,
           x=4, y=8, colour="#d03231")+
  annotate("text", label="variance  - 4.12 ",
           vjust="centre", hjust="right", family="Avenir Book",size=2.5,
           x=4, y=7)+
  facet_wrap(~dataset, nrow=2)+
  coord_equal(xlim = c(5, 20), ylim=c(3,13), # This focuses the x-axis on the range of interest
              clip = 'off') +
  theme(plot.margin = unit(c(1,1,1.5,2), "lines"),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size=10),
        panel.spacing = unit(3, "lines"))

ggsave(filename=here("figs", "01", "ancombe_plot.png"), plot=plot,width=7, height=4, dpi=500)


# Plot Anscombe's quartet data.
plot <- anscombe |> 
  pivot_longer(cols=everything(), names_to="var", values_to="value") |> arrange(var) |>  
  add_column(var_type=c(rep("x",44),rep("y",44)), row_index=rep(1:44,2)) |> 
  mutate(
    dataset=paste("dataset",str_sub(var,2,2)),
    xpos=as.numeric(str_sub(var,2,2))
    ) |> 
  select(-var) |> 
  pivot_wider(names_from=var_type, values_from=value) |> 
  group_by(dataset) |> 
  mutate(
    mean_y=round(mean(y),1), mean_x=format(round(mean(x),1), nsmall=1), var_x=round(var(x),1), var_y=round(var(y),1),
    cor=round(cor(x,y),1),
    ypos=row_number(), x=format(round(x),1, nsmall=1), y=round(y,1),
    ) |> 
  ungroup() |> 
  ggplot(aes(1, ypos))+
  geom_point(colour="#003c8f", fill="#003c8f", pch=21, size=3) +
  
  geom_text(data=. %>% filter(row_index %in% c(1,12,23,34)), aes(x=1-.5,y=12, label=paste0("x",xpos)), vjust=0, size=3) +
  geom_text(data=. %>% filter(row_index %in% c(1,12,23,34)), aes(x=1+.5,y=12, label=paste0("y",xpos)), vjust=0, size=3) +
  
  geom_text(data=. %>% filter(row_index %in% c(1,12,23,34)), aes(x=1-.5,y=-0.1, label=mean_x), vjust=0.5, size=3) +
  geom_text(data=. %>% filter(row_index %in% c(1,12,23,34)), aes(x=1+.5,y=-0.1, label=mean_y), vjust=0.5, size=3) +
  geom_text(data=. %>% filter(row_index %in% c(1,12,23,34)), aes(x=1-.5,y=-.8, label=var_x), vjust=0.5, size=3) +
  geom_text(data=. %>% filter(row_index %in% c(1,12,23,34)), aes(x=1+.5,y=-.8, label=var_y), vjust=0.5, size=3) +
  geom_text(data=. %>% filter(row_index %in% c(1,12,23,34)), aes(x=1,y=-1.5, label=cor), vjust=0.5, size=3) +
  
  
  geom_text(aes(x=1-.5,y=ypos, label=x), size=3) +
  geom_text(aes(x=1+.5,y=ypos, label=y), size=3) +
  geom_hline(yintercept=11.6, size=.35) +
  geom_hline(yintercept=.5, size=.2) +
  facet_wrap(~dataset, nrow=1) +
  scale_x_continuous(limits=c(0,2)) +
  
  scale_y_continuous(limits=c(-1.8,12), labels=c("mean","variance","cor r."), breaks=c(-.1, -.8, -1.5))+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=10),
    axis.title.y = element_blank(),
    strip.text = element_text(size=11),
    axis.title.x = element_blank(), axis.line = element_blank()
  )
  
ggsave(filename=here("figs", "01", "anscombe_data.png"), plot=plot,width=5.5, height=4.5, dpi=300)


rstudio <- image_read(here("figs", "01", "rstudio.png"))
image_write(rstudio, here("figs", "01", "rstudio_save.png"))
ratio <- 1692/2654

image <- image_fill(rstudio, 'none')
raster <- as.raster(image)

plot <- ggplot() +
  annotation_raster(raster, 0, 1, 0, 1*ratio) +
  annotate("text", x=-.015, y=.6, label="code editor\ndata viewer", size=3, hjust="right") +
  annotate("text", x=.97, y=.48, label="environment\n-- objects in session", size=3, hjust="right") +
  annotate("segment", x=-.005, y=.63, xend=-.005, yend=.565, linewidth=.2) +
  
  annotate("text", x=-.015, y=.22, label="assignment", size=3, hjust="right") +
  annotate("segment", x=-.011, y=.22, xend=.005, yend=.22, linewidth=.2) +
  
  annotate("text", x=-.015, y=.16, label="load package", size=3, hjust="right") +
  annotate("segment", x=-.011, y=.16, xend=.005, yend=.16, linewidth=.2) +
  
  annotate("text", x=-.015, y=.06, label="view data", size=3, hjust="right") +
  annotate("segment", x=-.011, y=.06, xend=.005, yend=.06, linewidth=.2) +
  
  annotate("text", x=.18, y=.04, label="query and\nuse function", size=3, hjust="left") +
  annotate("segment", x=.17, y=.03, xend=.17, yend=.06, linewidth=.2) +
  
  scale_x_continuous(limits=c(-.1, 1)) +
  scale_y_continuous(limits=c(0, 1*ratio)) +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

ggsave(here("figs", "01", "rstudio_save.png"), plot, dpi=700, width = 10.5, height=10*ratio)


# Annotate quarto screen in RSTudio.
quarto <- image_read(here("figs", "01", "quarto_save.png"))
image <- image_fill(quarto, 'none')
raster <- as.raster(image)

plot <- ggplot() +
  annotation_raster(raster, 0, 1, 0, 1*ratio) +
  
  annotate("text", x=.21, y=.55, label="YAML", size=3, hjust="left") +
  annotate("segment", x=.2, y=.53, xend=.2, yend=.56, linewidth=.2) +
  
  annotate("text", x=.3, y=.65, label="render document", size=3, hjust="left") +
  annotate("segment", x=.29, y=.64, xend=.25, yend=.62, linewidth=.2) +
  
  annotate("text", x=.5, y=.35, label="run code chunk", size=3, hjust="right") +
  annotate("segment", x=.51, y=.35, xend=.55, yend=.35, linewidth=.2) +
  
  annotate("text", x=.26, y=.25, label="heading", size=3, hjust="left") +
  annotate("segment", x=.25, y=.25, xend=.21, yend=.24, linewidth=.2) +
  
  annotate("text", x=.45, y=.09, label="link", size=3, hjust="left") +
  annotate("segment", x=.44, y=.095, xend=.4, yend=.111, linewidth=.2) +
  
  scale_x_continuous(limits=c(0, 1)) +
  scale_y_continuous(limits=c(0, 1*ratio+.1)) +
  
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

ggsave(here("figs", "01", "quarto_save.png"), plot, dpi=700, width = 9.5, height=10*ratio)

