###############################################################################
# Figures for vis4sds 
# Chapter 1
# Author: Roger Beecham
###############################################################################

library(tidyverse) 
library(here) 
library(patchwork)

###############################################################################
# C H  1
###############################################################################

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


plot <- plot +
  plot_annotation(
    title="Scatterplots of Anscombe's quartet", 
    subtitle="-- Annotated with traditional summary statistics", 
    caption="Data from {datasets} package"
  )
ggsave(filename=here("figs", "01", "anscombe_plot.svg"), plot=plot,width=7, height=5.5)



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
  geom_point(colour=site_colours$primary, fill=site_colours$primary, pch=21, size=3) +
  
  geom_text(data=. |> filter(row_index %in% c(1,12,23,34)), aes(x=1-.5,y=12, label=paste0("x",xpos)), vjust=0, size=3) +
  geom_text(data=. |> filter(row_index %in% c(1,12,23,34)), aes(x=1+.5,y=12, label=paste0("y",xpos)), vjust=0, size=3) +
  
  geom_text(data=. |> filter(row_index %in% c(1,12,23,34)), aes(x=1-.5,y=-0.1, label=mean_x), vjust=0.5, size=3) +
  geom_text(data=. |> filter(row_index %in% c(1,12,23,34)), aes(x=1+.5,y=-0.1, label=mean_y), vjust=0.5, size=3) +
  geom_text(data=. |> filter(row_index %in% c(1,12,23,34)), aes(x=1-.5,y=-.8, label=var_x), vjust=0.5, size=3) +
  geom_text(data=. |> filter(row_index %in% c(1,12,23,34)), aes(x=1+.5,y=-.8, label=var_y), vjust=0.5, size=3) +
  geom_text(data=. |> filter(row_index %in% c(1,12,23,34)), aes(x=1,y=-1.5, label=cor), vjust=0.5, size=3) +
  
  
  geom_text(aes(x=1-.5,y=ypos, label=x), size=3) +
  geom_text(aes(x=1+.5,y=ypos, label=y), size=3) +
  geom_hline(yintercept=11.6, size=.35) +
  geom_hline(yintercept=.5, size=.2) +
  facet_wrap(~dataset, nrow=1) +
  scale_x_continuous(limits=c(0,2))+
  
  scale_y_continuous(limits=c(-1.8,12), labels=c("mean","variance","cor r."), breaks=c(-.1, -.8, -1.5))+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=10),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )
  

plot <- plot +
  plot_annotation(
    title="Data from Anscombe's quartet", 
    subtitle="-- Each dot is an independent observation", 
    caption="Data from {datasets} package"
  ) 


ggsave(filename=here("figs", "01", "anscombe_data.svg"), plot=plot,width=7, height=5.5)

