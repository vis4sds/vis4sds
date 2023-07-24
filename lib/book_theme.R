###############################################################################
# ggplot2 theme for book
###############################################################################

update_geom_defaults("label", list(family = "Avenir Book"))
update_geom_defaults("text", list(family = "Avenir Book"))

theme_v_sds <- function(base_size = 14, base_family = "Avenir Book") {
  return <- theme_classic(base_size, base_family) +
    theme(plot.title = element_text(size = rel(1.2),
                                    family = "Avenir Medium"),
          plot.subtitle = element_text(size = rel(1.1),
                                       family = "Avenir Roman"),
          plot.caption = element_text(size = rel(.8), color = "grey50",
                                      family = "Avenir Book",
                                      margin = margin(t = 10)),
          plot.tag = element_text(size = rel(.9), color = "grey50",
                                  family = "Avenir Book"),
          strip.text = element_text(size = rel(.9),
                                    family = "Avenir Book"),
          strip.text.x = element_text(margin = margin(t = 1, b = 1)),
          panel.border = element_blank(),
          strip.background = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.line = element_line(linewidth = .2),
          legend.title = element_text(size = rel(0.8)),
          legend.position = "bottom")
  
  return
}

# Set ggplot2 theme
theme_set(theme_v_sds())

