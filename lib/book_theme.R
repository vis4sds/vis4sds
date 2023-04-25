###############################################################################
# T H E M E S
###############################################################################

site_colours <- list(
  primary = "#003c8f",
  primary_selected = "#1565c0",
  secondary = "#8e0000",
  secondary_selected = "#c62828"
)

update_geom_defaults("label", list(family = "Avenir Next"))
update_geom_defaults("text", list(family = "Avenir Next"))

theme_v_gds <- function(base_size = 14, base_family = "Avenir Next") {
  return <- theme_classic(base_size, base_family) +
    theme(plot.title = element_text(size = rel(1.2),
                                    family = "Avenir Next Demi Bold"),
          plot.subtitle = element_text(size = rel(1.1),
                                       family = "Avenir Next Medium"),
          plot.caption = element_text(size = rel(.8), color = "grey50",
                                      family = "Avenir Next",
                                      margin = margin(t = 10)),
          plot.tag = element_text(size = rel(.9), color = "grey50",
                                  family = "Avenir Next"),
          strip.text = element_text(size = rel(.9),
                                    family = "Avenir Next"),
          strip.text.x = element_text(margin = margin(t = 1, b = 1)),
          panel.border = element_blank(),
          strip.background = element_blank(),
          # plot.background = element_rect(fill="#eeeeee", colour = NA),
          axis.ticks = element_blank(),
          # panel.grid = element_line(colour="#e0e0e0"),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.line = element_line(linewidth = .2),
          #legend.margin = margin(t = 0),
          legend.title = element_text(size = rel(0.8)),
          legend.position = "bottom")
  
  return
}


# Set ggplot2 theme
theme_set(theme_v_gds())
