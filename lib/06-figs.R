# Filename: 02-figs.R 
#
# Figures for Chapter 2 of vis4sds 
# 
# Author: Roger Beecham
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


# install.packages("parlitools")
library(parlitools)
#install.packages("tidymodels")
library(tidymodels)
library(sf)
library(here)
library(jsonlite) 

source(here("lib", "book_theme.R"))

# ODI Hex layout for LADs
hex_data <- fromJSON(here("cons.hexjson"))  %>% as_tibble() %>%
  dplyr::select(-layout) %>%  mutate(cons_code=names(hexes)) %>%
  mutate(col_names=map(hexes, ~names(.x)),
         col_values=map(hexes, ~unname(.x))) %>% dplyr::select(-c(hexes)) %>%
  unnest(c(col_names, col_values)) %>%
  mutate(col_values=unlist(col_values)) %>%
  pivot_wider(names_from=col_names, values_from=col_values) %>%
  mutate(across(c(q,r), ~as.numeric(.x))) %>%
  mutate(row=r, col=q) %>%
  st_as_sf(coords = c("q","r"))

# Dimensions of hex
min_col <- min(hex_data$col)
max_col <- max(hex_data$col)
width <- max_col-min_col

min_row <- min(hex_data$row)
max_row <- max(hex_data$row)
height <- max_row-min_row

# Hexagon tesselation over points.
hex_grid <-
  st_sf(
    geom=st_make_grid(hex_data, n=c(width,height), what="polygons", square=FALSE, flat_topped=FALSE, cellsize=1)
  ) %>%
  mutate(id=row_number())

# Find centroids of hexagons.
hex_centroids <- hex_grid %>% st_centroid() %>% st_coordinates() %>% as_tibble() %>%
  rename("east"="X", "north"="Y")

# Add centroid locations to hex_grid object.
hex_grid <- hex_grid %>%
  add_column(hex_centroids %>% dplyr::select(east), hex_centroids %>% dplyr::select(north))

# Col positions. Find number of unique column positions.
col_positions <- hex_grid %>% st_drop_geometry() %>%  mutate(east=round(east,1)) %>% pull(east) %>% unique() %>%  length()

# Row positions. Find number of unique row positions.
row_positions <- hex_grid %>% st_drop_geometry() %>% pull(north) %>% unique() %>%  length()


# Rescaling function.
map_scale <- function(value, min1, max1, min2, max2) {
  return  (min2+(max2-min2)*((value-min1)/(max1-min1)))
}
# To match with hex_data rows must be integer positions.
hex_grid <- hex_grid %>%
  mutate(
    north_recode=
      floor(map_scale(dense_rank(north), 1, row_positions, min_row, min_row+row_positions)),
    
    north_recode=if_else(north_recode==36,35, north_recode)
    )


# Note that height should be 34 when in fact 40.
# Note that height should be 44 when in fact 52.
# Note that width should be 31 when in fact 63.
hex_grid |>  ggplot() + geom_sf() +
  geom_text(aes(x=east, y=north+.15, label=east), size=2) +
  geom_text( aes(x=east, y=north-.15, label=paste("r", north_recode)), size=2)

# Now transform column references depending on whether on an even/odd row.
hex_grid <- hex_grid |> 
  mutate(
    east_recode=if_else(north_recode %% 2 ==0, floor(round(east,2)), ceiling(round(east,2))))

hex_grid %>% ggplot() + geom_sf() +
  geom_text(aes(x=east, y=north+.15, label=east_recode), size=2) +
  geom_text( aes(x=east, y=north-.15, label=paste("r", north_recode)), size=2)

# And inner_join on hex_data in order to generate LA hexagon map.
hex_map <- hex_grid %>%
  inner_join(hex_data %>% st_drop_geometry() %>%
               dplyr::select(cons_code, col, row), by=c("east_recode"="col", "north_recode"="row"))


hex_map <- hex_data %>% st_drop_geometry() |> select(cons_code, col, row) |> 
  inner_join(hex_grid, by=c("col"="east_recode","row"="north_recode"))


hex_map |> ggplot() + geom_sf()

cons_hex <- hex_map |> inner_join(cons_data |> st_drop_geometry() |> select(pcon19cd, region), by=c("cons_code"="pcon19cd"))
st_write(cons_hex, here("../", "data", "ch6", "cons_hex.geojson"))
cons_hex <- st_read(here("../", "data", "ch6", "cons_hex.geojson"))


url <- "https://www.roger-beecham.com/datasets/cons_outline.geojson"
cons_outline <- st_read(url, crs=27700)

explanatory <- census_11 %>% 
  transmute(
    ons_const_id=ons_const_id, constituency_name=constituency_name, region=region,
    population=population, population_density=population_density,
    younger=age_20_to_24+age_25_to_29+age_30_to_44,
    own_home=house_owned,
    no_car=cars_none, white=ethnicity_white_british+ethnicity_white_irish,eu_born=born_other_eu, christian,
    professional=nssechigher_manager+nssechigher_professional, degree=qual_level_4,
    not_good_health=health_fair+health_bad+health_very_bad, heavy_industry=industry_manufacturing+industry_transport
  )
gb_leave <- .519
outcome <- leave_votes_west %>% 
  select(ons_const_id, constituency_name, leave=figure_to_use) %>% 
  inner_join(explanatory %>% select(ons_const_id, region)) %>% 
  mutate(resid_unform=leave-gb_leave)

cons_data <- cons_outline |> select(pcon19cd, bng_e, bng_n, st_lengths, st_areasha) |>  
  inner_join(outcome, by=c("pcon19cd"="ons_const_id"))

cons_data <- cons_data |> 
  inner_join(explanatory |> select(-c(region, constituency_name)),
             by=c("pcon19cd"="ons_const_id"))

write_csv(cons_data |>  ungroup(), here("../", "data", "ch6", "cons_data.csv"))

cons_data <- read_csv(here("../", "data", "ch6", "cons_data.csv"))

cons_hex <- st_read(here("../", "data", "ch6", "cons_hex.geojson"))

#-----------------------------------------
# 2. Concepts graphics
#-----------------------------------------

cons_outline <- cons_outline |> rmapshaper::ms_simplify(keep=.7)

gb_leave <- .519
cons_data <- cons_data |> mutate(resid_uniform = leave-gb_leave)

explanatory_z_scores <- cons_data |> 
  mutate(
    across(
      .cols=c(younger:heavy_industry), .fns=~(.x-mean(.x))/sd(.x)
    )
  )

max_resid <-max(abs(cons_data$resid_uniform))

map <- cons_outline |> inner_join(
  cons_data |> filter(constituency_name!="Orkney and Shetland"),
  by=c("pcon19cd"="pcon19cd")) |> 
  ggplot() +
  geom_sf(aes(fill=leave-gb_leave), colour="#757575", linewidth=0.1)+
  geom_sf(data=. %>% group_by(region) %>% summarise(), colour="#757575", 
          fill="transparent", linewidth=0.28)+
  coord_sf(crs=27700, datum=NA) +
  theme(legend.position = "right") +
  scale_fill_distiller(palette="RdBu", direction=1, 
                       limits=c(-max_resid, max_resid), guide="none")

map_hex <- cons_hex |> select(-region) |> 
  inner_join(cons_data |> st_drop_geometry(), by=c("cons_code"="pcon19cd")) |> 
  ggplot() +
  geom_sf(aes(fill=leave-gb_leave), colour="#757575", linewidth=0.12)+
  geom_sf(data=. %>% group_by(region) %>% summarise(), colour="#757575",
          fill="transparent", linewidth=0.4)+
  theme(legend.position = "right") +
  scale_fill_distiller(palette="RdBu", direction=1, 
                       limits=c(-max_resid, max_resid), guide="none") +
  theme_void()

bars <- cons_data |> st_drop_geometry() |> 
  ggplot(aes(x=reorder(constituency_name,-leave), y=resid_uniform, fill=resid_uniform))+
  geom_col(width = 1)+
  scale_fill_distiller(palette = "RdBu", type="div", direction=1, 
                       limits=c(-max_resid, max_resid), guide=FALSE)+
  scale_x_discrete(breaks=c("Hackney North and Stoke Newington","East Worthing and Shoreham","Boston and Skegness"), labels = function(x) str_wrap(x, width = 15)) +
  scale_y_continuous(limits=c(-max_resid, max_resid))+
  geom_hline(aes(yintercept=0), colour="#757575")+
  #annotate("text", x=4, y=-0.01, hjust=1, vjust=0, label="blue + pos > model", size=2) +
  #annotate("text", x=628, y=0.01, hjust=0, vjust=1, label="red + neg < model", size=2) +
  annotate("segment", x=316, xend=316, y=.005, yend=.3,
           arrow = arrow(ends = "both", length = unit(.1,"cm")), size=.18)+
  annotate("text", x=325, y=0.15, hjust=0.5, vjust=0, label=str_wrap("bar length is",30), size=3.8) +
  annotate("text", x=309, y=0.15, hjust=0.5, vjust=1, label=str_wrap("obs - model",30), size=3.8) +
  labs(x="Constituencies by Leave", y="GB 52% Leave")+
  coord_flip() 

plot <-  bars+map + map_hex 

ggsave(filename=here("figs", "06", "map_uniform.png"), plot=plot,width=11.5, height=6, dpi=500)


order_vars <- 
  cons_data |> st_drop_geometry() |>  
  select(-c(population, population_density)) |> 
  mutate(
    across(c(younger:heavy_industry), ~(.x-mean(.x))/sd(.x))
  ) |> 
  pivot_longer(cols=younger:heavy_industry, names_to="expl_var", values_to="prop") |> 
  group_by(expl_var) |>  
  summarise(cor=cor(leave,prop)) |>  ungroup() |>   arrange(cor) |>  
  pull(expl_var)


plot <- cons_data |> 
  select(-c(population, population_density)) |> 
  mutate(
    across(c(younger:heavy_industry), ~(.x-mean(.x))/sd(.x))
  ) |> 
  pivot_longer(cols=younger:heavy_industry, names_to="expl_var", values_to="prop") |>  
  group_by(expl_var) |> 
  mutate(cor=cor(leave,prop)) |>  ungroup() |> 
  mutate(expl_var=factor(expl_var, levels=order_vars)) |> 
  ggplot(aes(y=leave,x=prop)) +
  geom_point(alpha=.3, colour=site_colours$primary) +
  geom_text(data=. %>% filter(constituency_name=="Aberavon"), aes(y=.65,x=4.5, label=paste0("cor:\n",round(cor,2))), size=3.5, hjust="right")+
  facet_wrap(~expl_var) +
  labs(x="z-score", y="share of leave")

ggsave(filename=here("figs", "06", "scatters.png"), plot=plot,width=10, height=7, dpi=500)

# Additional dependency.
plot_data <- cons_data |> 
  mutate(across(c(younger:heavy_industry), ~(.x-mean(.x))/sd(.x))) |> 
  # Identify Leave/Remain majority and deciles.
  mutate(
    majority=if_else(leave>.5, "Leave", "Remain"),
    leave_transformed=(leave-mean(leave))/sd(leave),
    decile=ntile(leave, 10),
    is_extreme = decile > 9 | decile < 2
  )  |> 
  # Select out variables needed for plot.
  select(
    majority, decile, is_extreme, 
    #extreme_extent,
    region, constituency_name, leave=leave_transformed, 
    degree, professional, younger, eu_born, no_car, white, own_home, christian, not_good_health, heavy_industry
    ) |>  
  # Change polarity in selected variables.
  mutate(degree=-degree, professional=-professional, younger=-younger, eu_born=-eu_born, no_car=-no_car) |>  
  # Gather explanatory variables for along rows.
  pivot_longer(cols= c(leave:not_good_health), names_to="var", values_to="z_score") |> 
  ungroup() |> 
  # Recode new explanatory variable as factor variable ordered according to known
  # assocs. Reverse order here as I've used coord_flip.
  mutate(
    var=factor(var, levels=c("leave", order_vars)),
    var=fct_rev(var)
  ) 
# Holborn and St Pancras
# Basildon and Billericay

high <- plot_data |>   filter(decile==10) |>  sample_n(1) |> pull(constituency_name)
low <-  plot_data |>   filter(decile==1) |>  sample_n(1) |> pull(constituency_name)

annotate_data <- c(high, low)

# ----------- pcps

plot <- plot_data |>  
  ggplot(aes(x=var, y=z_score, group=c(constituency_name), colour=majority))+
  # Plot all constituencies.
  geom_path(alpha=0.15, linewidth=.2)+
  # Highlight extreme remain/leave constituencies.
  geom_path(
    data= . %>% filter(constituency_name %in% annotate_data),
    alpha=1, linewidth=.4
  )+
  
  geom_text(
      data= . %>% filter(constituency_name %in% annotate_data, var=="leave"),
      aes(x="leave", y=z_score, label=str_wrap(constituency_name,15), colour=majority), size=3.5, vjust="top", hjust="centre", nudge_x=+.5) +
  
  # Setting parameters.
  scale_colour_manual(values=c("#b2182b","#2166ac")) +
  labs(x="explanatory variable", y="z-score") +
  guides(colour="none") +
  coord_flip()

anim_cons <- plot_data |> 
  filter(decile %in% c(1,  10 )) |> 
  select(constituency_name, decile) |> 
  unique() |> 
  group_by(decile) |> 
  sample_n(25) |> 
  mutate(frame=row_number()) |> 
  ungroup() |> 
  select(-decile)

library(gganimate)

plot <- plot_data |> 
  ggplot(aes(x=var, y=z_score, group=c(constituency_name), colour=majority))+
  
  # Plot all constituencies.
  geom_path(alpha=0.15, linewidth=.2)+
  
  # Animate over remain/leave constituencies.
  geom_path(data= . %>% inner_join(anim_cons), alpha=1, linewidth=.4)+
  # Animate over remain/leave constituency names.
  geom_text(
    data= . %>% inner_join(anim_cons) %>% filter(var=="leave"),
    aes(x="leave", y=z_score, label=str_wrap(constituency_name,15), colour=majority), 
    size=3.5, vjust="top", hjust="centre", nudge_x=+.5) +
  
  # Setting parameters.
  scale_colour_manual(values=c("#b2182b","#2166ac")) +
  
  # Animate over extremes.
  transition_states(frame, transition_length = .2, state_length = .5)+
  
  labs(x="explanatory variable", y="z-score") +
  guides(colour="none") +
  coord_flip()

animate(plot, duration=23.2, start_pause=5, width=900, height=1000, res=150, renderer=gifski_renderer(here("figs", "06", "pcp-anim.gif")) )




transition_states(decile, transition_length = 2, state_length = 3)+

ggsave(filename=here("figs", "06", "pcps.png"), plot=plot,width=6, height=3.5, dpi=500)

plot_data |>  
  ggplot()+
  geom_path(
    aes(x=var, y=z_score, group=c(constituency_name), colour=majority), 
    alpha=0.15, linewidth=.2
  )+
  scale_colour_manual(values=c("#b2182b","#2166ac")) +
  coord_flip()




# Correlations plot 
transform_value <- function(x_i,y_i,lamda_r){
  return( (lamda_r*x_i + (1-lamda_r)*y_i )/ (sqrt( lamda_r^2 + (1-lamda_r)^2 ) ) ) 
}

lamda<- function(start,target){
  return(
    ( (start-1)*(target^2+start)+sqrt( target^2*((start^2-1)*(target^2-1)) ) ) /
      ( (start-1)*(2*(target^2)+start-1) )
  )
}
recentre <- function(value, current_mean, current_sd, new_mean, new_sd)
{
  ((value-current_mean)/current_sd)*new_sd+new_mean
}
targets<- c(1,.8,.3,-.3,-.8,-1)
labels <- c("perfect positive", "strong positive", "modest positive", 
            "modest negative", "strong negative", "perfect negative")
get_cor_data <- function(target, label){
  x<- rnorm(100, mean = 0, sd=2)
  y<- rnorm(100, mean = 0, sd=2)
  correlation_data <- data.frame(x,y)
  start<-cor(x,y)
  lamda_r<-lamda(start,target)
  correlation_data <- correlation_data %>%
    mutate(y_t=transform_value(x,y,lamda_r), 
           x_std = recentre(x, mean(x), sd(x), 0.5, 0.2),
           y_std = recentre(y_t, mean(y_t), sd(y_t), 0.5, 0.2),
           r=paste0(label,"\n",target),
           target=target
    )
  return(correlation_data)
}

cor_data <- map2_df(targets, labels, ~get_cor_data(.x, .y))
r_levels <- map2(targets, labels, ~paste0(.y,"\n",.x))

plot <- cor_data |> 
  mutate(r=fct_rev(factor(r,levels=r_levels)), x_std=if_else(target>0,x_std,-x_std)) %>% 
  ggplot(aes(x=x_std, y=y_std))+
  geom_point(colour=site_colours$primary, size=1.6) +
  facet_wrap(~r, nrow=1, scales="free_x") +
  labs(x="", y="")+
  theme(axis.text.x=element_blank(),axis.text.y=element_blank())

ggsave(filename=here("figs", "06", "cors.png"), plot=plot,width=10, height=3, dpi=300)



model <- cons_data |> st_drop_geometry() |> 
  mutate(
    across(.cols=c(younger:heavy_industry), .fns=~(.x-mean(.x))/sd(.x))
  ) %>% 
  lm(leave ~ degree, data=.)

summary(model)


# tidy() return estimated coefficients as a data frame.
tidy(model)
# # A tibble: 2 × 5
# term        estimate std.error statistic   p.value
# <chr>          <dbl>     <dbl>     <dbl>     <dbl>
#   1 (Intercept)   0.521    0.00290     180.  0        
# 2 degree       -0.0883   0.00290     -30.5 5.67e-126


# glance() returns a single row containing summaries of model fit.
glance(model)
# # A tibble: 1 × 12
# r.squared adj.r.squared  sigma statistic   p.value    df logLik    AIC    BIC
# <dbl>         <dbl>  <dbl>     <dbl>     <dbl> <dbl>  <dbl>  <dbl>  <dbl>
#   1     0.596         0.595 0.0728      928. 5.67e-126     1   760. -1514. -1501.
# # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

# augment() returns a data frame of residuals and predictions (fitted values) 
# for the model realisation.
augment(model)
# # A tibble: 632 × 8
# leave  degree .fitted   .resid    .hat .sigma   .cooksd .std.resid
# <dbl>   <dbl>   <dbl>    <dbl>   <dbl>  <dbl>     <dbl>      <dbl>
#   1 0.579 -0.211    0.539  0.0398  0.00165 0.0728 0.000247       0.547
# 2 0.678 -0.748    0.587  0.0914  0.00247 0.0728 0.00195        1.26 
# 3 0.386  1.63     0.376  0.00957 0.00582 0.0729 0.0000509      0.132
# 4 0.653 -0.964    0.606  0.0473  0.00306 0.0728 0.000648       0.650
# ...

model <- cons_data |> st_drop_geometry() |> 
  #select(-c(population, population_density)) |> 
  mutate(
    across(.cols=c(younger:heavy_industry), .fns=~(.x-mean(.x))/sd(.x))
  ) %>%
  lm(leave ~ degree  + eu_born + white  + no_car +  
       not_good_health + heavy_industry, data=.)

outputs <- tidy(model)
glance(model)
augment(model) %>% select(.resid)
plot <- outputs %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(
    aes(x=reorder(term, -estimate), 
        y=estimate,ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
  geom_pointrange(colour=site_colours$primary) +
  geom_hline(yintercept = 0, size=.2)+
  labs(y="estimated coefficient", x="explanatory variable") +
  coord_flip()

ggsave(filename=here("figs", "06", "outputs.png"), plot=plot,width=6.5, height=3.5, dpi=300)


single_model_fits <-  cons_data |> st_drop_geometry() |> 
  mutate(
    across(c(younger:heavy_industry), ~(.x-mean(.x))/sd(.x))
  ) |> 
  pivot_longer(cols=younger:heavy_industry, names_to="expl_var", values_to="z_score") |> 
  nest(data=-expl_var) |>   # Nest to generate list-column by expl_var.
  mutate(
    # Use map() to iterate over the list of datasets.
    model = map(data, ~lm(leave ~ z_score, data = .x)),
    # glance() for each model fit.
    fits = map(model, glance),
    # tidy() for coefficients.
    coefs = map(model, tidy),
    # augment() for predictions/residuals.
    values=map(model, augment),
  )

single_model_fits |> 
  unnest(cols = fits) |>  # unnest output from glance.
  select(-c(data, model)) # remove other list-columns.

# # A tibble: 10 × 15
# expl_var  r.squared adj.r.squared  sigma statistic   p.value    df logLik    AIC
# <chr>         <dbl>         <dbl>  <dbl>     <dbl>     <dbl> <dbl>  <dbl>  <dbl>
#   1 younger       0.289         0.288 0.0965      257. 1.05e- 48     1   582. -1158.
# 2 own_home      0.185         0.184 0.103       143. 7.42e- 30     1   539. -1071.
# 3 no_car        0.157         0.155 0.105       117. 3.81e- 25     1   528. -1050.
# 4 white         0.169         0.168 0.104       128. 3.79e- 27     1   532. -1059.
# 5 eu_born       0.233         0.232 0.100       191. 3.42e- 38     1   558. -1110.
# 6 christian     0.238         0.236 0.100       196. 4.95e- 39     1   560. -1114.
# 7 professi…     0.320         0.319 0.0944      296. 1.08e- 54     1   596. -1186.
# 8 degree        0.596         0.595 0.0728      928. 5.67e-126     1   760. -1514.
# 9 not_good…     0.316         0.315 0.0947      291. 5.93e- 54     1   594. -1182.
# 10 heavy_in…     0.504         0.503 0.0806      640. 5.43e- 98     1   696. -1385.
# # ℹ 6 more variables: BIC <dbl>, deviance <dbl>, df.residual <int>, nobs <int>,
# #   coefs <list>, values <list>


model <- cons_data |> st_drop_geometry() |> 
  mutate(across(c(younger:heavy_industry), ~(.x-mean(.x))/sd(.x))) %>%
  lm(leave ~ 
       degree + eu_born + white  + no_car + not_good_health + heavy_industry, 
     data=.)

tidy(model) |> 
  filter(term != "(Intercept)") |> 
  ggplot(
    aes(x=reorder(term, -estimate),
        y=estimate,ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
  geom_pointrange() +
  coord_flip()

cons_data |> st_drop_geometry() |> 
  mutate(
    across(c(younger:heavy_industry), ~(.x-mean(.x))/sd(.x)), 
    region=factor(region)) %>%
  lm(leave ~ 
       region + degree + eu_born + white  + no_car + not_good_health + heavy_industry -1, 
     # add -1 to the formula to remove the reference region.
     data=.) |> 
 glance()

# # A tibble: 1 × 12
# r.squared adj.r.squared  sigma statistic p.value    df logLik    AIC    BIC
# <dbl>         <dbl>  <dbl>     <dbl>   <dbl> <dbl>  <dbl>  <dbl>  <dbl>
#   1     0.995         0.995 0.0371     7625.       0    17  1193. -2351. -2270.
# # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

# # A tibble: 17 × 5
# term                           estimate std.error statistic  p.value
# <chr>                             <dbl>     <dbl>     <dbl>    <dbl>
#   1 (Intercept)                     0.530     0.00581    91.3   0       
# 2 regionEast of England           0.00363   0.00787     0.462 6.45e- 1
# 3 regionLondon                    0.0654    0.00948     6.90  1.30e-11
# 4 regionNorth East                0.00482   0.00945     0.510 6.10e- 1
# 5 regionNorth West               -0.0200    0.00728    -2.75  6.12e- 3
# 6 regionScotland                 -0.145     0.00843   -17.2   1.28e-54
# 7 regionSouth East                0.00377   0.00752     0.502 6.16e- 1
# 8 regionSouth West               -0.0233    0.00789    -2.95  3.26e- 3
# 9 regionWales                    -0.0547    0.00860    -6.36  3.87e-10
# 10 regionWest Midlands             0.0236    0.00745     3.17  1.59e- 3
# 11 regionYorkshire and The Humber  0.0112    0.00762     1.47  1.41e- 1
# 12 degree                         -0.0772    0.00339   -22.8   1.30e-83
# 13 eu_born                         0.0163    0.00308     5.29  1.72e- 7
# 14 white                           0.0303    0.00314     9.66  1.18e-20
# 15 no_car                         -0.0336    0.00292   -11.5   6.50e-28
# 16 not_good_health                 0.0102    0.00331     3.07  2.24e- 3
# 17 heavy_industry                  0.0132    0.00266     4.96  9.23e- 7    

# With reference region removed:

# A tibble: 17 × 5
# term                           estimate std.error statistic   p.value
# <chr>                             <dbl>     <dbl>     <dbl>     <dbl>
#   1 regionEast Midlands              0.530    0.00581     91.3  0        
# 2 regionEast of England            0.534    0.00525    102.   0        
# 3 regionLondon                     0.596    0.00683     87.3  0        
# 4 regionNorth East                 0.535    0.00756     70.7  1.18e-297
# 5 regionNorth West                 0.510    0.00466    110.   0        
# 6 regionScotland                   0.385    0.00578     66.5  3.50e-283
# 7 regionSouth East                 0.534    0.00450    119.   0        
# 8 regionSouth West                 0.507    0.00524     96.8  0        
# 9 regionWales                      0.476    0.00651     73.0  3.19e-305
# 10 regionWest Midlands              0.554    0.00528    105.   0        
# 11 regionYorkshire and The Humber   0.541    0.00528    103.   0        
# 12 degree                          -0.0772   0.00339    -22.8  1.30e- 83
# 13 eu_born                          0.0163   0.00308      5.29 1.72e-  7
# 14 white                            0.0303   0.00314      9.66 1.18e- 20
# 15 no_car                          -0.0336   0.00292    -11.5  6.50e- 28
# 16 not_good_health                  0.0102   0.00331      3.07 2.24e-  3
# 17 heavy_industry                   0.0132   0.00266      4.96 9.23e-  7



model <- cons_data |> 
  mutate(
    across(c(younger:heavy_industry), ~(.x-mean(.x))/sd(.x))
  ) |> 
  mutate(type="full_dataset") |> 
  nest(data=-type) |> 
  mutate(
    model=map(data, ~lm(leave ~ degree  + eu_born + white  + no_car +
                          not_good_health + heavy_industry, data=.x)),
    # glance() for each model fit
    fits = map(model, glance),
    # tidy() for coefficients
    coefs = map(model, tidy),
    # augment() for predictions/residuals
    values=map(model, augment)
  )


# Generate permuted data: randomly shuffling residuals values around 
# constituencies.
permuted_data <- model |>  
  mutate(
    resids=map(values, ~.x |>  select(.resid))
  ) |> 
  select(-c(coefs, model, fits, model, values)) |>  
  unnest(cols=c(data,resids)) |>  
  select(pcon19cd, .resid) |>  
  permutations(permute=c(pcon19cd), times=8, apparent=TRUE) |> 
  mutate(data=map(splits, ~rsample::analysis(.))) |> 
  select(id, data) |> 
  unnest(cols=data)

# Store max value of residuals for setting limits in map colour scheme.
max_resid <- max(abs(permuted_data$.resid))
# Store vector of permutation IDs for shuffling facets in the plots.
ids <- permuted_data %>% pull(id) %>% unique()
# Plot lineup.
plot <- cons_data |> filter(constituency_name!="Orkney and Shetland") |> 
  select(pcon19cd, region) |> 
  inner_join(permuted_data) |> 
  mutate(id=factor(id, sample(ids))) |> 
  ggplot() +
  geom_sf(aes(fill=.resid), colour="#636363", linewidth=0.02)+
  geom_sf(data=. %>% group_by(region) %>% summarise(), colour="#636363", linewidth=0.05, fill="transparent")+
  geom_sf(data=. %>% group_by(id) %>% summarise(), colour="#636363", linewidth=0.1, fill="transparent")+
  coord_sf(crs=27700, datum=NA) +
  facet_wrap(~id, ncol=3) +
  scale_fill_distiller(palette="RdBu", direction=1,
                       limits=c(-max_resid, max_resid), guide="none") +
  theme(
    strip.text.x = element_blank()
  )




plot_large <- cons_hex |> 
  select(cons_code, region) |> 
  inner_join(permuted_data, by=c("cons_code"="pcon19cd")) |> 
  filter(id=="Apparent") |> 
  ggplot() +
  geom_sf(aes(fill=.resid), colour="#636363", linewidth=0.03)+
  geom_sf(data=. %>% group_by(region) %>% summarise(), colour="#636363", linewidth=0.15, fill="transparent")+
  geom_sf(data=. %>% group_by(id) %>% summarise(), colour="#636363", linewidth=0.2, fill="transparent")+
  #coord_sf(crs=27700, datum=NA) +
  #facet_wrap(~id, ncol=3) +
  scale_fill_distiller(palette="RdBu", direction=1,
                       limits=c(-max_resid, max_resid), guide="none") +
  theme(
    # strip.text.x = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank()
  )

plot_small <- cons_hex |> 
  select(cons_code, region) |> 
  inner_join(permuted_data, by=c("cons_code"="pcon19cd")) |> 
  mutate(id=factor(id, sample(ids)), id=paste0("p", as.numeric(id))
  ) |> 
  ggplot() +
  geom_sf(aes(fill=.resid), colour="#636363", linewidth=0.02)+
  geom_sf(data=. %>% group_by(region) %>% summarise(), colour="#636363", linewidth=0.1, fill="transparent")+
  geom_sf(data=. %>% group_by(id) %>% summarise(), colour="#636363", linewidth=0.15, fill="transparent")+
  #coord_sf(crs=27700, datum=NA) +
  facet_wrap(~id, ncol=3) +
  scale_fill_distiller(palette="RdBu", direction=1,
                       limits=c(-max_resid, max_resid), guide="none") +
  theme(
    # strip.text.x = element_blank(),
    strip.text=element_text(size=10),
    axis.text = element_blank(),
    axis.line = element_blank()
  )

plot <- plot_large + plot_small + plot_layout(widths=c(.55,1))

ggsave(filename=here("figs", "06", "lineups_hex.png"), plot=plot,width=7, height=6.5, dpi=300)

library(magick)
# Load in as image to annotate.
img <- image_read(here("figs", "06", "lineups_hex.png")) |> 
  image_fill('none') |> 
  as.raster()

insight <- "1. Analyst observes apparent spatial autocorrelation structure in model residuals." 
task <- "2. Indepenent observer asked to pick real data from a group of decoys."
result <- "3. If the real is correctly identified, we reject the null of CSR in residuals."

ggplot() +
  annotation_raster(img, 0, 1, 0, 1) +
  annotate("text", 
           label=str_wrap(insight, 25),
           x=0, y=.87, size=2, hjust="left", vjust="top") +
  annotate("text", 
           label=str_wrap(task, 25),
           x=0.37, y=.19, size=2, hjust="right", vjust="top") +
  annotate("text", 
           label=str_wrap(result, 20),
           x=1.0, y=.9, size=2, hjust="left", vjust="top") +
  annotate("rect", xmin=.79, xmax=.97, ymin=.66, ymax=.94, fill="transparent", colour="#000000", linewidth=.03 )+
  scale_x_continuous(limits=c(0, 1.4)) +
  scale_y_continuous(limits=c(0, 1))


### Multi-level model experiments
install.packages("broom.mixed") 
library(broom.mixed)
library(multilevelmod)
model_data <- single_model_fits <- cons_data |> st_drop_geometry() |> 
  select(-c(population, population_density)) |> 
  mutate(
    across(c(younger:heavy_industry), ~(.x-mean(.x))/sd(.x))
  ) |> 
  pivot_longer(cols=younger:heavy_industry, names_to="expl_var", values_to="z_score") |> 
  mutate(region=as.factor(region)) 


single_model_fits <- model_data |> 
  nest(data=-expl_var) |>   # nest to generate list-column by expl_var.
  mutate(
    # Use map() to iterate over the list of datasets to fit a model to each nested dataset.
    model = map(data, ~
                  linear_reg() |>  set_engine("lmer") |> 
                  fit(leave ~ z_score + (1 | region), data = .x)),
    # glance() for each model fit
    fits = map(model, glance),
    # tidy() for coefficients
    coefs = map(model, tidy),
    # augment() for predictions/residuals
    values=map2(model, data, ~augment(.x, new_data=.y))
  )

# Multivariate model: Multilevel 
model <- cons_data |> st_drop_geometry() |> 
  select(-c(population, population_density)) |> 
  mutate(
    across(c(younger:heavy_industry), ~(.x-mean(.x))/sd(.x))
  ) |> 
  mutate(type="full_dataset", region=as.factor(region)) %>%
  nest(data=-type) %>%
  mutate(
    # Include `-1` to eliminate the constant term and include a dummy for every area
    model=map(data, ~ linear_reg() |>  set_engine("lmer") |> 
                fit(leave ~  degree  + eu_born + white  + no_car +
                          not_good_health + heavy_industry + (1 | region), data=.x)),
    # glance() for each model fit
    fits = map(model, glance),
    # tidy() for coefficients
    coefs = map(model, tidy),
    # augment() for predictions/residuals
    values=map2(model, data, ~augment(.x, new_data=.y)),
    model_r=map(data, ~lme4::lmer(leave ~  degree  + eu_born + white  + no_car +
                                      not_good_health + heavy_industry + (1 | region), data=.x)),
    randoms = map(model_r, ~lme4::ranef(.x) %>% as_tibble())
  )

plot <- model |> 
  unnest(cols = coefs) |>  # unnest output from glance
  mutate(
    is_fe=!str_detect(term, "region"),
    term=str_remove(term,"region"),
    coef_type=if_else(!is_fe, "FE constants", "coefficients"),
    coef_type=factor(coef_type, levels=c("FE constants", "coefficients")),
    yintercept=0
  ) |> 
  filter(effect=="fixed") |> 
  select(term, estimate, std.error, is_fe) |> 
  bind_rows(
    model |> select(randoms) |> unnest(cols=randoms) |> 
      select(term=grp, estimate=condval, std.error=condsd) |> mutate(is_fe=FALSE)
  ) |> 
  filter(term != "(Intercept)") |> 
  mutate(is_fe=if_else(is_fe, "Fixed effects", "Random intercepts")) |> 
  ggplot(
    aes(x=reorder(term, -estimate),
        y=estimate,ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
  geom_pointrange(colour=site_colours$primary) +
  geom_hline(aes(yintercept=0), size=.2)+
  scale_alpha_discrete(c(0,1), guide=FALSE)+
  facet_wrap(~is_fe, scales="free", shrink=TRUE) +
  coord_flip() +
  labs(y="", x="")
performance::performance(m)



# Multivariate model : FE
model <- cons_data |> st_drop_geometry() |> 
  select(-c(population, population_density)) |> 
  mutate(
    across(c(younger:heavy_industry), ~(.x-mean(.x))/sd(.x))
  ) |> 
  mutate(type="full_dataset", region=as.factor(region)) %>%
  nest(data=-type) %>%
  mutate(
    # Include `-1` to eliminate the constant term and include a dummy for every area
    model=map(data, ~lm(leave ~ region +  degree  + eu_born + white  + no_car +
                          not_good_health + heavy_industry -1, data=.x)),
    # glance() for each model fit
    fits = map(model, glance),
    # tidy() for coefficients
    coefs = map(model, tidy),
    # augment() for predictions/residuals
    values=map(model, augment)
  )

model %>%
  unnest(cols = fits) %>% # unnest output from glance
  select(-c(data, model, coefs, model, values)) %>% View() # remove other list-columns


plot <- model |> 
  unnest(cols = coefs) %>% # unnest output from glance
  select(-c(data, model, fits, model, values)) %>%
  mutate(
    is_fe=!str_detect(term, "region"),
    term=str_remove(term,"region"),
    coef_type=if_else(!is_fe, "FE constants", "coefficients"),
    coef_type=factor(coef_type, levels=c("FE constants", "coefficients")),
    yintercept=0
  ) %>%
  #filter(term != "(Intercept)") %>%
  ggplot(
    aes(x=reorder(term, -estimate),
        y=estimate,ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
  geom_pointrange(colour=site_colours$primary) +
  geom_hline(aes(yintercept=yintercept, alpha=is_fe), size=.2)+
  scale_alpha_discrete(c(0,1), guide=FALSE)+
  facet_wrap(~coef_type, scales="free", shrink=TRUE) +
  coord_flip() +
  labs(y="", x="")


ggsave(filename=here("figs", "06", "outputs_fe.png"), plot=plot,width=8.5, height=3.8, dpi=300)


# Multivariate model : FE
model <- cons_data |> st_drop_geometry() |> 
  select(-c(population, population_density)) |> 
  mutate(
    across(c(younger:heavy_industry), ~(.x-mean(.x))/sd(.x)),  
    type="full_dataset", region=as.factor(region)
    ) |> 
  nest(data=-type) %>%
  mutate(
    # Include `-1` to eliminate the constant term and include a dummy for every area
    model=map(data, ~lm(leave ~ region +  degree  + eu_born + white  + no_car +
                          not_good_health + heavy_industry -1, data=.x)),
    # augment() for predictions/residuals
    values=map(model, augment)
  )


# Generate permuted data: randomly shuffling residuals values around 
# constituencies.
permuted_data <- model |>  
  mutate(
    resids=map(values, ~.x |>  select(.resid))
  ) |> 
  select(-c(model, values)) |>  
  unnest(cols=c(data,resids)) |>  
  select(pcon19cd, .resid) |>  
  permutations(permute=c(pcon19cd), times=8, apparent=TRUE) |> 
  mutate(data=map(splits, ~analysis(.))) |> 
  select(id, data) |> 
  unnest(cols=data)

# Store max value of residuals for setting limits in map colour scheme.
max_resid <- max(abs(permuted_data$.resid))
max_resid <- .14
# Store vector of permutation IDs for shuffling facets in the plots.
ids <- permuted_data %>% pull(id) %>% unique()

plot <- #cons_data |> filter(constituency_name!="Orkney and Shetland") |> 
  cons_hex |>   
  select(cons_code, region) |> 
  inner_join(permuted_data, by=c("cons_code"="pcon19cd")) |> 
  mutate(id=factor(id, sample(ids)), id=paste0("p",as.numeric(id)),
         .resid=pmax(.resid,-.14)) |> 
  ggplot() +
  geom_sf(aes(fill=.resid), colour="#636363", linewidth=0.05)+
  geom_sf(data=. %>% group_by(region) %>% summarise(), colour="#636363", linewidth=0.2, fill="transparent")+
  geom_sf(data=. %>% group_by(id) %>% summarise(), colour="#636363", linewidth=0.25, fill="transparent")+
  #coord_sf(crs=27700, datum=NA) +
  facet_wrap(~id, ncol=5) +
  scale_fill_distiller(palette="RdBu", direction=1,
                       limits=c(-max_resid, max_resid), guide="none") +
  theme(
   # strip.text.x = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank()
  )

ggsave(filename=here("figs", "06", "lineups_fe.png"), plot=plot,width=12, height=6.5, dpi=300)


model <- cons_data |> st_drop_geometry() |> 
  mutate(
    across(c(younger:heavy_industry), ~(.x-mean(.x))/sd(.x))
  ) |> 
  mutate(type="full_dataset", region=as.factor(region), cons=1) %>%
  nest(data=-type) %>%
  mutate(
    # `:` Notation implies interaction variables
    model=map(data, ~lm(leave ~ 0 +  (cons + degree  + eu_born + white  + no_car +
                                        not_good_health + heavy_industry):(region), data=.x)),
    # glance() for each model fit
    fits = map(model, glance),
    # tidy() for coefficients
    coefs = map(model, tidy),
    # augment() for predictions/residuals
    values=map(model, augment)
  )


model %>%
  unnest(cols = fits) %>% View()

plot <- model %>%
  unnest(cols = coefs) %>% # unnest output from glance
  select(-c(data, model, fits, model, values)) %>%
  separate(term, into= c("term", "region"), sep=":") %>%
  mutate(
    region=str_remove(region,"region"),
    region=if_else(region=="Yorkshire and The Humber", "Yorkshire & Humber", region)
  ) %>%
  filter(term!="cons") %>%
  ggplot() +
  geom_col(aes(x=reorder(term, -estimate), y=estimate), fill=site_colours$primary, alpha=.3)+
  geom_pointrange(aes(x=reorder(term, -estimate),
                      y=estimate,ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), colour=site_colours$primary) +
  geom_hline(yintercept = 0, size=.2)+
  facet_wrap(~region) +
  coord_flip()+
  labs(y="estimated coefficient", x="explanatory variable")

ggsave(filename=here("figs", "06", "outputs_interact.png"), plot=plot,width=10, height=6, dpi=300)


model <- cons_data |> st_drop_geometry() |> 
  mutate(
    across(c(younger:heavy_industry), ~(.x-mean(.x))/sd(.x)),
    region=as.factor(region), cons=1) %>%
  lm(leave ~ 0 +  
       (cons + degree  + eu_born + white  + no_car + not_good_health + heavy_industry):(region), 
     data=.
  )

tidy(model) |> 
  separate(term, into= c("term", "region"), sep=":") |> 
  mutate(region=str_remove(region,"region")) |> 
  filter(term!="cons") |> 
  ggplot() +
  geom_col(aes(x=reorder(term, -estimate), y=estimate), alpha=.3)+
  geom_pointrange(aes(x=reorder(term, -estimate),
                      y=estimate,ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
  geom_hline(yintercept = 0, size=.2)+
  facet_wrap(~region) +
  coord_flip()


# Random slopes

# Multivariate model: Multilevel 
model <- cons_data |> st_drop_geometry() |> 
  select(-c(population, population_density)) |> 
  mutate(
    across(c(younger:heavy_industry), ~(.x-mean(.x))/sd(.x))
  ) |> 
  mutate(type="full_dataset", region=as.factor(region)) |> 
  nest(data=-type) |> 
  mutate(
    model_r=map(data, ~lme4::lmer(leave ~ 
                                        degree + 
                                        eu_born + (1 + eu_born | region) +
                                        white + 
                                        no_car + 
                                        not_good_health + 
                                        heavy_industry,
                                      data=.x))),
    randoms = map(model_r, ~lme4::ranef(.x) %>% as_tibble())
    # Include `-1` to eliminate the constant term and include a dummy for every area
    model=map(data, ~ linear_reg() |>  set_engine("lmer") |> 
                fit(leave ~ 
                      degree + eu_born + white + no_car + not_good_health + heavy_industry + 
                      (1 + degree + eu_born + white + no_car + not_good_health + heavy_industry | region) , 
                    data=.x))
    
  ),
    # glance() for each model fit
    fits = map(model, glance),
    # tidy() for coefficients
    coefs = map(model, tidy),
    # augment() for predictions/residuals
    values=map2(model, data, ~augment(.x, new_data=.y)),
  )


v |> 
  rename(region=groupID, estimate=mean, std.error=sd) |> 
  filter(term!="(Intercept)") |> 
  ggplot() +
  geom_col(aes(x=reorder(term, -estimate), y=estimate), fill=site_colours$primary, alpha=.3)+
  geom_pointrange(aes(x=reorder(term, -estimate),
                      y=estimate,ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), colour=site_colours$primary) +
  geom_hline(yintercept = 0, size=.2)+
  facet_wrap(~region) +
  coord_flip()+
  labs(y="estimated coefficient", x="explanatory variable")


# Task 1 ------------

order_vars <- cons_data |>
  mutate(across(c(younger:heavy_industry), ~(.x-mean(.x))/sd(.x))) |> 
  pivot_longer(cols=younger:heavy_industry, names_to="expl_var", values_to="prop") |> 
  group_by(expl_var) |>  
  summarise(cor=cor(leave,prop)) |> ungroup() |> arrange(cor) |>  
  pull(expl_var)

plot_data <- cons_data |> 
  mutate(
    majority=if_else(leave>.5, "Leave", "Remain"),
    across(c(leave, younger:heavy_industry), ~(.x-mean(.x))/sd(.x)),
    decile=ntile(leave, 10), is_extreme=decile > 9 | decile < 2
  )  |> 
  # Select out variables needed for plot.
  select(
    majority, is_extreme, decile, constituency_name, leave, 
    degree, professional, younger, eu_born, no_car, white, own_home, christian, not_good_health, heavy_industry
  ) |>  
  # Change polarity in selected variables.
  mutate(degree=-degree, professional=-professional, younger=-younger, eu_born=-eu_born, no_car=-no_car) |>  
  # Gather explanatory variables for along rows.
  pivot_longer(cols= c(leave:not_good_health), names_to="var", values_to="z_score") |> 
  # Recode new explanatory variable as factor ordered according to known
  # assocs. Reverse order here as coord_flip() used in plot.
  mutate(
    var=factor(var, levels=c("leave", order_vars)),
    var=fct_rev(var)
  ) 
annotate_data <- plot_data |> 
  filter(is_extreme) |> 
  group_by(decile) |> 
  sample_n(1) |> pull(constituency_name)

plot_data |>  
  ggplot(aes(x=var, y=z_score, colour=majority, group=c(constituency_name))) +
  geom_path(alpha=0.15, linewidth=.2) +
  
  # Highlight extreme remain/leave constituencies.
  geom_path(
    data= . %>% filter(constituency_name %in% annotate_data),
    alpha=1, linewidth=.4
  )+
  geom_text(
    data= . %>% filter(constituency_name %in% annotate_data, var=="leave"),
    aes(x="leave", y=z_score, label=str_wrap(constituency_name,15)), 
    size=3.5, vjust="top", hjust="centre", nudge_x=+.5
    ) +
  scale_colour_manual(values=c("#b2182b","#2166ac")) +
  coord_flip()

