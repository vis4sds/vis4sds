
# Coloured mosaics : no model
mosaic <- ped_veh |>
  left_join(london_squared) |>
  filter(!is.na(BOR), vehicle_type %in% c("Car", "Motorcycle","Taxi", "Bicycle")) |> 
  mutate(
    is_weekend=if_else(day_of_week %in% c("Saturday", "Sunday"), "weekend", "weekday"),
    vehicle_type=fct_rev(factor(vehicle_type, levels=c("Car", "Motorcycle","Taxi", "Bicycle"))),
    is_inner=local_authority_district %in% c("Camden",
                                             "Greenwich",
                                             "Hackney",
                                             "Hammersmith and Fulham",
                                             "Islington",
                                             "Kensington and Chelsea",
                                             "Lambeth",
                                             "Lewisham",
                                             "Southwark",
                                             "Tower Hamlets",
                                             "Wandsworth",
                                             "Westminster", "City of London")
    
  ) |> 
  filter(!is_inner) |> 
  ggplot() +
  geom_mosaic(aes(x=product(is_weekend, vehicle_type), alpha=is_weekend, fill=vehicle_type), offset = 0.008)+
  #scale_fill_manual(values=c("#377eb8","#ff7f00"))+
  #scale_fill_manual(values=c("#2171b5", "#525252"))+
  guides(fill="none", alpha="none") +
  theme_v_gds() +
  coord_flip() +
  facet_wrap(~BOR, ncol=7)+
  scale_x_productlist(position="top", name="outer London")+
  scale_alpha_discrete(range=c(.3,.9))+
  scale_fill_manual(values=c("#4daf4a", "#984ea3", "#377eb8", "#e41a1c"))+
  theme(
    panel.grid.major.y=element_blank(), panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(),
    aspect.ratio = 1
  ) 
plot_data <- ggplot_build(mosaic)$data |> as.data.frame() |> 
  group_by(PANEL) |>
  mutate(bor_total=sum(.wt)) |> ungroup() |>
  group_by(PANEL, x__vehicle_type) |>
  summarise(xmin=min(xmin),xmax=max(xmax), ymin=min(ymin),ymax=max(ymax),
            vehicle_prop_bor=sum(.wt)/sum(bor_total), bor_total=min(bor_total)) |> ungroup() |> 
  left_join(london_squared) 


mosaic <- ped_veh |>
  left_join(london_squared) |>
  filter(
    !is.na(BOR), 
    local_authority_district %in% c("Westminster", "Harrow"),
    #vehicle_type %in% c("Car", "Motorcycle","Taxi", "Bicycle")
    ) |> 
  mutate(
    is_weekend=if_else(day_of_week %in% c("Saturday", "Sunday"), "weekend", "weekday"),
    vehicle_type=fct_rev(vehicle_type),
    #vehicle_type=fct_rev(factor(vehicle_type, levels=c("Car", "Motorcycle","Taxi", "Bicycle"))), 
    local_authority_district=factor(local_authority_district, levels=c("Westminster", "Harrow"))
  ) |> 
  ggplot() +
  #geom_mosaic(aes(x=product(vehicle_type), fill=vehicle_type), alpha=.3, offset = 0.008)+
  geom_mosaic(aes(x=product(vehicle_type)), fill="#2171b5", alpha=.3, offset = 0.008)+
  #scale_fill_manual(values=c("#4daf4a", "#984ea3", "#377eb8", "#e41a1c"))+
  theme_v_gds() +
  coord_flip() +
  facet_wrap(~local_authority_district, nrow=2)+
  scale_x_productlist(position="top", name="")+
  theme(
    aspect.ratio = 1,
    panel.grid.major.y=element_blank(), panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank()
  ) 
plot_data <- ggplot_build(mosaic)$data |> as.data.frame() |> 
  group_by(PANEL) |>
  mutate(bor_total=sum(.wt)) |> ungroup() |>
  group_by(PANEL, x__vehicle_type) |>
  summarise(xmin=min(xmin),xmax=max(xmax), ymin=min(ymin),ymax=max(ymax),
            vehicle_prop_bor=sum(.wt)/sum(bor_total), bor_total=min(bor_total)) |> ungroup() |> 
  left_join(london_squared) |> 
  mutate(
    local_authority_district=factor(local_authority_district, levels=c("Westminster", "Harrow"))
  )

# Rescaling function.
map_scale <- function(value, min1, max1, min2, max2) {
  return  (min2+(max2-min2)*((value-min1)/(max1-min1)))
}

selected_borough_model <- ped_veh |>
  left_join(london_squared) |>
  filter(!is.na(BOR), local_authority_district %in% c("Westminster", "Harrow")) |>
  mutate(
    is_weekend=if_else(day_of_week %in% c("Saturday", "Sunday"), "weekend", "weekday"),
    vehicle_type=fct_rev(vehicle_type),
    local_authority_district=factor(local_authority_district, levels=c("Westminster", "Harrow"))
  ) |>
  group_by(local_authority_district, vehicle_type) |>
  mutate(row_total=n()) |>
  group_by(local_authority_district, is_weekend) |>
  mutate(col_total=n()) |>  ungroup() |>
  mutate(grand_total=n()) |>
  group_by(vehicle_type, is_weekend, local_authority_district) |>
  mutate(
    observed=n(),
    row_total=first(row_total),
    col_total=first(col_total),
    grand_total=first(grand_total),
    expected=(row_total*col_total)/grand_total,
    resid=(observed-expected)/sqrt(expected)
  ) |> ungroup() |>
  mutate(
    resid_max=max(abs(resid)),
    resid_range=cut_interval(resid, n=10)
    ) |>
  group_by(is_weekend, vehicle_type, local_authority_district, BOR) |>
  summarise(
    freq=n(), resid_range=first(resid_range), resid=first(resid),resid_max=first(resid_max)
  ) |> ungroup() |>
  mutate(
    resid_trans=map_scale(resid, -max_resid, max_resid, 0,1)
  )

exp <- ped_veh |>
  inner_join(london_squared) |>
  mutate(is_weekend=day_of_week %in% c("Saturday", "Sunday")) |> 
  #group_by(vehicle_type) |> 
  mutate(glob_exp=mean(is_weekend)) |> 
  group_by(local_authority_district, vehicle_type) |> 
  summarise(
    glob_exp=first(glob_exp), 
    freq=n(), 
    #obs=mean(is_weekend)*freq, 
    #exp=glob_exp*freq,
    obs=mean(is_weekend),
    exp=glob_exp,
    diff=obs-exp,
    resid=(obs-exp)/sqrt(exp)
    )





#exp <- selected_borough_model |> filter(is_weekend=="weekend")

borough_select_model <- mosaic + 
  geom_rect(
    data=plot_data %>% inner_join(exp, by=c("x__vehicle_type"="vehicle_type","local_authority_district"="local_authority_district")) |> 
    #data=plot_data %>% inner_join(exp, by=c("x__vehicle_type"="vehicle_type","local_authority_district"="local_authority_district")), 
    mutate(max_diff=max(abs(diff)), diff_rescaled=map_scale(diff, -max_diff, max_diff, 0,1), local_authority_district=factor(local_authority_district, levels=c("Westminster", "Harrow"))),
    aes(xmin=xmin, xmax=xmax, ymin=.5, ymax=diff_rescaled), fill="#2171b5"
  ) +
  geom_segment(
    data=plot_data, #%>% left_join(exp, by=c("x__vehicle_type"="vehicle_type")),
    aes(x=xmin, xend=xmax, y=.5, yend=.5),  size=.3
  ) +
  geom_text(
    data=plot_data, 
    aes(
      x=xmin+0.5*(xmax-xmin), y=ymin+0.5*(ymax-ymin),label=x__vehicle_type, size=vehicle_prop_bor
    ), 
    alpha=.7,family="Avenir Book")+
  geom_text(
    data=plot_data %>%
      filter(x__vehicle_type=="Car", BOR=="HRW"),
    aes(x=xmax-.05, y=1), hjust=1,
    label="more wknd",family="Avenir Book", colour="#08306b", size=2.5
  )+
  geom_text(
    data=plot_data %>%
      filter(x__vehicle_type=="Car", BOR=="HRW"),
    aes(x=xmax-.05, y=0), hjust=0,
    label="more wkdy",family="Avenir Book", colour="#08306b", size=2.5
  )+
  scale_size(range=c(0, 10))+
  guides(size=FALSE, alpha=FALSE)+
  labs(subtitle="direct encoded <br> <span style = 'color: #08306b;'>expectation</span>") +
  #theme_v_gds()+
  theme(
    panel.grid.major.y=element_blank(), panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), 
    #legend.position = "right",
    panel.spacing=unit(-0.1, "lines"),
    plot.margin = margin(r=25),
    plot.subtitle = ggtext::element_markdown(size=10, hjust=0),
    legend.text=element_text(size=6), legend.key.size = unit(.4, 'cm'), legend.title = element_text(size=7)
  ) 


mosaic <- ped_veh |>
  left_join(london_squared) |>
  filter(
    !is.na(BOR), 
    local_authority_district %in% c("Westminster", "Harrow"),
    vehicle_type %in% c("Car", "Motorcycle","Taxi", "Bicycle")
  ) |> 
  mutate(
    is_weekend=if_else(day_of_week %in% c("Saturday", "Sunday"), "weekend", "weekday"),
    #vehicle_type=fct_rev(vehicle_type),
    vehicle_type=fct_rev(factor(vehicle_type, levels=c("Car", "Motorcycle","Taxi", "Bicycle"))), 
    local_authority_district=factor(local_authority_district, levels=c("Westminster", "Harrow"))
  ) |> 
  ggplot() +
  geom_mosaic(aes(x=product(vehicle_type), fill=vehicle_type), alpha=.3, offset = 0.008)+
  #geom_mosaic(aes(x=product(vehicle_type)), fill="#2171b5", alpha=.3, offset = 0.008)+
  scale_fill_manual(values=c("#4daf4a", "#984ea3", "#377eb8", "#e41a1c"))+
  theme_v_gds() +
  coord_flip() +
  facet_wrap(~local_authority_district, nrow=2)+
  scale_x_productlist(position="top", name="")+
  theme(
    aspect.ratio = 1,
    panel.grid.major.y=element_blank(), panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank()
  ) 

plot_data <- ggplot_build(mosaic)$data |> as.data.frame() |> 
  group_by(PANEL) |>
  mutate(bor_total=sum(.wt)) |> ungroup() |>
  group_by(PANEL, x__fill__vehicle_type) |>
  summarise(xmin=min(xmin),xmax=max(xmax), ymin=min(ymin),ymax=max(ymax),
            vehicle_prop_bor=sum(.wt)/sum(bor_total), bor_total=min(bor_total)) |> ungroup() |> 
  left_join(london_squared, by=c("bor_total"="bor_total_filtered")) |> 
  mutate(
    local_authority_district=factor(local_authority_district, levels=c("Westminster", "Harrow"))
  )


exp <- ped_veh |>
  inner_join(london_squared) |>
  mutate(is_weekend=day_of_week %in% c("Saturday", "Sunday")) |> 
  filter(vehicle_type %in% c("Car", "Motorcycle","Taxi", "Bicycle")) |> 
  #group_by(vehicle_type) |> 
  mutate(glob_exp=mean(is_weekend)) |> 
  group_by(local_authority_district, vehicle_type) |> 
  summarise(
    glob_exp=first(glob_exp), 
    freq=n(), 
    #obs=mean(is_weekend)*freq, 
    #exp=glob_exp*freq,
    obs=mean(is_weekend),
    exp=glob_exp,
    diff=obs-exp,
    resid=(obs-exp)/sqrt(exp)
  )

borough_select_model_fill <- mosaic + 
  geom_rect(
    data=plot_data %>% inner_join(exp, by=c("x__fill__vehicle_type"="vehicle_type","local_authority_district"="local_authority_district")) |> 
      #data=plot_data %>% inner_join(exp, by=c("x__vehicle_type"="vehicle_type","local_authority_district"="local_authority_district")), 
      mutate(max_diff=max(abs(diff)), diff_rescaled=map_scale(diff, -max_diff, max_diff, 0,1), local_authority_district=factor(local_authority_district, levels=c("Westminster", "Harrow"))),
    aes(xmin=xmin, xmax=xmax, ymin=.5, ymax=diff_rescaled, fill=x__fill__vehicle_type)
  ) +
  geom_segment(
    data=plot_data, #%>% left_join(exp, by=c("x__vehicle_type"="vehicle_type")),
    aes(x=xmin, xend=xmax, y=.5, yend=.5),  size=.3
  ) +
  geom_text(
    data=plot_data, 
    aes(
      x=xmin+0.5*(xmax-xmin), y=ymin+0.5*(ymax-ymin),label=x__fill__vehicle_type, size=vehicle_prop_bor
    ), 
    alpha=.7,family="Avenir Book")+
  geom_text(
    data=plot_data %>%
      filter(x__fill__vehicle_type=="Car", BOR=="HRW"),
    aes(x=xmax-.05, y=1), hjust=1,
    label="more wknd",family="Avenir Book", size=2.5, alpha=.8
  )+
  geom_text(
    data=plot_data %>%
      filter(x__fill__vehicle_type=="Car", BOR=="HRW"),
    aes(x=xmax-.05, y=0), hjust=0,
    label="more wkdy",family="Avenir Book", size=2.5, alpha=.8
  )+
  scale_size(range=c(0, 10))+
  scale_alpha_discrete(range=c(0.3,0.9))+
  guides(size=FALSE, alpha=FALSE, fill=FALSE)+
  labs(subtitle="hue (associative) <br> direct encoded exp", 
       # caption=" <p>
       # <span style = 'color: #e41a1c;'>car</span>
       # <span style = 'color: #377eb8;'>mbike</span>
       # <span style = 'color: #984ea3;'>taxi</span>
       # <span style = 'color: #4daf4a;'>bike</span>
       # </p>"
  ) +
 # scale_fill_manual(values=c("#4daf4a", "#984ea3", "#377eb8", "#e41a1c"))+
  #theme_v_gds()+
  theme(
    panel.grid.major.y=element_blank(), panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), 
    #legend.position = "right",
    panel.spacing=unit(-0.1, "lines"),
    plot.margin = margin(r=25),
    plot.subtitle = ggtext::element_markdown(size=10, hjust=0),
    plot.caption=ggtext::element_markdown(size=9, hjust=.5), plot.caption.position = "panel",
    legend.key.size = unit(.4, 'cm'), legend.title = element_text(size=7)
  )



mosaic <- ped_veh |>
  left_join(london_squared) |>
  filter(
    !is.na(BOR), 
    local_authority_district %in% c("Westminster", "Harrow")#,
    #vehicle_type %in% c("Car", "Motorcycle","Taxi", "Bicycle")
  ) |> 
  mutate(
    is_weekend=if_else(day_of_week %in% c("Saturday", "Sunday"), "weekend", "weekday"),
    vehicle_type=fct_rev(vehicle_type),
    #vehicle_type=fct_rev(factor(vehicle_type, levels=c("Car", "Motorcycle","Taxi", "Bicycle"))), 
    local_authority_district=factor(local_authority_district, levels=c("Westminster", "Harrow"))
  ) |> 
  ggplot() +
  #geom_mosaic(aes(x=product(vehicle_type), fill=vehicle_type), alpha=.3, offset = 0.008)+
  geom_mosaic(aes(x=product(vehicle_type)), fill="#2171b5", alpha=.3, offset = 0.008)+
  #scale_fill_manual(values=c("#4daf4a", "#984ea3", "#377eb8", "#e41a1c"))+
  theme_v_gds() +
  coord_flip() +
  facet_wrap(~local_authority_district, nrow=2)+
  scale_x_productlist(position="top", name="")+
  theme(
    aspect.ratio = 1,
    panel.grid.major.y=element_blank(), panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank()
  ) 
plot_data <- ggplot_build(mosaic)$data |> as.data.frame() |> 
  group_by(PANEL) |>
  mutate(bor_total=sum(.wt)) |> ungroup() |>
  group_by(PANEL, x__vehicle_type) |>
  summarise(xmin=min(xmin),xmax=max(xmax), ymin=min(ymin),ymax=max(ymax),
            vehicle_prop_bor=sum(.wt)/sum(bor_total), bor_total=min(bor_total)) |> ungroup() |> 
  left_join(london_squared) |> 
  mutate(
    local_authority_district=factor(local_authority_district, levels=c("Westminster", "Harrow"))
  )


borough_select <- mosaic + 
  geom_rect(
    data=plot_data %>% inner_join(exp, by=c("x__vehicle_type"="vehicle_type","local_authority_district"="local_authority_district")) |> 
      mutate(max_diff=max(abs(diff)), diff_rescaled=map_scale(diff, -max_diff, max_diff, 0,1)),
    aes(xmin=xmin, xmax=xmax, ymin=.5, ymax=diff_rescaled), fill="#2171b5"
  ) +
  geom_text(
    data=plot_data, 
    aes(
      x=xmin+0.5*(xmax-xmin), y=ymin+0.5*(ymax-ymin),label=x__vehicle_type, size=vehicle_prop_bor
    ), 
    alpha=.7,family="Avenir Book")+
  scale_size(range=c(0, 10))+
  guides(size=FALSE, alpha=FALSE)+
  labs(subtitle="direct encoded <br> <span style = 'color: #08306b;'>expectation</span>") +
  #theme_v_gds()+
  theme(
    panel.grid.major.y=element_blank(), panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), 
    #legend.position = "right",
    panel.spacing=unit(-0.1, "lines"),
    plot.margin = margin(r=25),
    plot.subtitle = ggtext::element_markdown(size=10, hjust=1),
    legend.text=element_text(size=6), legend.key.size = unit(.4, 'cm'), legend.title = element_text(size=7)
  ) 



c("Car", "Motorcycle","Taxi", "Bicycle") 

c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a")
(red - blue - purple - green)


temp <- ped_veh |>
  left_join(london_squared) |> 
  filter(
    !is.na(BOR), 
    local_authority_district %in% c("Westminster", "Harrow"),
    vehicle_type %in% c("Car", "Motorcycle","Taxi", "Bicycle")
  ) |> 
  group_by(local_authority_district) |> 
  summarise(bor_total_filtered=n())


london_squared <- london_squared |> left_join(temp)

mosaic <- ped_veh |>
  left_join(london_squared) |>
  filter(
    !is.na(BOR), 
    local_authority_district %in% c("Westminster", "Harrow"),
    vehicle_type %in% c("Car", "Motorcycle","Taxi", "Bicycle")
  ) |> 
  mutate(
    is_weekend=if_else(day_of_week %in% c("Saturday", "Sunday"), "weekend", "weekday"),
    vehicle_type=fct_rev(factor(vehicle_type, levels=c("Car", "Motorcycle","Taxi", "Bicycle"))),
    local_authority_district=factor(local_authority_district, levels=c("Westminster", "Harrow"))
  ) |> 
  ggplot() +
  geom_mosaic(aes(x=product(is_weekend, vehicle_type), alpha=is_weekend, fill=vehicle_type), offset = 0.008)+
  #scale_fill_manual(values=c("#377eb8","#ff7f00"))+
  #scale_fill_manual(values=c("#2171b5", "#525252"))+
  theme_v_gds() +
  coord_flip() +
  facet_wrap(~local_authority_district, nrow=2)+
  scale_x_productlist(position="top", name="")+
  scale_alpha_discrete(range=c(.3,.9))+
  scale_fill_manual(values=c("#4daf4a", "#984ea3", "#377eb8", "#e41a1c"))+
  theme(
    aspect.ratio = 1,
    panel.grid.major.y=element_blank(), panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank()
  ) 
plot_data <- ggplot_build(mosaic)$data |> as.data.frame() |> 
  group_by(PANEL) |>
  mutate(bor_total=sum(.wt)) |> ungroup() |>
  group_by(PANEL, x__fill__vehicle_type) |>
  summarise(xmin=min(xmin),xmax=max(xmax), ymin=min(ymin),ymax=max(ymax),
            vehicle_prop_bor=sum(.wt)/sum(bor_total), bor_total_filtered=min(bor_total)) |> ungroup() |> 
  left_join(london_squared, by=c("bor_total_filtered"="bor_total_filtered")) |> 
  mutate(
    local_authority_district=factor(local_authority_district, levels=c("Westminster", "Harrow"))
  )


c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a")

borough_select_fill <- mosaic + 
  geom_text(
    data=plot_data, 
    aes(
      x=xmin+0.5*(xmax-xmin), y=ymin+0.5*(ymax-ymin),label=x__fill__vehicle_type, size=vehicle_prop_bor
    ), 
    alpha=.7,family="Avenir Book")+
  scale_size(range=c(0, 10))+
  scale_alpha_discrete(range=c(0.3,0.9))+
  guides(size=FALSE, alpha=FALSE, fill=FALSE)+
  labs(subtitle="hue - associative", 
       caption=" <p>
       <span style = 'color: #e41a1c;'>car</span>
       <span style = 'color: #377eb8;'>mbike</span>
       <span style = 'color: #984ea3;'>taxi</span>
       <span style = 'color: #4daf4a;'>bike</span>
       </p>"
       ) +
  #theme_v_gds()+
  theme(
    panel.grid.major.y=element_blank(), panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), 
    #legend.position = "right",
    panel.spacing=unit(-0.1, "lines"),
    plot.margin = margin(r=25),
    plot.subtitle = ggtext::element_markdown(size=10, hjust=0),
    plot.caption=ggtext::element_markdown(size=9, hjust=.5), plot.caption.position = "panel",
    legend.key.size = unit(.4, 'cm'), legend.title = element_text(size=7)
  )



plot_data <- ped_veh |> 
  filter(
    !is.na(casualty_imd_decile), !is.na(driver_imd_decile),
    casualty_imd_decile!="Data missing or out of range", 
    driver_imd_decile!="Data missing or out of range", !is.na(crash_quintile)) |>   
  mutate(grand_total=n()) |> 
  group_by(driver_imd_quintile) |> 
  mutate(row_total=n()) |> ungroup() |> 
  group_by(casualty_imd_quintile) |> 
  mutate(col_total=n()) |> ungroup() |> 
  group_by(casualty_imd_quintile, driver_imd_quintile) |> 
  summarise(
    observed=n(), 
    row_total=first(row_total), 
    col_total=first(col_total),
    grand_total=first(grand_total),
    expected=(row_total*col_total)/grand_total,
    expected_share_row=expected/row_total,
    row_marginal=row_total,
    col_marginal=col_total,
    expected_share_col=expected/col_total,
    resid=(observed-expected)/sqrt(expected),
    max_resid=max(abs(resid))
  ) |> ungroup()



plot_imd_driver <- plot_data |> 
  ggplot(aes(x=casualty_imd_quintile, y=driver_imd_quintile)) +
  geom_point()+
  geom_tile(aes(fill=expected), colour="#707070", size=.2) +
  #annotate("segment", x=0.5,xend=5.5, y=0.2,yend=0.2, arrow=arrow(type="closed", length = unit(0.2, "cm")), size=.1) +
  annotate("text", x=5.5,y=0.2, label="least deprived", hjust=1, size=3, family="Roboto Condensed Light") +
  annotate("text", x=0.5,y=0.2, label="most deprived", hjust=0, size=3, family="Roboto Condensed Light") +
  annotate("text", y=5.5,x=0.2, label="least deprived", hjust=1, size=3, angle=90, family="Roboto Condensed Light") +
  annotate("text", y=0.5,x=0.2, label="most deprived", hjust=0, size=3, angle=90, family="Roboto Condensed Light") +
  annotate("text", y=5.5,x=0, label="", hjust=1, size=2) +
  annotate("text", x=6.5,y=0, label="", hjust=1, size=2) +
  annotate("text", y=3,x=5.8, label="IMD of driver", hjust=.5, vjust=0, size=3.5, family="Roboto Condensed Light", angle=270) +
  annotate("text", y=6,x=3, label="IMD of pedestrian", hjust=.5, vjust=1, size=3.5, family="Roboto Condensed Light") +
  scale_fill_distiller(palette="Blues", direction=1) +
  theme_v_gds() +
  labs(subtitle="Obvs") +
  theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    axis.text.x=element_blank(), 
    axis.text.y = element_blank(), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position="right")

