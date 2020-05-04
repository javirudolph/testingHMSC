
fig3_spp %>% 
  filter(., scenario == "FIG3C") %>% 
  ggtern(aes(x = env, z = spa, y = codist, size = r2)) +
  scale_T_continuous(limits=c(0,1),
                     breaks=seq(0, 0.8,by=0.2),
                     labels=seq(0,0.8, by= 0.2)) +
  scale_L_continuous(limits=c(0,1),
                     breaks=seq(0, 0.8,by=0.2),
                     labels=seq(0, 0.8,by=0.2)) +
  scale_R_continuous(limits=c(0,1),
                     breaks=seq(0, 0.8,by=0.2),
                     labels=seq(0, 0.8,by=0.2)) +
  labs(title = " ",
       x = "E",
       xarrow = "Environment",
       y = "Co",
       yarrow = "Co-Distribution",
       z = "S", 
       zarrow = "Spatial Autocorrelation") +
  theme_bw() +
  theme_showarrows() +
  theme_arrowlong() +
  scale_size_area(limits = c(0,1), breaks = seq(0,1,0.2)) +
  theme(
    #panel.grid = element_line(color = "darkgrey", size = 0.6),
    plot.tag = element_text(size = 11),
    plot.title = element_text(size = 11, hjust = 0.1 , margin = margin(t = 10, b = -20)),
    tern.axis.arrow = element_line(size = 1),
    tern.axis.arrow.text = element_text(size = 5),
    axis.text = element_text(size = 4),
    axis.title = element_text(size = 6),
    # legend.position = "bottom",
    # legend.box = "vertical",
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8)
  ) -> base


base +
  geom_encircle(aes(group = dispersal, fill = as.factor(dispersal)), alpha = 0.2, size = 0.2) +
  #scale_fill_manual(values = c("#440154FF", "#482878FF", "#3E4A89FF"))
  scale_fill_viridis_d() +
  #geom_point(aes(shape = as.factor(nicheOpt))) +
  geom_point(aes(shape = as.factor(nicheCent), color = as.factor(nicheOpt))) +
  scale_color_manual(values = c("#440154FF", "#440154FF", "#3E4A89FF", "#3E4A89FF")) +
  scale_shape_manual(values = c(15, 16)) +
  guides(color = guide_colorbar(title = "Niche\n optima", order = 2),
         #color = guide_colorbar(title = "Dispersal", order = 2),
         shape = guide_legend(title = "Dispersal", order = 3),
         size = guide_legend(title = expression(R^2), order = 1))
  

scale_shape_manual(values = c(17:19), guide = FALSE) +
  #scale_color_viridis_c(limits = c(0, 1)) +
  scale_color_gradient(low = "#450256", high = "#F9E721") +
  #scale_color_gradient2(low = pal1[20], mid = pal1[2], high = pal1[15], midpoint = 0.5) +
  scale_fill_viridis_d() +
   +
   +
  
