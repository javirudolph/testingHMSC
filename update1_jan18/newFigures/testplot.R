datFig2 %>% filter(., scenario == "Fig2a") %>% 
  arrange(desc(r2)) %>% 
  ggtern(aes(x = env, z = spa, y = codist)) +
  scale_T_continuous(limits=c(0,1.0),
                     breaks=seq(0,1,by=0.1),
                     labels=seq(0,1,by=0.1)) + 
  scale_L_continuous(limits=c(0.0,1),
                     breaks=seq(0,1,by=0.1),
                     labels=seq(0,1,by=0.1)) +
  scale_R_continuous(limits=c(0.0,1.0),
                     breaks=seq(0,1,by=0.1),
                     labels=seq(0,1,by=0.1)) +
  theme_showarrows() +
  labs(xarrow = "Environment",
       yarrow = "Codistribution",
       zarrow = "Spatial") +
  geom_point(aes(size = r2, shape = iteration, color = nicheOpt), alpha = 0.5) +
  scale_color_viridis_c() +
  #theme_minimal() +
  guides(size = guide_legend(order = 1,
                             title = expression(paste(R^{2}))),
         shape = guide_legend(order = 2,
                              title = NULL,
                              override.aes = list(size = 4)),
         col = guide_colourbar(title = "Niche optima",
                               order = 3)) +
  theme(legend.position = "bottom", 
        legend.box = "vertical",
        axis.title = element_text(colour = "white"),
        panel.grid = element_line(colour = "darkgrey"),
        panel.border = element_rect(colour = "darkgrey"),
        panel.background = element_rect(fill = "white"))
