
#Functions to make the ternary plots...
# first at the species level

make_tern_plot <- function(figData, varShape = NULL, varColor = NULL){
  figData %>% 
    ggtern(aes(x = env, z = spa, y = codist, size = r2)) +
    scale_T_continuous(limits=c(0,1.0),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    scale_L_continuous(limits=c(0.0,1),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    scale_R_continuous(limits=c(0.0,1.0),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    labs(x = "Environment",
         y = "Codistribution",
         z = "Spatial") +
    geom_point(aes_string(shape = varShape, color = varColor), alpha = 0.7) +
    theme_minimal() +
    guides(size = guide_legend(order = 1,
                               title = expression(paste(R^{2}))),
           shape = guide_legend("none"),
           # shape = guide_legend(order = 2,
           #                      title = varShape,
           #                      override.aes = list(size = 3)),
           col = guide_colourbar(title = varColor,
                                 order = 3)) +
    theme(#legend.position = "bottom", legend.box = "vertical",
      panel.grid = element_line(color = "darkgrey"),
      axis.title = element_text(size = 7))
}