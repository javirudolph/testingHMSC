

outsfolderpath <- "metaco2-manuscript-figs/2019-05-07_hi_disp_30kchain/"

source("functions/output_processing_fx.R")


library(tidyverse)
library(ggtern)
library(corrplot)
library(HMSC)
library(coda)



scenarios <- c("FIG2A", "FIG2B", "FIG2C", "FIG2D", "FIG3A", "FIG3B", "FIG3C")
E <- readRDS("functions/fixedLandscapes/orig-no-seed-E.RDS")

# Get the species plots in a column for figure 2
spp_plotlist <- NULL
spp_data <- NULL
sites_plotlist <- NULL
sites_data <- NULL
for(i in 1:4){
  data <- get_species_data(outsfolderpath, scenarios[i])
  spp_data[[i]] <- data
  plotspp <- species_plot(data, plotMain ="", colorVar = "iteration")
  spp_plotlist[[i]] <- plotspp
}

for(i in 1:4){
  #SITES
  data_st <- get_sites_data(outsfolderpath, scenarios[i]) %>% 
    mutate(E = rep(E, 5), 
           Edev = abs(E-0.5))
  sites_data[[i]] <- data_st
  plotsites <- sites_plot(data_st, plotMain = "", 
                        colorVar = "Edev", colorLegend = "Environmental deviation") +
    scale_color_viridis_c()
  sites_plotlist[[i]] <- plotsites
}


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(spp_plotlist[[1]])




g <- arrangeGrob(spp_plotlist[[1]] + theme(legend.position = "none"),
                 spp_plotlist[[2]] + theme(legend.position = "none"), 
                 spp_plotlist[[3]] + theme(legend.position = "none"),
                 spp_plotlist[[4]] + theme(legend.position = "bottom"), nrow = 4)
ggsave("metaco2-manuscript-figs/2019-05-07_hi_disp_30kchain/g.tiff",
       g, height = 12, dpi = 600)
