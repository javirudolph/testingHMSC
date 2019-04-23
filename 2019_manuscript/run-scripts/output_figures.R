# This will create pdfs kind of fast for the species and sites, also the species interaction matrices

source("functions/output_processing_fx.R")

library(tidyverse)
library(ggtern)
library(corrplot)

outsfolderpath <- "give your path here"
scenarios <- c("FIG2A", "FIG2B", "FIG2C", "FIG2D", "FIG3A", "FIG3B", "FIG3C")




pdf(paste0(outsfolderpath, "speciesFigs.pdf"))
for(i in 1:7){
  data <- get_species_data(outsfolderpath, scenarios[i])
  plotspp <- species_plot(data, plotMain = paste0(scenarios[i], "_spp"), colorVar = "iteration")
  print(plotspp)
}
dev.off()


E <- readRDS("outputs/fixedLandscapes/orig-no-seed-E.RDS")

pdf(paste0(outsfolderpath, "sitesFigs.pdf"))
for(i in 1:7){
  data <- get_sites_data(outsfolderpath, scenarios[i]) %>% 
    mutate(E = rep(E, 5), 
           Edev = abs(E-0.5))
  plotspp <- sites_plot(data, plotMain = paste0(scenarios[i], "_sites"), 
                        colorVar = "Edev", colorLegend = "Environmental deviation") +
    scale_color_viridis_c()
  print(plotspp)
}
dev.off()


pdf(paste0(outsfolderpath, "interactionMatrices.pdf"))
for(i in 1:7){
  interaction_plot(outsfolderpath, scenarios[i])
}
dev.off()
