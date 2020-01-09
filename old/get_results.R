outsfolderpath <- "metaco2-manuscript-figs/2019-05-07_hi_disp_30kchain/"

source("functions/output_processing_fx.R")
source("functions/convergence_fx.R")


library(tidyverse)
library(ggtern)
library(corrplot)
library(HMSC)
library(coda)

scenarios <- c("FIG2A", "FIG2B", "FIG2C", "FIG2D", "FIG3A", "FIG3B", "FIG3C")


pdf(paste0(outsfolderpath, "speciesFigs.pdf"))
for(i in 1:7){
  data <- get_species_data(outsfolderpath, scenarios[i])
  plotspp <- species_plot(data, plotMain = paste0(scenarios[i], "_spp"), colorVar = "iteration")
  print(plotspp)
}
dev.off()


E <- readRDS("functions/fixedLandscapes/orig-no-seed-E.RDS")

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


# Convergence -------------------------------------------------------------

dir.create(paste0(outsfolderpath, "convergence/"))

folderpath <- paste0(outsfolderpath, "convergence/")


# Probable get a pdf for each model, but a gelman plot for each scenario

for(i in 1:7){
  listChains <- get_mcmc_lists(folderpath = outsfolderpath,
                               scenario = scenarios[i])
  
  assign(x = paste0(scenarios[i], "_chains"),
         value = listChains)
  
  gelman.diag(listChains)
  pdf(paste0(folderpath, scenarios[i], "gelman_diag.pdf"))
  gelman.plot(listChains)
  dev.off()
  
  
  
  pdf(paste0(folderpath, scenarios[i], "chain_plots.pdf"))
  plot(listChains)
  dev.off()
  
  raftery_on_one <- raftery.diag(listChains[[1]])
  write.csv(raftery_on_one$resmatrix, file = paste0(folderpath, scenarios[i], "raftery_diag.csv"))
  
}