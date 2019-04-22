
source("functions/output_processing_fx.R")
library(tidyverse)

outsfolderpath <- "outputs/MANUSCRIPT/"
scenarios <- c("FIG2A", "FIG2B", "FIG2C", "FIG2D", "FIG3A", "FIG3B", "FIG3C")


for(i in 1:7){
  dataspp <- get_species_data(folderpath = outsfolderpath, scenario = scenarios[i])
  write.csv(dataspp, file = paste0(outsfolderpath, scenarios[i], "spp.csv"))
  
  datasites <- get_sites_data(folderpath = outsfolderpath, scenario = scenarios[i])
  write.csv(datasites, file = paste0(outsfolderpath, scenarios[i], "sites.csv"))
}


csvFiles <- list.files("outputs/MANUSCRIPT/")
sppFiles <- csvFiles[c(grep(pattern = "spp", x = csvFiles))]
sitesFiles <- csvFiles[-c(grep(pattern = "spp", x = csvFiles))]


        


pdf("outputs/MANUSCRIPT/speciesFigs.pdf")
for(i in 1:7){
  data <- read.csv(paste0("outputs/MANUSCRIPT/", sppFiles[i])) 
  plotspp <- species_plot(data, plotMain = sppFiles[i], colorVar = "iteration")
  print(plotspp)
}
dev.off()

E <- readRDS("outputs/fixedLandscapes/orig-no-seed-E.RDS")

pdf("outputs/MANUSCRIPT/sitesFigs.pdf")
for(i in 1:7){
  data <- read.csv(paste0("outputs/MANUSCRIPT/", sitesFiles[i])) %>% 
    mutate(E = rep(E, 5), 
           Edev = abs(E-0.5))
  plotspp <- sites_plot(data, plotMain = sitesFiles[i], 
                        colorVar = "Edev", colorLegend = "Environmental deviation") +
    scale_color_viridis_c()
  print(plotspp)
}
dev.off()



