# Manuscript and Supplement Figures

# Setup ---------------------------------------------------------------

library(tidyverse)
library(ggtern)
library(ggpubr)
# library(scico)
# library(ggalt)
# library(corrplot)
library(HMSC)


E <- readRDS("manuscript_functions/fixedLandscapes/orig-no-seed-E.RDS")
source("manuscript_functions/output_processing_fx.R")


outsfolderpath <- "manuscript_outputs/"
tiff_path <- paste(outsfolderpath, "tiff_files/")

if(dir.exists(tiff_path) == FALSE){
  dir.create(tiff_path)
}

scenarios <- c("FIG2A", "FIG2B", "FIG2C", "FIG2D", "FIG3A", "FIG3B", "FIG3C")
new_scen <- LETTERS[1:7]

# Theme -------------------------------------------------------

mytheme <- function(data, plotMain = NULL, type = NULL){
  data %>% 
    ggtern(aes(x = env, z = spa, y = codist, size = r2, shape = iteration)) +
    scale_T_continuous(limits=c(0,1),
                       breaks=seq(0, 0.8,by=0.2),
                       labels=seq(0,0.8, by= 0.2)) +
    scale_L_continuous(limits=c(0,1),
                       breaks=seq(0, 0.8,by=0.2),
                       labels=seq(0, 0.8,by=0.2)) +
    scale_R_continuous(limits=c(0,1),
                       breaks=seq(0, 0.8,by=0.2),
                       labels=seq(0, 0.8,by=0.2)) +
    labs(title = plotMain,
         x = "E",
         xarrow = "Environment",
         y = "Co",
         yarrow = "Co-Distribution",
         z = "S", 
         zarrow = "Spatial Autocorrelation") +
    theme_bw() +
    theme_showarrows() +
    theme_arrowlong() +
    scale_shape_manual(values = c(21:25), guide = FALSE) +
    theme(
      #panel.grid = element_line(color = "darkgrey", size = 0.6),
      plot.tag = element_text(size = 11),
      plot.title = element_text(size = 11, hjust = 0.1 , margin = margin(t = 10, b = -20)),
      tern.axis.arrow = element_line(size = 1),
      tern.axis.arrow.text = element_text(size = 5),
      axis.text = element_text(size = 4),
      axis.title = element_text(size = 6),
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 8)
    ) +
    guides(size = guide_legend(title = expression(R^2), order = 1))
}
# Figures data ----------------------------------------------------------------

spp_data <- NULL
A2D_sites <- NULL
for(i in 1:4){
  spp <- get_species_data(outsfolderpath, scenarios[i]) %>% 
    mutate(scenario = new_scen[i])
  params <- get_fig2_params(outsfolderpath, scenario = scenarios[i]) %>% 
    mutate(dispersal = as.numeric(as.character(dispersal)),
           scenario = new_scen[i])
  
  left_join(spp, params) %>% 
    mutate(nicheCent = abs(nicheOptima - 0.5)) -> spp
  
  spp_data <- bind_rows(spp_data, spp)
  
  sites <- get_sites_data(outsfolderpath, scenarios[i]) %>% 
    mutate(E = rep(E, 5), 
           Edev = abs(E-0.5),
           scenario = new_scen[i])
  
  A2D_sites <- bind_rows(A2D_sites, sites)
}


E2G_sites <- NULL

for(i in 5:7){
  spp <- get_species_data(outsfolderpath, scenario = scenarios[i]) %>% 
    mutate(scenario = new_scen[i])
  params <- get_fig3_params(outsfolderpath, scenario = scenarios[i]) %>% 
    mutate(dispersal = as.numeric(as.character(dispersal)),
           scenario = new_scen[i])
  
  left_join(spp, params) %>% 
    mutate(nicheCent = abs(nicheOptima - 0.5)) -> spp
  
  spp_data <- bind_rows(spp_data, spp)
  
  sites <- get_sites_data(outsfolderpath, scenario = scenarios[i]) %>%
    mutate(E = rep(E, 5),
           Edev = abs(E-0.5),
           scenario = new_scen[i])
  
  
  E2G_sites <- bind_rows(E2G_sites, sites)
}
