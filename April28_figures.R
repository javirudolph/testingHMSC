# Manuscript Figures and Supplement figures

# Setup ---------------------------------------------------------------

library(tidyverse)
library(ggtern)
library(ggpubr)
library(corrplot)
library(HMSC)


E <- readRDS("manuscript_functions/fixedLandscapes/orig-no-seed-E.RDS")
source("manuscript_functions/output_processing_fx.R")


outsfolderpath <- "manuscript_outputs/"

dir.create(paste(outsfolderpath, "tiff_files/"))
tiff_path <- paste(outsfolderpath, "tiff_files/")

scenarios <- c("FIG2A", "FIG2B", "FIG2C", "FIG2D", "FIG3A", "FIG3B", "FIG3C")


# Plot functions ----------------------------------------------------------

base_spp_plot <- function(data, plotMain = NULL, colorVar = NULL, colorLegend = "none"){
  data %>% 
    ggtern(aes(x = env, z = spa, y = codist, size = r2)) +
    geom_point(aes_string(color = colorVar), alpha = 0.8) +
    scale_T_continuous(limits=c(0.0,1.0),
                       breaks=seq(0.0,1.0,by=0.1),
                       labels=seq(0.0,1.0,by=0.1)) +
    scale_L_continuous(limits=c(0.0,1),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    scale_R_continuous(limits=c(0.0,1.0),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    labs(title = plotMain,
         x = "E",
         xarrow = "Environment",
         y = "C",
         yarrow = "Co-Distribution",
         z = "S", 
         zarrow = "Spatial Autocorrelation") +
    theme_light() +
    theme_showarrows() +
    #scale_colour_brewer(palette = "Set1") +
    #scale_colour_brewer(palette = "Spectral") +
    #scale_color_viridis_d() +
    scale_size_area(limits = c(0,1), breaks = seq(0,1,0.2)) +
    guides(color = guide_legend(colorLegend, order = 2), 
           size = guide_legend(title = expression(R^2), order = 1)) +
    theme(panel.grid = element_line(color = "darkgrey"),
          axis.text = element_text(size =5),
          axis.title = element_text(size = 8),
          plot.title = element_text(size = 12, margin = margin(t = 10, b = -20)),
          tern.axis.arrow = element_line(size = 1))
}

base_sites_plot <- function(data, plotMain = NULL, colorVar = NULL, colorLegend = "none"){
  data %>% 
    ggtern(aes(x = env, z = spa, y = codist, size = r2)) +
    geom_point(aes_string(color = colorVar), alpha = 0.6) +
    scale_T_continuous(limits=c(0,1.0),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    scale_L_continuous(limits=c(0.0,1),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    scale_R_continuous(limits=c(0.0,1.0),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    labs(title = plotMain,
         x = "E",
         xarrow = "Environment",
         y = "C",
         yarrow = "Co-Distribution",
         z = "S", 
         zarrow = "Spatial Autocorrelation") +
    theme_light() +
    theme_showarrows() +
    #scale_colour_brewer(palette = "Set1") +
    #scale_colour_brewer(palette = "Spectral") +
    #scale_color_viridis_d() +
    scale_size_area(limits = c(0, 0.003), breaks = seq(0, 0.003, 0.0005)) +
    guides(color = guide_colorbar(colorLegend, order = 2), 
           size = guide_legend(title = expression(R^2), order = 1)) +
    theme(panel.grid = element_line(color = "darkgrey"),
          axis.text = element_text(size =5),
          axis.title = element_text(size = 8),
          plot.title = element_text(size = 12, margin = margin(t = 10, b = -20)),
          tern.axis.arrow = element_line(size = 1))
}
# Figure 2 - data ----------------------------------------------------------------

fig2_spp <- NULL
fig2_sites <- NULL
for(i in 1:4){
  i <- 1
  spp <- get_species_data(outsfolderpath, scenarios[i])
  sites <- get_sites_data(outsfolderpath, scenarios[i]) %>% 
    mutate(E = rep(E, 5), 
           Edev = abs(E-0.5))
  fig2_spp <- bind_rows(fig2_spp, spp)
  fig2_sites <- bind_rows(fig2_sites, sites)
}


# Figure 2 - species Figure ---------------------------------------------------------------

sp2a <- fig2_spp %>% 
  filter(., scenario == "FIG2A")



# Figure 3 ----------------------------------------------------------------

fig3_spp <- NULL
fig3_sites <- NULL

for(i in 5:7){
  spp <- get_species_data(outsfolderpath, scenario = scenarios[i])
  params <- get_fig3_params(outsfolderpath, scenario = scenarios[i]) %>% 
    mutate(dispersal = as.numeric(as.character(dispersal)))
  
  left_join(spp, params) %>% 
    mutate(nicheCent = abs(nicheOpt - 0.5)) -> spp
  
  fig3_spp <- bind_rows(fig3_spp, spp)
  
  sites <- get_sites_data(outsfolderpath, scenario = scenarios[i]) %>%
    mutate(E = rep(E, 5),
           Edev = abs(E-0.5))


  fig3_sites <- bind_rows(fig3_sites, sites)
}


# INTERACTION MATRICES FIGURE 3 -------------------------------------------


modelfile <- readRDS(paste0(outsfolderpath, "FIG3C", "-model.RDS"))

intplot <- function(modelfile, iteration){
  assoMat <- corRandomEff(modelfile[[iteration]])
  siteMean <- apply(assoMat[ , , , 1], 1:2, mean)
  
  siteDrawCol <- matrix(NA, nrow = nrow(siteMean), ncol = ncol(siteMean))
  siteDrawCol[which(siteMean > 0.4, arr.ind=TRUE)]<-"red"
  siteDrawCol[which(siteMean < -0.4, arr.ind=TRUE)]<-"blue"
  
  # Build matrix of "significance" for corrplot
  siteDraw <- siteDrawCol
  siteDraw[which(!is.na(siteDraw), arr.ind = TRUE)] <- 0
  siteDraw[which(is.na(siteDraw), arr.ind = TRUE)] <- 1
  siteDraw <- matrix(as.numeric(siteDraw), nrow = nrow(siteMean), ncol = ncol(siteMean))
  
  Colour <- colorRampPalette(c("blue", "white", "red"))(200)
  corrplot(siteMean, method = "color", col = Colour, type = "lower",
           diag = FALSE, p.mat = siteDraw, tl.srt = 45)
}

# tiff(paste(tiff_path, "Fig3Interactions.tiff"), res = 600, width = 5, height = 15, units = "in")
par(mfrow = c(5,1))
intplot(modelfile, 1)
intplot(modelfile, 2)
intplot(modelfile, 3)
intplot(modelfile, 4)
intplot(modelfile, 5)
# dev.off()

