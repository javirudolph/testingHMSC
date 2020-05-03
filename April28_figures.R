# Manuscript Figures and Supplement figures

# Setup ---------------------------------------------------------------

library(tidyverse)
library(ggtern)
library(ggpubr)
library(patchwork)
library(corrplot)
library(HMSC)


E <- readRDS("manuscript_functions/fixedLandscapes/orig-no-seed-E.RDS")
source("manuscript_functions/output_processing_fx.R")


outsfolderpath <- "manuscript_outputs/"
tiff_path <- paste(outsfolderpath, "tiff_files/")

if(dir.exists(tiff_path) == FALSE){
  dir.create(tiff_ppath)
}

scenarios <- c("FIG2A", "FIG2B", "FIG2C", "FIG2D", "FIG3A", "FIG3B", "FIG3C")


# Figure 2 - data ----------------------------------------------------------------

fig2_spp <- NULL
fig2_sites <- NULL
for(i in 1:4){
  spp <- get_species_data(outsfolderpath, scenarios[i])
  sites <- get_sites_data(outsfolderpath, scenarios[i]) %>% 
    mutate(E = rep(E, 5), 
           Edev = abs(E-0.5))
  fig2_spp <- bind_rows(fig2_spp, spp)
  fig2_sites <- bind_rows(fig2_sites, sites)
}




# Figure 2 - sites Figure ------------------------------------------------

fig2_sites_plot <- function(data, plotMain = NULL, colorVar = NULL, colorLegend = "none"){
  data %>% 
    ggtern(aes(x = env, z = spa, y = codist, size = r2, color = Edev)) +
    geom_point(alpha = 0.6) +
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
    scale_size_area(limits = c(0,maxSize), breaks = seq(0,maxSize, round(maxSize/7, digits=3))) +
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
    guides(color = guide_colorbar(colorLegend, order = 2), 
           size = guide_legend(title = expression(R^2), order = 1))
}


f2aSites <- fig2_sites %>% 
  filter(., scenario == "FIG2A") %>% 
  fig2_sites_plot(plotMain = "A'") +
  theme(legend.position = "none")

f2bSites <- fig2_sites %>% 
  filter(., scenario == "FIG2B") %>% 
  fig2_sites_plot(plotMain = "B'") +
  theme(legend.position = "none")

f2cSites <- fig2_sites %>% 
  filter(., scenario == "FIG2C") %>% 
  fig2_sites_plot(plotMain = "C'") +
  theme(legend.position = "none")

f2dSites <- fig2_sites %>% 
  filter(., scenario == "FIG2D") %>% 
  fig2_sites_plot(plotMain = "D'") +
  theme(legend.position = "none")

baseSites <- fig2_sites %>% 
  filter(., scenario == "FIG2D") %>% 
  fig2_sites_plot(colorLegend = "Environmental\n deviation") +
  theme(legend.position = "right",
        legend.box = "vertical")

f2SitesLeg <- get_legend(plot)
fig2sitesLegend <- as_ggplot(f2SitesLeg)
emptyplot <- ggplot() + theme_void()


sites2.grid <- grid.arrange(f2aSites, f2bSites,emptyplot, emptyplot, f2cSites, f2dSites,
                            ncol = 2, heights = c(1, 0.2, 1))
# ggsave(tern.grid, filename = "test.tiff", dpi = 600,
#        width = 5, height = 5)

sites2.grid.nice <- as_ggplot(sites2.grid)

f2sites <- ggpubr::ggarrange(sites2.grid.nice, fig2sitesLegend,
                             #ggarrange(fig2legend, fig2legend, nrow = 2, heights = c(1,2)),
                             widths = c(5,1))
f2sites
ggsave(f2sites, 
       #filename = paste0(tiff_path, "figure2_sites.tiff"),
       filename = "test2_sites.tiff",
       dpi = 600,
       width = 6, height = 5)

# Figure 2 - species Figure ---------------------------------------------------------------

fig2_spp_plot <- function(data, plotMain = NULL,
                          colorVar = NULL, colorLegend = NULL,
                          shapeVar = NULL, shapeLegend = NULL){
  data %>% 
    ggtern(aes(x = env, z = spa, y = codist, size = r2)) +
    geom_point(aes_string(color = colorVar, shape = shapeVar),
               alpha = 0.6) +
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
    scale_shape_manual(values = c(15:19), guide = FALSE) +
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
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 8)
    ) +
    guides(color = guide_legend(colorLegend, order = 2),
           #shape = guide_legend(shapeLegend, order = 3),
           size = guide_legend(title = expression(R^2), order = 1))
}

sp2a <- fig2_spp %>% 
  filter(., scenario == "FIG2A") %>% 
  fig2_spp_plot(shapeVar = "iteration", plotMain = "A.") +
  theme(legend.position = "none")

sp2b <- fig2_spp %>% 
  filter(., scenario == "FIG2B") %>% 
  fig2_spp_plot(shapeVar = "iteration", plotMain = "B.") +
  theme(legend.position = "none")

sp2c <- fig2_spp %>% 
  filter(., scenario == "FIG2C") %>% 
  fig2_spp_plot(shapeVar = "iteration", plotMain = "C.") +
  theme(legend.position = "none")

sp2d <- fig2_spp %>% 
  filter(., scenario == "FIG2D") %>% 
  fig2_spp_plot(shapeVar = "iteration", plotMain = "D.") +
  theme(legend.position = "none")

plot <- fig2_spp %>% 
  filter(., scenario == "FIG2D") %>% 
  fig2_spp_plot(shapeVar = "iteration") +
  theme(legend.position = "right")

leg <- get_legend(plot)
fig2legend <- as_ggplot(leg)
emptyplot <- ggplot() + theme_void()


tern.grid <- grid.arrange(sp2a, sp2b,emptyplot, emptyplot, sp2c, sp2d, ncol = 2, heights = c(1, 0.2, 1))
# ggsave(tern.grid, filename = "test.tiff", dpi = 600,
#        width = 5, height = 5)

tern.grid.nice <- as_ggplot(tern.grid)

figure2 <- ggpubr::ggarrange(tern.grid.nice, fig2legend,
                           #ggarrange(fig2legend, fig2legend, nrow = 2, heights = c(1,2)),
                           widths = c(5,1))
figure2
ggsave(figure2, 
       filename = paste0(tiff_path, "figure2_species.tiff"),
       #filename = "test2.tiff",
       dpi = 600,
       width = 6, height = 5)




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

