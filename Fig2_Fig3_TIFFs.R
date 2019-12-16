# Running out of patience here
# I'm fixing this with little elegance
# Making each figure individually and grouping with ppt
# I know! But, no more energy left to fight gridExtra and ggtern

library(tidyverse)
library(ggtern)
library(ggpubr)

E <- readRDS("manuscript_functions/fixedLandscapes/orig-no-seed-E.RDS")
source("manuscript_functions/output_processing_fx.R")


outsfolderpath <- "manuscript_outputs/"
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


# Figure 2 ----------------------------------------------------------------



fig2species <- NULL
fig2sites <- NULL
for(i in 1:4){
  spp <- get_species_data(outsfolderpath, scenarios[i])
  sites <- get_sites_data(outsfolderpath, scenarios[i]) %>% 
    mutate(E = rep(E, 5), 
           Edev = abs(E-0.5))
  fig2species[[i]] <- spp
  fig2sites[[i]] <- sites
}
names(fig2species) <- scenarios[1:4]
names(fig2sites) <- scenarios[1:4]

fig2_spp_plots <- NULL
for(i in 1:4){
  plot <- base_spp_plot(fig2species[[i]],
                        colorVar = "iteration", plotMain = LETTERS[i]) +
    theme(legend.position = "none")
  ggsave(filename = paste0("manuscript_outputs/", names(fig2species)[i], "species.tiff"),
         dpi = 600, units = "in", height = 5, width = 5)
  
  fig2_spp_plots[[i]] <- plot
  
}

plot <- base_spp_plot(fig2species$FIG2A, colorVar = "iteration") +
  theme(legend.position = "bottom")
leg <- get_legend(plot)
as_ggplot(leg)
ggsave(filename = "manuscript_outputs/legendFig2spp.tiff", 
       dpi = 600, units = "in", width = 5)


fig2_sites_plots <- NULL
for(i in 1:4){
  plot_sites <- base_sites_plot(data = fig2sites[[i]],colorVar = "Edev", colorLegend = "Environmental deviation") +
    scale_color_viridis_c() + theme(legend.position = "none")
  
  ggsave(filename = paste0("manuscript_outputs/", names(fig2sites)[i], "sites.tiff"),
         dpi = 600, units = "in", height = 5, width = 5)
  
  fig2_sites_plots[[i]] <- plot_sites
}


plot <- base_sites_plot(data = fig2sites[[1]],colorVar = "Edev", colorLegend = "Environmental deviation") +
  scale_color_viridis_c() + theme(legend.position = "bottom",
                                  legend.box = "vertical")
leg <- get_legend(plot)
as_ggplot(leg)
ggsave(filename = "manuscript_outputs/legendFig2sites.tiff", 
       dpi = 600, units = "in", width = 5)



# Figure 3 ----------------------------------------------------------------


fig3spp <- get_species_data(outsfolderpath, scenario = "FIG3C")
fig3params <- get_fig3_params(outsfolderpath, "FIG3C") %>% 
  mutate(dispersal = as.numeric(as.character(dispersal)))

left_join(fig3spp, fig3params) %>% 
  dplyr::select(species, env, spa, codist, r2, iteration,
                prevalence, nicheOpt, dispersal) %>% 
  mutate(nicheCent = abs(nicheOpt - 0.5)) -> fig3spp

fig3sites <- get_sites_data(outsfolderpath, "FIG3C") %>% 
  mutate(E = rep(E, 5), 
         Edev = abs(E-0.5))


# SPECIES PLOT

### Option1 - color for niche centrality, shape for dispersal level
fig3spp %>% 
  ggtern(aes(x = env, z = spa, y = codist, size = r2,
             color = nicheCent, shape = as.factor(dispersal))) +
  geom_point(alpha = 0.8) +
  scale_T_continuous(limits=c(0.0,1.0),
                     breaks=seq(0.0,1.0,by=0.1),
                     labels=seq(0.0,1.0,by=0.1)) +
  scale_L_continuous(limits=c(0.0,1),
                     breaks=seq(0,1,by=0.1),
                     labels=seq(0,1,by=0.1)) +
  scale_R_continuous(limits=c(0.0,1.0),
                     breaks=seq(0,1,by=0.1),
                     labels=seq(0,1,by=0.1)) +
  labs(x = "E",
       xarrow = "Environment",
       y = "C",
       yarrow = "Co-Distribution",
       z = "S", 
       zarrow = "Spatial Autocorrelation") +
  theme_light() +
  theme_showarrows() +
  scale_color_continuous(low = "orange", high = "red") +
  scale_size_area(limits = c(0,1), breaks = seq(0,1,0.2)) +
  guides(color = guide_colorbar("Niche Centrality", order = 2), 
         size = guide_legend(title = expression(R^2), order = 1),
         shape = guide_legend("Dispersal", order = 3)) +
  theme(panel.grid = element_line(color = "darkgrey"),
        axis.text = element_text(size =5),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 12, margin = margin(t = 10, b = -20)),
        tern.axis.arrow = element_line(size = 1),
        legend.box = "horizontal")
ggsave("manuscript_outputs/FIG3speciesOPTION2.tiff", width = 10, height = 5, units = "in", dpi = 100)

### Option2 - color for niche optima, shape for dispersal level
pal1 <- viridisLite::plasma(20)
fig3plot_spp <- fig3spp %>% 
  ggtern(aes(x = env, z = spa, y = codist, size = r2,
             color = nicheOpt, shape = as.factor(dispersal))) +
  geom_point(alpha = 0.8) +
  scale_T_continuous(limits=c(0.0,1.0),
                     breaks=seq(0.0,1.0,by=0.1),
                     labels=seq(0.0,1.0,by=0.1)) +
  scale_L_continuous(limits=c(0.0,1),
                     breaks=seq(0,1,by=0.1),
                     labels=seq(0,1,by=0.1)) +
  scale_R_continuous(limits=c(0.0,1.0),
                     breaks=seq(0,1,by=0.1),
                     labels=seq(0,1,by=0.1)) +
  labs(x = "E",
       xarrow = "Environment",
       y = "C",
       yarrow = "Co-Distribution",
       z = "S", 
       zarrow = "Spatial Autocorrelation") +
  theme_light() +
  theme_showarrows() +
  #scale_color_brewer(type = "div", palette = "BrBG") +
  # scale_color_gradient2(low = "#BBDF27FF", mid = "#414487FF",
  #                       high = "#BBDF27FF", midpoint = 0.5) +
  #scale_color_viridis_c(begin = 0.9, end = 0.2) +
  scale_color_gradient2(low = pal1[5], mid = pal1[18], high = pal1[9], midpoint = 0.5) +
  scale_size_area(limits = c(0,1), breaks = seq(0,1,0.2)) +
  guides(color = guide_colorbar("Niche \n Optima", order = 2, barwidth = 8, nbins = 2), 
         size = guide_legend(title = expression(R^2), order = 1),
         shape = guide_legend("Dispersal", order = 3, override.aes = list(size = 3))) +
  theme(panel.grid = element_line(color = "darkgrey"),
        axis.text = element_text(size =5),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 12, margin = margin(t = 10, b = -20)),
        tern.axis.arrow = element_line(size = 1),
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.position = "bottom",
        legend.text = element_text(size = 8))

fig3plot_spp + theme(legend.position = "none")
ggsave(filename = "manuscript_outputs/FIG3species.tiff", height = 5, width = 5, units = "in", dpi = 600)

leg <- get_legend(fig3plot_spp)
as_ggplot(leg)
ggsave(filename = "manuscript_outputs/legendFig3species.tiff", 
       dpi = 600, units = "in", width = 5)

# SITES PLOT FIG3

fig3plot_sites <- base_sites_plot(data = fig3sites,colorVar = "Edev", colorLegend = "Environmental deviation") +
  scale_color_viridis_c() + theme(legend.position = "bottom",
                                  legend.box = "vertical")

fig3plot_sites + theme(legend.position = "none")
ggsave(filename = "manuscript_outputs/FIG3sites.tiff", height = 5, width = 5, units = "in", dpi = 600)

leg <- get_legend(fig3plot_sites)
as_ggplot(leg)
ggsave(filename = "manuscript_outputs/legendFig3sites.tiff", 
       dpi = 600, units = "in", width = 5)

#Write csv files for Mathew

write.csv(fig3spp, file = "manuscript_outputs/Fig3c_species.csv")
write.csv(fig3sites, file = "manuscript_outputs/Fig3c_sites.csv")



# INTERACTION MATRICES FIGURE 3 -------------------------------------------

library(corrplot)
library(HMSC)

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

tiff("manuscript_outputs/Fig3Interactions.tiff", res = 600, width = 5, height = 15, units = "in")
par(mfrow = c(5,1))
intplot(modelfile, 1)
intplot(modelfile, 2)
intplot(modelfile, 3)
intplot(modelfile, 4)
intplot(modelfile, 5)
dev.off()



# SUP FIG1 ---------------------------------------------------------------

# This is what used to be Figure 3a, in which we have half of the species with interactions
# The other half don't


fig3aspp <- get_species_data(outsfolderpath, scenario = "FIG3A")
fig3aparams <- get_fig3_params(outsfolderpath, "FIG3A") %>% 
  mutate(dispersal = as.numeric(as.character(dispersal)))

left_join(fig3aspp, fig3aparams) %>% 
  dplyr::select(species, env, spa, codist, r2, iteration,
                prevalence, nicheOpt, dispersal, intercol, interext) %>% 
  mutate(nicheCent = abs(nicheOpt - 0.5)) -> fig3a_spp

fig3a_sites <- get_sites_data(outsfolderpath, "FIG3A") %>% 
  mutate(E = rep(E, 5), 
         Edev = abs(E-0.5))

# FIG3A or SUP1 PLOT
pal1 <- viridisLite::plasma(20)
fig3a_spp %>% 
  ggtern(aes(x = env, z = spa, y = codist, size = r2,
             color = nicheOpt, shape = intercol)) +
  geom_point(alpha = 0.8) +
  scale_T_continuous(limits=c(0.0,1.0),
                     breaks=seq(0.0,1.0,by=0.1),
                     labels=seq(0.0,1.0,by=0.1)) +
  scale_L_continuous(limits=c(0.0,1),
                     breaks=seq(0,1,by=0.1),
                     labels=seq(0,1,by=0.1)) +
  scale_R_continuous(limits=c(0.0,1.0),
                     breaks=seq(0,1,by=0.1),
                     labels=seq(0,1,by=0.1)) +
  labs(x = "E",
       xarrow = "Environment",
       y = "C",
       yarrow = "Co-Distribution",
       z = "S", 
       zarrow = "Spatial Autocorrelation") +
  theme_light() +
  theme_showarrows() +
  #scale_color_gradient(low = "orange", high = "black") +
  # scale_color_gradient2(low = "#BBDF27FF", mid = "#414487FF",
  #                       high = "#BBDF27FF", midpoint = 0.5) +
  #scale_color_viridis_c(option = "magma") +
  scale_color_gradient2(low = pal1[5], mid = pal1[18], high = pal1[9], midpoint = 0.5) +
  scale_size_area(limits = c(0,1), breaks = seq(0,1,0.2)) +
  guides(color = guide_colorbar("Niche \n Optima", order = 2, barwidth = 8, nbins = 2), 
         size = guide_legend(title = expression(R^2), order = 1),
         shape = guide_legend("Interactions", order = 3, override.aes = list(size = 3))) +
  theme(panel.grid = element_line(color = "darkgrey"),
        axis.text = element_text(size =5),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 12, margin = margin(t = 10, b = -20)),
        tern.axis.arrow = element_line(size = 1),
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.position = "bottom",
        legend.text = element_text(size = 8)) -> fig3aplot_spp

fig3aplot_spp + theme(legend.position = "none")
ggsave(filename = "manuscript_outputs/SUP1species.tiff", height = 5, width = 5, units = "in", dpi = 600)

leg <- get_legend(fig3aplot_spp)
as_ggplot(leg)
ggsave(filename = "manuscript_outputs/legendSUP1_species.tiff", 
       dpi = 600, units = "in", width = 5)

# SITES PLOT FIG3

fig3aplot_sites <- base_sites_plot(data = fig3a_sites,colorVar = "Edev", colorLegend = "Environmental deviation") +
  scale_color_viridis_c() + theme(legend.position = "bottom",
                                  legend.box = "vertical")

fig3aplot_sites + theme(legend.position = "none")
ggsave(filename = "manuscript_outputs/SUP1sites.tiff", height = 5, width = 5, units = "in", dpi = 600)

leg <- get_legend(fig3aplot_sites)
as_ggplot(leg)
ggsave(filename = "manuscript_outputs/legendSUP1sites.tiff", 
       dpi = 600, units = "in", width = 5)




# SUP FIG2 ---------------------------------------------------------------

# This is what used to be Figure 3a, in which we have half of the species with interactions
# The other half don't


fig3bspp <- get_species_data(outsfolderpath, scenario = "FIG3B")
fig3bparams <- get_fig3_params(outsfolderpath, "FIG3B") %>% 
  mutate(dispersal = as.numeric(as.character(dispersal)))

left_join(fig3bspp, fig3bparams) %>% 
  dplyr::select(species, env, spa, codist, r2, iteration,
                prevalence, nicheOpt, dispersal, intercol, interext) %>% 
  mutate(nicheCent = abs(nicheOpt - 0.5)) -> fig3b_spp

fig3b_sites <- get_sites_data(outsfolderpath, "FIG3B") %>% 
  mutate(E = rep(E, 5), 
         Edev = abs(E-0.5))

# FIG3B or SUP2 plot
pal1 <- viridisLite::plasma(20)
fig3b_spp %>% 
  ggtern(aes(x = env, z = spa, y = codist, size = r2,
             color = nicheOpt, shape = as.factor(dispersal))) +
  geom_point(alpha = 0.8) +
  scale_T_continuous(limits=c(0.0,1.0),
                     breaks=seq(0.0,1.0,by=0.1),
                     labels=seq(0.0,1.0,by=0.1)) +
  scale_L_continuous(limits=c(0.0,1),
                     breaks=seq(0,1,by=0.1),
                     labels=seq(0,1,by=0.1)) +
  scale_R_continuous(limits=c(0.0,1.0),
                     breaks=seq(0,1,by=0.1),
                     labels=seq(0,1,by=0.1)) +
  labs(x = "E",
       xarrow = "Environment",
       y = "C",
       yarrow = "Co-Distribution",
       z = "S", 
       zarrow = "Spatial Autocorrelation") +
  theme_light() +
  theme_showarrows() +
  #scale_color_gradient(low = "orange", high = "black") +
  # scale_color_gradient2(low = "#BBDF27FF", mid = "#414487FF",
  #                       high = "#BBDF27FF", midpoint = 0.5) +
  #scale_color_viridis_c(option = "magma") +
  scale_color_gradient2(low = pal1[5], mid = pal1[18], high = pal1[9], midpoint = 0.5) +
  scale_size_area(limits = c(0,1), breaks = seq(0,1,0.2)) +
  guides(color = guide_colorbar("Niche \n Optima", order = 2, barwidth = 8, nbins = 2), 
         size = guide_legend(title = expression(R^2), order = 1),
         shape = guide_legend("Dispersal", order = 3, override.aes = list(size = 3))) +
  theme(panel.grid = element_line(color = "darkgrey"),
        axis.text = element_text(size =5),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 12, margin = margin(t = 10, b = -20)),
        tern.axis.arrow = element_line(size = 1),
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.position = "bottom",
        legend.text = element_text(size = 8)) -> fig3bplot_spp

fig3bplot_spp + theme(legend.position = "none")
ggsave(filename = "manuscript_outputs/SUP2species.tiff", height = 5, width = 5, units = "in", dpi = 600)

leg <- get_legend(fig3bplot_spp)
as_ggplot(leg)
ggsave(filename = "manuscript_outputs/legendSUP2_species.tiff", 
       dpi = 600, units = "in", width = 5)

# SITES PLOT FIG3

fig3bplot_sites <- base_sites_plot(data = fig3b_sites,colorVar = "Edev", colorLegend = "Environmental deviation") +
  scale_color_viridis_c() + theme(legend.position = "bottom",
                                  legend.box = "vertical")

fig3bplot_sites + theme(legend.position = "none")
ggsave(filename = "manuscript_outputs/SUP2sites.tiff", height = 5, width = 5, units = "in", dpi = 600)

leg <- get_legend(fig3bplot_sites)
as_ggplot(leg)
ggsave(filename = "manuscript_outputs/legendSUP2sites.tiff", 
       dpi = 600, units = "in", width = 5)





