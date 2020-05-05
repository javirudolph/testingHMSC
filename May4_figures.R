# Manuscript and Supplement Figures

# Setup ---------------------------------------------------------------

library(tidyverse)
library(ggtern)
library(ggpubr)
library(ggalt)
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
    guides(size = guide_legend(title = expression(R^2), order = 1)) -> plot

}

# Figure 2 - data ----------------------------------------------------------------

fig2_spp <- NULL
fig2_sites <- NULL
for(i in 1:4){
  spp <- get_species_data(outsfolderpath, scenarios[i])
  params <- get_fig2_params(outsfolderpath, scenario = scenarios[i]) %>% 
    mutate(dispersal = as.numeric(as.character(dispersal)))
  
  left_join(spp, params) %>% 
    mutate(nicheCent = abs(nicheOptima - 0.5)) -> spp
  
  fig2_spp <- bind_rows(fig2_spp, spp)
  
  sites <- get_sites_data(outsfolderpath, scenarios[i]) %>% 
    mutate(E = rep(E, 5), 
           Edev = abs(E-0.5))
  
  fig2_sites <- bind_rows(fig2_sites, sites)
}




# No interactions, change niche breadth
# Scenario that says 


# Figure 2, no competition -----------------------------------------------------
 
fig2_spp %>% 
  filter(., scenario %in% c("FIG2A", "FIG2B")) %>%
  mutate(nicheBreadth = ifelse(nicheBreadth == 0.8, "Narrow niche", "Broad niche"),
         nicheBreadth = factor(nicheBreadth, levels = c("Narrow niche", "Broad niche")),
         iteration = str_remove(iteration, "iter_")) %>% 
  mytheme() +
  geom_point(fill = "black", alpha = 0.7) +
  scale_size_area(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  facet_wrap(~nicheBreadth, nrow = 2, strip.position = "left") +
  theme(
    #strip.background = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical"
  ) +
  guides(size = guide_legend(title = expression(R^2), nrow = 1, label.position = "bottom", order = 1),
         shape = guide_legend(title = "Iteration", order = 2)) -> A
#ggsave("test1.tiff", dpi = 600, width = 3, height = 6)



fig2_sites %>% 
  filter(., scenario %in% c("FIG2A", "FIG2B")) %>% 
  mytheme() +
  geom_point(aes(color = Edev, fill = Edev), alpha = 0.7) +
  scale_size_area(limits = c(0,0.005), breaks = seq(0,0.005, round(0.005/7, digits=3))) +
  scale_fill_viridis_c(guide = "none") +
  scale_color_viridis_c() +
  facet_wrap(~scenario, nrow = 2) +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    legend.position = "bottom", 
    legend.box = "vertical"
  ) +
  guides(size = guide_legend(title = expression(R^2), order = 1, nrow = 1, label.position = "bottom"),
         color = guide_colorbar(title = "Environmental\ndeviation", order = 2,
                                barheight = 0.3)) -> B

#ggsave("test2.tiff", dpi = 600, width = 3, height = 6)

AB <- grid.arrange(A, B, ncol = 2)
ggsave(filename = paste0(tiff_path, "niches_no_interaction.tiff"), plot = AB, dpi = 600, width = 6, height = 6)

# Figure 2 - YES competition --------------------------------------------------------
fig2_spp %>% 
  filter(., scenario %in% c("FIG2C", "FIG2D")) %>%
  mutate(nicheBreadth = ifelse(nicheBreadth == 0.8, "Narrow niche", "Broad niche"),
         nicheBreadth = factor(nicheBreadth, levels = c("Narrow niche", "Broad niche")),
         iteration = str_remove(iteration, "iter_")) %>% 
  mytheme(type = "species") +
  geom_point(fill = "black", alpha = 0.7) +
  
  scale_size_area(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  facet_wrap(~nicheBreadth, nrow = 2, strip.position = "left") +
  theme(
    #strip.background = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical"
  ) +
  guides(size = guide_legend(title = expression(R^2), nrow = 1, label.position = "bottom", order = 1),
         shape = guide_legend(title = "Iteration", order = 2)) -> A
#ggsave("test1.tiff", dpi = 600, width = 3, height = 6)

fig2_sites %>% 
  filter(., scenario %in% c("FIG2C", "FIG2D")) %>% 
  mytheme() +
  geom_point(aes(color = Edev, fill = Edev), alpha = 0.7) +
  scale_size_area(limits = c(0,0.005), breaks = seq(0,0.005, round(0.005/7, digits=3))) +
  scale_fill_viridis_c(guide = "none") +
  scale_color_viridis_c() +
  facet_wrap(~scenario, nrow = 2) +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    legend.position = "bottom", 
    legend.box = "vertical"
  ) +
  guides(size = guide_legend(title = expression(R^2), order = 1, nrow = 1, label.position = "bottom"),
         color = guide_colorbar(title = "Environmental\ndeviation", order = 2,
                                barheight = 0.3)) -> B

#ggsave("test2.tiff", dpi = 600, width = 3, height = 6)

AB <- grid.arrange(A, B, ncol = 2)
ggsave(filename = paste0(tiff_path, "niches_YES_interaction.tiff"), plot = AB, dpi = 600, width = 6, height = 6)

# Other ideas -------------------------------------------------------------------
fig2_spp %>% 
  mutate(nicheBreadth = ifelse(nicheBreadth == 0.8, "Narrow niche", "Broad niche"),
         nicheBreadth = factor(nicheBreadth, levels =  c("Narrow niche", "Broad niche")),
         interCol = ifelse(interCol == 0, "No competition", "With competition")) %>% 
  mytheme() +
  geom_point(fill = "black", alpha = 0.7) +
  scale_size_area(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  facet_grid(interCol~nicheBreadth, switch = "y") +
  theme(
    #strip.background = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 8)
  ) +
  guides(size = guide_legend(title = expression(R^2), nrow = 1, label.position = "bottom"))
ggsave(paste0(tiff_path, "F2_facet.tiff"), dpi = 600, width = 6, height = 6)



fig2_sites %>% 
  mutate(nicheBreadth = ifelse(scenario %in% c("FIG2A", "FIG2C"), "Narrow niche", "Broad niche"),
         nicheBreadth = factor(nicheBreadth, levels =  c("Narrow niche", "Broad niche")),
         interCol = ifelse(scenario %in% c("FIG2A", "FIG2B"), "No competition", "With competition")) %>% 
  mytheme() +
  geom_point(aes(color = Edev, fill = Edev), alpha = 0.7) +
  scale_size_area(limits = c(0,0.005), breaks = seq(0,0.005, round(0.005/7, digits=3))) +
  scale_fill_viridis_c(guide = "none") +
  scale_color_viridis_c() +
  facet_grid(interCol~nicheBreadth, switch = "y") +
  theme(
    strip.text = element_text(size = 8),
    legend.position = "bottom"
  ) +
  guides(size = guide_legend(title = expression(R^2), order = 1, nrow = 1, label.position = "bottom"),
         color = guide_colorbar(title = "Environmental\ndeviation", order = 2,
                                barheight = 0.3))

ggsave(paste0(tiff_path, "F2sites_facet.tiff"), dpi = 600, width = 6, height = 6)


# The everything figure

vars_keep <- c("env", "spa", "codist", "r2", "Edev", "iteration", "type1", "type2")

fig2_spp %>% 
  mutate(iteration = str_replace(iteration, "iter_", "iter"),
         nicheBreadth = ifelse(nicheBreadth == 0.8, "Narrow niche", "Broad niche"),
         interCol = ifelse(interCol == 0, "No competition", "With competition"),
         type1 = paste(nicheBreadth, "\n", interCol),
         type2 = "Species",
         Edev = NA) %>% 
  dplyr::select(one_of(vars_keep))-> P

head(P)

fig2_sites %>% 
  mutate(nicheBreadth = ifelse(scenario %in% c("FIG2A", "FIG2C"), "Narrow niche", "Broad niche"),
         interCol = ifelse(scenario %in% c("FIG2A", "FIG2B"), "No competition", "With competition"),
         type1 = paste(nicheBreadth, "\n", interCol),
         type2 = "Sites") %>% 
  dplyr::select(., one_of(vars_keep))-> Q

head(Q)

bind_rows(P, Q) %>% 
  mutate(type1 = factor(type1, levels = c("Narrow niche \n No competition", "Narrow niche \n With competition",
                                          "Broad niche \n No competition", "Broad niche \n With competition")),
         type2 = factor(type2, levels = c("Species", "Sites"))) -> PQ
  
PQ %>% 
  mytheme() +
  geom_point(aes(color = Edev, fill = Edev), alpha = 0.7) +
  scale_size_area(limits = c(0, 1), breaks = seq(0, 1, 0.2))  +
  scale_fill_viridis_c(guide = "none", na.value = "#000000") +
  scale_color_viridis_c(na.value = "#000000") +
  facet_grid(type1~type2, switch = "y") +
  theme(
    strip.text = element_text(size = 7, face = "bold"),
    strip.background = element_rect(color = NA),
    legend.position = "bottom",
    legend.box = "vertical"
  ) +
  guides(size = guide_legend(title = expression(R^2), order = 1, nrow = 1),
         color = guide_colorbar(title = "Environmental\ndeviation", order = 2, barheight = 0.3))

ggsave(paste0(tiff_path, "Full_spp_sites.tiff"), dpi = 600, width = 6, height = 8)
