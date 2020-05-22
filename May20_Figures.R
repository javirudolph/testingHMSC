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

# Load data ----------------------------------------------------------------

spp_data <- NULL
for(i in 1:4){
  spp <- get_species_data(outsfolderpath, scenarios[i]) %>% 
    mutate(scenario = new_scen[i])
  params <- get_fig2_params(outsfolderpath, scenario = scenarios[i]) %>% 
    mutate(dispersal = as.numeric(as.character(dispersal)),
           scenario = new_scen[i])
  
  left_join(spp, params) %>% 
    mutate(nicheCent = abs(nicheOptima - 0.5)) -> spp
  
  spp_data <- bind_rows(spp_data, spp)
}

for(i in 5:7){
  spp <- get_species_data(outsfolderpath, scenario = scenarios[i]) %>% 
    mutate(scenario = new_scen[i])
  params <- get_fig3_params(outsfolderpath, scenario = scenarios[i]) %>% 
    mutate(dispersal = as.numeric(as.character(dispersal)),
           scenario = new_scen[i])
  
  left_join(spp, params) %>% 
    mutate(nicheCent = abs(nicheOptima - 0.5)) -> spp
  
  spp_data <- bind_rows(spp_data, spp)
  
}

sites_data <- NULL
for (i in 1:7){
  sites <- get_sites_data(outsfolderpath, scenario = scenarios[i]) %>%
    mutate(E = rep(E, 5),
           Edev = abs(E-0.5),
           scenario = new_scen[i])
  
  
  sites_data <- bind_rows(sites_data, sites)
}

# We need to scale the R2 for sites so that the meaning is the same for sites and species.

A <- sites_data %>% 
  filter(scenario == "A",
         iteration == "iter1")
a <- spp_data %>% 
  filter(scenario == "A",
         iteration == "iter_1")

# Theme -------------------------------------------------------

mytheme <- function(data, plotMain = NULL, type = NULL){
  data %>% 
    ggtern(aes(x = env, z = spa, y = codist, size = r2, shape = iteration)) +
    scale_T_continuous(limits=c(0,1),
                       breaks=seq(0, 1,by=0.2),
                       labels=seq(0,1, by= 0.2)) +
    scale_L_continuous(limits=c(0,1),
                       breaks=seq(0, 1,by=0.2),
                       labels=seq(0, 1,by=0.2)) +
    scale_R_continuous(limits=c(0,1),
                       breaks=seq(0, 1,by=0.2),
                       labels=seq(0, 1,by=0.2)) +
    labs(title = plotMain,
         x = "E",
         xarrow = "Environment",
         y = "C",
         yarrow = "Co-Distribution",
         z = "S", 
         zarrow = "Spatial Autocorrelation") +
    theme_bw() +
    theme_showarrows() +
    theme_arrowlong() +
    scale_shape_manual(values = c(21:25), guide = FALSE) +
    theme(
      panel.grid = element_line(color = "darkgrey", size = 0.3),
      plot.tag = element_text(size = 11),
      plot.title = element_text(size = 11, hjust = 0.1 , margin = margin(t = 10, b = -20)),
      tern.axis.arrow = element_line(size = 1),
      tern.axis.arrow.text = element_text(size = 6),
      axis.text = element_text(size = 4),
      axis.title = element_text(size = 6),
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 8),
      strip.text = element_text(size = 8),
      strip.background = element_rect(color = NA),
    ) +
    guides(size = guide_legend(title = expression(R^2), order = 1))
}



# Figures 2 and 3 -----------------------------------------------------------------------------

# Join all the data for use with facets
#**************************************

vars_keep <- c("env", "spa", "codist", "r2", "Edev", "iteration", "type1", "type2", "interCol", "scenario")

spp_data %>% 
  filter(scenario %in% new_scen[1:4]) %>% 
  mutate(iteration = str_replace(iteration, "iter_", "iter"),
         nicheBreadth = ifelse(nicheBreadth == 0.8, "Narrow niche", "Broad niche"),
         interCol = ifelse(interCol == 0, "No competition", "With competition"),
         type1 = paste(nicheBreadth, "\n", interCol),
         type2 = "Species",
         Edev = NA) %>% 
  dplyr::select(one_of(vars_keep))-> P

head(P)

sites_data %>% 
  filter(scenario %in% new_scen[1:4]) %>% 
  mutate(nicheBreadth = ifelse(scenario %in% c("A", "C"), "Narrow niche", "Broad niche"),
         interCol = ifelse(scenario %in% c("A", "B"), "No competition", "With competition"),
         type1 = paste(nicheBreadth, "\n", interCol),
         type2 = "Sites",
         dispersal = NA) %>% 
  dplyr::select(., one_of(vars_keep))-> Q

head(Q)

bind_rows(P, Q) %>% 
  mutate(type1 = factor(type1, levels = c("Narrow niche \n No competition", "Narrow niche \n With competition",
                                          "Broad niche \n No competition", "Broad niche \n With competition")),
         type2 = factor(type2, levels = c("Species", "Sites"))) -> PQ

# Figure2: Scenarios A and B
#********
PQ %>%
  filter(interCol == "No competition") %>% 
  mytheme() +
  geom_point(aes(color = Edev, fill = Edev), alpha = 0.7) +
  scale_size_area(limits = c(0, 1), breaks = seq(0, 1, 0.2))  +
  scale_fill_viridis_c(guide = "none", na.value = "#000000") +
  scale_color_viridis_c(na.value = "#000000") +
  facet_grid(type1~type2, switch = "y") +
  theme(
    legend.position = "bottom",
    #legend.box = "vertical",
    legend.spacing.y = unit(0.01, "in")
  ) +
  guides(size = guide_legend(title = expression(R^2), order = 1, nrow = 1, label.position = "bottom"),
         color = guide_colorbar(title = "Environmental\ndeviation", order = 2, barheight = 0.3))
ggsave(paste0(tiff_path, "Figure2.tiff"), dpi = 600, width = 6, height = 6)

# Figure3: Scenarios C and D
#********
PQ %>%
  filter(interCol == "With competition") %>% 
  mytheme() +
  geom_point(aes(color = Edev, fill = Edev), alpha = 0.7) +
  scale_size_area(limits = c(0, 1), breaks = seq(0, 1, 0.2))  +
  scale_fill_viridis_c(guide = "none", na.value = "#000000") +
  scale_color_viridis_c(na.value = "#000000") +
  facet_grid(type1~type2, switch = "y") +
  theme(
    tern.axis.arrow.text = element_text(size = 7),
    legend.position = "bottom",
    #legend.box = "vertical",
    legend.spacing.y = unit(0.01, "in")
  ) +
  guides(size = guide_legend(title = expression(R^2), order = 1, nrow = 1, label.position = "bottom"),
         color = guide_colorbar(title = "Environmental\ndeviation", order = 2, barheight = 0.3))
ggsave(paste0(tiff_path, "Figure3.tiff"), dpi = 600, width = 6, height = 6)
