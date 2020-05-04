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

mytheme <- function(data, plotMain = NULL){
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
    guides(size = guide_legend(title = expression(R^2), order = 1))
  
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
# Species and sites, side by side
# Comparisson between broad and narrow niche

sp2a <- fig2_spp %>% 
  filter(., scenario == "FIG2A") %>% 
  mytheme(., plotMain = "A") +
  geom_point(fill = "black", alpha = 0.7) +
  theme(legend.position = "none")

sp2b <- fig2_spp %>% 
  filter(., scenario == "FIG2B") %>% 
  mytheme(., plotMain = "B") +
  geom_point(fill = "black", alpha = 0.7) +
  theme(legend.position = "none")

 
fig2_spp %>% 
  filter(., scenario %in% c("FIG2A", "FIG2B")) %>%
  mytheme() +
  geom_point(fill = "black", alpha = 0.7) +
  facet_wrap(~nicheBreadth, nrow = 2, strip.position = "left") +
  theme(
    strip.background = element_blank()
  )
ggsave("test.tiff", dpi = 600, width = 3, height = 5)