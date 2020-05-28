# Manuscript and Supplement Figures

# Setup ---------------------------------------------------------------

library(tidyverse)
library(ggtern)
library(ggpubr)
library(gridExtra)
# library(scico)
library(ggalt)
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

# Figure 2, scenarios A-D Species only --------------------------------------
spp_data %>% 
  filter(scenario %in% new_scen[1:4]) %>% 
  mutate(iteration = str_replace(iteration, "iter_", "Replicate"),
         nicheBreadth = ifelse(nicheBreadth == 0.8, "Narrow niche", "Broad niche"),
         nicheBreadth = factor(nicheBreadth, levels = c("Narrow niche", "Broad niche")),
         interCol = ifelse(interCol == 0, "No competition", "With competition")) %>% 
  mytheme() +
  facet_grid(interCol~nicheBreadth, switch = "y") +
  geom_point(aes(color = nicheOptima , fill = nicheOptima), alpha = 0.7) +
  scale_size_continuous(range = c(0.1,5),limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_gradient2(high = "#000004FF", low = "#56147DFF", mid = "#F4685CFF", limits = c(0, 1), midpoint = 0.5) +
  scale_color_gradient2(high = "#000004FF", low = "#56147DFF", mid = "#F4685CFF", limits = c(0, 1), midpoint = 0.5, guide = "none") +
  theme(tern.axis.arrow.text = element_text(size = 7),
        legend.position = "bottom") +
  guides(size = guide_legend(title = expression(R^2), order = 1, nrow = 1, label.position = "bottom"),
         fill = guide_colorbar(title = "Niche optima", title.position = "top", order = 2, barheight = 0.5, barwidth = 8))

ggsave(filename=paste0(tiff_path, "Figure2_color.tiff"), dpi = 600, width = 6, height = 6)

spp_data %>% 
  filter(scenario %in% new_scen[1:4]) %>% 
  mutate(iteration = str_replace(iteration, "iter_", "Replicate"),
         nicheBreadth = ifelse(nicheBreadth == 0.8, "Narrow niche", "Broad niche"),
         nicheBreadth = factor(nicheBreadth, levels = c("Narrow niche", "Broad niche")),
         interCol = ifelse(interCol == 0, "No competition", "With competition")) %>% 
  mytheme() +
  facet_grid(interCol~nicheBreadth, switch = "y") +
  geom_point(color = "black", fill = "black", alpha = 0.7) +
  scale_size_continuous(range = c(0.1,5),limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme(tern.axis.arrow.text = element_text(size = 7),
        legend.position = "bottom") +
  guides(size = guide_legend(title = expression(R^2), order = 1, nrow = 1, label.position = "bottom"))

ggsave(filename=paste0(tiff_path, "Figure2_bw.tiff"), dpi = 600, width = 6, height = 6)

# Figure 3, scenarios A-D Sites only --------------------------------------

nameColor <- bquote(atop(Contribution~by~phantom(),
                         sites~to~R^2))
sites_data %>% 
  filter(scenario %in% new_scen[1:4]) %>% 
  mutate(iteration = str_replace(iteration, "iter_", "Replicate"),
         nicheBreadth = ifelse(scenario %in% c("A", "C"), "Narrow niche", "Broad niche"),
         nicheBreadth = factor(nicheBreadth, levels = c("Narrow niche", "Broad niche")),
         interCol = ifelse(scenario %in% c("A", "B"), "No competition", "With competition")) %>% 
  mytheme() +
  facet_grid(interCol~nicheBreadth, switch = "y") +
  geom_point(aes(color = Edev, fill = Edev), alpha = 0.7) +
  scale_size_continuous(range = c(0.1,4),limits = c(0, 0.005), breaks = seq(0, 0.005, 0.001)) +
  #scale_size_area(limits = c(0, 1), breaks = seq(0, 1, 0.2))  +
  scale_fill_viridis_c(guide = "none", na.value = "#000000") +
  scale_color_viridis_c(na.value = "#000000", limits = c(0,0.5)) +
  theme(tern.axis.arrow.text = element_text(size = 7),
        legend.position = "bottom") +
  guides(size = guide_legend(title = nameColor, order = 1, nrow = 1, label.position = "bottom"),
         color = guide_colorbar(title = "Environmental deviation", title.position = "top", order = 2, barheight = 0.5, barwidth = 8))


ggsave(paste0(tiff_path, "Figure3_color.tiff"), dpi = 600, width = 6, height = 6)


# OLD FIGURES 2, 3 & 4 -----------------------------------------------------------------------------
# Figures 2 and 3 

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
  scale_size_continuous(range = c(0.1,4),limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  #scale_size_area(limits = c(0, 1), breaks = seq(0, 1, 0.2))  +
  scale_fill_viridis_c(guide = "none", na.value = "#000000") +
  scale_color_viridis_c(na.value = "#000000", limits = c(0,0.5)) +
  facet_grid(type1~type2, switch = "y") +
  theme(tern.axis.arrow.text = element_text(size = 7),
        legend.position = "bottom") +
  guides(size = guide_legend(title = expression(R^2), order = 1, nrow = 1, label.position = "bottom"),
         color = guide_colorbar(title = "Environmental deviation", title.position = "top", order = 2, barheight = 0.5, barwidth = 8))
ggsave(paste0(tiff_path, "Figure2.tiff"), dpi = 600, width = 6, height = 6)

# Figure3: Scenarios C and D
#********
PQ %>%
  filter(interCol == "With competition") %>% 
  mytheme() +
  geom_point(aes(color = Edev, fill = Edev), alpha = 0.7) +
  scale_size_continuous(range = c(0.1,4),limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  #scale_size_area(limits = c(0, 1), breaks = seq(0, 1, 0.2))  +
  scale_fill_viridis_c(guide = "none", na.value = "#000000") +
  scale_color_viridis_c(na.value = "#000000", limits = c(0, 0.5)) +
  facet_grid(type1~type2, switch = "y") +
  theme(
    tern.axis.arrow.text = element_text(size = 7),
    legend.position = "bottom") +
  guides(size = guide_legend(title = expression(R^2), order = 1, nrow = 1, label.position = "bottom"),
         color = guide_colorbar(title = "Environmental deviation", title.position = "top", order = 2, barheight = 0.5, barwidth = 8))
ggsave(paste0(tiff_path, "Figure3.tiff"), dpi = 600, width = 6, height = 6)

# Figure 4, scenario G --------------------------------------------

vars_keep <- c("env", "spa", "codist", "r2", "Edev", "iteration", "dispersal", "nicheOptima", "type")

spp_data %>% 
  filter(scenario == "G") %>% 
  mutate(iteration = str_replace(iteration, "iter_", "iter"),
         Edev = NA,
         type = "Species") %>% 
  dplyr::select(one_of(vars_keep))-> W

head(W)

sites_data %>% 
  filter(scenario == "G") %>% 
  mutate(dispersal = NA,
         nicheOptima = NA,
         type = "Sites") %>% 
  dplyr::select(., one_of(vars_keep))-> R

head(R)

bind_rows(W, R) %>% 
  mutate(type = factor(type, levels = c("Species", "Sites"))) -> WR
head(WR)

# FIGURE 4
#***************************

WR %>%
  filter(type == "Species") %>% 
  mytheme() +
  facet_wrap(~type, ncol=2) +
  geom_encircle(data = WR %>% filter(dispersal == 0.01), aes(group = dispersal), size = 0.7, color = NA, fill = "#FDE4A6FF",alpha = 0.6,
                expand = 0.01) +
  geom_encircle(data = WR %>% filter(dispersal == 0.05), aes(group = dispersal), size = 0.7, color = NA, fill = "#F4685CFF",alpha = 0.3,
                expand = 0.01) +
  geom_encircle(data = WR %>% filter(dispersal == 0.1), aes(group = dispersal), size = 0.7, color = NA, fill = "black",alpha = 0.3,
                expand = 0.0) +
  geom_point(aes(color = nicheOptima, fill = nicheOptima), alpha = 0.8) +
  scale_size_continuous(range = c(0.1,4),limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  #scale_size_area(limits = c(0, 1), breaks = seq(0, 1, 0.2)  +
  #scale_fill_viridis_c(na.value = "#666666") +
  scale_fill_gradient2(high = "#000004FF", low = "#56147DFF", mid = "#F4685CFF", limits = c(0, 1), midpoint = 0.5) +
  scale_color_gradient2(high = "#000004FF", low = "#56147DFF", mid = "#F4685CFF", limits = c(0, 1), midpoint = 0.5, guide = "none") +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    #legend.spacing.y = unit(0.01, "in"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.text = element_text(size = 4),
    legend.title = element_text(size = 6)
  ) +
  guides(size = guide_legend(title = expression(R^2), order = 1, nrow = 1, label.position = "bottom"),
         fill = guide_colorbar(title = "Niche optima", title.position = "top", order = 2, barheight = 0.5, barwidth = 8))-> W.plot

nameColor <- bquote(atop(Contribution~by~phantom(),
                         sites~to~R^2))

WR %>%
  filter(type == "Sites") %>% 
  mytheme() +
  facet_wrap(~type, ncol=2) +
  geom_point(aes(color = Edev, fill = Edev), alpha = 0.8) +
  scale_size_continuous(range = c(0.1,4),limits = c(0, 0.005), breaks = seq(0, 0.004, 0.001)) +
  #scale_size_area(limits = c(0, 1), breaks = seq(0, 1, 0.2)  +
  scale_fill_viridis_c(na.value = "#666666", guide = "none") +
  scale_color_viridis_c(na.value = "#666666", limits = c(0, 0.5)) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    #legend.spacing.y = unit(0.01, "in"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.text = element_text(size = 4),
    legend.title = element_text(size = 6)
  ) +
  guides(size = guide_legend(title = nameColor, order = 1, nrow = 1, label.position = "bottom"),
         color = guide_colorbar(title = "Environmental deviation", title.position = "top", title.hjust = 0, order = 2, barheight = 0.5, barwidth = 8)) -> R.plot


h.WR.plot <- ggtern::grid.arrange(W.plot, R.plot, ncol = 2)
ggsave(filename = paste0(tiff_path, "Figure4_h.tiff"), plot = h.WR.plot, dpi = 600, width = 6, height = 6)

# Vertical doesn't look great, size isn't good.
# v.WR.plot <- ggtern::grid.arrange(W.plot, R.plot, nrow=2)
# ggsave(filename = paste0(tiff_path, "Figure4_v.tiff"), plot = v.WR.plot, dpi = 600, width = 6, height = 6)

#*********************************
#*********************************
#*********************************


WR %>%
  filter(type == "Species") %>% 
  mytheme() +
  facet_wrap(~type, ncol=2) +
  geom_encircle(data = WR %>% filter(dispersal == 0.01), aes(group = dispersal), size = 0.7, color = NA, fill = "#FDE4A6FF",alpha = 0.6,
                expand = 0.01) +
  geom_encircle(data = WR %>% filter(dispersal == 0.05), aes(group = dispersal), size = 0.7, color = NA, fill = "#F4685CFF",alpha = 0.3,
                expand = 0.01) +
  geom_encircle(data = WR %>% filter(dispersal == 0.1), aes(group = dispersal), size = 0.7, color = NA, fill = "black",alpha = 0.3,
                expand = 0.0) +
  geom_point(aes(color = nicheOptima, fill = nicheOptima), alpha = 0.8) +
  scale_size_continuous(range = c(0.1,4),limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  #scale_size_area(limits = c(0, 1), breaks = seq(0, 1, 0.2)  +
  #scale_fill_viridis_c(na.value = "#666666") +
  scale_fill_gradient2(high = "#000004FF", low = "#56147DFF", mid = "#F4685CFF", limits = c(0, 1), midpoint = 0.5) +
  scale_color_gradient2(high = "#000004FF", low = "#56147DFF", mid = "#F4685CFF", limits = c(0, 1), midpoint = 0.5, guide = "none") +
  theme(
    legend.position = "right",
    #legend.box = "vertical",
    legend.spacing.y = unit(0.1, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.text = element_text(size = 4),
    legend.title = element_text(size = 6)
  ) +
  guides(size = guide_legend(title = expression(R^2), order = 1, label.position = "right"),
         fill = guide_colorbar(title = "Niche\noptima", title.hjust = 0 , title.position = "top", order = 2, barheight = 4, barwidth = 0.5)) -> Wv.plot


nameColor <- bquote(atop(Contribution~by~phantom(),
                         sites~to~R^2))
WR %>%
  filter(type == "Sites") %>% 
  mytheme() +
  facet_wrap(~type, ncol=2) +
  geom_point(aes(color = Edev, fill = Edev), alpha = 0.8) +
  scale_size_continuous(range = c(0.1,4),limits = c(0, 0.005), breaks = seq(0, 0.004, 0.001)) +
  #scale_size_area(limits = c(0, 1), breaks = seq(0, 1, 0.2)  +
  scale_fill_viridis_c(na.value = "#666666", guide = "none") +
  scale_color_viridis_c(na.value = "#666666", limits = c(0, 0.5)) +
  theme(
    legend.position = "right",
    #legend.box = "vertical",
    legend.spacing.y = unit(0.1, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.text = element_text(size = 4),
    legend.title = element_text(size = 6)
  ) +
  guides(size = guide_legend(title = nameColor, order = 1, label.position = "right"),
         color = guide_colorbar(title = "Environmental\ndeviation", title.position = "top", title.hjust = 0, order = 2, barheight = 4, barwidth = 0.5)) -> Rv.plot

v.WR.plot <- ggtern::grid.arrange(Wv.plot, Rv.plot, nrow=2)
ggsave(filename = paste0(tiff_path, "Figure4_v.tiff"), plot = v.WR.plot, dpi = 600, width = 6, height = 6)



# scales::show_col(viridis::viridis_pal(option = "A")(20))


# Figure 4, scenario G ----------------------------------------------
spp_data %>% 
  filter(scenario == "G") %>% 
  mutate(iteration = str_replace(iteration, "iter_", "Replicate"),
         nicheBreadth = ifelse(nicheBreadth == 0.8, "Narrow niche", "Broad niche"),
         nicheBreadth = factor(nicheBreadth, levels = c("Narrow niche", "Broad niche")),
         interCol = ifelse(interCol == 0, "No competition", "With competition")) %>% 
  mytheme() +
  #facet_grid(interCol~nicheBreadth, switch = "y") +
  geom_point(aes(color = nicheOptima , fill = nicheOptima), alpha = 0.7) +
  scale_size_continuous(range = c(0.1,5),limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_gradient2(high = "#000004FF", low = "#56147DFF", mid = "#F4685CFF", limits = c(0, 1), midpoint = 0.5) +
  scale_color_gradient2(high = "#000004FF", low = "#56147DFF", mid = "#F4685CFF", limits = c(0, 1), midpoint = 0.5, guide = "none") +
  theme(tern.axis.arrow.text = element_text(size = 7),
        legend.position = "bottom") +
  guides(size = guide_legend(title = expression(R^2), order = 1, nrow = 1, label.position = "bottom"),
         fill = guide_colorbar(title = "Niche optima", title.position = "top", order = 2, barheight = 0.5, barwidth = 8)) -> P

nameColor <- bquote(atop(Contribution~by~phantom(),
                         sites~to~R^2))
sites_data %>% 
  filter(scenario == "G") %>% 
  mutate(iteration = str_replace(iteration, "iter_", "Replicate"),
         nicheBreadth = ifelse(scenario %in% c("A", "C"), "Narrow niche", "Broad niche"),
         nicheBreadth = factor(nicheBreadth, levels = c("Narrow niche", "Broad niche")),
         interCol = ifelse(scenario %in% c("A", "B"), "No competition", "With competition")) %>% 
  mytheme() +
  #facet_grid(interCol~nicheBreadth, switch = "y") +
  geom_point(aes(color = Edev, fill = Edev), alpha = 0.7) +
  scale_size_continuous(range = c(0.1,4),limits = c(0, 0.005), breaks = seq(0, 0.005, 0.001)) +
  #scale_size_area(limits = c(0, 1), breaks = seq(0, 1, 0.2))  +
  scale_fill_viridis_c(guide = "none", na.value = "#000000") +
  scale_color_viridis_c(na.value = "#000000", limits = c(0,0.5)) +
  theme(tern.axis.arrow.text = element_text(size = 7),
        legend.position = "bottom") +
  guides(size = guide_legend(title = nameColor, order = 1, nrow = 1, label.position = "bottom"),
         color = guide_colorbar(title = "Environmental deviation", title.position = "top", order = 2, barheight = 0.5, barwidth = 8)) -> Q

PQ <- ggtern::grid.arrange(P, Q, nrow=2)
ggsave(filename = paste0(tiff_path, "Figure4_v.tiff"), plot = PQ, dpi = 600, width = 6, height = 6)


# Interaction matrices functions and data----------------------------------------

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat, diag = TRUE)]<- NA
  return(cormat)
}

intplot <- function(modelfile, iteration){
  assoMat <- corRandomEff(modelfile[[iteration]])
  siteMean <- apply(assoMat[ , , , 1], 1:2, mean)
  
  siteMean %>% 
    get_upper_tri() %>% 
    as_tibble() %>% 
    rownames_to_column(., var = "Specie1") %>% 
    pivot_longer(-Specie1, names_to = "Specie2", values_to = "value") %>% 
    drop_na(value) %>% 
    mutate(iteration = paste("Iteration", iteration),
           Specie2 = str_replace(Specie2, "y", ""),
           Specie1 = factor(Specie1, levels = c(1:12)),
           Specie2 = factor(Specie2, levels = c(12:1)),
           color = ifelse(value > 0.4, "red",
                          ifelse(value < -0.4, "blue", NA)), 
           signi = ifelse(is.na(color)== TRUE, "x", NA))
}

# The interaction matrix we provided:
A <- matrix(0,nr=12,nc=12)
d <- as.matrix(dist(c(1:12),upper=TRUE,diag=T))
A[d<=1] = -1
diag(A) = 0


A %>% 
  get_upper_tri( ) %>% 
  as_tibble() %>% 
  rownames_to_column(., var = "Specie1") %>% 
  pivot_longer(-Specie1, names_to = "Specie2", values_to = "value") %>% 
  drop_na(value) %>% 
  mutate(iteration = "Simulation",
         Specie2 = str_replace(Specie2, "V", ""),
         Specie1 = factor(Specie1, levels = c(1:12)),
         Specie2 = factor(Specie2, levels = c(12:1)),
         color = ifelse(value > 0.4, "red",
                        ifelse(value < -0.4, "blue", NA)), 
         signi = ifelse(is.na(color)== TRUE, "x", NA), 
         scenario = "original") -> orig_matrix

# Get all the data
intData <- NULL
for(i in 1:7){
  
  modelfile <- readRDS(paste0(outsfolderpath, scenarios[i], "-model.RDS"))
  
  b <- NULL
  for(j in 1:5){
    a <- intplot(modelfile, j)
    b <- bind_rows(b, a)
  } 
  
  b$scenario <- scenarios[i]
  intData <- bind_rows(intData, b)
}



# Figure 5 ----------------------------------------------


intData %>% 
  filter(., scenario == "FIG3C") %>% 
  bind_rows(., orig_matrix) %>% 
  mutate(iteration = str_replace(iteration,"Iteration", "Replicate"),
         iteration = factor(iteration, levels = c("Simulation", "Replicate 1", "Replicate 2", "Replicate 3",
                                                  "Replicate 4", "Replicate 5"))) %>% 
  ggplot(., aes(x = Specie1, y = Specie2, fill = value)) +
  facet_wrap(~iteration, ncol = 6) +
  geom_tile() +
  geom_text(aes(label = signi), size = 2)+
  scale_fill_gradient2(
    high = "#4d4d4d", mid = "white", low = "#cb181d",
    limits = c(-1, 1)) +
  #theme_bw() + 
  ggplot2::theme_minimal()+
  theme(legend.position = "right",
        axis.text = element_text(size = 4),
        axis.text.x = element_text(angle = 90),
        #axis.text = element_blank(),
        #axis.title = element_text(size = 6),
        axis.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8), 
        strip.text = element_text(size = 6, margin = margin(0.1,0.1,0.1,0.1, "cm")),
        strip.background = element_rect(color = NA, fill = "#dbdbd9")) +
  guides(fill = guide_colorbar(title = NULL,barwidth = 0.3, barheight = 5))

#ggsave(paste0(tiff_path, "Figure5.tiff"), dpi = 600, width = 6, height = 1.5)


# SUPPLEMENT------------------------------------------------
# Interaction matrices for all scenarios
#*****************************************************************

intData %>% 
  filter(., scenario != "FIG3C") %>% 
  mutate(iteration = str_replace(iteration,"Iteration", "Replicate"),
         iteration = factor(iteration, levels = c("Simulation", "Replicate 1", "Replicate 2", "Replicate 3",
                                                  "Replicate 4", "Replicate 5")),
         scenario = recode(scenario, FIG2A = "Scenario A", FIG2B = "Scenario B", FIG2C = "Scenario C", FIG2D = "Scenario D",
                           FIG3A = "Scenario E", FIG3B = "Scenario F")) %>% 
  ggplot(., aes(x = Specie1, y = Specie2, fill = value)) +
  facet_grid(scenario~iteration) +
  geom_tile() +
  geom_text(aes(label = signi), size = 2)+
  scale_fill_gradient2(
    high = "#4d4d4d", mid = "white", low = "#cb181d",
    limits = c(-1, 1)) +
  #theme_bw() + 
  ggplot2::theme_minimal()+
  theme(legend.position = "right",
        axis.text = element_text(size = 4),
        axis.text.x = element_text(angle = 90),
        #axis.text = element_blank(),
        #axis.title = element_text(size = 6),
        axis.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8), 
        strip.text = element_text(size = 6, margin = margin(0.1,0.1,0.1,0.1, "cm")),
        strip.background = element_rect(color = NA, fill = "#dbdbd9")) +
  guides(fill = guide_colorbar(title = NULL,barwidth = 0.3, barheight = 5))
  
ggplot2::ggsave(filename = paste0(tiff_path, "Sup_F2_interactions.tiff"))

# Ternary plots for scenarios E and F
#********************************************************************

vars_keep <- c("env", "spa", "codist", "r2", "Edev", "iteration", "dispersal", "nicheOptima", "scenario", "interCol",
               "type1", "type2")

spp_data %>% 
  filter(scenario %in% c("E", "F")) %>% 
  mutate(iteration = str_replace(iteration, "iter_", "iter"),
         textCol = ifelse(interCol == 0, "No competition", "With Competition"),
         type1 = ifelse(scenario == "E", "Half compete\n dispersal constant",
                        "All compete\n three level dispersal"),
         Edev = NA,
         type2 = "Species") %>% 
  dplyr::select(one_of(vars_keep))-> Y


sites_data %>% 
  filter(scenario %in% c("E", "F")) %>% 
  mutate(dispersal = NA,
         nicheOptima = NA,
         interCol = NA,
         type1 = ifelse(scenario == "E", "Half compete\n Dispersal is constant",
                        "All compete\n Three levels of dispersal"),
         type2 = "Sites") %>% 
  dplyr::select(., one_of(vars_keep))-> Z


bind_rows(Y, Z) %>% 
  mutate(type2 = factor(type2, levels = c("Species", "Sites")),
         scenario = recode(scenario, E = "Scenario E", F = "Scenario F")) -> YZ
head(YZ)

#***************************
YZ %>%
  filter(scenario == "Scenario E") %>% 
  mytheme() +
  facet_grid(type1~type2, switch = "y") +
  geom_point(aes(color = Edev, fill = interCol), alpha = 0.8) +
  scale_size_continuous(range = c(0.1,5),limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_fill_gradient(high = "#810f7c", low = "#b3cde3") +
    scale_color_viridis_c(na.value = "grey", limits = c(0, 0.5)) +
    theme(
      legend.position = "bottom",
      #legend.box = "vertical",
      #legend.spacing.y = unit(0.01, "in"),
      legend.spacing.x = unit(0.1, "cm"),
      legend.text = element_text(size = 4),
      legend.title = element_text(size = 6)
    ) +
    guides(size = guide_legend(title = expression(R^2), order = 2, nrow = 1, label.position = "bottom"),
           fill = guide_colorbar(title = "Dispersal level", title.position = "top", order = 1, barheight = 0.5, barwidth = 5),
           color = guide_colorbar(title = "Environmental deviation", title.position = "top", title.hjust = 1,
                                  order = 3, barheight = 0.5, barwidth = 5))

ggsave(paste0(tiff_path, "Sup_F3_scenE.tiff"), dpi = 600, width = 6, height = 4)

YZ %>%
  filter(scenario == "Scenario F") %>% 
  mytheme() +
  facet_grid(type1~type2, switch = "y") +
  geom_point(aes(color = Edev, fill = dispersal), alpha = 0.8) +
  scale_size_continuous(range = c(0.1,5),limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  #scale_fill_manual(values = c("#d8b365", "white" ,"#5ab4ac"))
  scale_fill_gradient2(high = "#5ab4ac", low = "#d8b365", mid = "white", midpoint = 0.05) +
  scale_color_viridis_c(na.value = "grey", limits = c(0, 0.5)) +
  theme(
    legend.position = "bottom",
    #legend.box = "vertical",
    #legend.spacing.y = unit(0.01, "in"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.text = element_text(size = 4),
    legend.title = element_text(size = 6)
  ) +
  guides(size = guide_legend(title = expression(R^2), order = 2, nrow = 1, label.position = "bottom"),
         fill = guide_colorbar(title = "Dispersal level", title.position = "top", order = 1, barheight = 0.5, barwidth = 5),
         color = guide_colorbar(title = "Environmental deviation", title.position = "top", title.hjust = 1, order = 3, barheight = 0.5, barwidth = 5))

ggsave(paste0(tiff_path, "Sup_F3_scenF.tiff"), dpi = 600, width = 6, height = 4)



# Summary Table ---------------------------------------------------------------------------
spp_data %>% 
  group_by(scenario) %>% 
  summarise(E = paste0(signif(mean(env), digits = 2), " (", signif(sd(env), digits=2), ")"),
            S = paste0(signif(mean(spa), digits = 2), " (", signif(sd(spa), digits=2), ")"),
            C = paste0(signif(mean(codist), digits = 2), " (", signif(sd(codist), digits=2), ")"),
            `1-R^2` = paste0(signif(mean(1-r2), digits = 2), " (", signif(sd(1-r2), digits=2), ")"))  -> A

  
sites_data %>% 
  mutate(scenario = recode(scenario, A = "A sites", B = "B sites", C = "C sites", D = "D sites", E = "E sites", F="F sites", G = "G sites")) %>% 
  group_by(scenario) %>% 
  summarise(E = paste0(signif(mean(env), digits = 2), " (", signif(sd(env), digits=2), ")"),
            S = paste0(signif(mean(spa), digits = 2), " (", signif(sd(spa), digits=2), ")"),
            C = paste0(signif(mean(codist), digits = 2), " (", signif(sd(codist), digits=2), ")"),
            `1-R^2` = paste0(signif(mean(1-r2), digits = 2), " (", signif(sd(1-r2), digits=2), ")")) -> B
bind_rows(A, B) -> C

write.csv(bind_rows(A,B), paste0(tiff_path, "summary_table.csv"))

# Make table as image
g <- tableGrob(C, rows = NULL)
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 2, b = nrow(g), l = 1, r = ncol(g))
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 1, l = 1, r = ncol(g))
grid::grid.draw(g)
