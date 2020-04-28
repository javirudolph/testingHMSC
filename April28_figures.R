# Second go at making figures


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


# INTERACTION MATRICES FIGURE 3 -------------------------------------------


modelfile <- readRDS(paste0(outsfolderpath, "FIG3C", "-model.RDS"))

assoMat <- NULL
for(i in 1:5){
  assoMat[[i]] <- corRandomEff(modelfile[[i]])
}



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

