# This script is used to run several models with two species, and changing their niche optima


# Libraries ---------------------------------------------------------------
# Load the libraries
library(HMSC)
library(doParallel)
library(tidyverse)
library(ggtern)


# Functions ---------------------------------------------------------------
# Load the functions
source("functions/prep_pars_fx.R")
source("functions/metacom_sim_fx.R")
source("functions/main_sim_fx.R")
source("functions/full_process_fx.R")
source("functions/data_wrangling_fx.R")
source("functions/make_tern_plot_fx.R")


# OrigLandscape -----------------------------------------------------------

XY <- readRDS("outputs/fixedLandscapes/orig-no-seed-XY.RDS")
E <- readRDS("outputs/fixedLandscapes/orig-no-seed-E.RDS")
MEMsel <- readRDS("outputs/fixedLandscapes/orig-no-seed-MEMsel.RDS")



# RunCycles ---------------------------------------------------------------
# These steps should happen for each combination of the niche optima parameters
# 1. Prepare Parameters
# 2. Simulate Metacommunity
# 3. Fit/Format HMSC
# 4. VP for species
# 5. VP for sites
# 6. Organize dataframes for plotting
# 7. Plot



# This loop would repeat for each niche scenario...
# Problem with identifiers - using the niche optima has a decimal point, filenames don't like periods.
# Label them in progression? both 0.5 is scenario1?


# Create Folder to save ---------------------------------------------------


savedate <- format(Sys.Date(), "%Y%m%d")
folderpath <- paste0("outputs/", savedate, "-two_spp_niche_comparisons/")

if(dir.exists(folderpath) == FALSE){
  dir.create(folderpath)
}

# Species Niche Optima ----------------------------------------------------

spp1niche <- seq(from = 0.5, to = 0.9, by = 0.05)
spp2niche <- rev(seq(from = 0.1, to = 0.5, by = 0.05))


# u_c <- matrix(c(spp1niche[2], spp2niche[2]), nrow = 1, ncol = 2)

#########################################################################################################
# This chunk runs the cycle and it takes a long time to finish. Run at night.

for(j in 1:length(spp1niche)){
  nicheOpt2spp <- matrix(c(spp1niche[j], spp2niche[j]), ncol = 2, nrow = 1)
  namesrds <- paste0("scenario", j)

  pars <- prep_pars(N = 1000, D = 1, R = 2,
                    nicheOpt = nicheOpt2spp, breadth = 0.2, alpha = 0.005,
                    interx_col = 0, interx_ext = 0, makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)

  sims <- metacom_sim4HMSC(XY = XY, E = E, pars = pars,
                            nsteps = 200, occupancy = 0.8, niter = 5,
                           makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)

  model <- metacom_as_HMSCdata(sims, numClusters = 4, E = E, MEMsel = MEMsel,
                               makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)

  vpSpp <- get_VPresults(model, MEMsel = MEMsel, numClusters = 4,
                         makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)

  vpSites <- get_VPresults_SITE(model, MEMsel = MEMsel, numClusters = 4,
                           makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)

}



# Organize and Plot -------------------------------------------------------

save_csv_and_plots <- function(scenario){
  sppcsv <- doItAll_twosppNichecomp(outPath = folderpath, scenarioNum = scenario, indSites = FALSE)
  write.csv(sppcsv, file = paste0(folderpath, "csvFiles/", scenario, "spp.csv"))
  make_tern_plot(sppcsv, varShape = "species", varColor = "nicheOpt") +
    labs(title = scenario)
  ggsave(filename = paste0(folderpath, "figures/", scenario, "spp.png"), dpi = 300, width = 9, height = 4.5)
  
  sitescsv <- doItAll_twosppNichecomp(outPath = folderpath, scenarioNum = scenario, indSites = TRUE)
  write.csv(sitescsv, file = paste0(folderpath, "csvFiles/", scenario, "sites.csv"))
  make_tern_plot(sitescsv, varShape = "iteration", varColor = "richness") +
    labs(title=scenario)
  ggsave(filename = paste0(folderpath, "figures/", scenario, "sites.png"), dpi = 300, width = 9, height = 4.5)
  
}

for(i in 1:9){
  loopscen <- paste0("scenario", i)
  save_csv_and_plots(loopscen)
}



# All in one plot ---------------------------------------------------------

allData <- NULL
for(i in 1:9){
  scenario <- paste0("scenario", i)
  onescenario <- doItAll_twosppNichecomp(outPath = folderpath, scenario)
  allData <- bind_rows(allData, onescenario)
  
}

allData %>% 
  make_tern_plot(., varColor = "nicheOpt") +
  scale_color_viridis_c()




