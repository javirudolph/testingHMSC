
# there is an error somewhere
# It completely erases the environmental component from Fig2A

# If using a local computer, you can set the number of cores according to you locals ability

ncores <- 10

# You can set the folderpath to the directory where the RDS files will be saved:
fileDate <- Sys.Date()
fileDescription <- "fig2A_bug"

folderpath <- paste0("outputs/", fileDate, "_", fileDescription, "/")


if(dir.exists(folderpath) == FALSE){
  dir.create(folderpath)
}


# General Parameters ------------------------------------------------------

# To make things faster when testing different parameters, these are the ones we are playing with

disp_low <- 0.015
disp_med <- 0.05
disp_hi <- 0.1

niche_broad <- 2
niche_narrow <- 0.8

hmscPars <- list(niter = 10000, nburn = 2000, thin = 2)

save.image(file = paste0(folderpath, "runInfo", ".RData"))


###################################################################################################
# No need to change anything in the following lines

# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(HMSC)
library(doParallel)
library(coda)
library(corrplot)

# FUNCTIONS ---------------------------------------------------------------

source("functions/prep_pars_fx.R")
source("functions/metacom_sim_fx.R")
source("functions/main_sim_fx.R")
source("functions/full_process_fx.R")


# ORIGINAL LANDSCAPE ------------------------------------------------------

XY <- readRDS("outputs/fixedLandscapes/orig-no-seed-XY.RDS")
E <- readRDS("outputs/fixedLandscapes/orig-no-seed-E.RDS")
MEMsel <- readRDS("outputs/fixedLandscapes/orig-no-seed-MEMsel.RDS")



#FIG2A: no interactions, narrow niche
scen1pars <- prep_pars(N = 1000, D = 1, R = 12, breadth = niche_narrow, nicheOpt = NULL, alpha = disp_med,
                       interx_col = 0, interx_ext = 0, makeRDS = TRUE,
                       whereToSave = folderpath, objName = "FIG2A")

# run cycles --------------------------------------------------------------



for(j in 1:1){
  namesrds <- paste0("FIG2", LETTERS[j])
  
  sims <- metacom_sim4HMSC(XY = XY, E = E, pars = fig2pars[[j]],
                           nsteps = 200, occupancy = 0.8, niter = 5,
                           makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)
  
  
  model <- metacom_as_HMSCdata(sims, numClusters = ncores, E = E, MEMsel = MEMsel,
                               hmscPars = hmscPars,
                               makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)
  
  vpSpp <- get_VPresults(model, MEMsel = MEMsel, numClusters = ncores,
                         makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)
  
  
  vpSites <- get_VPresults_SITE(model, MEMsel = MEMsel, numClusters = ncores,
                                makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)
  
}


# Get Results -------------------------------------------------------------

outsfolderpath <- folderpath
source("run-scripts/output_figures.R")
source("run-scripts/check_convergence.R")

pdf(paste0(outsfolderpath, "figs.pdf"))
data <- get_species_data(outsfolderpath, scenarios[i])
plotspp <- species_plot(data, plotMain = paste0(scenarios[i], "_spp"), colorVar = "iteration")
print(plotspp)

E <- readRDS("outputs/fixedLandscapes/orig-no-seed-E.RDS")

data <- get_sites_data(outsfolderpath, scenarios[i]) %>% 
  mutate(E = rep(E, 5), 
         Edev = abs(E-0.5))
plotsites <- sites_plot(data, plotMain = paste0(scenarios[i], "_sites"), 
                      colorVar = "Edev", colorLegend = "Environmental deviation") +
  scale_color_viridis_c()
print(plotsites)
dev.off()

