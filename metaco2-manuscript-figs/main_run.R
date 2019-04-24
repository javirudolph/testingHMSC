###############################################################################################
# FIGURES 2 AND 3 
# Created by: Javiera Rudolph
# Date: April 21, 2019
##############################################################################################

# This is the master script to generate the results for figures 2 and 3 that would be included in the manuscript
# Depending on where you are running this script, you can change the number of cores to use:

# If using HiperGator, you will provide the number of cores in the submission script, and can uncomment the following line.
#ncores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))


# If using a local computer, you can set the number of cores according to you locals ability

ncores <- 10

# You can set the folderpath to the directory where the RDS files will be saved:
fileDate <- Sys.Date()
fileDescription <- "orig_disp_10kchain"

folderpath <- paste0("metaco2-manuscript-figs/", fileDate, "_", fileDescription, "/")


if(dir.exists(folderpath) == FALSE){
  dir.create(folderpath)
}


# General Parameters ------------------------------------------------------

# To make things faster when testing different parameters, these are the ones we are playing with

disp_low <- 0.001
disp_med <- 0.005
disp_hi <- 0.015

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

# FUNCTIONS ---------------------------------------------------------------

source("functions/prep_pars_fx.R")
source("functions/metacom_sim_fx.R")
source("functions/main_sim_fx.R")
source("functions/full_process_fx.R")


# ORIGINAL LANDSCAPE ------------------------------------------------------

XY <- readRDS("functions/fixedLandscapes/orig-no-seed-XY.RDS")
E <- readRDS("functions/fixedLandscapes/orig-no-seed-E.RDS")
MEMsel <- readRDS("functions/fixedLandscapes/orig-no-seed-MEMsel.RDS")



##############################################################################################
# FIGURE2 -----------------------------------------------------------------

#FIG2A: no interactions, narrow niche
scen1pars <- prep_pars(N = 1000, D = 1, R = 12, breadth = niche_narrow, nicheOpt = NULL, alpha = disp_med,
                       interx_col = 0, interx_ext = 0, makeRDS = TRUE,
                     whereToSave = folderpath, objName = "FIG2A")

# NOTE: the nicheOpt is set to null to follow the default, which gives species evenly spaced optima for the number of species we state with R. 


#FIG2B: no interactions, broad niche
scen2pars <- prep_pars(N = 1000, D = 1, R = 12, breadth = niche_broad, nicheOpt = NULL, alpha = disp_med,
                       interx_col = 0, interx_ext = 0, makeRDS = TRUE,
                       whereToSave = folderpath, objName = "FIG2B")

#FIG2C: with interactions, narrow niche

scen3pars <- prep_pars(N = 1000, D = 1, R = 12, niche_narrow, nicheOpt = NULL, alpha = disp_med,
                       interx_col = 1.5, interx_ext = 1.5, makeRDS = TRUE,
                       whereToSave = folderpath, objName = "FIG2C")

#FIG2D: with interactions, broad niche

scen4pars <- prep_pars(N = 1000, D = 1, R = 12, breadth = niche_broad, nicheOpt = NULL, alpha = disp_med,
                       interx_col = 1.5, interx_ext = 1.5, makeRDS = TRUE,
                       whereToSave = folderpath, objName = "FIG2D")

fig2pars <- list(scen1pars = scen1pars, scen2pars = scen2pars, 
                 scen3pars = scen3pars, scen4pars = scen4pars)


# run cycles --------------------------------------------------------------



for(j in 1:4){
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


##############################################################################################

# FIGURE3 -----------------------------------------------------------------

# FIG3A: half of the species with interactions, the other half without

scen1pars <- list(scen1_a = prep_pars(N = 1000, D = 1, R = 6, breadth = niche_narrow, nicheOpt = NULL, 
                                      alpha = disp_med,
                                      interx_col = 0, interx_ext = 0, makeRDS = FALSE),
                  scen1_b = prep_pars(N = 1000, D = 1, R = 6, breadth = niche_narrow, nicheOpt = NULL, 
                                      alpha = disp_med,
                                      interx_col = 1.5, interx_ext = 1.5, makeRDS = FALSE))
saveRDS(scen1pars, file = paste0(folderpath, "FIG3A-params.RDS"))


# FIG3B: a third of the species with low, med and high dispersal levels, without interactions.

scen2pars <- list(scen2_a = prep_pars(N = 1000, D = 1, R = 4, breadth = niche_narrow, nicheOpt = NULL, 
                                      alpha = disp_low,
                                      interx_col = 0, interx_ext = 0, makeRDS = FALSE),
                  scen2_b = prep_pars(N = 1000, D = 1, R = 4, breadth = niche_narrow, nicheOpt = NULL, 
                                      alpha = disp_med,
                                      interx_col = 0, interx_ext = 0, makeRDS = FALSE),
                  scen2_c = prep_pars(N = 1000, D = 1, R = 4, breadth = niche_narrow, nicheOpt = NULL, 
                                      alpha = disp_hi,
                                      interx_col = 0, interx_ext = 0, makeRDS = FALSE))
saveRDS(scen2pars, file = paste0(folderpath, "FIG3B-params.RDS"))


# FIG3B: a third of the species with low, med and high dispersal levels, with interactions.

scen3pars <- list(scen3_a = prep_pars(N = 1000, D = 1, R = 4, breadth = niche_narrow, nicheOpt = NULL, 
                                      alpha = disp_low,
                                      interx_col = 1.5, interx_ext = 1.5, makeRDS = FALSE),
                  scen3_b = prep_pars(N = 1000, D = 1, R = 4, breadth = niche_narrow, nicheOpt = NULL, 
                                      alpha = disp_med,
                                      interx_col = 1.5, interx_ext = 1.5, makeRDS = FALSE),
                  scen3_c = prep_pars(N = 1000, D = 1, R = 4, breadth = niche_narrow, nicheOpt = NULL, 
                                      alpha = disp_hi,
                                      interx_col = 1.5, interx_ext = 1.5, makeRDS = FALSE))
saveRDS(scen3pars, file = paste0(folderpath, "FIG3B-params.RDS"))


scenarioPars <- list(scen1pars = scen1pars, scen2pars = scen2pars, 
                     scen3pars = scen3pars)


# Run cycles --------------------------------------------------------------

for(j in 1:3){
  namesrds <- paste0("FIG3", LETTERS[j])
  
  sims <- metacom_sim4HMSC_multParams(XY = XY, E = E, pars = scenarioPars[[j]],
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

