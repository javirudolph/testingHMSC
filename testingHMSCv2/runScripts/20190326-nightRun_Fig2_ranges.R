# This script is designed to run a series of scenarios with varying niche breadth so we can determine when we see species interactions. Seems like the niche breadth might be a little too restrictive to allow for species interactions to have an effect.

# Libraries ---------------------------------------------------------------
# Load the libraries
library(HMSC)
library(doParallel)
library(tidyverse)


# Functions ---------------------------------------------------------------
# Load the functions
source("functions/prep_pars_fx.R")
source("functions/metacom_sim_fx.R")
source("functions/main_sim_fx.R")
source("functions/full_process_fx.R")

# OrigLandscape -----------------------------------------------------------

XY <- readRDS("outputs/fixedLandscapes/orig-no-seed-XY.RDS")
E <- readRDS("outputs/fixedLandscapes/orig-no-seed-E.RDS")
MEMsel <- readRDS("outputs/fixedLandscapes/orig-no-seed-MEMsel.RDS")


# Create folder to save outputs -------------------------------------------

savedate <- format(Sys.Date(), "%Y%m%d")
folderpath <- paste0("outputs/", savedate, "-Fig2withInter/")

if(dir.exists(folderpath) == FALSE){
  dir.create(folderpath)
}


# Niche breadth -----------------------------------------------------------

nch_breadth <- seq(from = 0.5, to = 2, by = 0.1)
ncores <- 10 # in JMP lab desktop

# Run cycles --------------------------------------------------------------

for(j in 5:length(nch_breadth)){
  namesrds <- paste0("scenario", j)
  
  pars <- prep_pars(N = 1000, D = 1, R = 15,
                    nicheOpt = NULL, breadth = nch_breadth[j], alpha = 0.005,
                    interx_col = 1.5, interx_ext = 1.5, makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)
  
  sims <- metacom_sim4HMSC(XY = XY, E = E, pars = pars,
                           nsteps = 200, occupancy = 0.8, niter = 5,
                           makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)
  
  model <- metacom_as_HMSCdata(sims, numClusters = ncores, E = E, MEMsel = MEMsel,
                               makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)
  
  vpSpp <- get_VPresults(model, MEMsel = MEMsel, numClusters = ncores,
                         makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)
  
  vpSites <- get_VPresults_SITE(model, MEMsel = MEMsel, numClusters = ncores,
                                makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)
  
}


# Create folder to save outputs -------------------------------------------

savedate <- format(Sys.Date(), "%Y%m%d")
folderpath <- paste0("outputs/", savedate, "-Fig2woutInter/")

if(dir.exists(folderpath) == FALSE){
  dir.create(folderpath)
}


# Niche breadth -----------------------------------------------------------

nch_breadth <- seq(from = 0.5, to = 2, by = 0.1)


# Run cycles --------------------------------------------------------------

for(j in 1:length(nch_breadth)){
  namesrds <- paste0("scenario", j)
  
  pars <- prep_pars(N = 1000, D = 1, R = 15,
                    nicheOpt = NULL, breadth = nch_breadth[j], alpha = 0.005,
                    interx_col = 0, interx_ext = 0, makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)
  
  sims <- metacom_sim4HMSC(XY = XY, E = E, pars = pars,
                           nsteps = 200, occupancy = 0.8, niter = 5,
                           makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)
  
  model <- metacom_as_HMSCdata(sims, numClusters = ncores, E = E, MEMsel = MEMsel,
                               makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)
  
  vpSpp <- get_VPresults(model, MEMsel = MEMsel, numClusters = ncores,
                         makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)
  
  vpSites <- get_VPresults_SITE(model, MEMsel = MEMsel, numClusters = ncores,
                                makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)
  
}




