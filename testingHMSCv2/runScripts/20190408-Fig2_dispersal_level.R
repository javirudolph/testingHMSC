# The idea of this one is to test Figure 2 but with different dspersal levels
# And check across the range of niche breadth

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


# Niche breadth -----------------------------------------------------------

nch_breadth <- seq(from = 0.6, to = 2, by = 0.2) # changed to 0.2 so we have less scenarios
ncores <- 10 # in JMP lab desktop

alpha <- c(0.001, 0.005, 0.015, 0.05, 0.1)
namingWout <- paste0("disp", 1:5, "wout")
namingWith <- paste0("disp", 1:5, "with")

# Run cycles without interactions --------------------------------------------------------------

for(i in 1:5){
  savedate <- format(Sys.Date(), "%Y%m%d")
  folderpath <- paste0("outputs/", savedate, "-Fig2_disp/", namingWout[i], "/")
  
  if(dir.exists(folderpath) == FALSE){
    dir.create(folderpath)
  }
  myDisp <- alpha[i]
  
  for(j in 1:length(nch_breadth)){
    namesrds <- paste0("scenario", j)
    
    pars <- prep_pars(N = 1000, D = 1, R = 15,
                      nicheOpt = NULL, breadth = nch_breadth[j], alpha = myDisp,
                      interx_col = 0, interx_ext = 0, makeRDS = TRUE, 
                      whereToSave = folderpath, objName = namesrds)
    
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
}


# Run cycles with interactions --------------------------------------------------------------

for(i in 1:5){
  savedate <- format(Sys.Date(), "%Y%m%d")
  folderpath <- paste0("outputs/", savedate, "-Fig2_disp/", namingWith[i], "/")
  
  if(dir.exists(folderpath) == FALSE){
    dir.create(folderpath)
  }
  myDisp <- alpha[i]
  
  for(j in 1:length(nch_breadth)){
    namesrds <- paste0("scenario", j)
    
    pars <- prep_pars(N = 1000, D = 1, R = 15,
                      nicheOpt = NULL, breadth = nch_breadth[j], alpha = myDisp,
                      interx_col = 1.5, interx_ext = 1.5, makeRDS = TRUE, 
                      whereToSave = folderpath, objName = namesrds)
    
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
}