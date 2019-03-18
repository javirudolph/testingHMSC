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


# Replace with quadratic --------------------------------------------------
# I'm basically overwriting the original function with this one that has the quadratic term. 
# It'snot an elegant way, but we can make an if statement later on, so you can choose which one to go with. 
S_f <- function(E, u_c, s_c) {
  R <- ncol(u_c)
  N <- nrow(E)
  D <- ncol(E)
  S <- matrix(1, nr = N, nc = R)
  for(i in 1:D){
    optima <- matrix(u_c[i,],nrow = N,ncol = R,byrow = TRUE)
    breadth <- matrix(s_c[i,],nrow = N,ncol = R,byrow = TRUE)
    S <- S * ((-1 / (breadth/2)^2) * (E[,i] - optima)^2 + 1)
    S <- ifelse(S < 0, 0 , S)
    
  }
  return(S)
}


# Create folder to save outputs -------------------------------------------

savedate <- format(Sys.Date(), "%Y%m%d")
folderpath <- paste0("outputs/", savedate, "-fig2c_quad/")

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
                    interx_col = 1.5, interx_ext = 1.5, makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)
  
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





