


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



# Gaussian goes first -----------------------------------------------------

# Create Folder to save outputs ---------------------------------------------------


savedate <- format(Sys.Date(), "%Y%m%d")
folderpath <- paste0("outputs/", savedate, "-fifteen_species_fig3_gaussian/")

if(dir.exists(folderpath) == FALSE){
  dir.create(folderpath)
}


# Scenarios ---------------------------------------------------------------

# Scenario1: all parameters the same, but half the species have interactions

scen1pars <- list(scen1_a = prep_pars(N = 1000, D = 1, R = 7, breadth = 0.2, nicheOpt = NULL, alpha = 0.005,
                                      interx_col = 0, interx_ext = 0, makeRDS = FALSE),
                  scen1_b = prep_pars(N = 1000, D = 1, R = 8, breadth = 0.2, nicheOpt = NULL, alpha = 0.005,
                                      interx_col = 1.5, interx_ext = 1.5, makeRDS = FALSE))
saveRDS(scen1pars, file = paste0(folderpath, "scenario1-params.RDS"))


# Scenario2: Change dispersal level

scen2pars <- list(scen2_a = prep_pars(N = 1000, D = 1, R = 5, breadth = 0.2, nicheOpt = NULL, alpha = 0.001,
                                      interx_col = 0, interx_ext = 0, makeRDS = FALSE),
                  scen2_b = prep_pars(N = 1000, D = 1, R = 5, breadth = 0.2, nicheOpt = NULL, alpha = 0.005,
                                      interx_col = 0, interx_ext = 0, makeRDS = FALSE),
                  scen2_c = prep_pars(N = 1000, D = 1, R = 5, breadth = 0.2, nicheOpt = NULL, alpha = 0.015,
                                      interx_col = 0, interx_ext = 0, makeRDS = FALSE))
saveRDS(scen2pars, file = paste0(folderpath, "scenario2-params.RDS"))


# Scenario2: Change dispersal level and add interactions

scen3pars <- list(scen3_a = prep_pars(N = 1000, D = 1, R = 5, breadth = 0.2, nicheOpt = NULL, alpha = 0.001,
                                      interx_col = 1.5, interx_ext = 1.5, makeRDS = FALSE),
                  scen3_b = prep_pars(N = 1000, D = 1, R = 5, breadth = 0.2, nicheOpt = NULL, alpha = 0.005,
                                      interx_col = 1.5, interx_ext = 1.5, makeRDS = FALSE),
                  scen3_c = prep_pars(N = 1000, D = 1, R = 5, breadth = 0.2, nicheOpt = NULL, alpha = 0.015,
                                      interx_col = 1.5, interx_ext = 1.5, makeRDS = FALSE))
saveRDS(scen3pars, file = paste0(folderpath, "scenario3-params.RDS"))


scenarioPars <- list(scen1pars = scen1pars, scen2pars = scen2pars, 
                     scen3pars = scen3pars)


# Run cycles --------------------------------------------------------------

for(j in 1:3){
  namesrds <- paste0("scenario", j)
  
  sims <- metacom_sim4HMSC_multParams(XY = XY, E = E, pars = scenarioPars[[j]],
                                      nsteps = 200, occupancy = 0.8, niter = 5,
                                      makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)
  
  print(paste("sims", j, "created"))
  
  
  
  model <- metacom_as_HMSCdata(sims, numClusters = 4, E = E, MEMsel = MEMsel,
                               makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)
  print(paste("model", j, "created"))
  
  
  
  vpSpp <- get_VPresults(model, MEMsel = MEMsel, numClusters = 4,
                         makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)
  print(paste("vpspp", j, "created"))
  
  
  
  vpSites <- get_VPresults_SITE(model, MEMsel = MEMsel, numClusters = 4,
                                makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)
  print(paste("vpsites", j, "created"))
  
}



# QUADRATIC ---------------------------------------------------------------

#Overwrite the gaussian function with the quadratic response one
S_f <- function(E, u_c, s_c) {
  R <- ncol(u_c)
  N <- nrow(E)
  D <- ncol(E)
  S <- matrix(1, nr = N, nc = R)
  for(i in 1:D){
    optima <- matrix(u_c[i,],nrow = N,ncol = R,byrow = TRUE)
    breadth <- matrix(s_c[i,],nrow = N,ncol = R,byrow = TRUE)
    S <- S * ((-1 / (breadth/2)^2) * (E[,i] - optima)^2 + 1)
  }
  return(S)
}




# Create Folder to save outputs ---------------------------------------------------
  
  
savedate <- format(Sys.Date(), "%Y%m%d")
folderpath <- paste0("outputs/", savedate, "-fifteen_species_fig2_quadratic/")

if(dir.exists(folderpath) == FALSE){
  dir.create(folderpath)
}


# Fig2 scenarios ----------------------------------------------------------

#Scenario1: no interactions, narrow niche

scen1pars <- prep_pars(N = 1000, D = 1, R = 15, breadth = 0.2, nicheOpt = NULL, alpha = 0.005,
                       interx_col = 0, interx_ext = 0, makeRDS = TRUE, whereToSave = folderpath, objName = "scenario1")


#Scenario2: no interactions, broad niche

scen2pars <- prep_pars(N = 1000, D = 1, R = 15, breadth = 2, nicheOpt = NULL, alpha = 0.005,
                       interx_col = 0, interx_ext = 0, makeRDS = TRUE, whereToSave = folderpath, objName = "scenario2")

#Scenario3: with interactions, narrow niche

scen3pars <- prep_pars(N = 1000, D = 1, R = 15, breadth = 0.2, nicheOpt = NULL, alpha = 0.005,
                       interx_col = 1.5, interx_ext = 1.5, makeRDS = TRUE, whereToSave = folderpath, objName = "scenario3")

#Scenario4: with interactions, broad niche

scen4pars <- prep_pars(N = 1000, D = 1, R = 15, breadth = 2, nicheOpt = NULL, alpha = 0.005,
                       interx_col = 1.5, interx_ext = 1.5, makeRDS = TRUE, whereToSave = folderpath, objName = "scenario4")

fig2pars <- list(scen1pars = scen1pars, scen2pars = scen2pars, 
             scen3pars = scen3pars, scen4pars = scen4pars)


# run cycles --------------------------------------------------------------

for(j in 1:4){
  namesrds <- paste0("scenario", j)
  
  sims <- metacom_sim4HMSC(XY = XY, E = E, pars = fig2pars[[j]],
                           nsteps = 200, occupancy = 0.8, niter = 5,
                           makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)


  model <- metacom_as_HMSCdata(sims, numClusters = 4, E = E, MEMsel = MEMsel,
                               makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)
  
  vpSpp <- get_VPresults(model, MEMsel = MEMsel, numClusters = 4,
                         makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)

  
  vpSites <- get_VPresults_SITE(model, MEMsel = MEMsel, numClusters = 4,
                                makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)
  
}




# FIG3 QUADRATIC ----------------------------------------------------------

# Create Folder to save outputs ---------------------------------------------------


savedate <- format(Sys.Date(), "%Y%m%d")
folderpath <- paste0("outputs/", savedate, "-fifteen_species_fig3_quadratic/")

if(dir.exists(folderpath) == FALSE){
  dir.create(folderpath)
}

# Scenarios ---------------------------------------------------------------

# Scenario1: all parameters the same, but half the species have interactions

scen1pars <- list(scen1_a = prep_pars(N = 1000, D = 1, R = 7, breadth = 0.2, nicheOpt = NULL, alpha = 0.005,
                                      interx_col = 0, interx_ext = 0, makeRDS = FALSE),
                  scen1_b = prep_pars(N = 1000, D = 1, R = 8, breadth = 0.2, nicheOpt = NULL, alpha = 0.005,
                                      interx_col = 1.5, interx_ext = 1.5, makeRDS = FALSE))
saveRDS(scen1pars, file = paste0(folderpath, "scenario1-params.RDS"))


# Scenario2: Change dispersal level

scen2pars <- list(scen2_a = prep_pars(N = 1000, D = 1, R = 5, breadth = 0.2, nicheOpt = NULL, alpha = 0.001,
                                      interx_col = 0, interx_ext = 0, makeRDS = FALSE),
                  scen2_b = prep_pars(N = 1000, D = 1, R = 5, breadth = 0.2, nicheOpt = NULL, alpha = 0.005,
                                      interx_col = 1.5, interx_ext = 1.5, makeRDS = FALSE),
                  scen2_c = prep_pars(N = 1000, D = 1, R = 5, breadth = 0.2, nicheOpt = NULL, alpha = 0.015,
                                      interx_col = 0, interx_ext = 0, makeRDS = FALSE))
saveRDS(scen2pars, file = paste0(folderpath, "scenario2-params.RDS"))


# Scenario2: Change dispersal level and add interactions

scen3pars <- list(scen3_a = prep_pars(N = 1000, D = 1, R = 5, breadth = 0.2, nicheOpt = NULL, alpha = 0.001,
                                      interx_col = 1.5, interx_ext = 1.5, makeRDS = FALSE),
                  scen3_b = prep_pars(N = 1000, D = 1, R = 5, breadth = 0.2, nicheOpt = NULL, alpha = 0.005,
                                      interx_col = 1.5, interx_ext = 1.5, makeRDS = FALSE),
                  scen3_c = prep_pars(N = 1000, D = 1, R = 5, breadth = 0.2, nicheOpt = NULL, alpha = 0.015,
                                      interx_col = 1.5, interx_ext = 1.5, makeRDS = FALSE))
saveRDS(scen3pars, file = paste0(folderpath, "scenario3-params.RDS"))


scenarioPars <- list(scen1pars = scen1pars, scen2pars = scen2pars, 
                     scen3pars = scen3pars)


# Run cycles --------------------------------------------------------------

for(j in 1:3){
  namesrds <- paste0("scenario", j)
  
  sims <- metacom_sim4HMSC_multParams(XY = XY, E = E, pars = scenarioPars[[j]],
                                      nsteps = 200, occupancy = 0.8, niter = 5,
                                      makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)

  
  model <- metacom_as_HMSCdata(sims, numClusters = 4, E = E, MEMsel = MEMsel,
                               makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)

  
  vpSpp <- get_VPresults(model, MEMsel = MEMsel, numClusters = 4,
                         makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)

  
  vpSites <- get_VPresults_SITE(model, MEMsel = MEMsel, numClusters = 4,
                                makeRDS = TRUE, whereToSave = folderpath, objName = namesrds)

}


























