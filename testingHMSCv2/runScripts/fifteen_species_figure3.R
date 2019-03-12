
# Libraries ---------------------------------------------------------------
# Load the libraries
library(HMSC)
library(doParallel)
library(tidyverse)
library(ggtern)

# Create Folder to save outputs ---------------------------------------------------


savedate <- format(Sys.Date(), "%Y%m%d")
folderpath <- paste0("outputs/", savedate, "-fifteen_species_fig3_quadratic/")

if(dir.exists(folderpath) == FALSE){
  dir.create(folderpath)
  dir.create(paste0(folderpath, "csvFiles/"))
  dir.create(paste0(folderpath, "figures/"))
}


# Functions ---------------------------------------------------------------
# Load the functions
source("functions/prep_pars_fx.R")
source("functions/metacom_sim_fx.R")
source("functions/main_sim_fx.R")
source("functions/full_process_fx.R")
source("functions/data_wrangling_fx.R")
source("functions/make_tern_plot_fx.R")


# Replace with quadratic --------------------------------------------------
# I'm basically overwriting the original function with this one that has the quadratic term. 
# It's not an elegant way, but we can make an if statement later on, so you can choose which one to go with. 
if(TRUE){
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
}

# OrigLandscape -----------------------------------------------------------

XY <- readRDS("outputs/fixedLandscapes/orig-no-seed-XY.RDS")
E <- readRDS("outputs/fixedLandscapes/orig-no-seed-E.RDS")
MEMsel <- readRDS("outputs/fixedLandscapes/orig-no-seed-MEMsel.RDS")


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

