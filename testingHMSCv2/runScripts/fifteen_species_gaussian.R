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


# Create Folder to save ---------------------------------------------------


savedate <- format(Sys.Date(), "%Y%m%d")
folderpath <- paste0("outputs/", savedate, "-fifteen_species_scenarios_gaussian/")

if(dir.exists(folderpath) == FALSE){
  dir.create(folderpath)
}

#For csv files and figures
csvFilesFolderpath <- paste0(folderpath, "csvFiles/")
if(dir.exists(csvFilesFolderpath) == FALSE){
  dir.create(csvFilesFolderpath)
}

figuresFolderpath <- paste0(folderpath, "figures/")
if(dir.exists(figuresFolderpath) == FALSE){
  dir.create(figuresFolderpath)
}


# Scenarios ---------------------------------------------------------------
# In this script we are going to repeat the scenarios for the fifteen species we originally considered, but will use a quadratic response to the environment. 

#Scenario1: no interactions, narrow niche

scen1pars <- prep_pars(N = 1000, D = 1, R = 15, breadth = 0.2, nicheOpt = NULL, alpha = 0.005,
                  interx_col = 0, interx_ext = 0, makeRDS = TRUE, whereToSave = folderpath, objName = "scenario1")


#Scenario2: no interactions, broad niche

scen2pars <- prep_pars(N = 1000, D = 1, R = 15, breadth = 1, nicheOpt = NULL, alpha = 0.005,
                       interx_col = 0, interx_ext = 0, makeRDS = TRUE, whereToSave = folderpath, objName = "scenario2")

#Scenario3: with interactions, narrow niche

scen3pars <- prep_pars(N = 1000, D = 1, R = 15, breadth = 0.2, nicheOpt = NULL, alpha = 0.005,
                       interx_col = 1.5, interx_ext = 1.5, makeRDS = TRUE, whereToSave = folderpath, objName = "scenario3")

#Scenario4: with interactions, broad niche

scen4pars <- prep_pars(N = 1000, D = 1, R = 15, breadth = 1, nicheOpt = NULL, alpha = 0.005,
                       interx_col = 1.5, interx_ext = 1.5, makeRDS = TRUE, whereToSave = folderpath, objName = "scenario4")

pars <- list(scen1pars = scen1pars, scen2pars = scen2pars, 
             scen3pars = scen3pars, scen4pars = scen4pars)


# run cycles --------------------------------------------------------------

for(j in 1:4){
  namesrds <- paste0("scenario", j)
  
  sims <- metacom_sim4HMSC(XY = XY, E = E, pars = pars[[j]],
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


# Organize Data -----------------------------------------------------------

for(i in 1:4){
  loopscen <- paste0("scenario", i)
  save_csv_and_plots(loopscen)
  print(i)
}















