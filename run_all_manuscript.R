# This script runs everything to get all files and simulated data. 
# It sources several scripts, so make sure you are using the `testingHMSC` project so file directories work correctly.
# You can check the outline for this script by pressing `Ctrl + Shift + O` or the outline button if using RStudio

# Make sure Guillaume's HMSC is installed
# https://github.com/guiblanchet/HMSC

# Other packages needed. but you might already have these:
# install.packages('devtools')
# install.packages('tidyverse')
# install.packages('ggtern')
# install.packages('doParallel')
# install.packages('corrplot')

set.seed(77)

# This is the master script to generate the results for figures 2 and 3 that would be included in the manuscript
# Depending on where you are running this script, you can change the number of cores to use:

# If using HiperGator, you will provide the number of cores in the submission script, and can uncomment the following line.
# ncores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))


# If using a local computer, you can set the number of cores according to you locals ability
# Leibold lab computer is set to 8

ncores <- 7

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(HMSC)
library(doParallel)
library(ggtern)
library(corrplot)

# Directory ------------------------------------------------------
# setting the folder for the outputs for the manuscript

folderpath <- "manuscript_outputs/"
if(dir.exists(folderpath) == FALSE){
  dir.create(folderpath)
}

# General Parameters ------------------------------------------------------
# To make things faster when testing different parameters, these are the ones we are playing with

disp_low <- 0.01
disp_med <- 0.05
disp_hi <- 0.1

niche_broad <- 2
niche_narrow <- 0.8


# In the past, we tried several combinations of these parameters and there didn't seem to be much of a difference between them. According to the convergence plots it seems to happen pretty fast. We settled on the parameters in the next line, but you are free to try with others, example shown in the commented line after setting the current parameters. 

 hmscPars <- list(niter = 50000, nburn = 25000, thin = 10)
 
# hmscPars <- list(niter = 30000, nburn = 10000, thin = 10) # This is ok, but not enough for raftery.diag
# hmscPars <- list(niter = 10000, nburn = 5000, thin = 5)

# The parameters used for the run will get saved as RData in the outputs folder, just to keep track of them. The following line of code saves them. 
save.image(file = paste0(folderpath, "runInfo", ".RData"))


# Functions ---------------------------------------------------------------
# All of these scripts have functions only

# These will prepare a list of parameters in the right format
source("manuscript_functions/prep_pars_fx.R")

# Original functions for the metacommunity simulation. Note addition of a quadratic response to the environment. Which is what we've used here.
source("manuscript_functions/metacom_sim_fx.R")

# The actual metacommunity simulation that uses the functions above. It saves the snapshot of the metacommunity after 200 time steps.
source("manuscript_functions/main_sim_fx.R")

# This full process involves running the metacommunity simulation, getting in the hmsc format, doing variation partitioning for species and sites.
source("manuscript_functions/full_process_fx.R")

# Output processing involves changing structure of data from lists to dataframes and using those to get the figures
source("manuscript_functions/output_processing_fx.R")

# Functions to check convergence based on previous conversations with Guillaume. Raftery and Gelman plots
source("manuscript_functions/convergence_fx.R")

# Original landscape ------------------------------------------------------
# This is the original landscape provided in the Dropbox. They were previously saved as text files, it's the same data, just different format.

XY <- readRDS("manuscript_functions/fixedLandscapes/orig-no-seed-XY.RDS")
E <- readRDS("manuscript_functions/fixedLandscapes/orig-no-seed-E.RDS")
MEMsel <- readRDS("manuscript_functions/fixedLandscapes/orig-no-seed-MEMsel.RDS")


# FIGURE2 DATA -----------------------------------------------------------------
# This first part is only setting the parameters for the different scenarios. After that, the running cycles is actually running the metacommunity simulations, fitting the model and then variation partitioning.

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

# RUN THE CYCLES

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


# FIGURE3 DATA -----------------------------------------------------------------

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
saveRDS(scen3pars, file = paste0(folderpath, "FIG3C-params.RDS"))


scenarioPars <- list(scen1pars = scen1pars, scen2pars = scen2pars, 
                     scen3pars = scen3pars)


# RUN THE CYCLES

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

# Data processing --------------------------------------------------------------

scenarios <- c("FIG2A", "FIG2B", "FIG2C", "FIG2D", "FIG3A", "FIG3B", "FIG3C")

# This will create a pdf file with all the ternary plots for the different scenarios, where each point in the plot corresponds to one species, and each color is equivalent to one of the iterations we ran.
pdf(paste0(folderpath, "speciesFigs.pdf"))
for(i in 1:7){
  data <- get_species_data(folderpath, scenarios[i])
  plotspp <- species_plot(data, plotMain = paste0(scenarios[i], "_spp"), colorVar = "iteration")
  print(plotspp)
}
dev.off()

# Ternary plots at the site level, each point is a site, and the colors vary according to their environmental deviation (how close/far to the average environmental conditions are for that site.)
pdf(paste0(folderpath, "sitesFigs.pdf"))
for(i in 1:7){
  data <- get_sites_data(folderpath, scenarios[i]) %>% 
    mutate(E = rep(E, 5), 
           Edev = abs(E-0.5))
  plotspp <- sites_plot(data, plotMain = paste0(scenarios[i], "_sites"), 
                        colorVar = "Edev", colorLegend = "Environmental deviation") +
    scale_color_viridis_c()
  print(plotspp)
}
dev.off()


# Using the corRandomEff() function from Guillaume's package and largely following the examples provided by him in the vignette.

pdf(paste0(folderpath, "interactionMatrices.pdf"))
for(i in 1:7){
  interaction_plot(folderpath, scenarios[i])
}
dev.off()


# Convergence -------------------------------------------------------------

conv_folderpath <- paste0(folderpath, "convergence/")

if(dir.exists(conv_folderpath) == FALSE){
  dir.create(conv_folderpath)
}


# Probable get a pdf for each model, but a gelman plot for each scenario

for(i in 1:7){
  listChains <- get_mcmc_lists(folderpath = folderpath,
                               scenario = scenarios[i])
  
  assign(x = paste0(scenarios[i], "_chains"),
         value = listChains)
  
  gelman.diag(listChains)
  pdf(paste0(conv_folderpath, scenarios[i], "gelman_diag.pdf"))
  gelman.plot(listChains)
  dev.off()



  pdf(paste0(conv_folderpath, scenarios[i], "chain_plots.pdf"))
  plot(listChains)
  dev.off()

  raftery_on_one <- raftery.diag(listChains[[1]], s=0.9)
  write.csv(raftery_on_one$resmatrix, file = paste0(conv_folderpath, scenarios[i], "raftery_diag.csv"))
  
}

# Get Tiff files for the manuscript ------------------------------------------------------
# I'm still working on these (April 27, 2020)
source("manuscript_functions/Fig2_Fig3_TIFFs.R")

