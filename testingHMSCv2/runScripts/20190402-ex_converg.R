# I want to create a sample script for one community scenario in which we go all through the process of
# generating the community, fitting the model, checking for convergence and visualizing the variation partitioning

# Libraries ---------------------------------------------------------------
# Load the libraries
library(HMSC)
library(doParallel)
library(tidyverse)
library(coda)


# Functions ---------------------------------------------------------------
# Load the functions

# This script has a function to organize the parameters and get them in the format wanted for the simulations
source("functions/prep_pars_fx.R")
source("functions/metacom_sim_fx.R")
source("functions/main_sim_fx.R")
source("functions/full_process_fx.R")

# OrigLandscape -----------------------------------------------------------

XY <- readRDS("outputs/fixedLandscapes/orig-no-seed-XY.RDS")
E <- readRDS("outputs/fixedLandscapes/orig-no-seed-E.RDS")

# These include the 78 vectors originally included by Guillaume
MEMsel <- readRDS("outputs/fixedLandscapes/orig-no-seed-MEMsel.RDS") 


# Simple run --------------------------------------------------------------

# We will create one set of parameters for one simulated scenario and then fit HMSC one time to that.
# After that, we will check for convergence
# We will repeat the HMSC fit and get another model so that we can compare both

# Create parameters list
pars <- prep_pars(N = 1000, D = 1, R = 12, 
                  nicheOpt = NULL,
                  breadth = 0.8, alpha = 0.005, interx_col = 1.5, interx_ext = 1.5)

# Generate a community with these parameters, only creating one iteration
# this is basically a matrix of species occupancy at each of the sites
commSim <- metacom_sim4HMSC(XY = XY, E = E, pars = pars,
                                    nsteps = 200, occupancy = 0.8, niter = 1)


# HMSC part
# First format the data 
formData <- as.HMSCdata(Y = commSim[[1]], # This is the occupancy data
                        X = cbind(scale(E), scale(E)^2, MEMsel), # The explanatory variables which are the env and space by MEM
                        Random = as.factor(1:1000), # Site-level random effect
                        scaleX = TRUE, interceptX = TRUE)
# With formatted data, now fit the model
model <- hmsc(formData, family = "probit",
              niter = 100000, nburn = 1000, thin = 10)
model <- readRDS("outputs/testOutputs/conv_modl.RDS")
#saveRDS(model, file = "outputs/testOutputs/conv_modl.RDS")

# Chage the structure of the fitted model to be able to use functions from 'coda'
chain <- as.mcmc(model, parameters = "meansParamX", burning = TRUE)
# Get the summary and trace/density plots for the estimated parameters
summary(chain)
pdf("outputs/testOutputs/plotsChain1.pdf")
plot(chain)
dev.off()

raftery.diag(chain) # I find it strange that the burn in suggested is 1-3... 
# Also the lower bound min is suggested to be 3746 indep samples

# Look at the autocorrelation between these variables
pdf("outputs/testOutputs/autocorrPlots.pdf")
autocorr.plot(chain) # It shows no autocorrelation... which I find weird... 
dev.off()


pdf("outputs/testOutputs/quantileschain.pdf")
cumuplot(chain) # Look at the quantiles of the chain over time
dev.off()

# Repeat the process for a second chain so we can compare them
mod2 <- hmsc(formData, family = "probit",
              niter = 100000, nburn = 1000, thin = 10)
#mod2 <- readRDS("outputs/testOutputs/conv_mod2.RDS")
#saveRDS(mod2, file = "outputs/testOutputs/conv_mod2.RDS")
chain2 <- as.mcmc(mod2, parameters = "meansParamX")

combinedChains <- mcmc.list(chain, chain2)

pdf("outputs/testOutputs/combinedChains.pdf")
plot(combinedChains)
dev.off()

# Gelman diagnostic would tell us how different these two chains are... We want them to be similar since they are from the same data
# Values close to 1 mean that between and within chain variance are equal. Larger values indicate notabe differences between the chains

gelman.diag(combinedChains)

pdf("outputs/testOutputs/gelmanplot.pdf")
gelman.plot(combinedChains) # This would show the development of the -scale-reduction over chain steps.
dev.off()








