# If you just want to be up to date on the files that are not on Github:
# This is the .RDS files and the figures
# You can run this script to source and run all the others. 
# Warning: it will take a while, so maybe go for a walk and get coffee

# Make sure Guillaume's HMSC is installed
# https://github.com/guiblanchet/HMSC

set.seed(29)

dir.create("outputs")
dir.create("figures")

# Run Simulations------------------------------------------------------------------------
# The following lines will run the metacommunity simulations with the specified parameters
scenario_runs <- list.files(pattern = "scenario")
for(i in 1:length(scenario_runs)){
  print(paste("Start", scenario_runs[i]))
  source(scenario_runs[i])
  print(paste(scenario_runs[i], "completed"))
}

# Print parameters -----------------------------------------------------------------------
# This script is to generate tables with the real parameters used in the simulations
# It will save the parameter tables as figures in the figures/ directory
source("simParamsTable.R")

# HMSC and variation partitioning --------------------------------------------------------
# These scripts take a while
source("VP_Fig2.R")
source("VP_Fig3.R")

# Ternay plots ---------------------------------------------------------------------------
# This are the figures from this simulation and with the parameters saved earlier
source("Draw figures.R")








      