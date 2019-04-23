outsfolderpath <- "outputs/2019-04-23_orig_dispersal_10kchains/"

source("functions/output_processing_fx.R")

library(tidyverse)
library(ggtern)
library(corrplot)


source("functions/convergence_fx.R")
library(HMSC)
library(coda)

# Get figures
source("run-scripts/output_figures.R")

# Check convergence
source("run-scripts/check_convergence.R")

