# Just to save time


# Run simulations ---------------------------------------------------------

simFiles <- list.files(pattern = c("scenario", "Fig"))
simFiles

for(i in 1:length(simFiles)){
  source(simFiles[i])
}


# Run HMSC and VP ---------------------------------------------------------

source("VP_Fig2.R")

source("VP_Fig3.R")

# Make Figures -----------------------------------------------------------

source("Draw_for_md.R")


# VP for individual sites -------------------------------------------------

source("vpSites.R")
