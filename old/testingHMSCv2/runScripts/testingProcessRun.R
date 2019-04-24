
# Testing the whole process from the functions developed in the 'full_process_fx.R'

# Libraries used:

library(HMSC)
library(doParallel)
library(adespatial)
library(tidyverse)
library(ggtern)


# Source functions --------------------------------------------------------

# Function to organize parameters in a list
source("functions/prep_pars_fx.R")

# Individual functions to simulate the metacommunity
source("functions/metacom_sim_fx.R")

# The main function that actually outputs the metacommunity
source("functions/main_sim_fx.R")

# You could generate a landscape with the functions provided in:
source("functions/landscape_fx.R")
# We are doing this, since this is just a test run, with a smaller number of sites and species richness

# Or use the fixed landscape and MEMs from - beware this one takes a while
# source("runScripts/fixedLandscape_and_MEMs.R")
# Or, if available locally, just load the RDS files saved from that script in the outputs folder
# These RDS files should include the XY, E and MEMsel

# These are the functions that run the whole process
source("functions/full_process_fx.R")

# These functions organize the VP data and get it ready for plotting or further analysis



# Test cycle of all functions ---------------------------------------------

###########################################################################
# 1. Environment
N <- 300
D <- 1
R <- 7

testXY <- get_xy(N)
testE <- get_E(D, N)

# MEMs section is for later in the fitting, but should probably just do it now... 

ncluster <- 4
testMEM <- dbmem(testXY, MEM.autocor = "positive", thresh = 0.5)

if(FALSE){
  ### Set clusters
  clusters <- makeCluster(ncluster)
  registerDoParallel(clusters)
  
  ### test MEM autocorrelation
  testedMEM <- foreach(i=1:ncol(testMEM)) %dopar% {
    adespatial:::moran.randtest(testMEM[,i], attributes(testMEM)$listw, nrepet = 9999)
  }
  
  ### Stop clusters
  stopCluster(clusters)
}

pvals <- lapply(testedMEM, function(x) x$pvalue)
# min(which(pvals>0.05))

lastSignificant <- (min(which(pvals>0.05)) - 1)

# Select the MEMs based on their significance and then save that result
testMEMsel <- testMEM[, 1:lastSignificant]


############################################################################
# 2. We create the parameters

testParams <- prep_pars(N = N, D = D, R = R, breadth = 0.2,
                        alpha = 0.005, interx_col = 0, interx_ext = 0,
                        makeRDS = TRUE,
                        whereToSave = "outputs/testOutputs/", objName = "testParams")


###########################################################################
# 3. Metacommunity simulation with iterations
testMetacom <- metacom_sim4HMSC(XY = testXY, E = testE, pars = testParams,
                                nsteps = 20, occupancy = 0.8, niter = 3,
                                makeRDS = TRUE, whereToSave = "outputs/testOutputs/", objName = "testMetacom")


##########################################################################
# 4. Fit/Format HMSC to our simulated data. Remember you need the MEMsel for this

testHMSCmodel <- metacom_as_HMSCdata(metacomData = testMetacom, numClusters = 4, E = testE,
                                     MEMsel = testMEMsel, makeRDS = TRUE,
                                     whereToSave = "outputs/testOutputs/",
                                     objName = "testHMSCmodel")

##########################################################################
# 5. Get variation partitioning components
testVPresults <- get_VPresults(HMSCmodel = testHMSCmodel, MEMsel = testMEMsel, numClusters = 4,
                               makeRDS = TRUE,
                               whereToSave = "outputs/testOutputs/",
                               objName = "testVPresults")

testVPresultsSITE <- get_VPresults_SITE(HMSCmodel = testHMSCmodel, MEMsel = testMEMsel, numClusters = 4,
                                        makeRDS = TRUE,
                                        whereToSave = "outputs/testOutputs/",
                                        objName = "testVPresults_SITE")

#########################################################################
# 6. Organize the VP data object
testOrganizedVP <- organize_VPdata(VPdata = testVPresults)

#########################################################################
# 7. Join with parameters
testPlotData <- testOrganizedVP %>% left_join(., organize_params_plot(testParams))

#########################################################################
# 8. Plot the data
make_tern_plot(testPlotData, varShape = "iteration", varColor = "nicheOptima") +
  scale_color_viridis_c()
dateToday <- Sys.Date()
ggsave(filename = paste0("outputs/testOutputs/", dateToday, "-TestFig.png"))





