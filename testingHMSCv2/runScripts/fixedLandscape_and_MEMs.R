
# Since we are using the same landscape for all, we should probably just set a seed here and use the output for all simulations

library(adespatial)

# Source Functions --------------------------------------------------------

# source the functions to generate a landscape
source("functions/landscape_fx.R")



# Create landscape --------------------------------------------------------

set.seed(277)

XY <- get_xy(N = 1000)
E <- get_E(D = 1, N = 1000)

#saveRDS(XY, file = "outputs/XY_fixed.RDS")
#saveRDS(E, file = "outputs/E_fixed.RDS")


# Create MEMs -------------------------------------------------------------

ncluster <- 4
MEM <- dbmem(XY, MEM.autocor = "positive", thresh = 0.5)

if(TRUE){
  ### Set clusters
  clusters <- makeCluster(ncluster)
  registerDoParallel(clusters)
  
  ### test MEM autocorrelation
  testMEM <- foreach(i=1:ncol(MEM)) %dopar% {
    adespatial:::moran.randtest(MEM[,i], attributes(MEM)$listw, nrepet = 9999)
  }
  
  ### Stop clusters
  stopCluster(clusters)
}

pvals <- lapply(testMEM, function(x) x$pvalue)
# min(which(pvals>0.05))

lastSignificant <- min(which(pvals>0.05))
  
# Select the MEMs based on their significance and then save that result
MEMsel <- MEM[, 1:lastSignificant]

#saveRDS(MEMsel, file = "outputs/MEMsel.RDS")


