
# Since we are using the same landscape for all, we should probably just set a seed here and use the output for all simulations

library(adespatial)

library(doParallel)

# Source Functions --------------------------------------------------------

# source the functions to generate a landscape
source("functions/landscape_fx.R")



# Create landscape --------------------------------------------------------

seednum <- 98
set.seed(seednum)

Nsites <- 1000

XY <- get_xy(N = Nsites)
E <- get_E(D = 1, N = Nsites)

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


# SAVING ------------------------------------------------------------------
date <- format(Sys.Date(), "%Y%m%d")
descriptor <- paste0("-", Nsites, "sites-", seednum, "seed")

saveRDS(XY, file = paste0("outputs/fixedLandscapes/", date, descriptor, "-XY.RDS"))
saveRDS(E, file = paste0("outputs/fixedLandscapes/", date, descriptor, "-E.RDS"))
saveRDS(MEMsel, file = paste0("outputs/fixedLandscapes/", date, descriptor, "-MEMsel.RDS"))


