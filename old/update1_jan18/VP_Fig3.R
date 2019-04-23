#=============
### R packages
#=============
library(HMSC) # Package version 2.1.2 available at : https://github.com/guiblanchet/HMSC
library(doParallel)
library(adespatial)

#=========================
### Set parallel computing
#=========================
ncluster <- 4 #changed from 24
N <- 1000

#============
### Load data
#============
E <- as.matrix(read.table("E.txt")[1:N,])
xy <- as.matrix(read.table("XY.txt")[1:N,])

#============
### Build MEM
#============
MEM <- dbmem(xy, MEM.autocor = "positive", thresh = 0.5)

#------------
### Test MEMs
#------------
# if(FALSE){
#   ### Set clusters
#   clusters <- makeCluster(ncluster)
#   registerDoParallel(clusters)
# 
#   ### test MEM autocorrelation
#   testMEM <- foreach(i=1:ncol(MEM)) %dopar% {
#     adespatial:::moran.randtest(MEM[,i], attributes(MEM)$listw,nrepet = 9999)
#   }
# 
#   ### Stop clusters
#   stopCluster(clusters)
# }

MEMsel <- MEM[,1:78] # The first 78 MEMs were selected because it is at this point that the Moran's I started to get slowly less significant

for(i in 1:3){
  #============
  ### Load data
  #============
  run <- readRDS(paste("Fig3",letters[i],"_run.RDS",sep=""))
  nrun  <- length(run)

  #================================
  ### Estimate a model for each run
  #================================
  ### Set clusters
  clusters <- makeCluster(nrun)
  registerDoParallel(clusters)

  ### Estimate models
  model <- foreach(j = 1:nrun) %dopar% {
    library(HMSC)
    formData <- as.HMSCdata(Y = run[[j]], X = cbind(scale(E),scale(E)^2, MEMsel),
                            Random = as.factor(1:1000),
                            scaleX = TRUE, interceptX = TRUE)

    hmsc(formData, family = "probit", niter = 10000, nburn = 5000, thin = 5)
  }

  ### Stop clusters
  stopCluster(clusters)

  ### Save results
  saveRDS(model, file = paste("HMSC model scenario Fig3", letters[i], ".RDS",sep = ""))
}

#===================================================
### Perform variation partitioning using adjusted R2
#===================================================
for(i in 1:3){
  model <- readRDS(paste("HMSC model scenario Fig3", letters[i], ".RDS",sep = ""))
  nmodel <- length(model)

  #================================
  ### Estimate a model for each run
  #================================
  ### Set clusters
  clusters <- makeCluster(nmodel)
  registerDoParallel(clusters)

  ### Estimate models
  vpRes <- foreach(j = 1:nmodel) %dopar% {
    library(HMSC)
    variPart(model[[j]], groupX = c(rep("env",3),rep("spa",78)), 
             #family = "probit", 
             type = "III", R2adjust = TRUE)
  }

  ### Stop clusters
  stopCluster(clusters)

  ### Save results
  saveRDS(vpRes, file = paste("VP Fig3", letters[i], ".RDS",sep = ""))
  print(i)
}
