#=============
### R packages
#=============
library(HMSC) # Package version 2.1.2 available at : https://github.com/guiblanchet/HMSC
library(doParallel)
library(adespatial)

#=========================
### Set parallel computing
#=========================
#ncluster <- 24
# To run in Javi's computer this needs to be set to 4
# Leibold lab computer set to 8
ncluster <- 8

# Assuming N <- 1000 based on the community simulations
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
## JAVI: I have commented this section out, since testing for significance of the MEMs takes a lot of time
## Guillaume had previously tested this and determined which MEMs to select
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

for(i in 1:4){
  #============
  ### Load data
  #============
  print(paste("Start model for Fig2", letters[i]))
  run <- readRDS(paste("outputs/Fig2",letters[i],"_run.RDS",sep=""))
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

    hmsc(formData, family = "probit",
         niter = 10000, nburn = 5000, thin = 5)
  }

  ### Stop clusters
  stopCluster(clusters)

  ### Save results
  saveRDS(model, file = paste("outputs/HMSC model scenario Fig2", letters[i], ".RDS",sep = ""))
  print(paste("Fig2", letters[i], "done"))
}



#===================================================
### Perform variation partitioning using adjusted R2
#===================================================
for(i in 1:4){
  print(paste("Start VP for Fig2", letters[i]))
  model <- readRDS(paste("outputs/HMSC model scenario Fig2", letters[i], ".RDS",sep = ""))
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
             #family = "probit", # This line is commented out because an update in the package doesn't require specifying the family, since it gets it from the hmsc model
             type = "III", R2adjust = TRUE)
  }

  ### Stop clusters
  stopCluster(clusters)

  ### Save results
  saveRDS(vpRes, file = paste("outputs/VP Fig2", letters[i], ".RDS",sep = ""))
  print(paste("Fig2", letters[i], "VP done"))
}
