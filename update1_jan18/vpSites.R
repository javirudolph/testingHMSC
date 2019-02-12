# Variation Partitioning at the site level

#=============
### R packages
#=============
library(HMSC) # Package version 2.1.2 available at : https://github.com/guiblanchet/HMSC
library(doParallel)
library(adespatial)


# VP Fig2 -----------------------------------------------------------------


#===================================================
### Perform variation partitioning using adjusted R2
#===================================================
for(i in 1:4){
  model <- readRDS(paste("HMSC model scenario Fig2", letters[i], ".RDS",sep = ""))
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
    variPart(model[[j]], groupX = c(rep("env",3),rep("spa",78)), indSite = TRUE, 
             #family = "probit", 
             type = "III", R2adjust = TRUE)
  }
  
  ### Stop clusters
  stopCluster(clusters)
  
  ### Save results
  saveRDS(vpRes, file = paste("VPsiteFig2", letters[i], ".RDS",sep = ""))
  print(i)
}



# VP Fig3 -----------------------------------------------------------------

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
    variPart(model[[j]], groupX = c(rep("env",3),rep("spa",78)), indSite = TRUE,
             #family = "probit", 
             type = "III", R2adjust = TRUE)
  }
  
  ### Stop clusters
  stopCluster(clusters)
  
  ### Save results
  saveRDS(vpRes, file = paste("VPsiteFig3", letters[i], ".RDS",sep = ""))
  print(i)
}

