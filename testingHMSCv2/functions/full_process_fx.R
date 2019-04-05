# These functions should be carrying out the process of simulating
# a metacommunity, fitting HMSC and giving the ouput of the variation
# partitioning components.

# For these to work, needs to source the other subfunctions:

# source("functions/metacom_sim_fx.R")
# source("functions/main_sim_fx.R")



# Metacommunity Simulations -----------------------------------------------


# Simulate the metacommunity results to input in the HMSC function

metacom_sim4HMSC <- function(XY, E, pars, nsteps,
                             occupancy, niter,
                             makeRDS = FALSE,
                             whereToSave = NULL,
                             objName = NULL){

  N <- pars$N
  D <- pars$D
  R <- pars$R

  Y0 <- ifelse(matrix(runif(N * D), nrow = N, ncol = R) < occupancy, 1, 0)

  res <- vector("list", length = niter)
  for(i in 1:niter){
    run <- mainfx(XY, E, pars, Y0, nsteps)
    res[[i]] <- run[[nsteps]]
  }

  if(makeRDS == TRUE){
    nameFile <- paste0(whereToSave, objName, "-metacomSim.RDS")
    saveRDS(res, file = nameFile)
  }

  return(res)

}


# This function isn't fully tested, but it is for the case of having multiple parameters in one run
# Example is in Figure 3b, where 1/3 of the species have different dispersal parameters.

metacom_sim4HMSC_multParams <- function(XY, E, pars, nsteps,
                                        occupancy, niter,
                                        makeRDS = FALSE,
                                        whereToSave = NULL,
                                        objName = NULL){
  
    
    res <- vector("list", length = niter)
    for(i in 1:niter){
      bindruns <- NULL
      for(k in 1:length(pars)){
        subpars <- pars[[k]]
        
        N <- subpars$N
        D <- subpars$D
        R <- subpars$R
        
        Y0 <- ifelse(matrix(runif(N * D), nrow = N, ncol = R) < occupancy, 1, 0)
        run <- mainfx(XY, E, subpars, Y0, nsteps)
        lastrun <- run[[nsteps]]
        bindruns <- cbind(bindruns, lastrun)
      }
      res[[i]] <- bindruns
    }
  
  if(makeRDS == TRUE){
    nameFile <- paste0(whereToSave, objName, "-metacomSim.RDS")
    saveRDS(res, file = nameFile)
  }
  
  return(res)
  
}


# Fit HMSC ----------------------------------------------------------------

metacom_as_HMSCdata <- function(metacomData, numClusters, E, MEMsel,
                                makeRDS = FALSE,
                                whereToSave = NULL,
                                objName = NULL){
  
  N <- nrow(E)
  
  run <- metacomData
  nrun <- length(run)
  
  clusters <- makeCluster(numClusters)
  registerDoParallel(clusters)
  
  ### Estimate models
  model <- foreach(j = 1:nrun) %dopar% {
    library(HMSC)
    formData <- as.HMSCdata(Y = run[[j]], X = cbind(scale(E),scale(E)^2, MEMsel),
                            Random = as.factor(1:N),
                            scaleX = TRUE, interceptX = TRUE)
    
    hmsc(formData, family = "probit",
         niter = 100000, nburn = 5000, thin = 10)
  }
  
  ### Stop clusters
  stopCluster(clusters)
  
  if(makeRDS == TRUE){
    nameFile <- paste0(whereToSave, objName, "-model.RDS")
    saveRDS(model, file = nameFile)
  }
  
  return(model)
}



# Variation Partitioning --------------------------------------------------

get_VPresults <- function(HMSCmodel, MEMsel, numClusters,
                   makeRDS = FALSE,
                   whereToSave = NULL,
                   objName = NULL){

  model <- HMSCmodel
  nmodel <- length(model)

  clusters <- makeCluster(numClusters)
  registerDoParallel(clusters)
  
  ### Estimate models
  vpRes <- foreach(j = 1:nmodel) %dopar% {
    library(HMSC)
    variPart(model[[j]], groupX = c(rep("env",3),rep("spa",length(MEMsel))),
             type = "III", R2adjust = TRUE)
  }
  
  ### Stop clusters
  stopCluster(clusters)
  
  if(makeRDS == TRUE){
    nameFile <- paste0(whereToSave, objName, "-vpspp.RDS")
    saveRDS(vpRes, file = nameFile)
  }
  
  return(vpRes)

}

# This function shouldn't be necessary anymore
get_VPresults_SITE <- function(HMSCmodel, MEMsel, numClusters,
                               makeRDS = FALSE,
                               whereToSave = NULL,
                               objName = NULL){

  model <- HMSCmodel
  nmodel <- length(model)

  clusters <- makeCluster(numClusters)
  registerDoParallel(clusters)

  ### Estimate models
  vpRes <- foreach(j = 1:nmodel) %dopar% {
    library(HMSC)
    variPart(model[[j]], groupX = c(rep("env",3),rep("spa",length(MEMsel))),
             indSite = TRUE,
             type = "III", R2adjust = TRUE)
  }

  ### Stop clusters
  stopCluster(clusters)

  if(makeRDS == TRUE){
    nameFile <- paste0(whereToSave, objName, "-vpsites.RDS")
    saveRDS(vpRes, file = nameFile)
  }

  return(vpRes)
}


# Organize VP data --------------------------------------------------------

