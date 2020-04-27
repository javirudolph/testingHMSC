# These functions should be carrying out the process of simulating
# a metacommunity, fitting HMSC and giving the ouput of the variation
# partitioning components.

# For these to work, needs to source the other subfunctions:

# source("functions/metacom_sim_fx.R")
# source("functions/main_sim_fx.R")



# Metacommunity Simulations -----------------------------------------------


# Simulate the metacommunity results to input in the HMSC function

# This function works for figure two in which all the species basically share their parameters

#' @title Metacommunity Simulation one set of parameters
#' @description This function will use the set of given parameters and run the main metacommunity simulation for a set amount of time steps. The output is a list of matrices, where each one of the matrices is an iteration of the metacommunity with the same parameters. It gives the option to save RDS files to the specified directory so these RDS files can be accessed later by other HMSC functions.
#' @param XY coordinates for each of the sites or patches
#' @param E matrix of environmental variables measured at each site
#' @param pars list of parameters given by the output of the prep_pars() function
#' @param nsteps numeric, number of time steps before getting the "snapshot" of a metacommunity
#' @param occupancy numeric value between 0-1, to set as the initial conditions occupancy of sites by the different species.
#' @param niter number of iterations for simulating the metacommunity with the same parameters.
#' @param envResp type of response to the environment: "gaussian" or "quadratic"
#' @param makeRDS should the function make an RDS file from the output and save it? Default is FALSE
#' @param whereToSave file path for the RDS file to be saved in
#' @param objName should the RDS file be saved, what should it be called?

metacom_sim4HMSC <- function(XY, E, pars, nsteps,
                             occupancy, niter,
                             envResp = "quadratic",
                             makeRDS = FALSE,
                             whereToSave = NULL,
                             objName = NULL){

  N <- pars$N
  D <- pars$D
  R <- pars$R

  Y0 <- ifelse(matrix(runif(N * D), nrow = N, ncol = R) < occupancy, 1, 0)

  res <- vector("list", length = niter)
  for(i in 1:niter){
    run <- mainfx(XY, E, pars, Y0, nsteps, envResp = envResp)
    res[[i]] <- run[[nsteps]]
  }

  if(makeRDS == TRUE){
    nameFile <- paste0(whereToSave, objName, "-metacomSim.RDS")
    saveRDS(res, file = nameFile)
  }

  return(res)

}


# This function is for figure 3, in which we have separate groups of species with different dispersal or interaction parameters.

#' @title Metacommunity Simulation for multiple sets of parameters
#' @description This function will use the set of given parameters and run the main metacommunity simulation for a set amount of time steps. The output is a list of matrices, where each one of the matrices is an iteration of the metacommunity with the same parameters. It gives the option to save RDS files to the specified directory so these RDS files can be accessed later by other HMSC functions. It is different from the simulation with one set of parameters in that you can assign different dispersal and competition parameters to the species within your simulation.
#' @param XY coordinates for each of the sites or patches
#' @param E matrix of environmental variables measured at each site
#' @param pars in this case, the parameters are a list of prep_pars() outputs or the output of prep_multiparam()
#' @param nsteps numeric, number of time steps before getting the "snapshot" of a metacommunity
#' @param occupancy numeric value between 0-1, to set as the initial conditions occupancy of sites by the different species.
#' @param niter number of iterations for simulating the metacommunity with the same parameters.
#' @param envResp type of response to the environment: "gaussian" or "quadratic"
#' @param makeRDS should the function make an RDS file from the output and save it? Default is FALSE
#' @param whereToSave file path for the RDS file to be saved in
#' @param objName should the RDS file be saved, what should it be called?
#'
#'

metacom_sim4HMSC_multParams <- function(XY, E, pars, nsteps,
                                        occupancy, niter,
                                        envResp = "quadratic",
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
        run <- mainfx(XY, E, subpars, Y0, nsteps, envResp = envResp)
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

#' @title Format metacommunity data to "HMSC" classes
#' @description This function will use the simulated metacommunity matrices and format the data into "HMSC" classes
#' @param metacomData list of simulated metacommunity matrices. Output from metacom_sim4HMSC() or metacom_sim4HMSC_multiparam()
#' @param numClusters number of clusters based on number of cores available to work with DoParallel
#' @param E matrix of environmental variables associated to each site
#' @param MEMsel distance-based Moran' Eigenvector Map's eigenvector maps from a geographic distance matrix (xy matrix). This comes from adespatial::dbmem()
#' @param hmscPars list of parameters to be included for iterations, burn and thin components of the as.HMSCdata()
#' @param makeRDS should the function make an RDS file from the output and save it? Default is FALSE
#' @param whereToSave file path for the RDS file to be saved in
#' @param objName should the RDS file be saved, what should it be called?
#'
#' @importFrom doParallel registerDoParallel
#'

metacom_as_HMSCdata <- function(metacomData, numClusters, E, MEMsel,
                                hmscPars = NULL,
                                makeRDS = FALSE,
                                whereToSave = NULL,
                                objName = NULL){
  
  N <- nrow(E)
  
  run <- metacomData
  nrun <- length(run)
  
  clusters <- makeCluster(numClusters)
  registerDoParallel(clusters)
  
  if(is.null(hmscPars) == TRUE){
    hmscniter <- 100000
    hmscnburn <- 10000
    hmscthin <- 20
  } else{
    hmscniter <- hmscPars$niter
    hmscnburn <- hmscPars$nburn
    hmscthin <- hmscPars$thin
  }
  
  ### Estimate models
  model <- foreach(j = 1:nrun) %dopar% {
    library(HMSC)
    formData <- as.HMSCdata(Y = run[[j]], X = cbind(scale(E),scale(E)^2, MEMsel),
                            Random = as.factor(1:N),
                            scaleX = TRUE, interceptX = TRUE)
    
    hmsc(formData, family = "probit",
         niter = hmscniter, nburn = hmscnburn, thin = hmscthin)
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
#' @title Variation partinioning: species
#' @description use of the VariPart function from HMSC, gets results at the level of species
#' @param HMSCmodel output from the metacom_as_HMSCdata() or an object from as.HMSCdata()
#' @param MEMsel distance-based Moran' Eigenvector Map's eigenvector maps from a geographic distance matrix (xy matrix). This comes from adespatial::dbmem()
#' @param numClusters number of clusters based on number of cores available to work with DoParallel
#' @param makeRDS should the function make an RDS file from the output and save it? Default is FALSE
#' @param whereToSave file path for the RDS file to be saved in
#' @param objName should the RDS file be saved, what should it be called?
#'

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

# This function is necessary when running infor on sites.

#' @title Variation partinioning: sites
#' @description use of the VariPart function from HMSC, gets results at the level of species
#' @param HMSCmodel output from the metacom_as_HMSCdata() or an object from as.HMSCdata()
#' @param MEMsel distance-based Moran' Eigenvector Map's eigenvector maps from a geographic distance matrix (xy matrix). This comes from adespatial::dbmem()
#' @param numClusters number of clusters based on number of cores available to work with DoParallel
#' @param makeRDS should the function make an RDS file from the output and save it? Default is FALSE
#' @param whereToSave file path for the RDS file to be saved in
#' @param objName should the RDS file be saved, what should it be called?
#' @details I think this function can probably just be incorporated into the VariPart for species if we include and if statement for sites = TRUE
#'
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


