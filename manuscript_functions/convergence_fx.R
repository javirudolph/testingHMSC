# Using as.mcmc function from HMSC isn't working to include the burn section of the results

#' @title Get MCMC object
#' @description Using as.mcmc function from HMSC isn't working to include the burn section of the results
#' @param folderpath location of the file
#' @param scenario file name
#' @param modelNum select from the HMSC model object
#'

get_mcmc_obj <- function(folderpath = NULL,
                         scenario = NULL,
                         modelNum = NULL){
  
  
  
  manymodels <- readRDS(paste0(folderpath, scenario, "-model.RDS"))
  parameters <- "meansParamX"
  
  if(is.null(modelNum) ==  TRUE){
    x <- manymodels[[1]]
  } else{
    x <- manymodels[[modelNum]]
  }

  paramMCMCMat <- x$results$estimation[[parameters]]
  
  ### Name rows and columns of matrix
  rownames(paramMCMCMat) <- rownames(x$results$estimation[[parameters]])
  colnames(paramMCMCMat) <- colnames(x$results$estimation[[parameters]])
  
  paramBurnMCMC <- x$results$burning[[parameters]]
  rownames(paramBurnMCMC) <- rownames(x$results$burning[[parameters]])
  colnames(paramBurnMCMC) <- colnames(x$results$burning[[parameters]])
  paramMCMCMat <- rbind(paramBurnMCMC,paramMCMCMat)
  
  thinSet <- as.numeric(sub("iter", "", rownames(paramMCMCMat)[1:2]))
  modelThin <- thinSet[2] - thinSet[1]
  
  result <- mcmc(data = paramMCMCMat, thin = modelThin)
}





# Gelman ------------------------------------------------------------------


# Get them all in a list for gelman.diag
#' @title Gelman Diagnostics for all chains
#' @description Get all the mcmc objects in a list for gelman.diag
#' @param folderpath location of files
#' @param scenario specific file name


get_mcmc_lists <- function(folderpath = NULL,
                           scenario = NULL){
  
  folderpath <- folderpath
  scenario <- scenario
  
  chain1 <- get_mcmc_obj(folderpath = folderpath,
                         scenario = scenario,
                         modelNum = 1)
  
  chain2 <- get_mcmc_obj(folderpath = folderpath,
                         scenario = scenario,
                         modelNum = 2)
  
  chain3 <- get_mcmc_obj(folderpath = folderpath,
                         scenario = scenario,
                         modelNum = 3)
  
  chain4 <- get_mcmc_obj(folderpath = folderpath,
                         scenario = scenario,
                         modelNum = 4)
  
  chain5 <- get_mcmc_obj(folderpath = folderpath,
                         scenario = scenario,
                         modelNum = 5)
  
  listChains <- mcmc.list(chain1, chain2, chain3, chain4, chain5)
  return(listChains)
  
}



