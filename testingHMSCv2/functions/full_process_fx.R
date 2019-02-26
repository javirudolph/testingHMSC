# These functions should be carrying out the process of simulating a metacommunity, fitting HMSC and giving the ouput of the variation partitioning components. 

# For these to work, needs to source the other subfunctions:

# source("functions/metacom_sim_fx.R")
# source("functions/main_sim_fx.R")



# Metacommunity Simulations -----------------------------------------------


# Simulate the metacommunity results to input in the HMSC function

metacom_sim4HMSC <- function(XY, E, pars, nsteps,
                             occupancy, niter,
                             makeRDS = FALSE,
                             whereToSave = NULL){
  
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
    saveRDS(res, file = whereToSave)
  }
  
  return(res)

}






