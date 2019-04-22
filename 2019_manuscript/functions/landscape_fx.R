# These functions don't seem to be necessary more than once.
# Since the simulations would ideally keep the landscape the same for every metacommunity scenario, once these are run the files should be used for all other simulations. 

# Landscape ---------------------------------------------------------------

get_xy <- function(N = "Number of patches"){
  cbind(runif(N, min = 0, max = 1), runif(N, min = 0, max = 1))
}

get_xy_agg <- function(N = "Number of patches", Nclusters = "patch clusters", sd_xy = "separation between patches inside clusters"){
  xclust = runif(Nclusters)
  yclust = runif(Nclusters)
  
  X = rnorm(N, rep(xclust,N/Nclusters), sd_xy)
  Y = rnorm(N, rep(yclust,N/Nclusters), sd_xy)
  
  cbind(X, Y)
  
}


# Environmental variables -------------------------------------------------

get_E <- function(D = "Number of Environmental variables", N = "Number of patches"){
  matrix(runif((D*N)), nrow = N, ncol = D)
}
