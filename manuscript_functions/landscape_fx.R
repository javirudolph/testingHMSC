# These functions don't seem to be necessary more than once.
# Since the simulations would ideally keep the landscape the same for every metacommunity scenario, once these are run the files should be used for all other simulations. 

# Landscape ---------------------------------------------------------------
#' @title Random XY patches
#' @description generates random set of coordinates for N patches
#' @param N number of patches

get_xy <- function(N = "Number of patches"){
  cbind(runif(N, min = 0, max = 1), runif(N, min = 0, max = 1))
}

#' @title Aggregated XY patches
#' @description Generates random set of coordinates for aggregated patches
#' @param N number of patches
#' @param Nclusters number of clusters of patches
#' @param sd_xy separation between patches inside clusters
#'
get_xy_agg <- function(N = "Number of patches", Nclusters = "patch clusters", sd_xy = "separation between patches inside clusters"){
  xclust = runif(Nclusters)
  yclust = runif(Nclusters)
  
  X = rnorm(N, rep(xclust,N/Nclusters), sd_xy)
  Y = rnorm(N, rep(yclust,N/Nclusters), sd_xy)
  
  cbind(X, Y)
  
}

#' @title Environmental variables
#' @description Generate random environmental variables
#' @param D Number of environmental packages
#' @param N Number of patches
#'
# Environmental variables -------------------------------------------------

get_E <- function(D = "Number of Environmental variables", N = "Number of patches"){
  matrix(runif((D*N)), nrow = N, ncol = D)
}
