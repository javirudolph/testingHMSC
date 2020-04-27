
# These are the functions needed for the processes regarding the metacommunity simulations:
# Each individual function is explained in the Supplementary information
# 

# Each of these functions describes the processes going on for the metacommunity. We use these functions later on in the main_sim_fx.R script to generate an occupancy matrix for species in patches

###################################################################################################
# Colonization
###################################################################################################


# Immigration -------------------------------------------------------------

#' @title Metacommunity process: immigration
#' @description This function will calculate the propagule pressure. The effects of immigration are given as a weighted average of the occurrence probability of species i in neighborhood of z.
#' @param Y matrix of species occurrence
#' @param K connectivity matrix
#' @param m independent constant to account for immigration from outside the simulated metacommunity
#' @keywords immigration metacommunity
#'

I_f <- function(Y = "Species occurrence matrix", 
                K = "Patch connectivity matrix", 
                m = "outside immigration"){
  N <- nrow(Y)
  R <- ncol(Y)
  I <- (1-m) * (K %*% Y) / (K %*% matrix(1, nrow = N, ncol = R)) + m
  return(I)
}


# Patch Connectivity ------------------------------------------------------

get_K <- function(XY, alpha){
  N <- nrow(XY)
  distMat <- as.matrix(dist(XY, method = "euclidean", upper = T, diag = T))
  ConMat <- exp((-1/alpha)*distMat)
  diag(ConMat) <- 0
  return(ConMat)
}


# Environment -------------------------------------------------------------
# The effect of the environment on the local performance and colonization

S_f_gaussian <- function(E, u_c, s_c) {
  R <- ncol(u_c)
  N <- nrow(E)
  D <- ncol(E)
  S <- matrix(1, nr = N, nc = R)
  for(i in 1:D){
    optima <- matrix(u_c[i,],nrow = N,ncol = R,byrow = TRUE)
    breadth <- matrix(s_c[i,],nrow = N,ncol = R,byrow = TRUE)^2
    S <- S*exp(-(E[,i]-optima)^2 / breadth)
  }
  return(S)
}

# This function is used for a quadratic response to the environment

S_f_quadratic <- function(E, u_c, s_c) {
  R <- ncol(u_c)
  N <- nrow(E)
  D <- ncol(E)
  S <- matrix(1, nr = N, nc = R)
  for(i in 1:D){
    optima <- matrix(u_c[i,],nrow = N,ncol = R,byrow = TRUE)
    breadth <- matrix(s_c[i,],nrow = N,ncol = R,byrow = TRUE)
    S <- S * ((-1 / (breadth/2)^2) * (E[,i] - optima)^2 + 1)
    S <- ifelse(S < 0, 0 , S)
    
  }
  return(S)
}

# Interactions ------------------------------------------------------------
# First get the sum of ecological interactions

sum_interactions <- function(A = "Species interactions matrix",
                             Y = "Species occurrence matrix"){
  t(A %*% t(Y))
}

# Then the effect of interactions on colonization

C_f <- function(v = "Sum interactions",
                d_c = "Sensitivity to interactions",
                c_0 = "Colonization at zero interactions",
                c_max = "Colonization at max interactions"){
  
  c_max * (1 + ((1 / c_0) - 1) * exp(-v * d_c))^-1
  # Is this the same as: 
  # c_max / (1 + ((1 / c_0) - 1) * exp(-v * d_c))
  
}



###################################################################################################
# Extinction
###################################################################################################


# Environment Extinction --------------------------------------------------

M_f <- function(E = "Environmental variables",
                u_e = "extinction rate at optimum?",
                s_e = "extinction rate at optimum?") {
  R <- ncol(u_e)
  N <- nrow(E)
  D <- ncol(E)
  M <- matrix(1, nrow = N, ncol = R)
  for(i in 1:D){
    optima <- matrix(u_e[i,],nrow = N,ncol = R,byrow = TRUE)
    minima <- matrix(s_e[i,],nrow = N,ncol = R,byrow = TRUE)^2
    M <- M*(1 - exp(-(E[,i] - optima)^2 / minima))
  } 
  return(M)   
}


# Interaction Extinction --------------------------------------------------

E_f <- function(v = "Sum of interactions",
                d_e = "Sensitivity to interactions",
                e_0 = "Extinction at zero interactions",
                e_min = "Extinction at max intreractions?") {
  N <- nrow(v)
  R <- ncol(v)
  
  e_min_mat <- matrix(e_min, nrow = N, ncol = R, byrow=TRUE)
  
  e_min_mat + (1 / (1-e_min_mat) + (1/(e_0-e_min_mat) - 1 / (1 - e_min_mat)) * exp(d_e * v))^-1
  
}
