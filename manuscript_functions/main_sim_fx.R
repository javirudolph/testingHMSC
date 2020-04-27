
# This function will use all the process functions to generate a metacommunity with the given parameters. 

#' @title Metacommunity Simulation
#' @description This function will use all the process functions to generate a metacommunity with the given parameters. It will only store the occurrence matrix at the end of the simulation
#' @param XY coordinates for each of the patches
#' @param E environmental covariates measured for each patch. Our simulation only included one so far
#' @param pars output from the prep_pars function. It is a list of necessary parameters to run all the process functions
#' @param Y0 matrix of initial conditions, occurrence matrix
#' @param nsteps time steps to simulate metacomminuty dynamics
#' @param envResp choose between a "quadratic" or "gaussian" response to the environment. Default is "quadratic".
#'

mainfx <- function(XY, E, pars, Y0, nsteps, envResp = "quadratic"){
  with(pars, {
    
    # Initial conditions:
    Y <- Y0
    
    # Connectivity
    K <- get_K(XY, alpha)
    
    # Local performance
    if(envResp == "quadratic"){
      S <- S_f_quadratic(E, u_c, s_c)
    }
    
    if(envResp == "gaussian"){
      S <- S_f_gaussian(E, u_c, s_c)
    }
    
    M <- M_f(E, u_e, s_e)

    
    ## Store the results
    RES <- list()
    for(time in 1:nsteps){
      # Matrix to store changes
      delta <- matrix(0, nr = N, nc = R)
      
      ### Colonization ###
      # Compute elements of the colonization probability
      v <- sum_interactions(A, Y)
      I <- I_f(Y, K, m)
      C <- C_f(v, d_c, c_0, c_max)
      
      # Colonization prob
      P_col <- I * S * C
      
      # Perform the test
      rand <- matrix(runif(N * R), nrow = N, ncol = R)
      delta[Y == 0 & rand < P_col] <- 1
      
      ### Extinction ###
      # Compute the extinction probability
      Ex <- E_f(v, d_e, e_0, e_min)
      M <- M_f(E, u_e, s_e)
      P_ext <- M * (1 - Ex) + Ex
      
      # Perform the test
      rand <- matrix(runif(N*R), nrow = N, ncol = R)
      delta[Y == 1 & rand < P_ext] <- - 1	
      
      ### Apply changes ###
      Y <- Y + delta
      
      ## Record Results ##
      RES[[time]] = Y
    }
    RES
  })
}