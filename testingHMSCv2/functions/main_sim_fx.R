
# This function will use all the process functions to generate a metacommunity with the given parameters. 

mainfx <- function(XY, E, pars, Y0, nsteps){
  with(pars, {
    
    # Initial conditions:
    Y <- Y0
    
    # Connectivity
    K <- get_K(XY, alpha)
    
    # Local performance
    S <- S_f(E, u_c, s_c)
    M <- M_f(E, u_e, s_e)
    E0_E <- (1 - matrix(e_0,nr=N,nc=R,byrow=TRUE))*(1-M) + matrix(e_0,nr=N,nc=R,byrow=TRUE) # what is this?
    
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
      Ex <- E_f(v, d_e, e_0, e_min, N, R)
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