#####################
#
# Main function performing the spatially explicit model
# All of the different steps are functions, provided in other scripts
# Dominique Gravel
# June 2nd, 2016
# 
####################

# Main function
main = function(XY, E, Y0, pars, A, nsteps) {

  with(pars, {

	# Simulation parameters
	N = nrow(XY)
	R = ncol(u_c)

	# Initial conditions 
	Y = Y0 

	# Compute the connectivity matrix
	K = get_K(XY, alpha)

	# Compute the local performance
	S = S_f(E, u_c, s_c)
	M = M_f(E, u_e, s_e) 
  e0_E = (1 - matrix(e_0,nr=N,nc=R,byrow=TRUE))*(1-M) + matrix(e_0,nr=N,nc=R,byrow=TRUE)

  # Store the results
	RES = list()

  	# Main loop
  	for(time in 1:nsteps) {

  		# Matrix to store changes
  		delta = matrix(0, nr = N, nc = R)

  		### Colonization ###
  		# Compute elements of the colonization probability
  		v = sum_interactions(A, Y)
  		I = I_f(Y, K, m)
  		C = C_f(v, d_c, c_0, c_max)

  		# Colonization prob
  		P_col = I*S*C

  		# Perform the test
  		rand = matrix(runif(N*R), nr = N, nc = R)
  		delta[Y == 0 & rand < P_col] = 1

  		### Extinction ###
  		# Compute the extinction probability
  		Ex = E_f(v, d_e, e_0, e_min)
      M = M_f(E, u_e, s_e)
      P_ext = M * (1-Ex) + Ex

  		# Perform the test
  		rand = matrix(runif(N*R), nr = N, nc = R)
  		delta[Y == 1 & rand < P_ext] = - 1	

  		### Apply changes ###
  		Y = Y + delta

  		### Record results ###
  		RES[[time]] = Y
  	 
 #    cat("Step = ", time, '\n')
    } # End of loop 
  RES
  })
}






