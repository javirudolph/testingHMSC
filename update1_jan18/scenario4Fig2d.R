
#=======================
# Prepare parameter file
#=======================

 
scenario <- "Fig2d"
niter <- 5
res <- vector("list",length = niter)

for(i in 1:niter){
  # Description
  # Basic species sorting with dispersal limitations without ecological interactions

  #from here
  # # Number of patches
  N = 1000

  # # Number of environmental variables
  D = 1

  # # Regional species richness
  R = 15

  # # Effect of the environment on colonization
  u_c = matrix(nr = D, nc = R)
  u_c[1,] = seq(0.1,0.9, length=R)
  s_c = matrix(1, nr = D, nc = R)

  # # Effect of the environment on extinction
  u_e = matrix(nr = D, nc = R)
  u_e[1,] = rep(0.5, R)
  s_e = matrix(Inf, nr = D, nc = R)

  # # Mean dispersal
  alpha = 0.005

  # # Immigration
  m = 0.001

  # # Colonization function
  #c_0 = sample(c(rep(0.1,5), rep(0.4, 5), rep(0.7,5)))
  c_0 = rep(0.4, R)
  c_max = rep(1, R) # Colonization at max interactions

  # # Extinction function
  e_0 = rep(0.025, R) # Extinction at 0 interactions
  e_min = rep(0, R) # Exinction at max interactions

  # # Sensitivity to interactions
  d_c = 1.5
  d_e = 1.5

  # # Interaction matrix
  A = matrix(0,nr=R,nc=R)
  d = as.matrix(dist(c(1:R),upper=TRUE,diag=T))
  A[d<=1] = -1
  diag(A) = 0

  # # Collect all parameters into a single list
  pars = list(u_c = u_c, u_e = u_e, s_c = s_c, s_e = s_e, alpha = alpha, m = m,
  c_0 = c_0, e_0 = e_0, c_max = c_max, e_min = e_min, d_c = d_c, d_e = d_e, A = A)
  
  #to here

  #=======================
  # Run the model
  #=======================
  source("functions.R")
  source("landscape.R")
  source("main.R")

  # Landscape characteristics
  XY = read.table("XY.txt")[1:N,]
  E = as.matrix(read.table("E.txt")[1:N,])

  # Initial conditions
  Y0 = matrix(0, nr = N, nc = R)
  rand = matrix(runif(N*R), nr = N, nc = R)
  Y0[rand < 0.8] = 1

  # Run the model
  nsteps = 200

  run = main(XY,E,Y0,pars,A,nsteps)

  res[[i]] <- run[[nsteps]]
}

saveRDS(res, file = paste(scenario,"_run.RDS", sep=""))
