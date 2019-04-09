
# This function is used to prepare the parameters in a useful format and avoid lengthy code for every simulation

# Parameters --------------------------------------------------------------
# Input and organize parameters

# N = "Number of patches",
# D = "Number of environmental variables",
# R = "Number of species",
# breadth = "Niche breadth", 
# alpha = "Dispersal",
# interx = "Sensitivity to interactions"
# initocup = "initial occupancy"

prep_pars <- function(N = NULL,
                      D = NULL,
                      R = NULL,
                      nicheOpt = NULL,
                      breadth = NULL,
                      alpha = NULL,
                      interx_col = NULL,
                      interx_ext = NULL,
                      makeRDS = FALSE,
                      whereToSave = NULL,
                      objName = NULL){
  
  # Number of patches, environmental variables and species
  N <- N
  D <- D
  R <- R
  
  u_c <- nicheOpt
  
  # Effect of environment on colonization
  if(is.null(nicheOpt) == TRUE){
    u_c <- matrix(nrow = D, ncol = R)
    u_c[1,] <- seq(0.1, 0.9, length = R)
  }
  
  s_c <- matrix(breadth, nrow = D, ncol = R)
  
  # Effect of the environment on extinction
  u_e <- matrix(nrow = D, ncol = R)
  u_e[1,] <- rep(0.5, R)
  s_e <- matrix(Inf, nrow = D, ncol = R)
  
  # Mean dispersal
  alpha <- alpha
  
  # Immigration
  m <- 0.001
  
  # Colonication function
  c_0 <- rep(0.4, R) # Colonization at 0 interactions
  c_max <- rep(1, R) # Colonization at max interactions
  
  
  # Extinction function
  e_0 <- rep(0.025, R) # Extinction at 0 interactions
  e_min <- rep(0, R) # Extinction at max interactions
  
  # Sensitivity to interactions
  d_c <- interx_col
  d_e <- interx_ext
  
  # Interaction matrix
  A <- matrix(0, ncol = R, nrow = R)
  d <- as.matrix(dist(c(1:R), upper = TRUE, diag = TRUE))
  A[d <= 1] <- -1
  diag(A) <- 0
  
  
  pars <- list(N = N, D = D, R = R,
               u_c = u_c, u_e = u_e, s_c = s_c, s_e = s_e, alpha = alpha, m = m,
               c_0 = c_0, e_0 = e_0, c_max = c_max, e_min = e_min, d_c = d_c, d_e = d_e, A = A)
  
  if(makeRDS == TRUE){
    nameFile <- paste0(whereToSave, objName, "-params.RDS")
    saveRDS(pars, file = nameFile)
  }
  
  return(pars)
}


# Organize for plotting ---------------------------------------------------

organize_params_plot <- function(prepParsOutput){
  with(prepParsOutput, {enframe(u_c[1,], name = "species", value = "nicheOptima") %>% 
      left_join(., enframe(s_c[1,], name = "species", value = "nicheBreadth")) %>% 
      left_join(., enframe(c_0, name = "species", value = "colonizationProb")) %>% 
      mutate(., dispersal = alpha,
             species = as.character(species),
             speciesChr = as.character(paste0("spp_", species)),
             interCol = d_c ,
             interExt = d_e)})
}







