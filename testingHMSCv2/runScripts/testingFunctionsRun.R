
# Scratch script to test if functions work
library(HMSC)
library(doParallel)
library(adespatial)
library(vcd)


source("functions/landscape_fx.R")
source("functions/metacom_sim_fx.R")
source("functions/prep_pars_fx.R")
source("functions/main_sim_fx.R")



# Landscape ---------------------------------------------------------------

N <- 300
D <- 1
R <- 7
# Get an environment
set.seed(27)
XY <- get_xy(N)
E <- get_E(D, N)
# Remove the seed so it doesn't affect the following simulations
rm(.Random.seed, envir = .GlobalEnv)


# Set parameters ----------------------------------------------------------
pars <- prep_pars(N, D, R, breadth = 0.02, alpha = 0.005, interx_col = 1.5, interx_ext = 1.5)


# Main function -----------------------------------------------------------

nsteps <- 20
Y0 <- ifelse(matrix(runif(N * D), nrow = N, ncol = R) < 0.8, 1, 0)

main_sim <- mainfx(XY, E, pars, Y0, nsteps)

# Looks good, the main function works, let make some iterations
niter <- 5
res <- vector("list", length = niter)
for(i in 1:niter){
  run <- mainfx(XY, E, pars, Y0, nsteps)
  res[[i]] <- run[[nsteps]]
}

# You could save the res file as an RDS if bigger



# MEMs --------------------------------------------------------------------

ncluster <- 4
MEM <- dbmem(XY, MEM.autocor = "positive", thresh = 0.5)

if(FALSE){
  ### Set clusters
  clusters <- makeCluster(ncluster)
  registerDoParallel(clusters)

  ### test MEM autocorrelation
  testMEM <- foreach(i=1:ncol(MEM)) %dopar% {
    adespatial:::moran.randtest(MEM[,i], attributes(MEM)$listw, nrepet = 9999)
  }

  ### Stop clusters
  stopCluster(clusters)
}

# pvals <- lapply(testMEM, function(x) x$pvalue)
# min(which(pvals>0.05))

MEMsel <- MEM[,1:74]




# HMSC model --------------------------------------------------------------

run <- res
nrun <- length(run)

clusters <- makeCluster(ncluster)
registerDoParallel(clusters)

### Estimate models
model <- foreach(j = 1:nrun) %dopar% {
  library(HMSC)
  formData <- as.HMSCdata(Y = run[[j]], X = cbind(scale(E),scale(E)^2, MEMsel),
                          Random = as.factor(1:N),
                          scaleX = TRUE, interceptX = TRUE)
  
  hmsc(formData, family = "probit",
       niter = 10000, nburn = 5000, thin = 5)
}

### Stop clusters
stopCluster(clusters)

# Just curious about the X argument: 
cbind(scale(E),scale(E)^2, MEMsel)
# Y argument
run[[i]]


# The result: model, is what we use for the VP


# VP ----------------------------------------------------------------------


nmodel <- length(model)

clusters <- makeCluster(ncluster)
registerDoParallel(clusters)

### Estimate models
vpRes <- foreach(j = 1:nmodel) %dopar% {
  library(HMSC)
  variPart(model[[j]], groupX = c(rep("env",3),rep("spa",length(MEMsel))), 
           type = "III", R2adjust = TRUE)
}

### Stop clusters
stopCluster(clusters)

# Don't understand why for groupX, env is chosen 3 timees, and spatial the others... 


# Plot the example --------------------------------------------------------
# should really create a function to plot these things
# Env = a + f + 1/2d + 1/2g
# Spa = b + e + 1/2d + 1/2g
# Random = c


a <- vpRes[[1]]$overlap1[,3]
b <- vpRes[[1]]$overlap1[,2]
c <- vpRes[[1]]$overlap1[,1]
d <- vpRes[[1]]$overlap2[,3]
e <- vpRes[[1]]$overlap2[,1]
f <- vpRes[[1]]$overlap2[,2]
g <- vpRes[[1]]$overlap3[,1]

env <- a + f + 1/2 * d + 1/2 * g
spa <- b + e + 1/2 * d + 1/2 * g
random <- c

plotVP <- data.frame(env = ifelse(env < 0, 0, env),
                     spa = ifelse(spa < 0, 0, spa),
                     random = ifelse(random < 0, 0, random))

ternaryplot(plotVP,
            dimnames = c("Environment","Spatial\nAutocorrelation",
                         "Co-Distribution"),
            bg = "lightgray",
            grid_color = "white",
            cex = rowSums(plotVP))


