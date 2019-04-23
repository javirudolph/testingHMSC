#####################
#
# Functions used to generate landscape characteristics
# Current version is minimal, with random location of 
# plots and random distribution of the environmental conditions
# Dominique Gravel
# June 2nd, 2016
# 
####################

# Random XY coordinates
get_XY = function(N) cbind(runif(N),runif(N))

# Aggregation of XY coordinates
get_XY_agg = function(N, Nclusters, sd_xy) {

	Xclust = runif(Nclusters)
	Yclust = runif(Nclusters)

	X = rnorm(N, rep(Xclust,N/Nclusters), sd_xy)
	Y = rnorm(N, rep(Yclust,N/Nclusters), sd_xy)

	cbind(X,Y)
}

# Random uniform environmental values
get_E = function(D, N) matrix(runif(D*N), nr = N, nc = D)

