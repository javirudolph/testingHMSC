spR2_fn <- function(model, obs) {

	# LL for the model
	pred <- predict(model, type = "response")

	ll <- log(pred)
	ll[obs==0] <- log(1-pred[obs==0])
	obs_ll <- apply(ll, 2, sum)
	
	# LL for the null model 
	nsite <- nrow(obs)
	N <- apply(obs,2,sum) 
	S <- ncol(obs)
	p <- N / nsite
	null_ll <- N*log(p) + (nsite-N)*log(1-p)

	return(1 - obs_ll / null_ll)

}

patchR2_fn <- function(model, obs) {

	# LL for the model
	pred <- predict(model, type = "response")
	ll <- log(pred)
	ll[obs==0] <- log(1-pred[obs==0])
	obs_ll <- apply(ll, 2, sum)
	
	# LL for the null model 
	nsite <- nrow(obs)
	N <- apply(obs,2,sum) 
	S <- ncol(obs)
	p <- N / nsite
	null_ll <- N*log(p) + (nsite-N)*log(1-p)



	return(1 - obs_ll / null_ll)

}


diagnos = function(Y, E, XY) {	

	library(vegan)
	library(HMSC)

	# Prepare objects
	siteID = as.factor(c(1:nrow(E)))
	Z = pcnm(dist(XY))$vectors[,1:50]
	E2 = E^2
	X = cbind(E,E2,Z)

	# Run HMSC
	data_full <- as.HMSCdata(Y = Y, X = X, Random = siteID)
	model_full <- hmsc(data_full, family = "probit", niter = 10000, 
	nburn = 5000, thin = 10)

	# Variation partitioning
	groupX <- c("Intercept", "E", "E2")
	for(i in 1:50) groupX <- c(groupX, paste("PCNM",i,sep=""))
	VP <- variPart(model_full, groupX = groupX)
	Efrac <- apply(VP[,2:3],1,sum)
	Sfrac <- apply(VP[,4:53],1,sum)
	Cfrac <- VP[,54]

	return(list(model = model_full, Efrac = Efrac, Sfrac = Sfrac, Cfrac = Cfrac))

}

## IT DOES NOT WORK THAT WAY ! 
## PROBLEM OF SHARED VARIANCE, NEEDS TO BE DONE WITH PARTIAL R2

plotit <- function(diagnosObj, scenario, model, obs) {
library(vcd)

dev.new(width = 10, height = 10)
spR2 <- spR2_fn(model, obs)

ternaryplot(
	cbind(diagnosObj$Efrac,diagnosObj$Sfrac,diagnosObj$Cfrac), 
	pch = 19, 
#	cex = spR2*2,
	bg = "lightgray",
	grid_color = "white",
	#col = heat.colors(15),
	#col = c(rep("cornflowerblue", 7), rep("darkolivegreen", 8)),
	col = c(rep("goldenrod", 5), rep("darkorange3", 5), rep("firebrick4", 5)),
	main = scenario,
	dimnames = c("Environment", "Spatial autocorrelation", "Co-distribution")
)
dev.copy2pdf(file = paste("figures/",scenario,".pdf", dec = ""))

# Plot the heat maps
#pal <- viridis(200)
#dev.new(width = 10, height = 10)
#codistr_full <- apply(corRandomEff(diagnosObj$model)$Random, 1:2, mean)
#corrplot(codistr_full, method = "color", col = pal, type = "lower",
#diag = FALSE, tl.srt = 45)
#title(scenario, cex = 2)
#dev.copy2pdf(file = paste("figures/",scenario,".pdf", dec = ""))

}


plotreps <- function(res, scenario) {
library(vcd)

ternaryplot(
	cbind(res[,1],res[,2],res[,3]), 
	pch = 19, 
#	cex = res[,4],
	bg = "lightgray",
	grid_color = "white",
	col = "darkblue",
	main = scenario,
	dimnames = c("Environment", "Spatial autocorrelation", "Co-distribution")
)
dev.copy2pdf(file = paste("figures/",scenario,"_rep.pdf", dec = ""))

# Plot the heat maps
#pal <- viridis(200)
#dev.new(width = 10, height = 10)
#codistr_full <- apply(corRandomEff(diagnosObj$model)$Random, 1:2, mean)
#corrplot(codistr_full, method = "color", col = pal, type = "lower",
#diag = FALSE, tl.srt = 45)
#title(scenario, cex = 2)
#dev.copy2pdf(file = paste("figures/",scenario,".pdf", dec = ""))

}

