# Testing previous runs for convergence


library(HMSC)

manymodels <- readRDS("outputs/20190326-Fig2withInter/scenario4-model.RDS")


chain <- as.mcmc(manymodels[[1]], parameters = "meansParamX", thin = 5)
# The burning = TRUE doesn't work due to HMSC source code error
pdf("reports/scratch/convergence/exTraceDensity.pdf")
plot(chain)
dev.off()

chainAll <- as.mcmc(manymodels[[1]], parameters = "paramX", thin = 5)
pdf("reports/scratch/convergence/exTraceDensity_allParamX.pdf")
plot(chainAll)
dev.off()


summary(chain)
pdf("reports/scratch/convergence/autocorrChain1.pdf")
autocorr.plot(chain)
dev.off()

raftery.diag(chain)
raftery.diag(chain, r = 0.01)

# This is a nicer plot but it shows that the MEMs are kind of all over the place?
#pdf("reports/scratch/convergence/latticetypePlots.pdf")
acfplot(chain)
densityplot(chain)
#dev.off()

# Try another chain so we can calc gelman.diag
chain2 <- as.mcmc(manymodels[[2]], parameters = "meansParamX", thin = 5)
listChains <- mcmc.list(chain, chain2)

gelman.diag(listChains)
pdf("reports/scratch/convergence/gelmanplots.pdf")
gelman.plot(listChains)
dev.off()

