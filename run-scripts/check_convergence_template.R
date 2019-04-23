
source("functions/convergence_fx.R")
library(HMSC)
library(coda)

folderpath <- "write your folderpath to outputs here, don't forget the / at the end"
scenarios <- c("FIG2A", "FIG2B", "FIG2C", "FIG2D", "FIG3A", "FIG3B", "FIG3C")


# Probable get a pdf for each model, but a gelman plot for each scenario

for(i in 1:7){
  listChains <- get_mcmc_lists(folderpath = folderpath,
                                scenario = scenarios[i])
  
  assign(x = paste0(scenarios[i], "_chains"),
         value = listChains)
  
  gelman.diag(listChains)
  pdf(paste0(folderpath, scenarios[i], "gelman_diag.pdf"))
  gelman.plot(listChains)
  dev.off()
  
  
  
  pdf(paste0(folderpath, scenarios[i], "chain_plots.pdf"))
  plot(listChains)
  dev.off()
  
  raftery_on_one <- raftery.diag(listChains[[1]])
  write.csv(raftery_on_one$resmatrix, file = paste0(folderpath, scenarios[i], "raftery_diag.csv"))
  
}




