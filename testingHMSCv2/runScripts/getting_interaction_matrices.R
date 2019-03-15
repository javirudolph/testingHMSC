
library(tidyverse)
library(reshape2)

source("functions/metacom_sim_fx.R")
rastPlot <- function(data, title = "title", xlabel = "x", ylabel = "y"){
  ggplot(melt(data), aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    theme(axis.text = element_blank(),
          legend.title = element_blank()) +
    scale_fill_viridis_c(alpha = 0.5) +
    labs(title = title,
         x = xlabel,
         y = ylabel) +
    theme_bw()
}

pars <- readRDS("outputs/20190313-fifteen_species_fig2_quadratic/scenario1-params.RDS")
metacomSim <- readRDS("outputs/20190313-fifteen_species_fig2_quadratic/scenario1-metacomSim.RDS")



A <- pars$A
plot_A <- rastPlot(A, title = "A - Interaction matrix")
plot_A

Y <- metacomSim[[1]]
v <- sum_interactions(A, Y)
plot_v <- rastPlot(v, title = "v - Sum of interactions", x = "patches", y = "species")
plot_v

occurence <- as.data.frame(Y)
















