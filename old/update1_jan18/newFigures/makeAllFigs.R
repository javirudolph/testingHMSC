# Actually making the figures

library(tidyverse)
library(ggtern)

source("update1_jan18/newFigures/fxMakeAllFigs.R")

# load the data -----------------------------------------------------------

fig2Dataspp <- readRDS("update1_jan18/newFigures/Fig2DataParams.RDS")
fig3Dataspp <- readRDS("update1_jan18/newFigures/Fig3DataParams.RDS")

figDataSites <- readRDS("update1_jan18/newFigures/FigDataSites.RDS")

savepath <- "update1_jan18/newFigures/allFigs/"

#Figure size that looks nice is 8.05 x 5.29

# FIGURE 2 ---------------------------------------------------------------



listScenarios <- c("Fig2a", "Fig2b", "Fig2c", "Fig2d")

for(i in 1:4){
  scenarioFig <- listScenarios[i]
  
  ternPlot(figData = fig2Dataspp, selScenario = scenarioFig, 
           varShape = "iteration", varColor = "nicheOpt") + 
    scale_color_viridis_c() +
    labs(title = scenarioFig,
         caption = "Shapes correspond to iterations")
  
  #ggsave(filename = paste0(savepath, scenarioFig, "Spp.png"), dpi = 600)
  
}



# FIGURE 3 ----------------------------------------------------------------
scenarioFig <- "Fig3a"

ternPlot(figData = fig3Dataspp, selScenario = scenarioFig,
         varShape = "iteration", varColor = "intercol") +
  scale_color_viridis_c(begin = 0, end = 0.8) +
  labs(title = scenarioFig,
       caption = "Color corresponds to interactions \n Shape is the iteration")
#ggsave(filename = paste0(savepath, scenarioFig, "Spp.png"), dpi = 600)

scenarioFig <- "Fig3b"

ternPlot(figData = fig3Dataspp, selScenario = scenarioFig,
         varShape = "iteration", varColor = "dispersal") +
  scale_color_viridis_c(begin = 0, end = 0.8) +
  labs(title = scenarioFig,
       caption = "Color corresponds to dispersal \n Shape is the iteration \n no interactions")

#ggsave(filename = paste0(savepath, scenarioFig, "Spp.png"), dpi = 600)

scenarioFig <- "Fig3c"

ternPlot(figData = fig3Dataspp, selScenario = scenarioFig,
         varShape = "iteration", varColor = "dispersal") +
  scale_color_viridis_c(begin = 0, end = 0.8) +
  labs(title = scenarioFig,
       caption = "Color corresponds to dispersal \n Shape is the iteration \n with interactions")

#ggsave(filename = paste0(savepath, scenarioFig, "Spp.png"), dpi = 600)



# FIGURE SITES ------------------------------------------------------------


listScenarios <- c("Fig2a", "Fig2b", "Fig2c", "Fig2d", "Fig3a", "Fig3b", "Fig3c")

for(i in 1:7){
  scenarioFig <- listScenarios[i]
  
  ternPlot(figData = figDataSites, selScenario = scenarioFig, 
           varShape = "iteration", varColor = NULL) + 
    labs(title = paste(scenarioFig, "at sites"),
         caption = "Shapes correspond to iterations") -> bigPlot
  
  #ggsave(filename = paste0(savepath, scenarioFig, "Sites.png"), plot = bigPlot, dpi = 600)
  
}


