# Setup ---------------------------------------------------------------

library(tidyverse)
library(HMSC)

E <- readRDS("manuscript_functions/fixedLandscapes/orig-no-seed-E.RDS")
source("manuscript_functions/output_processing_fx.R")


outsfolderpath <- "manuscript_outputs/"

scenarios <- c("FIG2A", "FIG2B", "FIG2C", "FIG2D", "FIG3A", "FIG3B", "FIG3C")
new_scen <- LETTERS[1:7]

# Structure the data ----------------------------------------------------------------

spp_data <- NULL
for(i in 1:4){
  spp <- get_species_data(outsfolderpath, scenarios[i]) %>% 
    mutate(scenario = new_scen[i])
  params <- get_fig2_params(outsfolderpath, scenario = scenarios[i]) %>% 
    mutate(dispersal = as.numeric(as.character(dispersal)),
           scenario = new_scen[i])
  
  left_join(spp, params) %>% 
    mutate(nicheCent = abs(nicheOptima - 0.5)) -> spp
  
  spp_data <- bind_rows(spp_data, spp)
}

for(i in 5:7){
  spp <- get_species_data(outsfolderpath, scenario = scenarios[i]) %>% 
    mutate(scenario = new_scen[i])
  params <- get_fig3_params(outsfolderpath, scenario = scenarios[i]) %>% 
    mutate(dispersal = as.numeric(as.character(dispersal)),
           scenario = new_scen[i])
  
  left_join(spp, params) %>% 
    mutate(nicheCent = abs(nicheOptima - 0.5)) -> spp
  
  spp_data <- bind_rows(spp_data, spp)
  
}
saveRDS(spp_data, file = paste0(outsfolderpath, "spp_data.RDS"))

sites_data <- NULL
for (i in 1:7){
  sites <- get_sites_data(outsfolderpath, scenario = scenarios[i]) %>%
    mutate(E = rep(E, 5),
           Edev = abs(E-0.5),
           scenario = new_scen[i])
  
  
  sites_data <- bind_rows(sites_data, sites)
}

saveRDS(sites_data, file = paste0(outsfolderpath, "sites_data.RDS"))

# Interaction matrices functions and data 

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat, diag = TRUE)]<- NA
  return(cormat)
}

intplot <- function(modelfile, iteration){
  assoMat <- corRandomEff(modelfile[[iteration]])
  siteMean <- apply(assoMat[ , , , 1], 1:2, mean)
  
  siteMean %>% 
    get_upper_tri() %>% 
    as_tibble() %>% 
    rownames_to_column(., var = "Specie1") %>% 
    pivot_longer(-Specie1, names_to = "Specie2", values_to = "value") %>% 
    drop_na(value) %>% 
    mutate(iteration = paste("Iteration", iteration),
           Specie2 = str_replace(Specie2, "y", ""),
           Specie1 = factor(Specie1, levels = c(1:12)),
           Specie2 = factor(Specie2, levels = c(12:1)),
           color = ifelse(value > 0.4, "red",
                          ifelse(value < -0.4, "blue", NA)), 
           signi = ifelse(is.na(color)== TRUE, "x", NA))
}

intData <- NULL
for(i in 1:7){
  
  modelfile <- readRDS(paste0(outsfolderpath, scenarios[i], "-model.RDS"))
  
  b <- NULL
  for(j in 1:5){
    a <- intplot(modelfile, j)
    b <- bind_rows(b, a)
  } 
  
  b$scenario <- scenarios[i]
  intData <- bind_rows(intData, b)
}

saveRDS(intData, file = paste0(outsfolderpath, "intData.RDS"))
