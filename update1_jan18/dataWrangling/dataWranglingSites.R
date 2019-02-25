# This script provides the functions necessary to edit, organize, clean and format the output data from VariPart in the HMSC package

# The structure of the files will determine which function to use, as species level or patch level have a different number of hierarchies in the data

# The parameters associated to each of these scenarios has to be brough in from the simulation component, which requires use of initial scripts. 

# Starting with one example at the species level:
library(tidyverse)


# FUNCTIONS ---------------------------------------------------------------

dataLoader <- function(scenario){
  filename <- paste0("update1_jan18/VPsite", scenario, ".RDS")
  VPdata <- readRDS(filename)
  
  VPdata %>% 
    set_names(imap(., ~ paste0("iter_", .y))) -> VPdata
  return(VPdata)
}



# Function for the VP site level. It gets back

tidyVPsites <- function(vp){
  overlap1 <- map(vp, "overlap1")
  overlap2 <- map(vp, "overlap2")
  overlap3 <- map(vp, "overlap3")
  
  vpALL <- vector("list", length = 5)
  for(i in 1:5){
    workingVP1 <- overlap1[[i]]
    workingVP2 <- overlap2[[i]]
    workingVP3 <- overlap3[[i]]
    
    c <- rowSums(workingVP1[,,1])/15
    b <- rowSums(workingVP1[,,2])/15
    a <- rowSums(workingVP1[,,3])/15
    
    e <- rowSums(workingVP2[,,1])/15
    f <- rowSums(workingVP2[,,2])/15
    d <- rowSums(workingVP2[,,3])/15
    
    g <- rowSums(workingVP3)/15
    
    env <- a + f + 1/2 * d + 1/2 * g
    spa <- b + e + 1/2 * d + 1/2 * g
    random <- c
    iteration <- factor(paste0("iter", i), levels = paste0("iter", 1:5))
    
    cleanData <- cbind.data.frame(env, spa, random, iteration)
    cleanData$site <- paste0(row.names(cleanData))
    
    vpALL[[i]] <- cleanData
  }
  
  vpALL %>% bind_rows()
}

# Site Variables ----------------------------------------------------------

siteEnv <- as.data.frame(read.table("update1_jan18/E.txt")[1:1000,])
names(siteEnv) <- "siteEnv"

siteXY <- read.table("update1_jan18/XY.txt")[1:1000,]
names(siteXY) <- c("siteX", "siteY")

siteInfor <- bind_cols(siteEnv, siteXY) %>% rowid_to_column(var = "siteNum") %>% mutate(site = paste0("site", siteNum))


# Fig2a -------------------------------------------------------------------

datfig2a <- dataLoader("Fig2a") %>% 
  tidyVPsites() %>% 
  mutate(scenario = "Fig2a") %>% 
  left_join(., siteInfor)


# fig2b -------------------------------------------------------------------

datfig2b <- dataLoader("Fig2b") %>% 
  tidyVPsites() %>% 
  mutate(scenario = "Fig2b") %>% 
  left_join(., siteInfor)

# fig2c -------------------------------------------------------------------

datfig2c <- dataLoader("Fig2c") %>% 
  tidyVPsites() %>% 
  mutate(scenario = "Fig2c") %>% 
  left_join(., siteInfor)

# fig2d -------------------------------------------------------------------

datfig2d <- dataLoader("Fig2d") %>% 
  tidyVPsites() %>% 
  mutate(scenario = "Fig2d") %>% 
  left_join(., siteInfor)

# fig3a -------------------------------------------------------------------

datfig3a <- dataLoader("Fig3a") %>% 
  tidyVPsites() %>% 
  mutate(scenario = "Fig3a") %>% 
  left_join(., siteInfor)

# fig3b -------------------------------------------------------------------

datfig3b <- dataLoader("Fig3b") %>% 
  tidyVPsites() %>% 
  mutate(scenario = "Fig3b") %>% 
  left_join(., siteInfor)

# fig3c -------------------------------------------------------------------

datfig3c <- dataLoader("Fig3c") %>% 
  tidyVPsites() %>% 
  mutate(scenario = "Fig3c") %>% 
  left_join(., siteInfor)




# Compile all -------------------------------------------------------------

FigDataSites <- bind_rows(datfig2a, datfig2b, datfig2c, datfig2d, datfig3a, datfig3b, datfig3c)
#write.csv(FigDataSites, file = "update1_jan18/dataWrangling/FigDataSites.csv")
#saveRDS(FigDataSites, file = "update1_jan18/newFigures/FigDataSites.RDS")



