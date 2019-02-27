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
    env <- ifelse(env < 0, 0, env)
    spa <- b + e + 1/2 * d + 1/2 * g
    spa <- ifelse(spa < 0, 0, spa)
    random <- c
    codist <- ifelse(random < 0, 0, random)
    r2 <- env + spa + codist
    iteration <- factor(paste0("iter", i), levels = paste0("iter", 1:5))
    
    cleanData <- cbind.data.frame(env, spa, codist, r2, iteration)
    cleanData$site <- paste0(row.names(cleanData))
    
    vpALL[[i]] <- cleanData
  }
  
  vpALL %>% bind_rows() %>% mutate(identifier = paste(site, iteration, sep = "_"))
}

# Site Species Richness

spp_richness_site <- function(filenameWithPath){
  
  readRDS(paste(filenameWithPath)) %>% 
    set_names(imap(., ~ paste0("iter", .y))) %>% 
    map(., rowSums) %>%
    bind_rows() %>% 
    rownames_to_column(var = "sites") %>% 
    gather(., key = "iteration", value = "richness", -sites) %>% 
    mutate(identifier = paste0("site", sites, "_", iteration)) %>% 
    select(., -c(sites, iteration))
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
  left_join(., siteInfor) %>% 
  left_join(., spp_richness_site("update1_jan18/Fig2a_run.RDS"), by = "identifier")


# fig2b -------------------------------------------------------------------

datfig2b <- dataLoader("Fig2b") %>% 
  tidyVPsites() %>% 
  mutate(scenario = "Fig2b") %>% 
  left_join(., siteInfor) %>% 
  left_join(., spp_richness_site("update1_jan18/Fig2b_run.RDS"), by = "identifier")

# fig2c -------------------------------------------------------------------

datfig2c <- dataLoader("Fig2c") %>% 
  tidyVPsites() %>% 
  mutate(scenario = "Fig2c") %>% 
  left_join(., siteInfor) %>% 
  left_join(., spp_richness_site("update1_jan18/Fig2c_run.RDS"), by = "identifier")

# fig2d -------------------------------------------------------------------

datfig2d <- dataLoader("Fig2d") %>% 
  tidyVPsites() %>% 
  mutate(scenario = "Fig2d") %>% 
  left_join(., siteInfor) %>% 
  left_join(., spp_richness_site("update1_jan18/Fig2d_run.RDS"), by = "identifier")

# fig3a -------------------------------------------------------------------

datfig3a <- dataLoader("Fig3a") %>% 
  tidyVPsites() %>% 
  mutate(scenario = "Fig3a") %>% 
  left_join(., siteInfor) %>% 
  left_join(., spp_richness_site("update1_jan18/Fig3a_run.RDS"), by = "identifier")

# fig3b -------------------------------------------------------------------

datfig3b <- dataLoader("Fig3b") %>% 
  tidyVPsites() %>% 
  mutate(scenario = "Fig3b") %>% 
  left_join(., siteInfor) %>% 
  left_join(., spp_richness_site("update1_jan18/Fig3b_run.RDS"), by = "identifier")

# fig3c -------------------------------------------------------------------

datfig3c <- dataLoader("Fig3c") %>% 
  tidyVPsites() %>% 
  mutate(scenario = "Fig3c") %>% 
  left_join(., siteInfor) %>% 
  left_join(., spp_richness_site("update1_jan18/Fig3c_run.RDS"), by = "identifier")




# Compile all -------------------------------------------------------------

Fig2DataSites <- bind_rows(datfig2a, datfig2b, datfig2c, datfig2d)
#write.csv(Fig2DataSites, file = "update1_jan18/dataWrangling/Fig2DataSites.csv")
#saveRDS(Fig2DataSites, file = "update1_jan18/newFigures/Fig2DataSites.RDS")

Fig3DataSites <- bind_rows(datfig3a, datfig3b, datfig3c)
#write.csv(Fig3DataSites, file = "update1_jan18/dataWrangling/Fig3DataSites.csv")
#saveRDS(Fig3DataSites, file = "update1_jan18/newFigures/Fig3DataSites.RDS")



