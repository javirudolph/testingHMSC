# This script provides the functions necessary to edit, organize, clean and format the output data from VariPart in the HMSC package

# The structure of the files will determine which function to use, as species level or patch level have a different number of hierarchies in the data

# The parameters associated to each of these scenarios has to be brough in from the simulation component, which requires use of initial scripts. 

# Starting with one example at the species level:
library(tidyverse)


# FUNCTIONS ---------------------------------------------------------------

# Load the data and give names
dataLoader <- function(scenario){
  filename <- paste0("update1_jan18/VP ", scenario, ".RDS")
  VPdata <- readRDS(filename)
  
  VPdata %>% 
    set_names(imap(., ~ paste0("iter_", .y))) -> VPdata
  return(VPdata)
}

# Partial source to get the parameters
sourcePartial <- function(fn,startTag='#from here',endTag='#to here') {
  lines <- scan(fn, what=character(), sep="\n", quiet=TRUE)
  st<-grep(startTag,lines)
  en<-grep(endTag,lines)
  tc <- textConnection(lines[(st+1):(en-1)])
  source(tc)
  close(tc)
}

# Compile parameters in useful format
compileFig2Params <- function(filename){
  sourcePartial(filename)
  enframe(u_c[1,], name = "species",value =  "nicheOpt") %>% 
    left_join(., enframe(s_c[1,], name = "species", value =  "nicheBreadth")) %>% 
    left_join(., enframe(c_0, name = "species", value =  "colProb")) %>% 
    mutate(dispersal = alpha, 
           species = as.character(species), 
           intercol = d_c, 
           interext = d_e) -> params
}


# Manage variation partition components
variPartTidy <- function(VPdata, params){
  fullData <- list()
  for(i in 1:length(VPdata)){
    fullData[[i]] <- VPdata[[i]] %>% 
      map(as_tibble) %>%
      bind_cols() %>% 
      rownames_to_column() %>% 
      set_names(c("species", "c", "b", "a", "e", "f", "d", "g")) %>% 
      transmute(species = species,
                env = a + f + 0.5 * d + 0.5 * g,
                env = ifelse(env < 0, 0, env),
                spa = b + e + 0.5 * d + 0.5 * g,
                spa = ifelse(spa < 0, 0, spa),
                codist = c,
                codist = ifelse(codist < 0, 0, codist),
                r2 = env + spa + codist,
                iteration = names(VPdata[i])) %>% 
      left_join(., params)
    
  }
  
  fullData %>% 
    bind_rows() %>% 
    mutate(identifier = paste0("spp", species, "_", iteration)) -> fullData
  return(fullData)
}

# Get the prevalence
prevalenceForSpp <- function(filenameWithPath){

  readRDS(paste(filenameWithPath)) %>% 
    set_names(imap(., ~ paste0("iter_", .y))) %>% 
    map(., colSums) %>%
    bind_cols() %>% 
    rownames_to_column(var = "species") %>% 
    gather(., key = "iteration", value = "prevalence", -species) %>% 
    mutate(identifier = paste0("spp", species, "_", iteration)) %>% 
    select(., -c(species, iteration))
}

# Fig2a -------------------------------------------------------------------

datFig2a <- dataLoader("Fig2a")
prmsFilenam <- "update1_jan18/scenario1Fig2a.R"
prmsFig2a <- compileFig2Params(prmsFilenam)
fullFig2a <- variPartTidy(datFig2a, prmsFig2a) %>% 
  mutate(scenario = "Fig2a") %>% 
  left_join(., prevalenceForSpp("update1_jan18/Fig2a_run.RDS"), by = "identifier")

# Fig2b -------------------------------------------------------------------
datFig2b <- dataLoader("Fig2b")
prmsFilenam <- "update1_jan18/scenario2Fig2b.R"
prmsFig2b <- compileFig2Params(prmsFilenam)
fullFig2b <- variPartTidy(datFig2b, prmsFig2b) %>% 
  mutate(scenario = "Fig2b") %>% 
  left_join(., prevalenceForSpp("update1_jan18/Fig2b_run.RDS"), by = "identifier")




# Fig2c -------------------------------------------------------------------

datFig2c <- dataLoader("Fig2c")
prmsFilenam <- "update1_jan18/scenario3Fig2c.R"
prmsFig2c <- compileFig2Params(prmsFilenam)
fullFig2c <- variPartTidy(datFig2c, prmsFig2c) %>% 
  mutate(scenario = "Fig2c") %>% 
  left_join(., prevalenceForSpp("update1_jan18/Fig2c_run.RDS"), by = "identifier")


# Fig2d -------------------------------------------------------------------

datFig2d <- dataLoader("Fig2d")
prmsFilenam <- "update1_jan18/scenario4Fig2d.R"
prmsFig2d <- compileFig2Params(prmsFilenam)
fullFig2d <- variPartTidy(datFig2d, prmsFig2d) %>% 
  mutate(scenario = "Fig2d") %>% 
  left_join(., prevalenceForSpp("update1_jan18/Fig2d_run.RDS"), by = "identifier")


# Compile all -------------------------------------------------------------

Figure2Data <- bind_rows(fullFig2a, fullFig2b, fullFig2c, fullFig2d)
write.csv(Figure2Data, file = "update1_jan18/dataWrangling/Fig2DataParams.csv")
saveRDS(Figure2Data, file = "update1_jan18/newFigures/Fig2DataParams.RDS")



         