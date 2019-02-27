# This script provides the functions necessary to edit, organize, clean and format the output data from VariPart in the HMSC package for Figure 3

# This is different to Figure 2 since the not all the species have the same dispersal parameters and are not simulated in a single loop.

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
compileFig3Params <- function(plusSpp = 0){
  enframe(u_c[1,], name = "species",value =  "nicheOpt") %>% 
    left_join(., enframe(s_c[1,], name = "species", value =  "nicheBreadth")) %>% 
    left_join(., enframe(c_0, name = "species", value =  "colProb")) %>% 
    mutate(dispersal = alpha, 
           species = as.character(species + plusSpp), 
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



# Figure3a ----------------------------------------------------------------

datFig3a <- dataLoader("Fig3a")
filename <- "update1_jan18/scenario1Fig3a.R"

sourcePartial(filename)
prms1_Fig3a <- compileFig3Params()

sourcePartial(filename, startTag = "#secondStart", endTag = "secondEnd")
prms2_Fig3a <- compileFig3Params(plusSpp = 7)

prmsFig3a <- bind_rows(prms1_Fig3a, prms2_Fig3a)

fullFig3a <- variPartTidy(datFig3a, prmsFig3a) %>% 
  mutate(scenario = "Fig3a") %>% 
  left_join(., prevalenceForSpp("update1_jan18/Fig3a_run.RDS"), by = "identifier")


# Figure3b ----------------------------------------------------------------

datFig3b <- dataLoader("Fig3b")
filename <- "update1_jan18/scenario2Fig3b.R"

sourcePartial(filename)
prms1_Fig3b <- compileFig3Params()

sourcePartial(filename, startTag = "#secondStart", endTag = "secondEnd")
prms2_Fig3b <- compileFig3Params(plusSpp = 5)

sourcePartial(filename, startTag = "#thirdStart", endTag = "thirdEnd")
prms3_Fig3b <- compileFig3Params(plusSpp = 10)

prmsFig3b <- bind_rows(prms1_Fig3b, prms2_Fig3b, prms3_Fig3b)

fullFig3b <- variPartTidy(datFig3b, prmsFig3b) %>% 
  mutate(scenario = "Fig3b")



# Figure3c ----------------------------------------------------------------


datFig3c <- dataLoader("Fig3c")
filename <- "update1_jan18/scenario3Fig3c.R"

sourcePartial(filename)
prms1_Fig3c <- compileFig3Params()

sourcePartial(filename, startTag = "#secondStart", endTag = "secondEnd")
prms2_Fig3c <- compileFig3Params(plusSpp = 5)

sourcePartial(filename, startTag = "#thirdStart", endTag = "thirdEnd")
prms3_Fig3c <- compileFig3Params(plusSpp = 10)

prmsFig3c <- bind_rows(prms1_Fig3c, prms2_Fig3c, prms3_Fig3c)

fullFig3c <- variPartTidy(datFig3c, prmsFig3c) %>% 
  mutate(scenario = "Fig3c")

# Compile all -------------------------------------------------------------

Figure3Data <- bind_rows(fullFig3a, fullFig3b, fullFig3c)
#write.csv(Figure3Data, file = "update1_jan18/dataWrangling/Fig3DataParams.csv")
#saveRDS(Figure3Data, file = "update1_jan18/newFigures/Fig3DataParams.RDS")






