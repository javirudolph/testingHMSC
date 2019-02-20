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
    bind_rows() -> fullData
  return(fullData)
}


# Figure3a ----------------------------------------------------------------

datFig3a <- dataLoader("Fig3a")
filename <- "update1_jan18/scenario1Fig3a.R"

sourcePartial(filename)
prms1_Fig3a <- compileFig3Params()

sourcePartial(filename, startTag = "#secondStart", endTag = "secondEnd")
prms2_Fig3a <- compileFig3Params(plusSpp = 7)

prmsFig3a <- bind_rows(prms1_Fig3a, prms2_Fig3a)






