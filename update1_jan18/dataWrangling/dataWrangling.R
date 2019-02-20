# This script provides the functions necessary to edit, organize, clean and format the output data from VariPart in the HMSC package

# The structure of the files will determine which function to use, as species level or patch level have a different number of hierarchies in the data

# The parameters associated to each of these scenarios has to be brough in from the simulation component, which requires use of initial scripts. 

# Starting with one example at the species level:
library(tidyverse)

# Partial source to get the parameters
sourcePartial <- function(fn,startTag='#from here',endTag='#to here') {
  lines <- scan(fn, what=character(), sep="\n", quiet=TRUE)
  st<-grep(startTag,lines)
  en<-grep(endTag,lines)
  tc <- textConnection(lines[(st+1):(en-1)])
  source(tc)
  close(tc)
}

VPdata <- readRDS("update1_jan18/VP Fig2a.RDS")

VPdata %>% 
  set_names(imap(., ~ paste0("iter_", .y))) -> VPdata

sourcePartial("update1_jan18/scenario1Fig2a.R")
enframe(u_c[1,], name = "species",value =  "nicheOpt") %>% 
  left_join(., enframe(s_c[1,], name = "species", value =  "nicheBreadth")) %>% 
  left_join(., enframe(c_0, name = "species", value =  "colProb")) %>% 
  mutate(dispersal = alpha, 
         species = as.character(species)) -> params

  
VPdata[[1]] %>% 
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
            iteration = names(VPdata[1])) %>% 
  left_join(., params)


