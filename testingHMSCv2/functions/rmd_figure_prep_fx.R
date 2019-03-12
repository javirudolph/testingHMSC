# functions required for the Rmarkdown reports to organize data, parameters and such, to create the figures and their associated parameters.

# Get a table of parameters for the rmd report. It will give the average prevalence as well
get_params_rmd <- function(folderpath, scenario){
  params <- with(readRDS(paste0(folderpath, scenario, "-params.RDS")), {
    enframe(u_c[1,], name = "species",value =  "nicheOpt") %>% 
      left_join(., enframe(s_c[1,], name = "species", value =  "nicheBreadth")) %>% 
      left_join(., enframe(c_0, name = "species", value =  "colProb")) %>% 
      mutate(dispersal = alpha, 
             species = as.character(species), 
             intercol = d_c, 
             interext = d_e)
  })
  
  prevalence <- readRDS(paste0(folderpath, scenario, "-metacomSim.RDS")) %>% 
    set_names(imap(., ~ paste0("iter_", .y))) %>% 
    map(., colSums) %>%
    bind_cols() %>% 
    rownames_to_column(var = "species") %>% 
    gather(., key = "iteration", value = "prevalence", -species) %>% 
    group_by(., species) %>% 
    summarise(., avPrevalence = mean(prevalence)) 
  
  allpars <- left_join(params, prevalence)
}