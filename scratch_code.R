


get_sites_data <- function(folderpath, scenario){
  richness <- readRDS(paste0(folderpath, scenario, "-metacomSim.RDS")) %>% 
    set_names(imap(., ~ paste0("iter", .y))) %>% 
    map(., rowSums) %>%
    bind_rows() %>% 
    rownames_to_column(var = "sites") %>% 
    gather(., key = "iteration", value = "richness", -sites) %>% 
    mutate(identifier = paste0("site", sites, "_", iteration)) %>% 
    dplyr::select(., -c(sites, iteration))
  
  
  vp <- readRDS(paste0(folderpath, scenario, "-vpsites.RDS"))
  
  out <- NULL
  for(i in 1:5){
    siteSingle <- as.data.frame(apply(vp[[1]][[1]], c(1,3), max)) %>%
      rename(., codist = random) %>% 
      rownames_to_column(var = "site") %>% 
      mutate(iteration = factor(paste0("iter", i), levels = paste0("iter", 1:5)),
             r2 = env + codist + spa)
    out <- rbind.data.frame(out, siteSingle)
  }
  
  out %>% 
    mutate(identifier = paste(site, iteration, sep = "_"),
           scenario = scenario) %>% 
    left_join(., richness)
}
