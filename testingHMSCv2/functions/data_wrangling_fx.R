

# Functions to modify and organize the dataframes from the VP output and get ready for plotting

doItAll_twosppNichecomp <- function(outPath, scenarioNum, indSites = FALSE){
  
  
  if(indSites == TRUE){
    richness <- readRDS(paste0(outPath, scenarioNum, "-metacomSim.RDS")) %>% 
      set_names(imap(., ~ paste0("iter", .y))) %>% 
      map(., rowSums) %>%
      bind_rows() %>% 
      rownames_to_column(var = "sites") %>% 
      gather(., key = "iteration", value = "richness", -sites) %>% 
      mutate(identifier = paste0("site", sites, "_", iteration)) %>% 
      select(., -c(sites, iteration))
    
    
    
    vp <- readRDS(paste0(outPath, scenarioNum, "-vpsites.RDS"))
    
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
    
    vpALL %>% 
      bind_rows() %>% 
      mutate(identifier = paste(site, iteration, sep = "_"),
             scenario = scenarioNum) %>% 
      left_join(., richness) -> vpALL
    
    return(vpALL)
  }
  
  
  
  
  
  params <- with(readRDS(paste0(outPath, scenarioNum, "-params.RDS")), {
    enframe(u_c[1,], name = "species",value =  "nicheOpt") %>% 
      left_join(., enframe(s_c[1,], name = "species", value =  "nicheBreadth")) %>% 
      left_join(., enframe(c_0, name = "species", value =  "colProb")) %>% 
      mutate(dispersal = alpha, 
             species = as.character(species), 
             intercol = d_c, 
             interext = d_e)
  })
  
  prevalence <- readRDS(paste0(outPath, scenarioNum, "-metacomSim.RDS")) %>% 
    set_names(imap(., ~ paste0("iter_", .y))) %>% 
    map(., colSums) %>%
    bind_cols() %>% 
    rownames_to_column(var = "species") %>% 
    gather(., key = "iteration", value = "prevalence", -species) %>% 
    mutate(identifier = paste0("spp", species, "_", iteration)) %>% 
    select(., -c(species, iteration))
  
  
  
  
  readRDS(paste0(outPath, scenarioNum, "-vpspp.RDS")) %>% 
    set_names(imap(., ~ paste0("iter_", .y))) -> VPdata
  
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
    mutate(identifier = paste0("spp", species, "_", iteration), 
           scenario = scenarioNum) %>% 
    left_join(., prevalence) -> fullData
  return(fullData)
  
}
