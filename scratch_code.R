


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
  
  overlap1 <- map(vp, "overlap1")
  nspp <- as.numeric(dim(overlap1[[1]])[2])
  overlap2 <- map(vp, "overlap2")
  overlap3 <- map(vp, "overlap3")
  
  vpALL <- vector("list", length = 5)
  for(i in 1:5){
    workingVP1 <- ifelse(overlap1[[i]]<0, 0, overlap1[[i]])
    workingVP2 <- ifelse(overlap2[[i]]<0, 0, overlap2[[i]])
    workingVP3 <- ifelse(overlap3[[i]]<0, 0, overlap3[[i]])
    
    c <- rowSums(workingVP1[,,1])
    b <- rowSums(workingVP1[,,2])
    a <- rowSums(workingVP1[,,3])
    
    e <- rowSums(workingVP2[,,1])
    f <- rowSums(workingVP2[,,2])
    d <- rowSums(workingVP2[,,3])
    
    g <- rowSums(workingVP3)
    
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
           scenario = scenario) %>% 
    left_join(., richness)
}
