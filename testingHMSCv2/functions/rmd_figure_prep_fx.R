# functions required for the Rmarkdown reports to organize data, parameters and such, to create the figures and their associated parameters.

#Just get the parameters
get_params_table <- function(folderpath, scenario){
  params <- with(readRDS(paste0(folderpath, scenario, "-params.RDS")), {
    enframe(u_c[1,], name = "species",value =  "nicheOpt") %>% 
      left_join(., enframe(s_c[1,], name = "species", value =  "nicheBreadth")) %>% 
      left_join(., enframe(c_0, name = "species", value =  "colProb")) %>% 
      mutate(dispersal = alpha, 
             species = as.character(species), 
             intercol = d_c, 
             interext = d_e)
  })
  
}

# Get a table of parameters for the rmd report. It will give the average prevalence as well
get_params_with_prev <- function(folderpath, scenario){
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





# Master function ---------------------------------------------------------


# Organize the VP overlaps into actual env, spa and codist components
organize_vp_overlaps <- function(folderpath, scenario, indSites = FALSE){
  
  
  if(indSites == TRUE){
    richness <- readRDS(paste0(folderpath, scenario, "-metacomSim.RDS")) %>% 
      set_names(imap(., ~ paste0("iter", .y))) %>% 
      map(., rowSums) %>%
      bind_rows() %>% 
      rownames_to_column(var = "sites") %>% 
      gather(., key = "iteration", value = "richness", -sites) %>% 
      mutate(identifier = paste0("site", sites, "_", iteration)) %>% 
      select(., -c(sites, iteration))
    
    
    
    vp <- readRDS(paste0(folderpath, scenario, "-vpsites.RDS"))
    
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
             scenario = scenario) %>% 
      left_join(., richness) -> vpALL
    
    return(vpALL)
  }
  
  
  
  
  
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
    mutate(identifier = paste0("spp", species, "_", iteration)) %>% 
    select(., -c(species, iteration))
  
  
  
  
  readRDS(paste0(folderpath, scenario, "-vpspp.RDS")) %>% 
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
           scenario = scenario) %>% 
    left_join(., prevalence) -> fullData
  return(fullData)
  
}


# Plotting function -------------------------------------------------------

make_tern_plot <- function(figData, varShape = NULL, varColor = NULL){
  figData %>% 
    ggtern(aes(x = env, z = spa, y = codist, size = r2)) +
    scale_T_continuous(limits=c(0,1.0),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    scale_L_continuous(limits=c(0.0,1),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    scale_R_continuous(limits=c(0.0,1.0),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    labs(x = "Environment",
         y = "Codistribution",
         z = "Spatial") +
    geom_point(aes_string(shape = varShape, color = varColor), alpha = 0.7) +
    theme_minimal() +
    guides(size = guide_legend(order = 1,
                               title = expression(paste(R^{2}))),
           shape = guide_legend("none"),
           # shape = guide_legend(order = 2,
           #                      title = varShape,
           #                      override.aes = list(size = 3)),
           col = guide_colourbar(title = varColor,
                                 order = 3)) +
    theme(#legend.position = "bottom", legend.box = "vertical",
      panel.grid = element_line(color = "darkgrey"),
      axis.title = element_text(size = 7))
}
  

# Arrange plots -----------------------------------------------------------
arrange_plots <- function(folderpath, scenario, colorSpp, colorSites, leftlabel){
  organize_vp_overlaps(folderpath, scenario, indSites = FALSE) %>% 
    make_tern_plot(., varShape = "iteration", varColor = colorSpp) -> sppPlot
  
  organize_vp_overlaps(folderpath, scenario, indSites = TRUE) %>% 
  make_tern_plot(., varShape = "iteration", varColor = colorSites) -> sitesPlot
  
  grid.arrange(arrangeGrob(sppPlot),
               arrangeGrob(sitesPlot),
               ncol = 2, left = leftlabel)
}


