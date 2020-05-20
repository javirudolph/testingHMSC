# ** get_sites_data() to organize site level data and get ready for plot

# This function takes the simulated metacommunity to get the species richness for each site
# It then binds that information to the Variation Partinioning output from the HMSC::variPart()
# This function also takes the output of HMSC::variPart() and organizes each fraction:
#   into the env, spa and codist components
# The output of the get_sites_data() function is a dataframe that is ready to plot

# Modify the data from vpsites

#library(tidyverse)
#library(ggtern)

#' @title Organize and format site level data
#' @description Manuscript specific function. This function takes the simulated metacommunity to get the species richness for each site. It then binds that information to the Variation Partinioning output from the HMSC::variPart(). This function also takes the output of HMSC::variPart() and organizes each fraction: into the env, spa and codist components
#' @param folderpath path to stored RDS files from the get_VP_results(), variation partitioning
#' @param scenario the specific name of the file to use. Should be able to access both the metaco simulation, the model and the VP results with this name.
#'
#'

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
    workingVP1 <- overlap1[[i]]
    workingVP2 <- overlap2[[i]]
    workingVP3 <- overlap3[[i]]
    
    c <- rowSums(workingVP1[,,1])/nspp
    b <- rowSums(workingVP1[,,2])/nspp
    a <- rowSums(workingVP1[,,3])/nspp
    
    e <- rowSums(workingVP2[,,1])/nspp
    f <- rowSums(workingVP2[,,2])/nspp
    d <- rowSums(workingVP2[,,3])/nspp
    
    g <- rowSums(workingVP3)/nspp
    
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


# ** get_species_data() this function organizes species level data and gets it ready to plot.
# It takes the RDS file that is the output of the HMSC::variPart() function and reorganizes it into the 
# env, codist and spa components.
# It also calculates the prevalence information for each species.
#' @title Organize and Format VP results at the species level
#' @description this function organizes species level data and gets it ready to plot. It takes the RDS file that is the output of the HMSC::variPart() function and reorganizes it into the env, codist and spa components.It also calculates the prevalence information for each species.
#' organizes each fraction: into the env, spa and codist components
#' @param folderpath path to stored RDS files from the get_VP_results()
#' @param scenario the specific name of the file to use. Should be able to access the both the metacomSim, the model and the VP results with this name.
#'


get_species_data <- function(folderpath, scenario){
  
  prevalence <- readRDS(paste0(folderpath, scenario, "-metacomSim.RDS")) %>% 
    set_names(imap(., ~ paste0("iter_", .y))) %>% 
    map(., colSums) %>%
    bind_cols() %>% 
    rownames_to_column(var = "species") %>% 
    gather(., key = "iteration", value = "prevalence", -species) %>% 
    mutate(identifier = paste0("spp", species, "_", iteration)) %>% 
    dplyr::select(., -c(species, iteration))
  
  
  
  
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
                iteration = names(VPdata[i]))
    
  }
  
  fullData %>% 
    bind_rows() %>% 
    mutate(identifier = paste0("spp", species, "_", iteration), 
           scenario = scenario) %>% 
    left_join(., prevalence)
  
}


# ** get_fig2_params() This function is for Figure 2 only. 
# It takes the RDS params file and organizes it from a list to a dataframe, so that it can be joined to the
# Variation Partition results at the species level. That way we can know what niche values each species has.
# The output of this function can be joined to the VP results at the species level and then plot

#' @title Get parameters for Figure 2
#' @description  Manuscript specific. This function is for Figure 2 only.It takes the RDS params file and organizes it from a list to a dataframe, so that it can be joined to the Variation Partition results at the species level. That way we can know what niche values each species has. The output of this function can be joined to the VP results at the species level and then plot.
#' @param folderpath path to stored RDS files from the get_VP_results()
#' @param scenario the specific name of the file to use. Should be able to access the both the metacomSim, the model and the VP results with this name.
#'

get_fig2_params <- function(folderpath, scenario){
  pars <- readRDS(paste0(folderpath, scenario, "-params.RDS"))
  
  with(pars, {enframe(u_c[1,], name = "species", value = "nicheOptima") %>% 
      left_join(., enframe(s_c[1,], name = "species", value = "nicheBreadth")) %>% 
      left_join(., enframe(c_0, name = "species", value = "colonizationProb")) %>% 
      mutate(., dispersal = alpha,
             species = as.character(species),
             #speciesChr = as.character(paste0("spp_", species)),
             interCol = d_c ,
             interExt = d_e)})
}

# ** get_fig3_params() this is for Figure 3 only. 
# Figure 3 has three sets of parameters because we divided simulations into groups.
# Half of the species in each simulation has interactions, and the other half doesn't.
# Or, a third of the species is assigned a different dispersal level, etc...
#' @title Get parameters for Figure 3
#' @description Manuscript specific, figure 3 only.Figure 3 has three sets of parameters because we divided simulations into groups.Half of the species in each simulation has interactions, and the other half doesn't.Or, a third of the species is assigned a different dispersal level.
#' @param folderpath path to stored RDS files from the get_VP_results()
#' @param scenario the specific name of the file to use. Should be able to access the both the metacomSim, the model and the VP results with this name.
#'

get_fig3_params <- function(folderpath, scenario){
  parsList <- readRDS(paste0(folderpath, scenario, "-params.RDS"))
  fullPars <-  data.frame()
  for(i in 1:length(parsList)){
    
    nspp <- nrow(fullPars)
    
    i_pars <- with(parsList[[i]], {
      enframe(u_c[1,], name = "species",value =  "nicheOptima") %>% 
        left_join(., enframe(s_c[1,], name = "species", value =  "nicheBreadth")) %>% 
        left_join(., enframe(c_0, name = "species", value =  "colonizationProb")) %>% 
        mutate(dispersal = as.factor(alpha), 
               species = as.character(species + nspp), 
               interCol = d_c, 
               interExt = d_e)})
    
    
    fullPars <- rbind(fullPars, i_pars)
    
  }
  
  return(fullPars)
}



# Make the Figures
#' @title Base Plots: Species
#' @description Base species plot
#' @param data formatted data from get_spp_data()
#' @param plotMain main title for the figure
#' @param colorVar variable for the color variation. Color by species or by niche optima
#' @param colorLegend should the color legend be included, what is the title?
#'

species_plot <- function(data, plotMain = NULL, colorVar = NULL, colorLegend = "none"){
  data %>% 
    ggtern(aes(x = env, z = spa, y = codist, size = r2)) +
    geom_point(aes_string(color = colorVar), alpha = 0.8) +
    scale_T_continuous(limits=c(0,1.0),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    scale_L_continuous(limits=c(0.0,1),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    scale_R_continuous(limits=c(0.0,1.0),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    labs(title = plotMain,
         x = "E",
         xarrow = "Environment",
         y = "C",
         yarrow = "Co-Distribution",
         z = "S", 
         zarrow = "Spatial Autocorrelation") +
    theme_light() +
    theme_showarrows() +
    #scale_colour_brewer(palette = "Set1") +
    #scale_colour_brewer(palette = "Spectral") +
    #scale_color_viridis_d() +
    scale_size_area(limits = c(0,1), breaks = seq(0,1,0.2)) +
    guides(color = guide_legend(colorLegend, order = 2), 
           size = guide_legend(title = expression(R^2), order = 1)) +
    theme(panel.grid = element_line(color = "darkgrey"),
          axis.title = element_text(size = 8))
}

# Plot sites by dots

#' @title Base Plots: Sites
#' @description Base species plot
#' @param data formatted data from get_sites_data()
#' @param plotMain main title for the figure
#' @param colorVar variable for the color variation. Color by species or by niche optima
#' @param colorLegend should the color legend be included, what is the title?
#'
sites_plot <- function(data, plotMain = NULL, colorVar = NULL, colorLegend = "none"){
  data %>% 
    ggtern(aes(x = env, z = spa, y = codist, size = r2)) +
    geom_point(aes_string(color = colorVar), alpha = 0.6) +
    scale_T_continuous(limits=c(0,1.0),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    scale_L_continuous(limits=c(0.0,1),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    scale_R_continuous(limits=c(0.0,1.0),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    labs(title = plotMain,
         x = "E",
         xarrow = "Environment",
         y = "C",
         yarrow = "Co-Distribution",
         z = "S", 
         zarrow = "Spatial Autocorrelation") +
    theme_light() +
    theme_showarrows() +
    #scale_colour_brewer(palette = "Set1") +
    #scale_colour_brewer(palette = "Spectral") +
    #scale_color_viridis_d() +
    scale_size_area(limits = c(0, 0.003), breaks = seq(0, 0.003, 0.0005)) +
    guides(color = guide_colorbar(colorLegend), 
           size = guide_legend(title = expression(R^2))) +
    theme(panel.grid = element_line(color = "darkgrey"),
          axis.title = element_text(size = 8))
}


# To create the species correlation matrices:
# This will mostly follow the code provided from the HMSC vignette, therefore it is not in a tidyverse framework and requires additional packages

#' @title Create the species correlation matrices
#' @description This mostly follows the code provided from the HMSC vignette, therefore it is not in a tidyverse framework and requires the corrplot package
#' @param folderpath path to stored RDS files from the get_VP_results()
#' @param scenario the specific name of the file to use. Should be able to access the HMSC model.
#' @param iteration Which iteration of the model should it use? Default is 1
#' @param corTitle Title for the correlation plot


interaction_plot <- function(folderpath, scenario, iteration = NULL, corTitle = NULL){
  
  if(is.null(iteration) == TRUE){
    iteration <- 1
  }
  
  
  modelfile <- readRDS(paste0(folderpath, scenario, "-model.RDS"))
  assoMat <- corRandomEff(modelfile[[iteration]])
  siteMean <- apply(assoMat[ , , , 1], 1:2, mean)
  
  siteDrawCol <- matrix(NA, nrow = nrow(siteMean), ncol = ncol(siteMean))
  siteDrawCol[which(siteMean > 0.4, arr.ind=TRUE)]<-"red"
  siteDrawCol[which(siteMean < -0.4, arr.ind=TRUE)]<-"blue"
  
  # Build matrix of "significance" for corrplot
  siteDraw <- siteDrawCol
  siteDraw[which(!is.na(siteDraw), arr.ind = TRUE)] <- 0
  siteDraw[which(is.na(siteDraw), arr.ind = TRUE)] <- 1
  siteDraw <- matrix(as.numeric(siteDraw), nrow = nrow(siteMean), ncol = ncol(siteMean))
  
  Colour <- colorRampPalette(c("blue", "white", "red"))(200)
  corrplot(siteMean, method = "color", col = Colour, type = "lower",
           diag = FALSE, p.mat = siteDraw, tl.srt = 45, title = corTitle)
  
}



# Figures functions from Fig2and3
#' @title Base Plots: Species
#' @description Base species plot
#' @param data formatted data from get_spp_data()
#' @param plotMain main title for the figure
#' @param colorVar variable for the color variation. Color by species or by niche optima
#' @param colorLegend should the color legend be included, what is the title?
#'
base_spp_plot <- function(data, plotMain = NULL, colorVar = NULL, colorLegend = "none"){
  data %>% 
    ggtern(aes(x = env, z = spa, y = codist, size = r2)) +
    geom_point(aes_string(color = colorVar), alpha = 0.8) +
    scale_T_continuous(limits=c(0.0,1.0),
                       breaks=seq(0.0,1.0,by=0.1),
                       labels=seq(0.0,1.0,by=0.1)) +
    scale_L_continuous(limits=c(0.0,1),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    scale_R_continuous(limits=c(0.0,1.0),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    labs(title = plotMain,
         x = "E",
         xarrow = "Environment",
         y = "C",
         yarrow = "Co-Distribution",
         z = "S", 
         zarrow = "Spatial Autocorrelation") +
    theme_light() +
    theme_showarrows() +
    #scale_colour_brewer(palette = "Set1") +
    #scale_colour_brewer(palette = "Spectral") +
    #scale_color_viridis_d() +
    scale_size_area(limits = c(0,1), breaks = seq(0,1,0.2)) +
    guides(color = guide_legend(colorLegend, order = 2), 
           size = guide_legend(title = expression(R^2), order = 1)) +
    theme(panel.grid = element_line(color = "darkgrey"),
          axis.text = element_text(size =5),
          axis.title = element_text(size = 8),
          plot.title = element_text(size = 12, margin = margin(t = 10, b = -20)),
          tern.axis.arrow = element_line(size = 1))
}

#' @title Base Plots: Sites
#' @description Base species plot
#' @param data formatted data from get_sites_data()
#' @param plotMain main title for the figure
#' @param colorVar variable for the color variation. Color by species or by niche optima
#' @param colorLegend should the color legend be included, what is the title?
#'
#'
base_sites_plot <- function(data, plotMain = NULL, colorVar = NULL, colorLegend = "none"){
  data %>% 
    ggtern(aes(x = env, z = spa, y = codist, size = r2)) +
    geom_point(aes_string(color = colorVar), alpha = 0.6) +
    scale_T_continuous(limits=c(0,1.0),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    scale_L_continuous(limits=c(0.0,1),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    scale_R_continuous(limits=c(0.0,1.0),
                       breaks=seq(0,1,by=0.1),
                       labels=seq(0,1,by=0.1)) +
    labs(title = plotMain,
         x = "E",
         xarrow = "Environment",
         y = "C",
         yarrow = "Co-Distribution",
         z = "S", 
         zarrow = "Spatial Autocorrelation") +
    theme_light() +
    theme_showarrows() +
    #scale_colour_brewer(palette = "Set1") +
    #scale_colour_brewer(palette = "Spectral") +
    #scale_color_viridis_d() +
    scale_size_area(limits = c(0, 0.003), breaks = seq(0, 0.003, 0.0005)) +
    guides(color = guide_colorbar(colorLegend, order = 2), 
           size = guide_legend(title = expression(R^2), order = 1)) +
    theme(panel.grid = element_line(color = "darkgrey"),
          axis.text = element_text(size =5),
          axis.title = element_text(size = 8),
          plot.title = element_text(size = 12, margin = margin(t = 10, b = -20)),
          tern.axis.arrow = element_line(size = 1))
}

