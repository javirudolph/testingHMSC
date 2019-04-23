
library(tidyverse)
library(vcd)


for(k in 1:4){

  vp <- readRDS(paste("VPsiteFig2", letters[k],".RDS",sep=""))
  
  ### Extract and organise each fractions
  
  
  overlap1 <- map(vp, "overlap1")
  overlap2 <- map(vp, "overlap2")
  overlap3 <- map(vp, "overlap3")
  
  vpALL <- vector("list", length = 5)
  for(i in 1:5){
    workingVP1 <- overlap1[[i]]
    workingVP2 <- overlap2[[i]]
    workingVP3 <- overlap3[[i]]
    
    c <- rowSums(workingVP1[,,1])
    b <- rowSums(workingVP1[,,2])
    a <- rowSums(workingVP1[,,3])
    
    e <- rowSums(workingVP2[,,1])
    f <- rowSums(workingVP2[,,2])
    d <- rowSums(workingVP2[,,3])
    
    g <- rowSums(workingVP3)
    
    env <- a + f + 1/2 * d + 1/2 * g
    spa <- b + e + 1/2 * d + 1/2 * g
    random <- c
    iteration <- factor(paste0("iter", i), levels = paste0("iter", 1:5))
    
    vpALL[[i]] <- cbind.data.frame(env, spa, random, iteration)
    
  }
  
  #Just to check
  
  head(vpALL[[1]])
  head(vpALL[[2]])
  
  #####################################
  # Env = a + f + 1/2d + 1/2g
  # Spa = b + e + 1/2d + 1/2g
  # Random = c
  
  vpPlot <- vpALL %>% 
    reduce(bind_rows) %>% 
    mutate(env = ifelse(env < 0, 0, env),
           spa = ifelse(spa < 0, 0, spa),
           random = ifelse(random < 0, 0, random))
  
  
  # ggtern(vpPlot, aes(x = env, y = spa, z = random, color = iteration)) +
  #   geom_point(cex = rowSums(vpPlot[,1:3])) +
  #   scale_color_viridis_d() +
  #   labs(x = "Environment", y = "Spatial \n autocorrelation", z = "Co-distribution")
  
  cols <- NULL
  for(i in 1:5){
    a <- rep(rainbow(5)[i], 1000)
    cols <- c(cols, a)
  }
  
  png(filename = paste0("sitesfig2", letters[k], ".png"))
  plot(0,0,type="n",axes= FALSE,xlab= "", ylab = "")
  points(seq(0.6,1,by = 0.1), rep(0.9,5), pch = 19, cex = seq(0.2,1.2,by = 0.2)*2)
  text(seq(0.6,1,by = 0.1), rep(0.85,5), labels = seq(0.2,1,by = 0.2), pos = 1, cex = 0.75)
  par(xpd = TRUE)
  ternaryplot(vpPlot[,1:3] + 0.000001,
              dimnames = c("Environment","Spatial\nAutocorrelation",
                           "Co-Distribution"),
              bg = "lightgray",
              grid_color = "white",
              col = cols,
              cex = rowSums(vpPlot[1:3]),
              main = letters[k],
              newpage = FALSE)
  dev.off()
  
}

# Figure 3
for(k in 1:3){
  vp <- readRDS(paste("VPsiteFig3", letters[k],".RDS",sep=""))
  
  ### Extract and organise each fractions
  
  
  overlap1 <- map(vp, "overlap1")
  overlap2 <- map(vp, "overlap2")
  overlap3 <- map(vp, "overlap3")
  
  vpALL <- vector("list", length = 5)
  for(i in 1:5){
    workingVP1 <- overlap1[[i]]
    workingVP2 <- overlap2[[i]]
    workingVP3 <- overlap3[[i]]
    
    c <- rowSums(workingVP1[,,1])
    b <- rowSums(workingVP1[,,2])
    a <- rowSums(workingVP1[,,3])
    
    e <- rowSums(workingVP2[,,1])
    f <- rowSums(workingVP2[,,2])
    d <- rowSums(workingVP2[,,3])
    
    g <- rowSums(workingVP3)
    
    env <- a + f + 1/2 * d + 1/2 * g
    spa <- b + e + 1/2 * d + 1/2 * g
    random <- c
    iteration <- factor(paste0("iter", i), levels = paste0("iter", 1:5))
    
    vpALL[[i]] <- cbind.data.frame(env, spa, random, iteration)
    
  }
  
  #Just to check
  
  head(vpALL[[1]])
  head(vpALL[[2]])
  
  #####################################
  # Env = a + f + 1/2d + 1/2g
  # Spa = b + e + 1/2d + 1/2g
  # Random = c
  
  vpPlot <- vpALL %>% 
    reduce(bind_rows) %>% 
    mutate(env = ifelse(env < 0, 0, env),
           spa = ifelse(spa < 0, 0, spa),
           random = ifelse(random < 0, 0, random))
  
  
  # ggtern(vpPlot, aes(x = env, y = spa, z = random, color = iteration)) +
  #   geom_point(cex = rowSums(vpPlot[,1:3])) +
  #   scale_color_viridis_d() +
  #   labs(x = "Environment", y = "Spatial \n autocorrelation", z = "Co-distribution")
  
  cols <- NULL
  for(i in 1:5){
    a <- rep(rainbow(5)[i], 1000)
    cols <- c(cols, a)
  }
  
  png(filename = paste0("sitesfig3", letters[k], ".png"))
  plot(0,0,type="n",axes= FALSE,xlab= "", ylab = "")
  points(seq(0.6,1,by = 0.1), rep(0.9,5), pch = 19, cex = seq(0.2,1.2,by = 0.2)*2)
  text(seq(0.6,1,by = 0.1), rep(0.85,5), labels = seq(0.2,1,by = 0.2), pos = 1, cex = 0.75)
  par(xpd = TRUE)
  ternaryplot(vpPlot[,1:3] + 0.000001,
              dimnames = c("Environment","Spatial\nAutocorrelation",
                           "Co-Distribution"),
              bg = "lightgray",
              grid_color = "white",
              col = cols,
              cex = rowSums(vpPlot[1:3]),
              main = letters[k],
              newpage = FALSE)
  dev.off()
  
}



##############################################################################################

# Means instead of sums ---------------------------------------------------

# If we take the mean instead of the sum accross species. 

for(k in 1:4){
  
  vp <- readRDS(paste("VPsiteFig2", letters[k],".RDS",sep=""))
  
  ### Extract and organise each fractions
  
  
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
    spa <- b + e + 1/2 * d + 1/2 * g
    random <- c
    iteration <- factor(paste0("iter", i), levels = paste0("iter", 1:5))
    
    vpALL[[i]] <- cbind.data.frame(env, spa, random, iteration)
    
  }
  
  #Just to check
  
  head(vpALL[[1]])
  head(vpALL[[2]])
  
  #####################################
  # Env = a + f + 1/2d + 1/2g
  # Spa = b + e + 1/2d + 1/2g
  # Random = c
  
  vpPlot <- vpALL %>% 
    reduce(bind_rows) %>% 
    mutate(env = ifelse(env < 0, 0, env),
           spa = ifelse(spa < 0, 0, spa),
           random = ifelse(random < 0, 0, random))
  
  
  # ggtern(vpPlot, aes(x = env, y = spa, z = random, color = iteration)) +
  #   geom_point(cex = rowSums(vpPlot[,1:3])) +
  #   scale_color_viridis_d() +
  #   labs(x = "Environment", y = "Spatial \n autocorrelation", z = "Co-distribution")
  
  cols <- NULL
  for(i in 1:5){
    a <- rep(rainbow(5)[i], 1000)
    cols <- c(cols, a)
  }
  
  png(filename = paste0("sitesMeansfig2", letters[k], ".png"))
  plot(0,0,type="n",axes= FALSE,xlab= "", ylab = "")
  points(seq(0.6,1,by = 0.1), rep(0.9,5), pch = 19, cex = seq(0.2,1.2,by = 0.2)*2)
  text(seq(0.6,1,by = 0.1), rep(0.85,5), labels = seq(0.2,1,by = 0.2), pos = 1, cex = 0.75)
  par(xpd = TRUE)
  ternaryplot(vpPlot[,1:3] + 0.000001,
              dimnames = c("Environment","Spatial\nAutocorrelation",
                           "Co-Distribution"),
              bg = "lightgray",
              grid_color = "white",
              col = cols,
              cex = rowSums(vpPlot[1:3]),
              main = letters[k],
              newpage = FALSE)
  dev.off()
  
}

# Figure 3
for(k in 1:3){
  vp <- readRDS(paste("VPsiteFig3", letters[k],".RDS",sep=""))
  
  ### Extract and organise each fractions
  
  
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
    spa <- b + e + 1/2 * d + 1/2 * g
    random <- c
    iteration <- factor(paste0("iter", i), levels = paste0("iter", 1:5))
    
    vpALL[[i]] <- cbind.data.frame(env, spa, random, iteration)
    
  }
  
  #Just to check
  
  head(vpALL[[1]])
  head(vpALL[[2]])
  
  #####################################
  # Env = a + f + 1/2d + 1/2g
  # Spa = b + e + 1/2d + 1/2g
  # Random = c
  
  vpPlot <- vpALL %>% 
    reduce(bind_rows) %>% 
    mutate(env = ifelse(env < 0, 0, env),
           spa = ifelse(spa < 0, 0, spa),
           random = ifelse(random < 0, 0, random))
  
  
  # ggtern(vpPlot, aes(x = env, y = spa, z = random, color = iteration)) +
  #   geom_point(cex = rowSums(vpPlot[,1:3])) +
  #   scale_color_viridis_d() +
  #   labs(x = "Environment", y = "Spatial \n autocorrelation", z = "Co-distribution")
  
  cols <- NULL
  for(i in 1:5){
    a <- rep(rainbow(5)[i], 1000)
    cols <- c(cols, a)
  }
  
  png(filename = paste0("sitesMeansfig3", letters[k], ".png"))
  plot(0,0,type="n",axes= FALSE,xlab= "", ylab = "")
  points(seq(0.6,1,by = 0.1), rep(0.9,5), pch = 19, cex = seq(0.2,1.2,by = 0.2)*2)
  text(seq(0.6,1,by = 0.1), rep(0.85,5), labels = seq(0.2,1,by = 0.2), pos = 1, cex = 0.75)
  par(xpd = TRUE)
  ternaryplot(vpPlot[,1:3] + 0.000001,
              dimnames = c("Environment","Spatial\nAutocorrelation",
                           "Co-Distribution"),
              bg = "lightgray",
              grid_color = "white",
              col = cols,
              cex = rowSums(vpPlot[1:3]),
              main = letters[k],
              newpage = FALSE)
  dev.off()
  
}
