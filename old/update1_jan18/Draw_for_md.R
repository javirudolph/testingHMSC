library(vcd)

par(xpd = TRUE, mfrow=c(1,1))


# Figure 2 - original -----------------------------------------------------


for(k in 1:4){
  vp <- readRDS(paste("VP Fig2", letters[k],".RDS",sep=""))
  
  ### Extract and organise each fractions
  vpOverlap1 <- lapply(vp, function(x) x$overlap1)
  vpOverlap2 <- lapply(vp, function(x) x$overlap2)
  vpOverlap3 <- lapply(vp, function(x) x$overlap3)
  
  ### Result object
  vpAll <- vpOverlap1
  
  ### Names of the individual fractions
  overlap1Names <- colnames(vpOverlap1[[1]])
  
  ### Find fractions that overlaps twice
  overlap2Names <- sapply(strsplit(colnames(vpOverlap2[[1]]),"-"),function(x) x)
  noverlap2Names <- ncol(overlap2Names)
  
  overlap2Random <- which(overlap2Names == "random",arr.ind=TRUE)[,2]
  overlap2NotRandom <- (1:noverlap2Names)[-overlap2Random]
  overlap2NamesRandom <- overlap2Names[,overlap2Random]
  overlap2RandomNotRandom <- which(overlap2NamesRandom != "random",arr.ind=TRUE)
  
  ### Add the right random fraction to the spatial and environmental fractions, respectively
  for(i in 1:length(overlap2Random)){
    addTo <- which(overlap1Names == overlap2Names[overlap2RandomNotRandom[i,1],overlap2RandomNotRandom[i,2]])
    for(j in 1:length(vpAll)){
      vpAll[[j]][,addTo] <- vpAll[[j]][,addTo] + vpOverlap2[[j]][,overlap2Random[i]]
    }
  }
  
  ### Add half of the env-spa fraction to the spatial and environmental fractions, respectively
  envFrac <- which(colnames(vpAll[[1]]) == "env")
  spaFrac <- which(colnames(vpAll[[1]]) == "spa")
  
  for(j in 1:length(vpAll)){
    vpAll[[j]][,envFrac] <- vpAll[[j]][,envFrac] + vpOverlap2[[j]][,overlap2NotRandom]/2
    vpAll[[j]][,spaFrac] <- vpAll[[j]][,spaFrac] + vpOverlap2[[j]][,overlap2NotRandom]/2
  }
  
  ### Add half of the env-spa-random fraction to the spatial and environmental fractions, respectively
  envFrac <- which(colnames(vpAll[[1]]) == "env")
  spaFrac <- which(colnames(vpAll[[1]]) == "spa")
  
  for(j in 1:length(vpAll)){
    vpAll[[j]][,envFrac] <- vpAll[[j]][,envFrac] + vpOverlap3[[j]]/2
    vpAll[[j]][,spaFrac] <- vpAll[[j]][,spaFrac] + vpOverlap3[[j]]/2
  }
  
  ### Reorganise for plotting
  vpAllOrg<-vpAll
  for(j in 1:length(vpAll)){
    vpAllOrg[[j]] <- vpAll[[j]][,c(3,2,1)]
  }
  
  ### replace negative values by 0
  vpAllClean <- lapply(vpAllOrg,function(x) ifelse(x < 0, 0, x))
  
  couleur <- rep(rainbow(15), 5)
  
  png(filename = paste0("fig2", letters[k], ".png"))
  vpAllToPlot <- rbind(vpAllClean[[1]], vpAllClean[[2]], vpAllClean[[3]], vpAllClean[[4]], vpAllClean[[5]])
  plot(0,0,type="n",axes= FALSE,xlab= "", ylab = "")
  points(seq(0.6,1,by = 0.1), rep(0.9,5), pch = 19, cex = seq(0.2,1.2,by = 0.2)*2)
  text(seq(0.6,1,by = 0.1), rep(0.85,5), labels = seq(0.2,1,by = 0.2), pos = 1, cex = 0.75)
  
  ternaryplot(vpAllToPlot,
              dimnames = c("Environment","Spatial\nAutocorrelation",
                           "Co-Distribution"),
              bg = "lightgray",
              grid_color = "white",
              col = couleur,
              cex = rowSums(vpAllToPlot),
              main = letters[k],
              newpage = FALSE)
  dev.off()
}



# Figure 2 - indiv fractions ----------------------------------------------

for(k in 1:4){
  vp <- readRDS(paste("VP Fig2", letters[k],".RDS",sep=""))
  
  ### Extract and organise each fractions
  vpOverlap1 <- lapply(vp, function(x) x$overlap1)
  vpOverlap2 <- lapply(vp, function(x) x$overlap2)
  vpOverlap3 <- lapply(vp, function(x) x$overlap3)
  
  ### Result object
  vpAll <- vpOverlap1
  
  ### Names of the individual fractions
  overlap1Names <- colnames(vpOverlap1[[1]])
  
  ### Find fractions that overlaps twice
  # overlap2Names <- sapply(strsplit(colnames(vpOverlap2[[1]]),"-"),function(x) x)
  # noverlap2Names <- ncol(overlap2Names)
  # 
  # overlap2Random <- which(overlap2Names == "random",arr.ind=TRUE)[,2]
  # overlap2NotRandom <- (1:noverlap2Names)[-overlap2Random]
  # overlap2NamesRandom <- overlap2Names[,overlap2Random]
  # overlap2RandomNotRandom <- which(overlap2NamesRandom != "random",arr.ind=TRUE)
  
  ### Add the right random fraction to the spatial and environmental fractions, respectively
  # for(i in 1:length(overlap2Random)){
  #   addTo <- which(overlap1Names == overlap2Names[overlap2RandomNotRandom[i,1],overlap2RandomNotRandom[i,2]])
  #   for(j in 1:length(vpAll)){
  #     vpAll[[j]][,addTo] <- vpAll[[j]][,addTo] + vpOverlap2[[j]][,overlap2Random[i]]
  #   }
  # }
  # 
  # ### Add half of the env-spa fraction to the spatial and environmental fractions, respectively
  # envFrac <- which(colnames(vpAll[[1]]) == "env")
  # spaFrac <- which(colnames(vpAll[[1]]) == "spa")
  # 
  # for(j in 1:length(vpAll)){
  #   vpAll[[j]][,envFrac] <- vpAll[[j]][,envFrac] + vpOverlap2[[j]][,overlap2NotRandom]/2
  #   vpAll[[j]][,spaFrac] <- vpAll[[j]][,spaFrac] + vpOverlap2[[j]][,overlap2NotRandom]/2
  # }
  # 
  # ### Add half of the env-spa-random fraction to the spatial and environmental fractions, respectively
  # envFrac <- which(colnames(vpAll[[1]]) == "env")
  # spaFrac <- which(colnames(vpAll[[1]]) == "spa")
  
  # for(j in 1:length(vpAll)){
  #   vpAll[[j]][,envFrac] <- vpAll[[j]][,envFrac] + vpOverlap3[[j]]/2
  #   vpAll[[j]][,spaFrac] <- vpAll[[j]][,spaFrac] + vpOverlap3[[j]]/2
  # }
  
  ### Reorganise for plotting
  vpAllOrg<-vpAll
  for(j in 1:length(vpAll)){
    vpAllOrg[[j]] <- vpAll[[j]][,c(3,2,1)]
  }
  
  ### replace negative values by 0
  vpAllClean <- lapply(vpAllOrg,function(x) ifelse(x < 0, 0, x))
  
  couleur <- rep(rainbow(15), 5)
  
  png(filename = paste0("purefig2", letters[k], ".png"))
  vpAllToPlot <- rbind(vpAllClean[[1]], vpAllClean[[2]], vpAllClean[[3]], vpAllClean[[4]], vpAllClean[[5]])
  plot(0,0,type="n",axes= FALSE,xlab= "", ylab = "")
  points(seq(0.6,1,by = 0.1), rep(0.9,5), pch = 19, cex = seq(0.2,1.2,by = 0.2)*2)
  text(seq(0.6,1,by = 0.1), rep(0.85,5), labels = seq(0.2,1,by = 0.2), pos = 1, cex = 0.75)
  
  par(xpd = TRUE)
  ternaryplot(vpAllToPlot,
              dimnames = c("Environment","Spatial\nAutocorrelation",
                           "Co-Distribution"),
              bg = "lightgray",
              grid_color = "white",
              col = couleur,
              cex = rowSums(vpAllToPlot),
              main = letters[k],
              newpage = FALSE)
  dev.off()
}

#########################################################################################################
# FIGURE 3

# Figure 3 - original -----------------------------------------------------

for(k in 1:3){
  vp <- readRDS(paste("VP Fig3", letters[k],".RDS",sep=""))
  
  ### Extract and organise each fractions
  vpOverlap1 <- lapply(vp, function(x) x$overlap1)
  vpOverlap2 <- lapply(vp, function(x) x$overlap2)
  vpOverlap3 <- lapply(vp, function(x) x$overlap3)
  
  ### Result object
  vpAll <- vpOverlap1
  
  ### Names of the individual fractions
  overlap1Names <- colnames(vpOverlap1[[1]])
  
  ### Find fractions that overlaps twice
  overlap2Names <- sapply(strsplit(colnames(vpOverlap2[[1]]),"-"),function(x) x)
  noverlap2Names <- ncol(overlap2Names)
  
  overlap2Random <- which(overlap2Names == "random",arr.ind=TRUE)[,2]
  overlap2NotRandom <- (1:noverlap2Names)[-overlap2Random]
  overlap2NamesRandom <- overlap2Names[,overlap2Random]
  overlap2RandomNotRandom <- which(overlap2NamesRandom != "random",arr.ind=TRUE)
  
  ### Add the right random fraction to the spatial and environmental fractions, respectively
  for(i in 1:length(overlap2Random)){
    addTo <- which(overlap1Names == overlap2Names[overlap2RandomNotRandom[i,1],overlap2RandomNotRandom[i,2]])
    for(j in 1:length(vpAll)){
      vpAll[[j]][,addTo] <- vpAll[[j]][,addTo] + vpOverlap2[[j]][,overlap2Random[i]]
    }
  }
  
  ### Add half of the env-spa fraction to the spatial and environmental fractions, respectively
  envFrac <- which(colnames(vpAll[[1]]) == "env")
  spaFrac <- which(colnames(vpAll[[1]]) == "spa")
  
  for(j in 1:length(vpAll)){
    vpAll[[j]][,envFrac] <- vpAll[[j]][,envFrac] + vpOverlap2[[j]][,overlap2NotRandom]/2
    vpAll[[j]][,spaFrac] <- vpAll[[j]][,spaFrac] + vpOverlap2[[j]][,overlap2NotRandom]/2
  }
  
  ### Add half of the env-spa-random fraction to the spatial and environmental fractions, respectively
  envFrac <- which(colnames(vpAll[[1]]) == "env")
  spaFrac <- which(colnames(vpAll[[1]]) == "spa")
  
  for(j in 1:length(vpAll)){
    vpAll[[j]][,envFrac] <- vpAll[[j]][,envFrac] + vpOverlap3[[j]]/2
    vpAll[[j]][,spaFrac] <- vpAll[[j]][,spaFrac] + vpOverlap3[[j]]/2
  }
  
  ### Reorganise for plotting
  vpAllOrg<-vpAll
  for(j in 1:length(vpAll)){
    vpAllOrg[[j]] <- vpAll[[j]][,c(3,2,1)]
  }
  
  ### replace negative values by 0
  vpAllClean <- lapply(vpAllOrg,function(x) ifelse(x < 0, 0, x))
  
  if(k == 1){
    couleur <- rep(c(colors()[498:504],rgb(0,0,seq(0.2,0.8,length.out = 8))), 5)
  }
  
  if(k == 2){
    couleur <- rep(c(colors()[498:502],colors()[547:551], rgb(0,0,seq(0.2,0.8,length.out = 5))), 5)
  }
  
  if(k == 3){
    couleur <- rep(colorRampPalette(c("orange","blue"))(15), 5)
  }
  vpAllToPlot <- rbind(vpAllClean[[1]], vpAllClean[[2]], vpAllClean[[3]], vpAllClean[[4]], vpAllClean[[5]])
  plot(0,0,type="n",axes= FALSE,xlab= "", ylab = "")
  points(seq(0.6,1,by = 0.1), rep(0.9,5), pch = 19, cex = seq(0.2,1.2,by = 0.2)*2)
  text(seq(0.6,1,by = 0.1), rep(0.85,5), labels = seq(0.2,1,by = 0.2), pos = 1, cex = 0.75)
  
  par(xpd = TRUE)
  png(paste0("fig3", letters[k], ".png"))
  ternaryplot(vpAllToPlot,
              dimnames = c("Environment","Spatial\nAutocorrelation",
                           "Co-Distribution"),
              bg = "lightgray",
              grid_color = "white",
              col = couleur,
              cex = rowSums(vpAllToPlot),
              main = letters[k],
              newpage = FALSE)
  dev.off()
}


# Figure3 pur -------------------------------------------------------------


for(k in 1:3){
  vp <- readRDS(paste("VP Fig3", letters[k],".RDS",sep=""))
  
  ### Extract and organise each fractions
  vpOverlap1 <- lapply(vp, function(x) x$overlap1)
  vpOverlap2 <- lapply(vp, function(x) x$overlap2)
  vpOverlap3 <- lapply(vp, function(x) x$overlap3)
  
  ### Result object
  vpAll <- vpOverlap1
  
  ### Names of the individual fractions
  overlap1Names <- colnames(vpOverlap1[[1]])
  
  ### Find fractions that overlaps twice
  # overlap2Names <- sapply(strsplit(colnames(vpOverlap2[[1]]),"-"),function(x) x)
  # noverlap2Names <- ncol(overlap2Names)
  # 
  # overlap2Random <- which(overlap2Names == "random",arr.ind=TRUE)[,2]
  # overlap2NotRandom <- (1:noverlap2Names)[-overlap2Random]
  # overlap2NamesRandom <- overlap2Names[,overlap2Random]
  # overlap2RandomNotRandom <- which(overlap2NamesRandom != "random",arr.ind=TRUE)
  # 
  # ### Add the right random fraction to the spatial and environmental fractions, respectively
  # for(i in 1:length(overlap2Random)){
  #   addTo <- which(overlap1Names == overlap2Names[overlap2RandomNotRandom[i,1],overlap2RandomNotRandom[i,2]])
  #   for(j in 1:length(vpAll)){
  #     vpAll[[j]][,addTo] <- vpAll[[j]][,addTo] + vpOverlap2[[j]][,overlap2Random[i]]
  #   }
  # }
  # 
  # ### Add half of the env-spa fraction to the spatial and environmental fractions, respectively
  # envFrac <- which(colnames(vpAll[[1]]) == "env")
  # spaFrac <- which(colnames(vpAll[[1]]) == "spa")
  # 
  # for(j in 1:length(vpAll)){
  #   vpAll[[j]][,envFrac] <- vpAll[[j]][,envFrac] + vpOverlap2[[j]][,overlap2NotRandom]/2
  #   vpAll[[j]][,spaFrac] <- vpAll[[j]][,spaFrac] + vpOverlap2[[j]][,overlap2NotRandom]/2
  # }
  # 
  # ### Add half of the env-spa-random fraction to the spatial and environmental fractions, respectively
  # envFrac <- which(colnames(vpAll[[1]]) == "env")
  # spaFrac <- which(colnames(vpAll[[1]]) == "spa")
  # 
  # for(j in 1:length(vpAll)){
  #   vpAll[[j]][,envFrac] <- vpAll[[j]][,envFrac] + vpOverlap3[[j]]/2
  #   vpAll[[j]][,spaFrac] <- vpAll[[j]][,spaFrac] + vpOverlap3[[j]]/2
  # }
  
  ### Reorganise for plotting
  vpAllOrg<-vpAll
  for(j in 1:length(vpAll)){
    vpAllOrg[[j]] <- vpAll[[j]][,c(3,2,1)]
  }
  
  ### replace negative values by 0
  vpAllClean <- lapply(vpAllOrg,function(x) ifelse(x < 0, 0, x))
  
  if(k == 1){
    couleur <- rep(c(colors()[498:504],rgb(0,0,seq(0.2,0.8,length.out = 8))), 5)
  }
  
  if(k == 2){
    couleur <- rep(c(colors()[498:502],colors()[547:551], rgb(0,0,seq(0.2,0.8,length.out = 5))), 5)
  }
  
  if(k == 3){
    couleur <- rep(colorRampPalette(c("orange","blue"))(15), 5)
  }
  vpAllToPlot <- rbind(vpAllClean[[1]], vpAllClean[[2]], vpAllClean[[3]], vpAllClean[[4]], vpAllClean[[5]])
  plot(0,0,type="n",axes= FALSE,xlab= "", ylab = "")
  points(seq(0.6,1,by = 0.1), rep(0.9,5), pch = 19, cex = seq(0.2,1.2,by = 0.2)*2)
  text(seq(0.6,1,by = 0.1), rep(0.85,5), labels = seq(0.2,1,by = 0.2), pos = 1, cex = 0.75)
  
  par(xpd = TRUE)
  png(paste0("purefig3", letters[k], ".png"))
  ternaryplot(vpAllToPlot,
              dimnames = c("Environment","Spatial\nAutocorrelation",
                           "Co-Distribution"),
              bg = "lightgray",
              grid_color = "white",
              col = couleur,
              cex = rowSums(vpAllToPlot),
              main = letters[k],
              newpage = FALSE)
  dev.off()
}
