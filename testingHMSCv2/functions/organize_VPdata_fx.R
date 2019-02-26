
# Organize VP data into the right components

organize_VPdata_species <- function(VPdata){
  VPdata %>% 
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
    bind_rows() -> fullData
  return(fullData)
}

# Still need to do this for Site level stuff