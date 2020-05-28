#
# The RDS files that I'll be loading here are the direct output of VariPart()

# Load libraries-------------
library(tidyverse)
library(ggtern)


# Load data and just consider the first iteration------------
vp_spp <- readRDS("FIG2A-vpspp.RDS")[[1]]
vp_sites <- readRDS("FIG2A-vpsites.RDS")[[1]]

# Species ------------------
vp_spp %>%
	map(as_tibble) %>%
	bind_cols() %>% 
	rownames_to_column() %>% 
	set_names(c("species", "c", "b", "a", "e", "f", "d", "g")) %>% 
	transmute(species = species,
						env = a + f + 0.5 * d + 0.5 * g,
						#env = ifelse(env < 0, 0, env),
						spa = b + e + 0.5 * d + 0.5 * g,
						#spa = ifelse(spa < 0, 0, spa),
						codist = c,
						#codist = ifelse(codist < 0, 0, codist),
						r2 = env + spa + codist) %>% 
	mutate_all(., function(x) ifelse(x<0, 0, x)) -> spp

# So, for species, the mean of R2 is the metacommunity R2
R2 <- signif(mean(spp$r2), digits = 3)

# Plot it:
spp %>% 
	ggtern(aes(x = env, z = spa, y = codist, size = r2)) +
				 	geom_point() +
	ggtitle(paste("Metacommunity R2 = ", R2))






# Sites ---------------------
vp_sites %>% 
	map(as_tibble) %>% 
	bind_cols() %>% 
	rownames_to_column(var = "site") %>% 
	pivot_longer(-site, names_to = "header", values_to = "values") %>% 
	mutate(specie = str_split(header, pattern = "\\.", n = 2, simplify = TRUE)[,1],
				 specie = as.numeric(str_replace(specie, "y", "")),
				 fraction = str_split(header, pattern = "\\.", n = 2, simplify = TRUE)[,2],
				 fraction = ifelse(fraction == "", "env-spa-random", fraction)) %>% 
	select(-header) %>% 
	pivot_wider(names_from = fraction, values_from = values) %>% 
	set_names(c("site", "species", "c", "b", "a", "e", "f", "d", "g")) %>% 
	mutate(site = as.numeric(site),
				 env = a + f + 0.5 * d + 0.5 * g,
				 #env = ifelse(env < 0, 0, env),
				 spa = b + e + 0.5 * d + 0.5 * g,
				 #spa = ifelse(spa < 0, 0, spa),
				 codist = c,
				 #codist = ifelse(codist < 0, 0, codist),
				 r2 = env + spa + codist) -> sites



# In the past, to calculate the R2 and fractions for sites, I would take the mean per site across all species
sites %>% 
	group_by(site) %>% 
	summarize_all(., mean) %>% 
	mutate_all(., function(x) ifelse(x<0, 0, x))-> k

# We check the mean R2 across sites, 
mean(k$r2)
# It's the metacommunity R2 (the one we got above for species) but divided by the number of sites
# so in this case, each site is making an absolute contribution to the metacommunity R2?
sum(k$r2)

# Plot it:	
# ***** r2 values per site coming from averaging across species
# Where each site contributes to the overall R2 of the metacommunity
# In the case of species, each species contributes proportionally (it's an average)

k %>% 
	ggtern(aes(x = env, z = spa, y = codist, size = r2)) +
	geom_point() +
	ggtitle("Sum of all sites give metacommunity R2") +
	labs(caption = "Notice the scale of R2 is smaller than in the case for species")

# Here you see the value of the R2 is much lower, and the plot uses a different scale than in species

# Scaling site R2---------------------------

#If we have 1000 sites
k %>% 
	ggtern(aes(x = env, z = spa, y = codist, size = r2*1000)) +
	geom_point() +
	ggtitle("Sum of all sites give metacommunity R2") +
	labs(caption = "Notice sites R2 going over 1")

# The idea was perhaps a weighted sum, but weighted by what?
# Mathew says the location of the points is ok, just the R2 doesn't have the same meaning here as it does in species
# We could weigh all the values by each species r2?
# We can only do this when we are re-structuring the lists from the vp:

vp_sites %>% 
	map(as_tibble) %>% 
	bind_cols() %>% 
	rownames_to_column(var = "site") %>% 
	pivot_longer(-site, names_to = "header", values_to = "values") %>% 
	mutate(specie = str_split(header, pattern = "\\.", n = 2, simplify = TRUE)[,1],
				 specie = as.numeric(str_replace(specie, "y", "")),
				 fraction = str_split(header, pattern = "\\.", n = 2, simplify = TRUE)[,2],
				 fraction = ifelse(fraction == "", "env-spa-random", fraction)) %>% 
	select(-header) %>% 
	mutate(values = values * rep(spp$r2, 7000)) %>% 
	pivot_wider(names_from = fraction, values_from = values) %>% 
	set_names(c("site", "species", "c", "b", "a", "e", "f", "d", "g")) %>% 
	mutate(site = as.numeric(site),
				 env = a + f + 0.5 * d + 0.5 * g,
				 #env = ifelse(env < 0, 0, env),
				 spa = b + e + 0.5 * d + 0.5 * g,
				 #spa = ifelse(spa < 0, 0, spa),
				 codist = c,
				 #codist = ifelse(codist < 0, 0, codist),
				 r2 = env + spa + codist) -> scaled


scaled %>% 
	select(site, species, env, spa, codist, r2) %>% 
	group_by(site) %>% 
	summarize_all(., mean) %>% 
	mutate_all(., function(x) ifelse(x<0, 0, x)) %>% 
	ggtern(aes(x = env, z = spa, y = codist, size = r2)) +
	geom_point()
	
	
# The values stil look pretty small and I am not sure of the significance of this R2 now. 
# Since we've multiplied each component by the species' r2, does it mean then that each site is a weighted average of the species, which is weighted by each species-level r2? So then the sites r2, what are they saying about the community level r2?

# Also, if I check now, the mean R2 for the sites is a different value than the mean r2 of the species, but I think we need them to be the same? or maybe not.

scaled %>% 
	select(site, species, env, spa, codist, r2) %>% 
	group_by(site) %>% 
	summarize_all(., mean) %>% 
	mutate_all(., function(x) ifelse(x<0, 0, x)) -> a

mean(a$r2)


