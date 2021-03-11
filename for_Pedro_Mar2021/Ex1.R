# The previous code I had written was taking the process all the way from the beginning, from simulating the metacommunity data, to fitting the models, and variation partitioning
# Here, I'm just bringing in the data to make the figures. 

library(ggtern)
library(tidyverse)

# functions:
labFnc <- function(x,digits=3) format(round(unique(x),digits),digits=digits)

# Load simulated metacommunity data
comdata <- readRDS("FIG3B-metacomSim.RDS")

# Variation partitioning
# this was done using the HMSC::VariPart() from Guillaume's package

sppvp <- readRDS("FIG3B-vpspp.RDS")

sitesvp <- readRDS("FIG3B-vpsites.RDS")

# Since we had 5 simulated metacommunities, I'll just focus on one of them. 
# You can run each line of the following code to see the output before the pipe (%>%)
# The output of the VP function gives out overlaps, so I used Figure 1A, the one in the supplement to combine the different partitions.

sppvp[[1]]

# Figure A1, the different partitions were combined so that a unique value was associated to environment (fractions [a], [d]/2, [f] and [g]/2), co-distribution (fraction [c]), space (fractions [b], [d]/2, [e] and [g]/2) and the unexplained portion of the variation (fraction [h]).

sppvp[[1]] %>%  
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
            r2 = env + spa + codist) -> spp_toplot

# And essentially, here the R2 is calculated as just the sum of the other fractions and used later as the size of each point in the plots


# Species plot
# This is a basic plot, no aesthetic changes:
spp_toplot %>% 
  ggtern(aes(x = env, z = spa, y = codist, size = r2)) +
  geom_point(alpha = 0.8)

# I can't find info on the scaling the plotting function does. I'm going to focus on the values for just one species
spp_toplot[2,]
spp_toplot[2,] %>% 
  ggtern(aes(x = env, z = spa, y = codist, size = r2)) +
  geom_point(alpha = 0.8)


# The transformation done by the plot is by taking the fractional value of each axis based on the sum of the three axes (which in our case it turns out to be r2 because of the calculation above)

# So we can check the values for that species, and the following plot shows where each of those gridlines would be, except they don't agree with the location of the actual point, since there is a transformation going on. 

df <- as.data.frame(spp_toplot[2,])
df
df %>% 
  ggtern(aes(x = env, z = spa, y = codist, size = r2)) +
  scale_T_continuous(breaks = unique(df$codist), labels=labFnc(df$codist)) +
  scale_L_continuous(breaks=unique(df$env),labels=labFnc(df$env)) +
  scale_R_continuous(breaks=unique(df$spa),labels=labFnc(df$spa)) +
  theme_bw() +
  theme_nogrid_minor() + 
  geom_point()


# So, the actual position of the point is the fractional value for each axis based on that rowsum is:
df2 <- spp_toplot[2,2:4]/as.numeric(spp_toplot[2,5])
df2
df2 %>% 
  ggtern(aes(x = env, z = spa, y = codist)) +
  scale_T_continuous(breaks=unique(df2$codist),labels=labFnc(df2$codist)) +
  scale_L_continuous(breaks=unique(df2$env),labels=labFnc(df2$env)) +
  scale_R_continuous(breaks= df2$spa, labels=labFnc(df2$spa)) +
  theme_bw() +
  theme_nogrid_minor() + 
  geom_point()

# Comparison between the scaled data and non scaled
# note that the actual position of the points doesn't really change.
df[2:4]
df2

# These are my interpretations of the scaling or transformations going on.
# But this forum has a response from the package developer:
# https://stackoverflow.com/questions/49716425/how-to-use-axis-range-and-labels-from-original-data-in-ggtern
# https://stackoverflow.com/questions/33873763/how-to-change-labels-of-a-ternary-plot-made-by-ggtern?rq=1


# The following is the base code I had use to make the species figures for the manuscript.
# I had added scaling to each axis independently so it wouldn't go over 1, but now I'm not sure that was correct.

spp_toplot %>% 
  ggtern(aes(x = env, z = spa, y = codist, size = r2)) +
  geom_point(alpha = 0.8) +
  scale_T_continuous(limits=c(0,1.0),
                     breaks=seq(0,1,by=0.1),
                     labels=seq(0,1,by=0.1)) +
  scale_L_continuous(limits=c(0.0,1),
                     breaks=seq(0,1,by=0.1),
                     labels=seq(0,1,by=0.1)) +
  scale_R_continuous(limits=c(0.0,1.0),
                     breaks=seq(0,1,by=0.1),
                     labels=seq(0,1,by=0.1)) +
  labs(title = "Species",
       x = "E",
       xarrow = "Environment",
       y = "C",
       yarrow = "Co-Distribution",
       z = "S", 
       zarrow = "Spatial Autocorrelation") +
  theme_light() +
  theme_showarrows() +
  scale_size_area(limits = c(0,1), breaks = seq(0,1,0.2)) +
  guides(size = guide_legend(title = expression(R^2), order = 1)) +
  theme(panel.grid = element_line(color = "darkgrey"),
        axis.title = element_text(size = 8))

# FOR THE CASE OF THE SITES, the vp data also comes from HMSC::varipart() but with indSite = TRUE

# Also focusing on the first metacommunity simulation only.
data <- sitesvp[[1]]
nspp <- 12

# From the output, we average across all the species to get each contribution at the species level. 
c <- rowSums(data$overlap1[,,1])/nspp
b <- rowSums(data$overlap1[,,2])/nspp
a <- rowSums(data$overlap1[,,3])/nspp

e <- rowSums(data$overlap2[,,1])/nspp
f <- rowSums(data$overlap2[,,2])/nspp
d <- rowSums(data$overlap2[,,3])/nspp

g <- rowSums(data$overlap3)/nspp

# And then follow the same calculations as done with species based on the figure in the supplement that describes how we combined those fractions.
env <- a + f + 1/2 * d + 1/2 * g
env <- ifelse(env < 0, 0, env)
spa <- b + e + 1/2 * d + 1/2 * g
spa <- ifelse(spa < 0, 0, spa)
random <- c
codist <- ifelse(random < 0, 0, random)
r2 <- env + spa + codist

sitedata <- cbind.data.frame(env, spa, codist, r2)

# And then the plot for that site data:
# The size of the points is adjusted on line 180, where I made the largest size be equal to the largest value of R2 we had. If you want it on the same scale as with the species, turn off line 180 and run it with 181



sitedata %>% 
  ggtern(aes(x = env, z = spa, y = codist, size = r2)) +
  geom_point(alpha = 0.6) +
  scale_T_continuous(limits=c(0,1.0),
                     breaks=seq(0,1,by=0.1),
                     labels=seq(0,1,by=0.1)) +
  scale_L_continuous(limits=c(0.0,1),
                     breaks=seq(0,1,by=0.1),
                     labels=seq(0,1,by=0.1)) +
  scale_R_continuous(limits=c(0.0,1.0),
                     breaks=seq(0,1,by=0.1),
                     labels=seq(0,1,by=0.1)) +
  labs(title = "Sites",
       x = "E",
       xarrow = "Environment",
       y = "C",
       yarrow = "Co-Distribution",
       z = "S", 
       zarrow = "Spatial Autocorrelation") +
  theme_light() +
  theme_showarrows() +
  scale_size_area(limits = c(0, 0.003), breaks = seq(0, 0.003, 0.0005)) +
  #scale_size_area(limits = c(0,1), breaks = seq(0,1,0.2))
  guides(size = guide_legend(title = expression(R^2))) +
  theme(panel.grid = element_line(color = "darkgrey"),
        axis.title = element_text(size = 8))
