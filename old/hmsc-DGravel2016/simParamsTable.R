## ---- simParamsTable --------------------------------------------
# Created by Javi Rudolph


# Gathering the parameters for the different scenarios and figures:

library(stringr)
# library(dplyr)

# Source partial sections of the code so that I obtain parameters real-time
# Don't change file names or else it won't work properly.

sourcePartial <- function(fn,startTag='#from here',endTag='#to here') {
  lines <- scan(fn, what=character(), sep="\n", quiet=TRUE)
  st<-grep(startTag,lines)
  en<-grep(endTag,lines)
  tc <- textConnection(lines[(st+1):(en-1)])
  source(tc)
  close(tc)
}


# Set up dataframe so that all the parameters get organized when they get sourced. 

#simScripts <- list.files() %>% str_subset(., "scenario" )
simScripts <- c("scenario1Fig2a.R", "scenario1Fig3a.R", "scenario2Fig2b.R", "scenario2Fig3b.R", "scenario3Fig2c.R", "scenario3Fig3c.R", "scenario4Fig2d.R")

origParams <- data.frame(Description = 
                           c("Number of Patches",
                             "Environmental vars",
                             "Species Richness",
                             "Niche breadth",
                             "Dispersal",
                             "Colonization",
                             "Interactions c",
                             "Interactions e"),
                         Param = c("N", "D", "R",
                                   "s_c", "alpha", "c_0",
                                   "d_c", "d_e"))

fig2Scripts <- simScripts %>% str_subset(., "Fig2")

fig2Params <- origParams

for(i in 1:length(fig2Scripts)){
  sourcePartial(fig2Scripts[i])
  a <- i+2
  fig2Params[,a] <- c(N, D, R,
                      as.character(unique(s_c[1,])),
                      alpha,
                      str_c(unique(c_0), collapse = " , "),
                      d_c,
                      d_e)
  names(fig2Params)[a] <- str_sub(fig2Scripts[i], 10, 14)
}



######################################################################################################

# Fig3 Params -------------------------------------------------------------



fig3Scripts <- simScripts %>% str_subset(., "Fig3")
fig3Params <- data.frame(Description = 
                           c("Number of Patches",
                             "Environmental vars",
                             "Species Richness",
                             "Niche breadth",
                             "Dispersal",
                             "Colonization",
                             "Interactions c",
                             "Interactions e"),
                         Param = c("N", "D", "R",
                                   "s_c", "alpha", "c_0",
                                   "d_c", "d_e"))

bindPrms <- function() {
  bindPrms <- c(N, D, R,
                as.character(unique(s_c[1,])),
                alpha,
                str_c(unique(c_0), collapse = " , "),
                d_c,
                d_e)
  bindPrms
}

## Fig3a
sourcePartial(fig3Scripts[1], startTag = "#from here", endTag = "to here")
prmsA1 <- bindPrms()

sourcePartial(fig3Scripts[1], startTag = "secondStart", endTag = "secondEnd")
prmsA2 <- bindPrms()

fig3Params$Fig3a <- str_c(prmsA1, prmsA2, sep = " / ")

## Fig 3b

sourcePartial(fig3Scripts[2], startTag = "#from here", endTag = "to here")
prmsB1 <- bindPrms()

sourcePartial(fig3Scripts[2], startTag = "secondStart", endTag = "secondEnd")
prmsB2 <- bindPrms()

sourcePartial(fig3Scripts[2], startTag = "thirdStart", endTag = "thirdEnd")
prmsB3 <- bindPrms()

fig3Params$Fig3b <- str_c(prmsB1, prmsB2, prmsB3, sep = " / ")


## Fig 3c
sourcePartial(fig3Scripts[3], startTag = "#from here", endTag = "to here")
fig3Params$Fig3c <- bindPrms()

