################################################################################
## ENVIRONMENTAL HEALTH DATA 
# obtained from washington environmental health disparities map



##

# create polygons based on geoids 

# rasterize polygons 

# buffer 1000m around camera point 

# merge to one DF 

# create percentiles 












waenv1 <- read.csv(here("data", "WA Environmental Effects.csv"))
waenv2 <- read.csv(here("data", "WA Environmental Exposures.csv"))

waenv3 <- merge(waenv1, waenv2, by = "State.FIPS.Code")
head(waenv3)

# average effects and exposures
waenv3$Rank <- rowMeans(waenv3[,c('Rank.x', 'Rank.y')])
waenv <- waenv3 %>% dplyr::select(-c("Rank.x","Rank.y"))
head(waenv)

#get only our counties of interest 
waenv <- waenv %>% dplyr::filter(substr(State.FIPS.Code, 1, 5) 
                                 %in% c("53033", "53053", # king pierce 
                                        "53061", "53035", # snoho kitsap
                                        "53029", "53057")) # island skagit

# rename col names to match
colnames(waenv) <- c("GEOID", "Rank")
colnames(calenv) <- c("GEOID", "Rank")

#write_csv(waenv, here("data", "env_health_ranks_wa2.csv"))
