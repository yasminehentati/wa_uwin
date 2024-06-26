## cleaning count data 

library(data.table)
library(sf)
library(sp)
library(dplyr)
library(here)
library(readr)
library(terra)
library(tidyr)

##### seattle data 

se_caps <- read_csv("data/raw_data_from_uwin/SEWA_capturetimes.csv")
se_meta <- read_csv("data/raw_data_from_uwin/SEWA_metadata.csv")
se_counts <- read_csv("data/raw_data_from_uwin/SEWA_capturecounts.csv")
se_smalls <- read_csv("data/raw_data_from_uwin/SEWA_squirrel_bunny.csv")

# group captures 
se_caps <- se_caps %>% group_by(species, station, season) %>% summarize(count = n())
se_smalls<- se_smalls %>% group_by(species, station, season) %>% summarize(count = n())

# bind large and small mammal data
se_caps <- rbind(se_caps, se_smalls)

# join metadata and counts 
se_counts <- left_join(se_caps, se_meta, by = c("station","season"))


#### tacoma data


# read in data
all_data <- read_csv("data/raw_data_from_uwin/yasmine_tawa_19_20.csv") 
length(unique(all_data$locationAbbr))
class(all_data)

# check data
head(all_data)
nrow(all_data)
unique(all_data$commonName)

# read in detection history data set
detections <- read_csv("data/raw_data_from_uwin/yasmine_samps_new.csv")

# remove all sites with 0 days running from detection data

(unique(detections$Site))

detections2 <- detections[detections$J != 0, ]

(unique(detections2$Site))

nrow(detections)
nrow(detections2)

# collapse data so each instance of city/location/season/species is a new column

counts <- all_data %>% group_by(commonName, locationID, city, season) %>% summarize(count = n())
nrow(counts)
nrow(all_data)

colnames(all_data)
colnames(counts)

# take site info
all_data <- all_data %>%
  dplyr::select(locationID, locationAbbr, longitude, latitude) %>% distinct()
head(all_data)

# join counts to site data 
ta_counts <- left_join(counts, all_data, by = "locationID")
nrow(ta_counts)
length(unique(ta_counts$locationAbbr))
# now we have counts for each site/season combo 



# change all city into lowercase
ta_counts$city <- tolower(ta_counts$city)

# change colnames
colnames(ta_counts) <- c("species", "locationID", "city", "season",
                       "count", "Site", "long", "lat")

head(ta_counts)

##############################################################
## removing/merging sites 





# use mason's functions to collapse sites by location 
source("scripts/mason_site_collapse_code/qaqc_sites.R")
source("scripts/mason_site_collapse_code/long_to_zone.R")
source("scripts/mason_site_collapse_code/fix_site_names.R")

new_dat_1 <- qaqc_sites(x = ta_counts, cities="city", sites = "Site",
                        my_coords=c("long","lat"), my_crs=4326)

# this function will tell us what sites need to be merged or removed based
# on proximity to the next site(s)
# the "remove" process may not be totally accurate for this data set because it is 
# made to keep the site with more data in an occupancy data set, but 
# we have counts. for now we will keep these separate 

fix_site_names(new_dat_1)

# manually decide which sites to remove from count data after comparing 

# merge  
# P-TE2 and P-TE1 
# P-GW1, P-GW2, P-GW3
# P-MKP2, P-MKP3
# P-OV1, P-OV2
# G-CBP1, G-CBP2
# G-KP1, G-KP2
# G-WLP1, G-WLP2
# G-FN1, G-FN2
# G-TNC1, G-TNC2

# this is based on the fix_site_names function 

ta_counts$Site[ta_counts$Site == "P-TE2"] <- "P-TE1" # keep
ta_counts$Site[ta_counts$Site == "P-GW2"] <- "P-GW1" # keep
ta_counts$Site[ta_counts$Site == "P-GW3"] <- "P-GW1" # keep
ta_counts$Site[ta_counts$Site == "P-MKP3"] <- "P-MKP2" # keep
ta_counts$Site[ta_counts$Site == "P-OV2"] <- "P-OV1" # keep
ta_counts$Site[ta_counts$Site == "G-CBP2"] <- "G-CBP1" # keep
ta_counts$Site[ta_counts$Site == "G-KP2"] <- "G-KP1" # keep
ta_counts$Site[ta_counts$Site == "G-WLP2"] <- "G-WLP1" # keep
ta_counts$Site[ta_counts$Site == "G-FN2"] <- "G-FN1" # keep
ta_counts$Site[ta_counts$Site == "G-TNC2"] <- "G-TNC1" # keep
ta_counts$Site[ta_counts$Site == "P-TE2"] <- "P-TE1"
ta_counts$Site[ta_counts$Site == "G-KP"] <- "G-KP1"




detections$Site[detections$Site == "P-TE2"] <- "P-TE1" # keep
detections$Site[detections$Site == "P-GW2"] <- "P-GW1" # keep
detections$Site[detections$Site == "P-GW3"] <- "P-GW1" # keep
detections$Site[detections$Site == "P-MKP3"] <- "P-MKP2" # keep
detections$Site[detections$Site == "P-OV2"] <- "P-OV1" # keep
detections$Site[detections$Site == "G-CBP2"] <- "G-CBP1" # keep
detections$Site[detections$Site == "G-KP2"] <- "G-KP1" # keep
detections$Site[detections$Site == "G-WLP2"] <- "G-WLP1" # keep
detections$Site[detections$Site == "G-FN2"] <- "G-FN1" # keep
detections$Site[detections$Site == "G-TNC2"] <- "G-TNC1" # keep
detections$Site[detections$Site == "P-TE2"] <- "P-TE1"
detections$Site[detections$Site == "G-KP"] <- "G-KP1"

# there are a few more sites in the detection data set that need to be renamed 
# pcna2 -- is pcna 1 in detection data 
# scp2 is scp1
# clp3 to clp2

detections$Site[detections$Site == "P-PCNA1"] <- "P-PCNA2"
detections$Site[detections$Site == "G-CLP2"] <- "G-CLP3"
detections$Site[detections$Site == "P-SCP1"] <- "P-SCP2"



# now we need to collapse the detection data to only keep rows without 0s 

(unique(detections$Site))

detections <- detections[detections$J != 0, ]

nrow(detections)


# we will remove sites that don't have enough data in detection data
# the detection data only includes sites that have been fully tagged for a season

# first see which sites are not present in count data
sites_det_only <- setdiff(unique(detections$Site),unique(ta_counts$Site))

# now see which sites are not present in detection data 
sites_count_only <- setdiff(unique(ta_counts$Site),unique(detections$Site))

sites_count_only

### G-AGC, GKP1, G-FN1, G-TCC1 not in detection data 
# mason probablyr emoved these for a reason so let's check them out
# G-AGC -- only one season of data 
# G-KP1 -- needs to be merged with G-KP, kinda sparse data 
# G-FN1 -- only 2 seasons
# G-TCC1 -- 3 seasons, probs worth keeping but no detection ? can add manually later 

# we'll remove all of these for now since we need days active 


ta_counts <- subset(ta_counts, Site != "G-AGC")
ta_counts <- subset(ta_counts, Site != "G-FN1")
ta_counts <- subset(ta_counts, Site != "G-TCC1")









### merge seattle sites 






# use mason's functions to collapse sites by location 
head(se_counts)


# add city column to seattle
se_counts <- se_counts %>%
  mutate(City = "sewa")


# rename cols 
colnames(se_counts) <- c("species", "Site", "season",
                         "count",  "lat", "long", "days.active", "City")

new_dat_1 <- qaqc_sites(x = se_counts, cities = "City", sites = "Site",
                        my_coords=c("long","lat"), my_crs=4326)


# this function will tell us what sites need to be merged or removed based
# on proximity to the next site(s)
# the "remove" process may not be totally accurate for this data set because it is 
# made to keep the site with more data in an occupancy data set, but 
# we have counts. for now we will keep these separate 

fix_site_names(new_dat_1)

# manually decide which sites to remove from count data after comparing 

# merge  
# BGP2, BGP1
# MBC1, MBC2
# MWP2, MWP1
# LWP2, LWP3
# BRF1, BRF2
# PGP2, PGP1

# this is based on the fix_site_names function 

# rename accordingly 
se_counts$Site[se_counts$Site == "BGP2"] <- "BGP1" # keep
se_counts$Site[se_counts$Site == "MBC2"] <- "MBC1" # keep
se_counts$Site[se_counts$Site == "MWP2"] <- "MWP1" # keep
se_counts$Site[se_counts$Site == "LWP3"] <- "LWP2" # keep
se_counts$Site[se_counts$Site == "BRF2"] <- "BRF1" # keep
se_counts$Site[se_counts$Site == "PHY1"] <- "PHN1" # keep
se_counts$Site[se_counts$Site == "PGP2"] <- "PGP1" # keep

################################################################################

#### clean up / make data sets look the same 


# add days active to the tacoma count data

# change colnames to match dets 
colnames(detections)
colnames(ta_counts)
colnames(ta_counts) <- c("species", "locationID", "city", "Season",
                         "count", "Site", "long", "lat")


ta_counts <- ta_counts %>% left_join(detections[,c("Site", "Season", "J")], by = c("Site", "Season"))

# this added a lot of repeats so only keep the first unique row of each 
# season / site combo

ta_counts <- ta_counts %>% distinct()


# change all city into lowercase
ta_counts$city <- tolower(ta_counts$city)

# change colnames
colnames(ta_counts) <- c("species", "locationID", "city", "season",
                         "count", "site", "long", "lat", "days.active")
head(ta_counts)


# rename cols 
colnames(se_counts) <- c("species", "site", "season",
                         "count",  "lat", "long", "days.active", "city")


# remove tacoma columns not needed 
ta_counts <- ta_counts %>% as.data.frame() %>% dplyr::select(-c("locationID"))
head(ta_counts)

## merging spp 

ta_counts$species[ta_counts$species == "Mule deer"] <- "Black-tailed deer"

# merge all rabbit
ta_counts$species
ta_counts$species[ta_counts$species == "Rabbit"] <- "Eastern cottontail rabbit"
ta_counts$species[ta_counts$species == "Rabbit (cannot ID)"] <- "Eastern cottontail rabbit"


#### 

se_counts$species

# reorder columns to match
colnames(se_counts)
colnames(ta_counts)
se_counts <- se_counts[, c("city", "site", "season", "species", 
                      "count", "lat", "long", "days.active")]  
ta_counts<- ta_counts[, c("city", "site", "season", "species", 
                          "count", "lat", "long", "days.active")] 

colnames(ta_counts) == colnames(se_counts)
nrow(ta_counts)
nrow(se_counts)

# rename tacoma seasons
se_counts$season
ta_counts$season[ta_counts$season == "OC20"] <- "Fall20"
ta_counts$season[ta_counts$season == "JU20"] <- "Summer20"
ta_counts$season[ta_counts$season == "AP20"] <- "Spring20"
ta_counts$season[ta_counts$season == "JA20"] <- "Winter20"
ta_counts$season[ta_counts$season == "OC19"] <- "Fall19"
ta_counts$season[ta_counts$season == "JU19"] <- "Summer19"
ta_counts$season[ta_counts$season == "AP19"] <- "Spring19"
ta_counts$season[ta_counts$season == "JA19"] <- "Winter19"

# rename seattle species
se_counts$species[se_counts$species == "Canis latrans"] <- "Coyote"
se_counts$species[se_counts$species == "Procyon lotor"] <- "Raccoon"
se_counts$species[se_counts$species == "Didelphis virginiana"] <- "Virginia opossum"
se_counts$species[se_counts$species == "Lynx rufus"] <- "Bobcat"
se_counts$species[se_counts$species == "Odocoileus hemionus"] <- "Black-tailed deer"
se_counts$species[se_counts$species == "Procyon lotor"] <- "Raccoon"
se_counts$species[se_counts$species == "Ursus americanus"] <- "Black bear"
se_counts$species[se_counts$species == "Lontra canadensis"] <- "River otter"
se_counts$species[se_counts$species == "Mephitis mephitis"] <- "Striped skunk"
se_counts$species[se_counts$species == "Sylvilagus floridanus"] <- "Eastern cottontail rabbit"
se_counts$species[se_counts$species == "Sciurus carolinensis"] <- "Eastern gray squirrel"

# remove cat and dog from tacoma 

ta_counts <- ta_counts %>% dplyr::filter(species != "Domestic dog", species != "Domestic cat")
ta_counts$species

# rename skunks to match
ta_counts$species[ta_counts$species == "Striped Skunk"] <- "Striped skunk"

# merge data 
wa_counts <- rbind(ta_counts, se_counts)

# save merged data
write_csv(wa_counts, "data/wa_counts.csv")


### Calculations for manuscript

wa_counts <- read_csv("data/wa_counts.csv")

head(wa_counts)

# sum all active trap nights 
sum(wa_counts$days.active, na.rm = TRUE)

# Table of all raw counts -- both 

wa_counts %>% 
  group_by(species) %>% 
  summarise(count = sum(count))

# Table of raw counts -- by city 

# seattle 
wa_counts %>% 
group_by(species) %>% 
  filter(city == "sewa") %>% 
  summarise(count = sum(count))
  
# tacoma 
wa_counts %>% 
  group_by(species) %>% 
  filter(city == "tawa") %>% 
  summarise(count = sum(count))
  