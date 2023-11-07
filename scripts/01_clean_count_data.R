## cleaning count data 

library(data.table)
library(sf)
library(sp)
library(igraph)
library(dplyr)
library(here)
library(readr)
library(terra)
library(tidyr)

##### seattle data 

se_caps <- read_csv("data/raw_data_from_uwin/SEWA_capturetimes.csv")
se_meta <- read_csv("data/raw_data_from_uwin/SEWA_metadata.csv")
se_counts <- read_csv("data/raw_data_from_uwin/SEWA_capturecounts.csv")
head(se_caps)
head(se_meta)


se_caps <- se_caps %>% group_by(species, station, season) %>% summarize(count = n())


se_counts <- left_join(se_caps, se_meta, by = c("station","season"))


### change names from latin to common 


#### tacoma data


# read in data
all_data <- read_csv("data/raw_data_from_uwin/yasmine_west_coast_data.csv") 
length(unique(all_data$locationAbbr))
class(all_data)

# check data
head(all_data)
nrow(all_data)
unique(all_data$commonName)

# read in detection history data set
detections <- read_csv("data/raw_data_from_uwin/initial_data_yasmine.csv")

# remove all sites with 0 days running from detection data
detections <- detections[detections$J != 0, ]
(unique(detections$Site))

# collapse data so each instance of city/location/season/species is a new column

counts <- all_data %>% group_by(commonName, locationID, city, season) %>% summarize(count = n())
nrow(counts)
nrow(all_data)

colnames(all_data)
colnames(counts)

# take site info
all_data <- all_data %>%
  dplyr::select(locationID, locationAbbr, utmEast, utmNorth, utmZone) %>% distinct()
head(all_data)

# join counts to site data 
joined_data <- left_join(counts, all_data, by = "locationID")
nrow(joined_data)
length(unique(joined_data$locationAbbr))

joined_data
# now we have counts for each site/season combo 
# but we only want tacoma data for this project
head(joined_data)
ta_counts <- joined_data %>% filter(city == "TAWA")
  
head(ta_counts)

# let's change UTM to lat long

# first make all of our zones just 10

ta_counts$utmZone[ta_counts$utmZone == "10N"] <- 10

unique(ta_counts$utmZone)

# get lat long for all points in zone 10
utm1 <- data.frame(x=ta_counts$utmEast, y=ta_counts$utmNorth) 
coordinates(utm1) <- ~x+y 
class(utm1)
proj4string(utm1) <- CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +ellps=WGS84") 
utm2 <- spTransform(utm1,CRS("+proj=longlat +datum=WGS84"))
utm2

ta_counts$long <- utm2$x
ta_counts$lat <- utm2$y

nrow(ta_counts)


# change all city into lowercase
ta_counts$city <- tolower(ta_counts$city)

# change colnames
colnames(ta_counts) <- c("Species", "locationID", "city", "season",
                       "count", "Site", "utmEast", "utmNorth",
                       "utmZone", "long", "lat")

head(ta_counts)
################################################################################
## removing/merging sites 


# we will remove sites that don't have enough data in detection data
# the detection data only includes sites that have been fully tagged for a season

# first see which sites are not present in count data
sites_det_only <- setdiff(unique(detections$Site),unique(ta_counts$Site))

# now see which sites are not present in detection data 
sites_count_only <- setdiff(unique(ta_counts$Site),unique(detections$Site))


# remove all sites that don't exist in detection dataset 

new_dat_cut <- subset(ta_counts, Site != "P-MCP")
new_dat_cut

# now remove all seasons of a site that don't exist in detection data set 

# take closer look at detection data set 
uniquesiteseason <- unique(detections[c("Site", "Season")])
uniques <- arrange(uniquesiteseason, Site, Season)
table(uniquesiteseason$Site)

# same with counts 
uniquesiteseason2 <- unique(new_dat_cut[c("Site", "season")])
table(uniquesiteseason2$Site)

# let's remove oct 21 since it doesn't exist in detections data and is probably
# incomplete 

ta_counts <- subset(new_dat_cut, season != "OC21")

################################################################################

#### clean up / make data sets look the same 
#### 
# change colnames

colnames(ta_counts) <- c("Species", "locationID", "City", "Season",
                         "count", "Site", "utmEast", "utmNorth",
                         "utmZone", "long", "lat") 

head(ta_counts)
colnames(se_counts)
head(se_counts)

# add city column to seattle
se_counts <- se_counts %>%
  mutate(City = "sewa")

# rename cols 
colnames(se_counts) <- c("Species", "Site", "Season",
                         "count",  "lat", "long", "days.active", "City")


# remove tacoma columns not needed 
ta_counts <- ta_counts %>% as.data.frame() %>% select(-c("locationID", "utmEast", "utmNorth", "utmZone"))
head(ta_counts)

## merging spp 

ta_counts$Species[ta_counts$Species == "Mule deer"] <- "Black-tailed deer"

# merge all rabbit to "Rabbit"
ta_counts$Species[ta_counts$Species == "Eastern cottontail rabbit"] <- "Rabbit"
ta_counts$Species[ta_counts$Species == "Rabbit (cannot ID)"] <- "Eastern cottontail rabbit"

#### 


# also add days active to the tacoma count data
ta_counts

ta_counts <- ta_counts %>% left_join(detections[,c("Site", "Season", "J")], by = c("Site", "Season"))

# this added a lot of rpeats so only keep the first unique row of each repeat

ta_counts <- ta_counts %>% distinct()

# replace NAs with 0
ta_counts$J <- ta_counts$J %>% replace_na(0)

# rename 
ta_counts$days.active <- ta_counts$J
ta_counts <- ta_counts %>% select(-J)




# reorder columns to match
colnames(se_counts)
colnames(ta_counts)
se_counts <- se_counts[, c("City", "Site", "Season", "Species", 
                      "count", "lat", "long", "days.active")]  
ta_counts<- ta_counts[, c("City", "Site", "Season", "Species", 
                          "count", "lat", "long", "days.active")] 

colnames(ta_counts) == colnames(se_counts)

# rename tacoma seasons
se_counts$Season
ta_counts$Season[ta_counts$Species == "OC20"] <- "Fall20"
ta_counts$Season[ta_counts$Species == "JA21"] <- "Winter21"
ta_counts$Season[ta_counts$Species == "AP21"] <- "Spring21"
ta_counts$Season[ta_counts$Species == "JU21"] <- "Summer21"
# ta_counts$Season[ta_counts$Species == "OC21"] <- "Fall21"


# rename seattle species
se_counts$Species[se_counts$Species == "Canis latrans"] <- "Coyote"
se_counts$Species[se_counts$Species == "Procyon lotor"] <- "Raccoon"
se_counts$Species[se_counts$Species == "Didelphis virginiana"] <- "Virginia opossum"
se_counts$Species[se_counts$Species == "Lynx rufus"] <- "Bobcat"
se_counts$Species[se_counts$Species == "Odocoileus hemionus"] <- "Black-tailed deer"
se_counts$Species[se_counts$Species == "Procyon lotor"] <- "Raccoon"
se_counts$Species[se_counts$Species == "Ursus americanus"] <- "Black bear"

# squirrel / rabbit 
# se_counts$Species[ta_counts$Species == "Sylvilagus floridanus"] <- "Eastern cottontail rabbit"
# new_dat$Species[new_dat$Species == "California Ground Squirrel"] <- "Squirrel"
# new_dat$Species[new_dat$Species == "Douglas squirrel"] <- "Squirrel"
# new_dat$Species[new_dat$Species == "Fox squirrel"] <- "Squirrel"
# new_dat$Species[new_dat$Species == "Western gray squirrel"] <- "Squirrel"

wa_counts <- rbind(ta_counts, se_counts)


nrow(ta_counts)
View(se_counts)
nrow(se_counts)
