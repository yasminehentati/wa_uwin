################################################################################
################# Washington Env Health GLMM Analysis  #########################
################# Step 2: urbanization metrics         #########################
#################   Yasmine Hentati yhentati@uw.edu    #########################
################################################################################

library(here)
library(readr)
library(dplyr)
library(sf)
library(mapview)
## packages 
library(raster)
library(sf)
library(dplyr)
library(mapview) # note that mapview is somewhat demanding, consider skipping
# mapview functions if your computer is slow  - these are just used to check 
# that shapefiles look correct 
library(rgdal)
library(raster)
library(here)
library(remotes)
library(tidycensus)
# remotes::install_github("walkerke/crsuggest") 
library(crsuggest)
library(tidyr)
library(terra)
install.packages("spatialEco")
library(spatialEco)
library(readr)
library(ggfortify)
library(rgeos)
install.packages("exactextractr")
library(exactextractr)

##### get data from lat/long to utms 


# load all sites
all_sites <- read.csv(here("data", "wa_counts.csv"), stringsAsFactors = FALSE)

# order data by Location Name 
all_sites <- all_sites[order(all_sites$site),]
nrow(all_sites)
head(all_sites)

# create new column for utm zone 

# transform into UTM - WA
# all_sites <- data.frame(x=all_sites$long, y=all_sites$lat) 
all_sites <- st_as_sf(all_sites, coords=c("long", "lat"), crs="EPSG:4326")
all_sites <- st_transform(all_sites, crs = "EPSG:32610")

head(all_sites)

all_sites <- all_sites %>%
  dplyr::mutate(utm_east = sf::st_coordinates(.)[,1],
                utm_north = sf::st_coordinates(.)[,2])
head(all_sites)

class(all_sites)

# only keep 1 row for each site 
points_wa <- all_sites %>% distinct(site, .keep_all = TRUE) 

head(points_wa)

# check that it looks ok
mapview(points_wa)

st_write(tractsKP_crop, here("data", "cameras", "points_wa.shp"),
    append = FALSE)


################################################################################
## INCOME DATA
## median household income data from American Community Survey

Sys.getenv("CENSUS_API_KEY")
# load census API key
# census_api_key("e649b78a1d98fe7e1c9ff7039840781976777eb6",
 #    install = TRUE)
# readRenviron("~/.Renviron")

# load in ACS variables  from 2019 
v17 <- load_variables(2019, "acs5", cache = TRUE)


# load in shapefile of subdivisions  
# with median household income as variable of interest

# for washington 
tractincomeWA <- get_acs(state = "WA", 
                         geography = "tract", 
                         variables = c(medincome="B19013_001"), geometry = TRUE)

# cut out everything except king and pierce county from our WA shp 
# specify dplyr because rgdal also has filter fxn 
# do this using the GEOID codes for each county 

tractsKP <- tractincomeWA %>% dplyr::filter(substr(GEOID, 1, 5) 
                                            %in% c("53033", "53053"))

# mapview(tractsKP) # looks good


# crop to actual study area 
tractsKP <- st_transform(tractsKP, crs=4326)
tractsKP_crop <- st_crop(tractsKP, c(xmin= -121.7, ymin = 46.7, xmax = -122.8, ymax = 47.8))
mapview(tractsKP_crop)


# write shapefiles for cropped area 
# st_write(tractsKP_crop, here("data", "income_maps", "seatac_urban_med_income.shp"),
  #    append = FALSE)



################################################################################
## HOUSING DENSITY DATA 
## read in housing density  data - starting with WA 
# data from: ???

wa_housing <- st_read(here("data", "housing_maps", 
                           "wa_blk10_Census_change_1990_2010_PLA2.shp"))

# filter to only king and pierce county
wa_housing <- wa_housing %>% dplyr::filter(substr(BLK10, 1, 5) 
                                           %in% c("53033", "53053"))

# we only need 2010 housing data - select relevant info

colnames(wa_housing)
wa_housing <- wa_housing %>% dplyr::select(BLK10, WATER10, POP10, 
                                           HU10, HUDEN10, HHUDEN10,
                                           PUBFLAG:geometry)
colnames(wa_housing)

# crop to actual study area 
sf_use_s2(FALSE)
wa_housing <- st_transform(wa_housing, crs=4326)
wa_housing <- st_crop(wa_housing, c(xmin= -121.7, ymin = 46.7, xmax = -122.8, ymax = 47.8))

# mapview(wa_housing)

st_write(wa_housing, here("data", "housing_maps", "wa_urban_huden_2010.shp"),
    append = FALSE)





################################################################################
## VEGETATION DATA 


################################################################################
## ENVIRONMENTAL HEALTH DATA 
# obtained from washington environmental health disparities map


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

write_csv(waenv, here("data", "env_health_ranks_wa2.csv"))

################################################################################
## impervious surface calculation 
library(here)

# load impervious cover raster map
imp_map <- rast(here("data", "NLCD_imp", 
                       "nlcd_2019_impervious_descriptor_l48_20210604.img"))

# again reproject  points to match raster
# these are just the points not the buffers

points_WA <- st_transform(points_WA, st_crs(imp_map))

# crop raster to study area 

# create an extent 
# xmin, xmax, ymin, and ymax (in that order)
new_extent <- as(extent(-122.8, -121.7, 46.7, 47.8), "SpatialPolygons")

class(new_extent)
new_extent <- st_transform(new_extent, st_crs(imp_map))
projection(new_extent)
# crop imp map to our new extent 
imp_map <- terra::crop(imp_map, new_extent)


points_wa <- points_wa[!st_is_empty(points_wa),]
points_wa <- as(points_wa, "Spatial")


#create a buffer around the points
sp_buffer <-st_buffer(st_as_sf(points_wa),500) 

head(points_wa)


# reproject impervious surface map
imp_map <- terra::project(imp_map,crs("EPSG:32610"))

# extract the mean impervious cover around each point using 500 m radius buffer
imp <- terra::extract(imp_map, points_wa, buffer = 500, fun=mean, df=TRUE)

imp
?terra::extract
points_WA$imp_surf <- imp$nlcd_2019_impervious_descriptor_l48_20210604


#########################
# scratch

nalc <- raster("data/landcover/north_america_2015_v2/NA_NALCMS_2015_v2_land_cover_30m/NA_NALCMS_2015_v2_land_cover_30m.tif")

# Impervious Surface Cover for the Contiguous US
is <-raster("C:/data/NLCD_imp/nlcd_2016_impervious_l48/nlcd_2016_impervious_l48_20210604.img")

# Survey Points

cam_wa <- readOGR(dsn = "data/cameras/points_wa.shp") %>% spTransform(crs(nalc))



cam_is <- cam_wa %>%
  spTransform(crs(is))


# plotlandcover 
plot(nalc); plot(cam_wa, add = TRUE)

# view US points vs percent impervious
plot(is); plot(cam_us, col = "white",add = TRUE)


## get imp surf for each site 

sites <- cam_wa #%>% filter(city == "chil")

is_us <- data.frame(
  sites@data,
  "Impervious" = rep(NA, nrow(sites@data))
)

i <- 1          # select the site
buffer <- 1000   # choose the buffer size, in meters


# Takes about 8 minutes
Sys.time()


for(i in 1:length(sites)){
  pt <- sites[i,]                                         # select a point
  buff <- gBuffer(pt, width = buffer, quadsegs = 25)      # create buffer around the point. This determines the size of the landscape
  
  landscape <- is %>%
    crop(extent(buff)) %>%
    mask(buff)
  
  is_us$Impervious[i] <- extract(landscape, buff, fun = mean, na.rm = TRUE)
  
  #plot(landscape); plot(pt, add = TRUE)
  print(i)
}
Sys.time()

is_us <- is_us %>%
  dplyr::select(c(site, city, city_site, Impervious))
is_us

