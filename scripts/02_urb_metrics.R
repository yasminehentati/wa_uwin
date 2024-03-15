################################################################################
################# Washington Env Health GLMM Analysis  #########################
################# Step 2: urbanization metrics         #########################
#################   Yasmine Hentati yhentati@uw.edu    #########################
################################################################################

library(dplyr)
library(sp)
library(raster)
library(here)
library(sf)
require(devtools)
#install.packages("Rtools")
#install_version("mapview", 
 #               version = "2.11.0", 
  #              repos = "http://cran.us.r-project.org")
library(mapview)
 # note that mapview is somewhat demanding, consider skipping
# mapview functions if your computer is slow  - these are just used to check 
# that shapefiles look correct 
library(rgdal)
library(here)
library(remotes)
library(tidycensus)
# remotes::install_github("walkerke/crsuggest") 
library(crsuggest)
library(tidyr)
library(terra)
library(spatialEco)
library(readr)
library(ggfortify)
library(rgeos)
library(exactextractr)
library(spatstat)   # for point pattern analysis
library(spdep)      # for spatial autocorrelation
library(landscapemetrics)    # for FRAGSTATS metrics
library(tmap)
library(viridis)
library(gghighlight)
library(dplyr)
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

all_sites <- all_sites %>%
  dplyr::mutate(utm_east = sf::st_coordinates(.)[,1],
                utm_north = sf::st_coordinates(.)[,2])

# only keep 1 row for each site 
points_wa <- all_sites %>% distinct(site, .keep_all = TRUE) 

# check that it looks ok
mapview(points_wa)

# st_write(points_wa, here("data", "cameras", "points_wa.shp"),
  #    append = FALSE)


################################################################################
## HOUSING DENSITY DATA 
## read in housing density  data - starting with WA 
# data from: Silvis lab https://silvis.forest.wisc.edu/data/housing-block-change/
# Using 2020 data

wa_housing <- st_read(here("data", "housing_maps", 
                           "WA_block20_change_1990_2020_PLA4.shp"))

# filter to only king and pierce county
colnames(wa_housing)
wa_housing <- wa_housing %>% dplyr::filter(substr(BLK20, 1, 5) 
                                           %in% c("53033", "53053"))

# we only need 2010 housing data - select relevant info

colnames(wa_housing)
wa_housing <- wa_housing %>% dplyr::select(BLK20, WATER20, POP2020,
                                           POPDEN2020, HUDEN2020,
                                           Shape_Leng:geometry)
colnames(wa_housing)

# crop to actual study area 
sf_use_s2(FALSE)
wa_housing <- st_transform(wa_housing, crs=4326)
wa_housing <- st_crop(wa_housing, c(xmin= -121.7, ymin = 46.7, xmax = -122.8, ymax = 47.8))

# mapview(wa_housing)

st_write(wa_housing, here("data", "housing_maps", "wa_urban_huden_2020.shp"),
    append = FALSE)


# let's make all polygons with water (WATER20) NA so they don't get 
# counted in the calculation (otherwise will show up at pop den 0)
wa_housing <- wa_housing %>%
  mutate(POPDEN2020 = ifelse(WATER20 == 1, NA, POPDEN2020))

mapview(wa_housing)
## transform polygon into raster 

#### 1. Calculate population density
template <- rast(vect(wa_housing),res=0.005)

pop_rast <- terra::rasterize(vect(wa_housing), template, field = "POPDEN2020")
class(pop_rast)


# reproject 
pop_rast <- terra::project(pop_rast, "EPSG:32610")


# is a spatraster, convert to raster 
pop_rast <- raster(pop_rast)

points_wa <- readOGR(dsn = "data/cameras/points_wa.shp")

sites <- points_wa %>%
  spTransform(crs(pop_rast))
crs(pop_rast)



plot(pop_rast,col=rainbow(100)); plot(sites,col="blue", add = TRUE)


pop_dat <- data.frame(
  sites@data,
  "pop_density" = rep(NA, nrow(sites@data))
)

i <- 1          # select the site
buffer <- 1000   # choose the buffer size, in meters


# Takes about 8 minutes
Sys.time()

# calculate buffer 1000m around camera point 


for(i in 1:length(sites)){
  pt <- sites[i,]                                         # select a point
  buff <- gBuffer(pt, width = buffer, quadsegs = 25)      # create buffer around the point. This determines the size of the landscape
  
  popden <- pop_rast %>%
    crop(extent(buff)) %>%
    mask(buff)
  
  pop_dat$pop_density[i] <- extract(popden, buff, fun = mean, na.rm = TRUE)
  
  plot(popden); plot(pt, add = TRUE)
  print(i)
}
Sys.time()

pop_dat <- pop_dat %>%
  dplyr::select(c(site, city, pop_density))
pop_dat







#########################
# Landscape metrics / landcover 

# Jeff Haight

nalc <- raster("data/landcover/USA_NALCMS_landcover_2015v3_30m.tif")

# Impervious Surface Cover for the Contiguous US
is <-raster("data/NLCD_imp/nlcd_2019_impervious_descriptor_l48_20210604.img")

# Survey Points

cam_wa <- readOGR(dsn = "data/cameras/points_wa.shp") %>% spTransform(crs(nalc))

plot(cam_wa)

cam_is <- cam_wa %>%
  spTransform(crs(is))
mapview(cam_is)

# plotlandcover 
plot(nalc); plot(cam_wa, add = TRUE)

# view US points vs percent impervious
plot(is); plot(cam_wa, col = "white",add = TRUE)


##### get imp surf for each site 

sites <- cam_is #%>% filter(city == "chil")

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
    raster::crop(extent(buff)) %>%
    raster::mask(buff)
  
  is_us$Impervious[i] <- extract(landscape, buff, fun = mean, na.rm = TRUE)
  
  plot(landscape); plot(pt, add = TRUE)
  print(i)
}
Sys.time()

is_us <- is_us %>%
  dplyr::select(c(site, city, Impervious))
is_us

##########################################################

sites <- cam_wa

# directions on how to reclassify land cover categories
m <- c(1, 20,  # set all the natural land cover types to 20
       2, 20,
       3, 20,
       4, 20,
       5, 20,
       6, 20,
       7, 20,
       8, 20,
       9, 20,
       10, 20,
       11, 20,
       12, 20,
       13, 20,
       14, 20,
       16, 20
)

i <- 7          # select the site
buffer <- 1000   # choose the buffer size, in meters

Sys.time()
pt <- sites[i,]                                         # select a point
buff <- gBuffer(pt, width = buffer, quadsegs = 25)      # create buffer around the point. This determines the size of the landscape
#plot(buff); plot(pt, add = TRUE)

ls <- nalc %>%
  crop(extent((buff))) %>%   # cropping even further reduces the processing time
  mask(buff)

# reclassify the landscape categories

rcmat <- matrix(m, ncol = 2, byrow = TRUE)

#ls <- reclassify(ls, rcmat)


ca <- lsm_c_ca(ls)        # total area of each class (land cover type), in hectares
mpa <- lsm_c_area_mn(ls)   # mean patch size/area for each class (land cover type), in hectares
#lsm_l_area_mn(ls)        # mean patch size/area of all patches of all classes, in hectares
sdhi <- lsm_l_shdi(ls)
pd <- lsm_c_pd(ls)         # Patch density for each class
ed <- lsm_c_ed(ls)        # Edge density for each class
enn <- lsm_c_enn_mn(ls)   # Mean Euclidean nearest-neighbor distance ('overall patch isolation')

ca
pd
sdhi
ed
enn

# Urban land cover proportion
lsm_type <- ca %>%                               
  filter(class == 17)
lsm_type$value
lsm_type$value/sum(ca$value)  
#if(length(lsm_type$value) > 0){
#    metrics_comp[i,1] <- lsm_type$value/sum(ca$value)
#}

#plot(ls); plot(pt, add = TRUE); plot(buff, add = TRUE)
tm_shape(ls)+
  tm_raster(style = "cat",
            title = "Land Cover")+
  tm_layout(legend.outside = TRUE)

#show_patches(ls)
#show_cores(ls)

# 1000 meter buffer = 100pi hectares

Sys.time()             # ~147 seconds more to calculate these three sets of landscape metrics at 4000 m. Plotting is quick



### North America: All sites, Multiple Metrics, One Landscape Size


# Choose the buffer/landscape size that you will use
buffer <- 1000

# Choose the set of points you are assessing
sites <- cam_wa

# Specify the number of sites
nsite <- nrow(sites)
#nbuff <- length(buffers)

# Compositional metrics (land cover proportions)
metrics_comp <- matrix(0, nrow = nsite, ncol = 19)  # there are 19 land cover classes in the Canada LC
colnames(metrics_comp) <- c("for_ever_temp",
                            "for_ever_taig",   #
                            "for_ever_trop",
                            "for_dec_trop",
                            "for_dec_temp",
                            "for_mix", 
                            "shrub_trop", 
                            "shrub_temp", 
                            "grass_trop",
                            "grass_temp",
                            "shrub_lich",
                            "grass_lich",
                            "barren_lich",
                            "wetland",
                            "cropland",
                            "barren", 
                            "urban", 
                            "water",
                            "ice" 
)

# Configurational metrics (patchiness/heterogeneity)
metrics_conf <- matrix(0, nrow = nsite, ncol = 5)
colnames(metrics_conf) <- c("mpa_undev", "pd_undev", "ed_undev", "enn_mn_undev", "sdhi")





i <- 3
pt <- sites[i,]                                         # select a point
buff <- gBuffer(pt, width = buffer, quadsegs = 25)      # Create buffer around the point
#plot(buff); plot(pt, add = TRUE)

ls_pre <- nalc %>%
  crop(extent(buff)) %>%
  mask(buff)

##### Calculate the first set of metrics
# No reclassification necessary
ca <- lsm_c_ca(ls_pre)        


#plot(ls); plot(pt, add = T)   # plot (very optional)

# Proportional area of each land cover class for each 
# filter out the the data for each metric
# convert from hectares to m^2 and calculate cover proportion based on buffer (10000 m^2/hectare / pi*r^2 square meters)
# add to original array/matrix only if there is a value for that land cover class to add]

# Temperate needleleaf (evergreen) forest
lsm_type <- ca %>%                               
  filter(class == 1)
if(length(lsm_type$value) > 0){
  metrics_comp[i,1] <- lsm_type$value/sum(ca$value)
}

# repeat for each land cover class
# Subpolar taiga needleleaf (evergreen) forest
lsm_type <- ca %>%                              
  filter(class == 2)
if(length(lsm_type$value) > 0){
  metrics_comp[i,2] <- lsm_type$value/sum(ca$value)
}

# Tropical and sub-tropical evergreen broadleaf forest
lsm_type <- ca %>%                              
  filter(class == 3)
if(length(lsm_type$value) > 0){
  metrics_comp[i,3] <- lsm_type$value/sum(ca$value)
}
# Tropical and sub-tropical deciduous broadleaf forest
lsm_type <- ca %>%                              
  filter(class == 4)
if(length(lsm_type$value) > 0){
  metrics_comp[i,4] <- lsm_type$value/sum(ca$value)
}

# Temperate deciduous broadleaf forest
lsm_type <- ca %>%                              
  filter(class == 5)
if(length(lsm_type$value) > 0){
  metrics_comp[i,5] <- lsm_type$value/sum(ca$value)
}

# Mixed forest
lsm_type <- ca %>%                              
  filter(class == 6)
if(length(lsm_type$value) > 0){
  metrics_comp[i,6] <- lsm_type$value/sum(ca$value)
}

# Shrubland, tropical
lsm_type <- ca %>%                              
  filter(class == 7)
if(length(lsm_type$value) > 0){
  metrics_comp[i,7] <- lsm_type$value/sum(ca$value)
}

# Shrubland, temperate
lsm_type <- ca %>%                              
  filter(class == 8)
if(length(lsm_type$value) > 0){
  metrics_comp[i,8] <- lsm_type$value/sum(ca$value)
}

# Grassland, tropical
lsm_type <- ca %>%                              
  filter(class == 9)
if(length(lsm_type$value) > 0){
  metrics_comp[i,9] <- lsm_type$value/sum(ca$value)
}

# Grassland, temperate
lsm_type <- ca %>%                              
  filter(class == 10)
if(length(lsm_type$value) > 0){
  metrics_comp[i,10] <- lsm_type$value/sum(ca$value)
}


# Shrubland, polar lichen-moss
lsm_type <- ca %>%                              
  filter(class == 11)
if(length(lsm_type$value) > 0){
  metrics_comp[i,11] <- lsm_type$value/sum(ca$value)
}

# Grassland, polar lichen-moss
lsm_type <- ca %>%                              
  filter(class == 12)
if(length(lsm_type$value) > 0){
  metrics_comp[i,11] <- lsm_type$value/sum(ca$value)
}

# Barren, polar lichen-moss
lsm_type <- ca %>%                              
  filter(class == 13)
if(length(lsm_type$value) > 0){
  metrics_comp[i,11] <- lsm_type$value/sum(ca$value)
}

# Wetland
lsm_type <- ca %>%                              
  filter(class == 14)
if(length(lsm_type$value) > 0){
  metrics_comp[i,11] <- lsm_type$value/sum(ca$value)
}

# Cropland
lsm_type <- ca %>%                              
  filter(class == 15)
if(length(lsm_type$value) > 0){
  metrics_comp[i,11] <- lsm_type$value/sum(ca$value)
}

# Barren
lsm_type <- ca %>%                              
  filter(class == 16)
if(length(lsm_type$value) > 0){
  metrics_comp[i,11] <- lsm_type$value/sum(ca$value)
}

# Urban
lsm_type <- ca %>%                              
  filter(class == 17)
if(length(lsm_type$value) > 0){
  metrics_comp[i,11] <- lsm_type$value/sum(ca$value)
}

# Water
lsm_type <- ca %>%                              
  filter(class == 18)
if(length(lsm_type$value) > 0){
  metrics_comp[i,11] <- lsm_type$value/sum(ca$value)
}

# Ice and snow
lsm_type <- ca %>%                              
  filter(class == 19)
if(length(lsm_type$value) > 0){
  metrics_comp[i,11] <- lsm_type$value/sum(ca$value)
}


##### Calculate next set of metrics
# Reclassify, setting all the natural land cover types to 20
m <- c(1, 20, 2, 20, 3, 20, 4, 20, 5, 20,
       6, 20, 7, 20, 8, 20, 9, 20, 10, 20,
       11, 20, 12, 20, 13, 20, 14, 20, 16, 20
)
rcmat <- matrix(m, ncol = 2, byrow = TRUE)

ls <- reclassify(ls_pre, rcmat)


# Class-level metrics
mpa <- lsm_c_area_mn(ls)   # Mean patch size/area for each class (land cover group), in hectares
pd <- lsm_c_pd(ls)         # Patch density for each class
ed <- lsm_c_ed(ls)         # Edge density for each class
enn <- lsm_c_enn_mn(ls)   # Mean Euclidean nearest-neighbor distance ('overall patch isolation')


# Mean patch area for the natural patches
lsm_type <- mpa %>%                               
  filter(class == 20)
if(length(lsm_type$value) > 0){
  metrics_conf[i,1] <- lsm_type$value
}

# Patch density of natural patches
lsm_type <- pd %>%                               
  filter(class == 20)
if(length(lsm_type$value) > 0){
  metrics_conf[i,2] <- lsm_type$value
}

# Edge density of natural patches
lsm_type <- ed %>%                               
  filter(class == 20)
if(length(lsm_type$value) > 0){
  metrics_conf[i,3] <- lsm_type$value
}

# Overall isolation of natural patches
lsm_type <- enn %>%                               
  filter(class == 20)
if(length(lsm_type$value) > 0){
  metrics_conf[i,4] <- lsm_type$value
}



# Landscape-level metrics
sdhi <- lsm_l_shdi(ls)     # Shannon's index for patch diversity
metrics_conf[i,5] <- sdhi$value

plot(ls); plot(pt, add = T)   # plot (very optional)

print(i)
print(sites[i,]$site)




########
# now do it for all 
start_time <- Sys.time()
for(i in 1:nsite){
  pt <- sites[i,]                                         # select a point
  buff <- gBuffer(pt, width = buffer, quadsegs = 25)      # Create buffer around the point
  #plot(buff); plot(pt, add = TRUE)
  
  ls_pre <- nalc %>%
    crop(extent(buff)) %>%
    mask(buff)
  
  ##### Calculate the first set of metrics
  # No reclassification necessary
  ca <- lsm_c_ca(ls_pre)        
  
  
  #plot(ls); plot(pt, add = T)   # plot (very optional)
  
  # Proportional area of each land cover class for each 
  # filter out the the data for each metric
  # convert from hectares to m^2 and calculate cover proportion based on buffer (10000 m^2/hectare / pi*r^2 square meters)
  # add to original array/matrix only if there is a value for that land cover class to add]
  
  # Temperate needleleaf (evergreen) forest
  lsm_type <- ca %>%                               
    filter(class == 1)
  if(length(lsm_type$value) > 0){
    metrics_comp[i,1] <- lsm_type$value/sum(ca$value)
  }
  
  # repeat for each land cover class
  # Subpolar taiga needleleaf (evergreen) forest
  lsm_type <- ca %>%                              
    filter(class == 2)
  if(length(lsm_type$value) > 0){
    metrics_comp[i,2] <- lsm_type$value/sum(ca$value)
  }
  
  # Tropical and sub-tropical evergreen broadleaf forest
  lsm_type <- ca %>%                              
    filter(class == 3)
  if(length(lsm_type$value) > 0){
    metrics_comp[i,3] <- lsm_type$value/sum(ca$value)
  }
  # Tropical and sub-tropical deciduous broadleaf forest
  lsm_type <- ca %>%                              
    filter(class == 4)
  if(length(lsm_type$value) > 0){
    metrics_comp[i,4] <- lsm_type$value/sum(ca$value)
  }
  
  # Temperate deciduous broadleaf forest
  lsm_type <- ca %>%                              
    filter(class == 5)
  if(length(lsm_type$value) > 0){
    metrics_comp[i,5] <- lsm_type$value/sum(ca$value)
  }
  
  # Mixed forest
  lsm_type <- ca %>%                              
    filter(class == 6)
  if(length(lsm_type$value) > 0){
    metrics_comp[i,6] <- lsm_type$value/sum(ca$value)
  }
  
  # Shrubland, tropical
  lsm_type <- ca %>%                              
    filter(class == 7)
  if(length(lsm_type$value) > 0){
    metrics_comp[i,7] <- lsm_type$value/sum(ca$value)
  }
  
  # Shrubland, temperate
  lsm_type <- ca %>%                              
    filter(class == 8)
  if(length(lsm_type$value) > 0){
    metrics_comp[i,8] <- lsm_type$value/sum(ca$value)
  }
  
  # Grassland, tropical
  lsm_type <- ca %>%                              
    filter(class == 9)
  if(length(lsm_type$value) > 0){
    metrics_comp[i,9] <- lsm_type$value/sum(ca$value)
  }
  
  # Grassland, temperate
  lsm_type <- ca %>%                              
    filter(class == 10)
  if(length(lsm_type$value) > 0){
    metrics_comp[i,10] <- lsm_type$value/sum(ca$value)
  }
  
  
  # Shrubland, polar lichen-moss
  lsm_type <- ca %>%                              
    filter(class == 11)
  if(length(lsm_type$value) > 0){
    metrics_comp[i,11] <- lsm_type$value/sum(ca$value)
  }
  
  # Grassland, polar lichen-moss
  lsm_type <- ca %>%                              
    filter(class == 12)
  if(length(lsm_type$value) > 0){
    metrics_comp[i,12] <- lsm_type$value/sum(ca$value)
  }
  
  # Barren, polar lichen-moss
  lsm_type <- ca %>%                              
    filter(class == 13)
  if(length(lsm_type$value) > 0){
    metrics_comp[i,13] <- lsm_type$value/sum(ca$value)
  }
  
  # Wetland
  lsm_type <- ca %>%                              
    filter(class == 14)
  if(length(lsm_type$value) > 0){
    metrics_comp[i,14] <- lsm_type$value/sum(ca$value)
  }
  
  # Cropland
  lsm_type <- ca %>%                              
    filter(class == 15)
  if(length(lsm_type$value) > 0){
    metrics_comp[i,15] <- lsm_type$value/sum(ca$value)
  }
  
  # Barren
  lsm_type <- ca %>%                              
    filter(class == 16)
  if(length(lsm_type$value) > 0){
    metrics_comp[i,16] <- lsm_type$value/sum(ca$value)
  }
  
  # Urban
  lsm_type <- ca %>%                              
    filter(class == 17)
  if(length(lsm_type$value) > 0){
    metrics_comp[i,17] <- lsm_type$value/sum(ca$value)
  }
  
  # Water
  lsm_type <- ca %>%                              
    filter(class == 18)
  if(length(lsm_type$value) > 0){
    metrics_comp[i,18] <- lsm_type$value/sum(ca$value)
  }
  
  # Ice and snow
  lsm_type <- ca %>%                              
    filter(class == 19)
  if(length(lsm_type$value) > 0){
    metrics_comp[i,19] <- lsm_type$value/sum(ca$value)
  }
  
  
  ##### Calculate next set of metrics
  # Reclassify, setting all the natural land cover types to 20
  m <- c(1, 20, 2, 20, 3, 20, 4, 20, 5, 20,
         6, 20, 7, 20, 8, 20, 9, 20, 10, 20,
         11, 20, 12, 20, 13, 20, 14, 20, 16, 20
  )
  rcmat <- matrix(m, ncol = 2, byrow = TRUE)
  
  ls <- reclassify(ls_pre, rcmat)
  
  
  # Class-level metrics
  mpa <- lsm_c_area_mn(ls)   # Mean patch size/area for each class (land cover group), in hectares
  pd <- lsm_c_pd(ls)         # Patch density for each class
  ed <- lsm_c_ed(ls)         # Edge density for each class
  enn <- lsm_c_enn_mn(ls)   # Mean Euclidean nearest-neighbor distance ('overall patch isolation')
  
  
  # Mean patch area for the natural patches
  lsm_type <- mpa %>%                               
    filter(class == 20)
  if(length(lsm_type$value) > 0){
    metrics_conf[i,1] <- lsm_type$value
  }
  
  # Patch density of natural patches
  lsm_type <- pd %>%                               
    filter(class == 20)
  if(length(lsm_type$value) > 0){
    metrics_conf[i,2] <- lsm_type$value
  }
  
  # Edge density of natural patches
  lsm_type <- ed %>%                               
    filter(class == 20)
  if(length(lsm_type$value) > 0){
    metrics_conf[i,3] <- lsm_type$value
  }
  
  # Overall isolation of natural patches
  lsm_type <- enn %>%                               
    filter(class == 20)
  if(length(lsm_type$value) > 0){
    metrics_conf[i,4] <- lsm_type$value
  }
  
  
  
  # Landscape-level metrics
  sdhi <- lsm_l_shdi(ls)     # Shannon's index for patch diversity
  metrics_conf[i,5] <- sdhi$value
  
  #plot(ls); plot(pt, add = T)   # plot (very optional)
  
  #print(i)
  #print(sites[i,]$site)  
}

end_time <- Sys.time()

elapsed_time <- difftime(end_time, start_time, units='mins')
cat(paste(paste('Metrics calculated in ', elapsed_time, sep=''), ' minutes', sep=''))

data_export <- data.frame(cam_wa@data, metrics_comp, metrics_conf)
data_export







# Make sure 'city' is a factor
data_export$city <- as.factor(data_export$city)

# add columns combining class areas

data_export <- data_export %>% 
  mutate(
    # all forest types
    for_all = (for_ever_temp + for_ever_taig + for_ever_trop + for_dec_trop + for_dec_temp + for_mix),
    # all shrubland types
    shrub_all = (shrub_trop + shrub_temp + shrub_lich),
    # all grassland types
    grass_all = (grass_trop + grass_temp + grass_lich),
    # all woody vegetation cover types (excluding herbaceous grassland cover)
    veg_woody = (for_ever_temp + for_ever_taig + for_ever_trop + for_dec_trop + for_dec_temp + for_mix + shrub_trop + shrub_temp + shrub_lich),  
    # all vegetation cover types (woody and herbaceous). 'Wetland' does not necessarily represent
    veg_all = (for_ever_temp + for_ever_taig + for_ever_trop + for_dec_trop + for_dec_temp + for_mix + shrub_trop + shrub_temp + shrub_lich + grass_trop + grass_temp + grass_lich),      
    # all natural/undeveloped/minimally-developed land cover classes
    natural = (for_ever_temp + for_ever_taig + for_ever_trop + for_dec_trop + for_dec_temp + for_mix + shrub_trop + shrub_temp + grass_trop + grass_temp + shrub_lich + grass_lich + barren_lich + wetland + barren + ice),
    # anthropogenic land cover types
    develop = (urban + cropland)
  ) %>%
  arrange(city, site)



# make sure the city-site index column is filled
data_export$city_site <- paste(data_export$city, data_export$site, sep = "_")
is_us$city_site <- paste(is_us$city, is_us$site, sep = "_")
is_us$city_site

#  add percent impervious surface

#data_export <- read.csv("C:/Research/urban/UWIN/data/3_spatial/covariates/sitecov_UWIN_landscapemetrics_1000m_20cities_allsites.csv")

data_export <- data_export %>% arrange(city_site)
is_all <- is_us %>% arrange(city_site)
length(match(unique(data_export$city_site),unique(is_all$city_site))) # make sure all the sites match

# Add the impervious surface percentage
data_export$Impervious <- is_all$Impervious
colnames(data_export)
# remove unneeded columns
data_export <- data_export %>% dplyr::select(-c(season, species, count, dys_ctv))



##### RELATIUONSHIPS BETWEEN lANDSCAPE METRICS

library(ggcorrplot)
library(GGally)

data_ls <- read.csv("data/covariates/sitecov_landscapemetrics_1000m_sewatawa_allsites.csv", header = TRUE)
data_ls$ice <- as.numeric(data_ls$ice)    # 'ice' should be a proportion like the rest, even though that proportion is 0

#landscape_correlations <- round(cor(data_ls[,c(8,10:27)], use = "complete.obs"), 1)
landscape_correlations <- cor(data_ls[,c(8,10:27)], use = "complete.obs")
ggcorrplot(landscape_correlations, 
           #method = "circle", 
           #hc.order = TRUE, 
           type = "lower",
           outline.col = "white", 
           lab = TRUE, 
           digits = 1) +
  ggplot2::labs(x = 'X label', y = 'Y label') +
  ggplot2::theme(
    axis.title.x = element_text(angle = 0, color = 'grey20'),
    axis.title.y = element_text(angle = 90, color = 'grey20'))
#ggpairs(data_ls[,c(8,10:21,23:24)], title="correlogram with ggpairs()")  # takes a minute and looks a bit messy
#landscape_correlations <- pairs(data_ls[,c(8,10:21,23:27)])

#landscape_correlations





### Developed Land Cover vs. Mean Patch Area

ggplot(data = data_ls, aes(x = develop, y = mpa_undev, group = city, color = city, fill = city)) +
  geom_point() +
  #geom_smooth(method = "lm", lwd = 0.5, alpha = 0.1, se = FALSE) +    # se = TRUE or FALSE are both interesting
  #gghighlight(city %in% c("phaz")) +
  #scale_color_brewer()+
  labs(x = "Developed Land Cover Proportion", y = "Mean Undeveloped Patch Area")



### Developed Land Cover vs. Patch Density

ggplot(data = data_ls, aes(x = develop, y = pd_undev, group = city, color = city, fill = city)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.5, alpha = 0.1, se = TRUE)+
  #geom_smooth(method = "lm", lwd = 0.5, alpha = 0.1, se = FALSE) +    # se = TRUE or FALSE are both interesting
  #gghighlight(city %in% c("phaz")) +
  #scale_color_brewer()+
  labs(x = "Developed Land Cover Proportion", y = "Undeveloped Patch Density")


### Developed Land Cover vs. Edge Density
ggplot(data = data_ls, aes(x = develop, y = ed_undev, group = city, color = city, fill = city)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.5, alpha = 0.1, se = FALSE)+
  #geom_smooth(method = "lm", lwd = 0.5, alpha = 0.1, se = FALSE) +    # se = TRUE or FALSE are both interesting
  #gghighlight(city %in% c("chil")) +
  #scale_color_brewer()+
  labs(x = "Developed Land Cover Proportion", y = "Undeveloped Edge Density")


### Developed Land Cover vs. Overall Patch Isolation
ggplot(data = data_ls, aes(x = develop, y = enn_mn_undev, color = city)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.7, alpha = 0.1, se = FALSE)+
  #gghighlight(city %in% c("phaz")) +
  labs(x = "Developed Land Cover Proportion", y = "Patch Isolation")


### Developed Land Cover vs. Shannon's Index for Patch Diversity
ggplot(data = data_ls, aes(x = develop, y = sdhi, color = city)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.7, alpha = 0.1, se = FALSE)+
  #gghighlight(city %in% c("phaz")) +
  labs(x = "Developed Land Cover Proportion", y = "Shannon's Diversity Index")




### Developed Land Cover vs. Undeveloped Land Cover Proportion, colored by City
# Interesting how there is what seems to be a perpendicular gradient. Let's see if that corresponds to agriculture

ggplot(data = data_ls, aes(x = develop, y = natural, group = city, color = city, fill = city)) +
  geom_point() +
  geom_smooth(method = "lm", lwd = 0.5, alpha = 0.1, se = FALSE) +    # se = TRUE or FALSE are both interesting
  #gghighlight(city %in% c("phaz")) +
  #scale_color_brewer()+
  labs(x = "Developed Land Cover Proportion", y = "Undeveloped Land Cover Proportion")


### Developed Land Cover vs. Undeveloped Land Cover Proportion, colored by Cropland

ggplot(data = data_ls, aes(x = develop, y = natural, color = cropland)) +
  geom_point()

### Ternary Plot of Land Cover: Developed vs. Undeveloped vs. Cropland
install.packages("ggtern")
library(ggtern)

# can use ggtern() instead of ggplot() + coord_tern()
landcover_ternary <- ggtern(data = data_ls, aes(x = develop, y = natural, z = cropland, color = sdhi)) +
  geom_point() +
  scale_color_viridis()+
  labs(x = "Developed", y = "Undeveloped", z = "Agriculture")

landcover_ternary



### Patch Density vs. Patch Isolation (Undeveloped patches)

ggplot(data = data_ls, aes(x = pd_undev, y = enn_mn_undev, color = city)) +
  geom_point() +
  #stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.7, alpha = 0.1, se = FALSE)+
  gghighlight(city %in% c("chil")) +
  labs(x = "Undeveloped Patch Density", y = "Undeveloped Patch Isolation")



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
                         variables = c(medincome="B19013_001"),
                         year = 2019, geometry = TRUE) # need to specify 2015-2019 data 
# since acs now automatically calls 2017-2021 

# cut out everything except king and pierce county from our WA shp 
# specify dplyr because rgdal also has filter fxn 
# do this using the GEOID codes for each county 

tractsKP <- tractincomeWA %>% dplyr::filter(substr(GEOID, 1, 5) 
                                            %in% c("53033", "53053"))

 mapview(tractsKP) # looks good


# crop to actual study area 
tractsKP <- st_transform(tractsKP, crs=4326)
tractsKP_crop <- st_crop(tractsKP, c(xmin= -121.7, ymin = 46.7, xmax = -122.8, ymax = 47.8))
mapview(tractsKP_crop)


# write shapefiles for cropped area 
st_write(tractsKP_crop, here("data", "income_maps", "seatac_urban_med_income.shp"),
     append = FALSE)




#  transform to raster
template <- rast(vect(tractsKP_crop),res=0.005)

wa_inc_rast <- terra::rasterize(vect(tractsKP_crop), template, field = "estimate")
plot(wa_inc_rast,col="red"); plot(points_wa, add = TRUE)


# reproject 
wa_inc_rast <- terra::project(wa_inc_rast, "EPSG:32610")

# is a spatraster, convert to raster 

wa_inc_rast <- wa_inc_rast %>% raster()

# transform points to same proj 
cam_inc <- cam_wa %>%
  spTransform(crs(wa_inc_rast))
crs(wa_inc_rast)
sites <- cam_inc

med_inc <- data.frame(
  sites@data,
  "med_income" = rep(NA, nrow(sites@data))
)

i <- 1          # select the site
buffer <- 1000   # choose the buffer size, in meters


# Takes about 8 minutes
Sys.time()


for(i in 1:length(sites)){
  pt <- sites[i,]                                         # select a point
  buff <- gBuffer(pt, width = buffer, quadsegs = 25)      # create buffer around the point. This determines the size of the landscape
  
  income <- wa_inc_rast %>%
    crop(extent(buff)) %>%
    mask(buff)
  
  med_inc$med_income[i] <- extract(income, buff, fun = mean, na.rm = TRUE)
  
  plot(income); plot(pt, add = TRUE)
  print(i)
}
Sys.time()

med_inc <- med_inc %>%
  dplyr::select(c(site, city, med_income))
med_inc


# add city site 
med_inc$city_site <- paste(med_inc$city, med_inc$site, sep = "_")


#  add to data export 

data_export <- data_export %>% arrange(city_site)
med_inc <- med_inc %>% arrange(city_site)
length(match(unique(data_export$city_site),unique(med_inc$city_site))) # make sure all the sites match



################################################
# NDVI

# GEE composite code from Gina Rosa Cova
# https://code.earthengine.google.com/3a9b724aa57b64d3c54b30249761db56
# Took ~10 minutes to download 
# 30m resolution

# NDVI Extraction code from Travis Gallo

data_export <- read_csv("data/covariates/sitecov_1000m_sewatawa_allsites.csv")

plot(ndvi_kp)
# load LandSat NDVI raster from GRC GEE code 

ndvi_kp <- raster(here("data", "NDVI_data", "NDVI2020SEWATAWA3038_1.tif"))

suggest_top_crs(ndvi_kp)
ndvi_kp <- projectRaster(ndvi_kp, crs = "EPSG:6599")

# reproject points to raster crs 
points_wa <- st_transform(points_wa, crs = st_crs(ndvi_kp))

# extract the proportion of the buffer that has an NDVI greater than 0.2 (vegetation cover)
# this returns a list, so we can use lapply to calculate the proportion for each site
ndvi_extract <- raster::extract(ndvi_kp, points_wa, buffer = 1000)

# calculate the proportion of a site that is covered in vegetation
prop_ndvi_greater0.2 <- lapply(ndvi_extract, function (x){
  # turn values greater than 0.6 to 1 (these are cells that are covered in vegetation)
  x[which(x > 0.6)] <- 1
  # turn values less than 0.5 to 0 (these are cells that are not vegetation)
  x[which(x <= 0.6)] <- 0
  # proportion of cells that are vegetation
  sum(x, na.rm = TRUE)/length(x) })


# turn list into a vector
prop_veg <- do.call(c, prop_ndvi_greater0.2)
prop_veg

# add to points_wa
points_wa$prop_veg <- prop_veg


# rearrange points_wa and data_export to match 

head(data_export)
head(points_wa)

data_export <- data_export %>% arrange(city_site)

points_wa$city_site <- paste(points_wa$city, points_wa$site, sep = "_")
points_wa <- points_wa %>% arrange(city_site)


data_export$prop_veg <- points_wa$prop_veg

# Add the impervious surface percentage
data_export$med_income <- med_inc$med_income
colnames(data_export)

# add city site 
pop_dat$city_site <- paste(pop_dat$city, pop_dat$site, sep = "_")

data_export <- data_export %>% arrange(city_site)
pop_dat <- pop_dat %>% arrange(city_site)
length(match(unique(data_export$city_site),unique(pop_dat$city_site))) # make sure all the sites match
data_export$pop_density <- pop_dat$pop_density

## export 
write.csv(data_export,  "data/covariates/sitecov_1000m_sewatawa_allsites.csv", row.names = FALSE)


