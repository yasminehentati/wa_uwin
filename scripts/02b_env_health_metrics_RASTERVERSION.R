################################################################################
## ENVIRONMENTAL HEALTH DATA 
# obtained from washington environmental health disparities map

library(pacman)

p_load(dplyr,sp,raster,here,sf,mapview,raster,remotes,tidycensus,
       tidyr,terra,spatialEco,readr,ggfortify,exactextractr,spatstat,
       spdep,landscapemetrics,tmap,viridis,gghighlight,purrr,plainview,tidyterra)



# note that mapview is somewhat demanding, consider skipping
# mapview functions if your computer is slow  - these are just used to check 
# that shapefiles look correct 

 # remotes::install_github("walkerke/crsuggest") 
library(crsuggest)

##
## read in data 


# read in env health data 
lead <- read_csv(here("data", "env_health_data", "Lead_Risk_from_Housing.csv"),
                 col_names=c("County_Name", "Census_Tract", "Units_Lead", "Total_Units", "Pct_Units_Lead", "IBL_Rank"))

pm25 <- read_csv(here("data", "env_health_data", "PM2.5_Concentration.csv"), 
                 col_names=c("County_Name", "YearGroup", "Census_Tract", "PM2.5_Count", "IBL_Rank"))

prox_haz_waste <- read_csv(here("data", "env_health_data", 
                                "Proximity_to_Hazardous_Waste_Treatment_Storage_and_Disposal_Facilities_(TSDFs).csv"),
                           col_names=c("County_Name", "Census_Tract", "Average_PTSDF", "IBL_Rank"))

prox_heavy_traff <- read_csv(here("data", "env_health_data", 
                                          "Proximity_to_Heavy_Traffic_Roadways.csv"),
                                     col_names=c("County_Name", "Census_Tract", "Prox_Heavy_Traffic_Roadways", "IBL_Rank"))

prox_superfund <- read_csv(here("data", "env_health_data", "Proximity_to_National_Priorities_List_Facilities_(Superfund_Sites).csv"),
                           col_names=c("County_Name", "Census_Tract", "Average_PNPL", "IBL_Rank"))

toxic_release <- read_csv(here("data", "env_health_data", "Toxic_Releases_from_Facilities_(RSEI_Model).csv"),
                          col_names=c("County_Name", "Census_Tract", "Average_RSEI_Concentrations", "IBL_Rank"))

wastewater <- read_csv(here("data", "env_health_data", "Wastewater_Discharge.csv"),
                       col_names=c("County_Name", "Census_Tract", "Average_PWDIS", "IBL_Rank"))

diesel_nox <- read_csv(here("data", "env_health_data", "Diesel_Emission_Levels_of_NOx_(Annual_Tons_Km2).csv"),
                       col_names=c("County_Name", "Census_Tract", "Annual Tons Km2 Diesel NOx", "IBL_Rank"))

# combine all into one DF 

# first list data frames together 
listdf <- list(lead, pm25, prox_haz_waste, prox_heavy_traff, prox_superfund,
               toxic_release, wastewater, diesel_nox)

# remove ranking number 
listdf2 <- lapply(listdf, subset, select = -IBL_Rank)

# combine data by GEOID 
envdat <- listdf2 %>% reduce(full_join, by = c("Census_Tract", "County_Name"))

# remove first two rows 
envdat <- tail(envdat, -2)

#get only our counties of interest 
envdat <- envdat %>% dplyr::filter(substr(Census_Tract, 1, 5) 
                                 %in% c("53033", "53053")) # king pierce 
                             #            "53061", "53035", # snoho kitsap
                                #        "53029", "53057")) # island skagit


# rename census tract to geoid 
envdat <- rename(envdat,c("GEOID" = "Census_Tract"))

envdat$Pct_Units_Lead
## create polygons for env health data based on geoids 
# we'll use the income data to do this 

# read in income data
tractsKP <- st_read(here("data", "income_maps", "seatac_urban_med_income.shp")) 

colnames(tractsKP)
# merge env health data to polygons 

# join based on GEOID 
envdatSP <- merge(tractsKP, envdat, by = "GEOID") 
envdatSP$Pct_Units_Lead

##### read in camera data so we have our points for the buffers 
# same proj 


camdat <- readOGR(dsn = "data/cameras/points_wa.shp") %>% spTransform(crs(tractsKP))


# plot one of our variables to check 
# same proj 

mapview(camdat)
mapview(list(camdat, envdatSP),
        zcol = list(NULL, "Average_PNPL"))


########### transform polygon into raster for each variable

#### 1. Lead risk 
template <- rast(vect(envdatSP),res=0.005)

lead_rast <- terra::rasterize(vect(envdatSP), template, field = "Pct_Units_Lead")
class(lead_rast)


# reproject 
lead_rast <- terra::project(lead_rast, "EPSG:32610")


# is a spatraster, convert to raster 
lead_rast <- raster(lead_rast)

cam_env <- camdat %>%
  spTransform(crs(lead_rast))
crs(lead_rast)
sites <- cam_env


plot(lead_rast,col=rainbow(100)); plot(camdat,col="blue", add = TRUE)


lead_dat <- data.frame(
  sites@data,
  "Pct_Units_Lead" = rep(NA, nrow(sites@data))
)

i <- 1          # select the site
buffer <- 1000   # choose the buffer size, in meters

cam_env <- camdat %>%
  spTransform(crs(lead_rast))

# Takes about 8 minutes
Sys.time()

# calculate buffer 1000m around camera point 


for(i in 1:length(sites)){
  pt <- sites[i,]                                         # select a point
  buff <- gBuffer(pt, width = buffer, quadsegs = 25)      # create buffer around the point. This determines the size of the landscape
  
  lead <- lead_rast %>%
    crop(extent(buff)) %>%
    mask(buff)
  
  lead_dat$Pct_Units_Lead[i] <- extract(lead, buff, fun = mean, na.rm = TRUE)
  
  plot(lead); plot(pt, add = TRUE)
  print(i)
}
Sys.time()

lead_dat <- lead_dat %>%
  dplyr::select(c(site, city, Pct_Units_Lead))
lead_dat

########################## 2. PM2.5 

pm25_rast <- terra::rasterize(vect(envdatSP), template, field = "PM2.5_Count")

# reproject 
pm25_rast <- terra::project(pm25_rast, "EPSG:32610")

# is a spatraster, convert to raster 
pm25_rast <- raster(pm25_rast)

plot(pm25_rast,col=rainbow(100)); plot(camdat,col="blue", add = TRUE)


pm25_dat <- data.frame(
  sites@data,
  "PM2.5_Count" = rep(NA, nrow(sites@data))
)

i <- 1          # select the site
buffer <- 1000   # choose the buffer size, in meters

# Takes about 8 minutes
Sys.time()

# calculate buffer 1000m around camera point 

for(i in 1:length(sites)){
  pt <- sites[i,]                                         # select a point
  buff <- gBuffer(pt, width = buffer, quadsegs = 25)      # create buffer around the point. This determines the size of the landscape
  
  pm25 <- pm25_rast %>%
    crop(extent(buff)) %>%
    mask(buff)
  
  pm25_dat$PM2.5_Count[i] <- extract(pm25, buff, fun = mean, na.rm = TRUE)
  
  plot(pm25); plot(pt, add = TRUE)
  print(i)
}
Sys.time()

pm25_dat <- pm25_dat %>%
  dplyr::select(c(site, city, PM2.5_Count))
pm25_dat

##########################  3. Proximity to hazardous waste

hazwaste_rast <- terra::rasterize(vect(envdatSP), template, field = "Average_PTSDF")

# reproject 
hazwaste_rast <- terra::project(hazwaste_rast, "EPSG:32610")

# is a spatraster, convert to raster 
hazwaste_rast  <- raster(hazwaste_rast)

plot(hazwaste_rast,col=rainbow(100)); plot(camdat,col="blue", add = TRUE)


hazwaste_dat <- data.frame(
  sites@data,
  "Average_PTSDF" = rep(NA, nrow(sites@data))
)

i <- 1          # select the site
buffer <- 1000   # choose the buffer size, in meters

# Takes about 8 minutes
Sys.time()

# calculate buffer 1000m around camera point 

for(i in 1:length(sites)){
  pt <- sites[i,]                                         # select a point
  buff <- gBuffer(pt, width = buffer, quadsegs = 25)      # create buffer around the point. This determines the size of the landscape
  
  hazwaste <- hazwaste_rast %>%
    crop(extent(buff)) %>%
    mask(buff)
  
  hazwaste_dat$Average_PTSDF[i] <- extract(hazwaste, buff, fun = mean, na.rm = TRUE)
  
  plot(hazwaste); plot(pt, add = TRUE)
  print(i)
}
Sys.time()

hazwaste_dat <- hazwaste_dat %>%
  dplyr::select(c(site, city, Average_PTSDF))
hazwaste_dat

########################## 4. Proximity to heavy traffic roadways

traffic_rast <- terra::rasterize(vect(envdatSP), template, field = "Prox_Heavy_Traffic_Roadways")

# reproject 
traffic_rast <- terra::project(traffic_rast, "EPSG:32610")

# is a spatraster, convert to raster 
traffic_rast  <- raster(traffic_rast)

plot(traffic_rast,col=rainbow(100)); plot(camdat,col="blue", add = TRUE)


traffic_dat <- data.frame(
  sites@data,
  "Prox_Heavy_Traffic_Roadways" = rep(NA, nrow(sites@data))
)

i <- 1          # select the site
buffer <- 1000   # choose the buffer size, in meters

# Takes about 8 minutes
Sys.time()

# calculate buffer 1000m around camera point 

for(i in 1:length(sites)){
  pt <- sites[i,]                                         # select a point
  buff <- gBuffer(pt, width = buffer, quadsegs = 25)      # create buffer around the point. This determines the size of the landscape
  
  traffic <- traffic_rast %>%
    crop(extent(buff)) %>%
    mask(buff)
  
  traffic_dat$Prox_Heavy_Traffic_Roadways[i] <- extract(traffic, buff, fun = mean, na.rm = TRUE)
  
  plot(traffic); plot(pt, add = TRUE)
  print(i)
}
Sys.time()

traffic_dat <- traffic_dat %>%
  dplyr::select(c(site, city, Prox_Heavy_Traffic_Roadways))
traffic_dat


########################## 5. Proximity to superfund sites 

pnpl_rast <- terra::rasterize(vect(envdatSP), template, field = "Average_PNPL")

# reproject 
pnpl_rast <- terra::project(pnpl_rast, "EPSG:32610")

# is a spatraster, convert to raster 
pnpl_rast  <- raster(pnpl_rast)

plot(pnpl_rast,col=rainbow(100)); plot(camdat,col="blue", add = TRUE)


pnpl_dat <- data.frame(
  sites@data,
  "Average_PNPL" = rep(NA, nrow(sites@data))
)

i <- 1          # select the site
buffer <- 1000   # choose the buffer size, in meters

# Takes about 8 minutes
Sys.time()

# calculate buffer 1000m around camera point 

for(i in 1:length(sites)){
  pt <- sites[i,]                                         # select a point
  buff <- gBuffer(pt, width = buffer, quadsegs = 25)      # create buffer around the point. This determines the size of the landscape
  
  pnpl <- pnpl_rast %>%
    crop(extent(buff)) %>%
    mask(buff)
  
  pnpl_dat$Average_PNPL[i] <- extract(pnpl, buff, fun = mean, na.rm = TRUE)
  
  plot(pnpl); plot(pt, add = TRUE)
  print(i)
}
Sys.time()

pnpl_dat <- pnpl_dat %>%
  dplyr::select(c(site, city, Average_PNPL))
pnpl_dat

##########################  6. Toxic release 

toxic_rast <- terra::rasterize(vect(envdatSP), template, field = "Average_RSEI_Concentrations")

# reproject 
toxic_rast <- terra::project(toxic_rast, "EPSG:32610")

# is a spatraster, convert to raster 
toxic_rast  <- raster(toxic_rast)

plot(toxic_rast,col=rainbow(100)); plot(camdat,col="blue", add = TRUE)


toxic_dat <- data.frame(
  sites@data,
  "Average_RSEI_Concentrations" = rep(NA, nrow(sites@data))
)

i <- 1          # select the site
buffer <- 1000   # choose the buffer size, in meters

# Takes about 8 minutes
Sys.time()

# calculate buffer 1000m around camera point 

for(i in 1:length(sites)){
  pt <- sites[i,]                                         # select a point
  buff <- gBuffer(pt, width = buffer, quadsegs = 25)      # create buffer around the point. This determines the size of the landscape
  
  toxic <- toxic_rast %>%
    crop(extent(buff)) %>%
    mask(buff)
  
  toxic_dat$Average_RSEI_Concentrations[i] <- extract(toxic, buff, fun = mean, na.rm = TRUE)
  
  plot(toxic); plot(pt, add = TRUE)
  print(i)
}
Sys.time()

toxic_dat <- toxic_dat %>%
  dplyr::select(c(site, city, Average_RSEI_Concentrations))
toxic_dat

##########################  7. Wastewater discharge 

water_rast <- terra::rasterize(vect(envdatSP), template, field = "Average_PWDIS")

# reproject 
water_rast <- terra::project(water_rast, "EPSG:32610")

# is a spatraster, convert to raster 
water_rast  <- raster(water_rast)

plot(water_rast,col=rainbow(100)); plot(camdat,col="blue", add = TRUE)


water_dat <- data.frame(
  sites@data,
  "Average_PWDIS" = rep(NA, nrow(sites@data))
)

i <- 1          # select the site
buffer <- 1000   # choose the buffer size, in meters

# Takes about 8 minutes
Sys.time()

# calculate buffer 1000m around camera point 

for(i in 1:length(sites)){
  pt <- sites[i,]                                         # select a point
  buff <- gBuffer(pt, width = buffer, quadsegs = 25)      # create buffer around the point. This determines the size of the landscape
  
  water <- water_rast %>%
    crop(extent(buff)) %>%
    mask(buff)
  
  water_dat$Average_PWDIS[i] <- extract(water, buff, fun = mean, na.rm = TRUE)
  
  plot(water); plot(pt, add = TRUE)
  print(i)
}
Sys.time()

water_dat <- water_dat %>%
  dplyr::select(c(site, city, Average_PWDIS))
water_dat


##########################  8. Diesel NOx emissions


diesel_rast <- terra::rasterize(vect(envdatSP), template, field = "Annual Tons Km2 Diesel NOx")

# reproject 
diesel_rast <- terra::project(diesel_rast, "EPSG:32610")


# is a spatraster, convert to raster 
diesel_rast  <- raster(diesel_rast)

plot(diesel_rast,col=rainbow(100)); plot(camdat,col="blue", add = TRUE)

diesel_dat <- data.frame(
  sites@data,
  "Annual_Tons_Km2_Diesel_NOx" = rep(NA, nrow(sites@data))
)

i <- 1          # select the site
buffer <- 1000   # choose the buffer size, in meters

# Takes about 8 minutes
Sys.time()

# calculate buffer 1000m around camera point 
  
  for(i in 1:length(sites)){
    pt <- sites[i,]                                         # select a point
    buff <- gBuffer(pt, width = buffer, quadsegs = 25)      # create buffer around the point. This determines the size of the landscape
    
    diesel <- diesel_rast %>%
      crop(extent(buff)) %>%
      mask(buff)
    
    diesel_dat$Annual_Tons_Km2_Diesel_NOx[i] <- extract(diesel, buff, fun = mean, na.rm = TRUE)
    
    plot(diesel); plot(pt, add = TRUE)
    print(i)
  }
  Sys.time()
  
diesel_dat <- diesel_dat %>%
    dplyr::select(c(site, city, Annual_Tons_Km2_Diesel_NOx))
diesel_dat
  

###### Combine all 
# below code by cesar estien

### bring in data 


#now lets make one big pollutant dataframe for the zonal stats

# first list data frames together 
listdfs <- list(lead_dat, pm25_dat, hazwaste_dat, traffic_dat, pnpl_dat,toxic_dat, water_dat, diesel_dat)


# combine data by city/site
pollutants <- listdfs %>% reduce(full_join, by = c("city", "site"))


#create percentile for each metric. we'll use this to create the pollution burden score later

pollutants <- pollutants %>%
  mutate(LEAD_PCT = ntile(pollutants$Pct_Units_Lead, 100)) %>%
  mutate(PM25_PCT = ntile(pollutants$PM2.5_Count, 100)) %>%
  mutate(HAZARD_PCT = ntile(pollutants$Average_PTSDF, 100)) %>%
  mutate(TRAFFIC_PCT = ntile(pollutants$Prox_Heavy_Traffic_Roadways, 100)) %>%
  mutate(PNPL_PCT = ntile(pollutants$Average_PNPL, 100)) %>% 
  mutate(TOXIC_PCT = ntile(pollutants$Average_RSEI_Concentrations, 100)) %>% 
  mutate(PWDIS_PCT = ntile(pollutants$Average_PWDIS, 100)) %>% 
  mutate(DIESEL_PCT = ntile(pollutants$Annual_Tons_Km2_Diesel_NOx, 100)) 


#CALCULATE OUR OWN POLLUTION BURDEN. We will follow WA Env Health Map Methods

#first we need to average the exposures (pm25, diesel, toxic releases)
# and environmental effects (cleanupsites, lead, gw threat, and hazards for us)

pollutants <- pollutants %>%
  mutate(EXPOSURE = (PM25_PCT + DIESEL_PCT +  TOXIC_PCT) / 3) %>%
  mutate(EFFECT = (LEAD_PCT + PWDIS_PCT + HAZARD_PCT + PNPL_PCT) / 4) %>% 
#then we need to get the indicator for both. exposure stays the same by effects is given HALF weight. so multiply the EFFECTS COLUMN by .5
  mutate(EFFECT = EFFECT * 0.5) %>% 
#BURDEN: now we sum the EXPOSURE AND EFFECT COLUMN (Pollution Burden is calculated as the average of its two component scores, with the Environmental Effects component halfweighted.)
  mutate(BURDEN = EFFECT + EXPOSURE) %>% 
#THEN WE divide it by the weights. + (1 / .5)
  mutate(BURDEN = BURDEN/1.5) %>% 
#then we want to scale it (0-10) so find the max and divide it by that number  
  mutate(BURDEN_SCALED = (((BURDEN /max(BURDEN)) * 10))) %>% 
#then we can bin it as a percentile to help with scale
  mutate(BURDEN_PERCENTILE = ntile(BURDEN_SCALED, 100))

head(pollutants)

# save just the GEOIDS and pollution data 




# bring in urbanizatioon covariate data and merge 

urbcovs <- read_csv(here("data", "covariates", "sitecov_1000m_sewatawa_allsites.csv"))

all_covs <- left_join(urbcovs, pollutants, by = c("city", "site"))

# write 
write_csv(all_covs, here("data", "covariates", "ALL_ENV_URB_SITES_1000m.csv"))



# bind count data to covariate data 

counts <- read_csv(here("data", "wa_counts.csv"))

all_data <- left_join(counts, all_covs, by = c("city", "site"))

write_csv(all_data, here("data", "covariates", "COUNTS_ALL_ENV_URB_SITES_1000m.csv"))




## do not run -- changes after adding housing density

urbcovs <- read_csv(here("data", "covariates", "sitecov_1000m_sewatawa_allsites.csv"))
colnames(urbcovs)
urbcovs <- urbcovs %>% select(c(city, site, pop_density))

all_covs <- read_csv(here("data", "covariates", "ALL_ENV_URB_SITES_1000m.csv"))
all_covs <- left_join(all_covs, urbcovs, by = c("city", "site"))
colnames(all_covs)
write_csv(all_covs, here("data", "covariates", "ALL_ENV_URB_SITES_1000m.csv"))

counts <- read_csv(here("data", "wa_counts.csv"))

all_data <- left_join(counts, all_covs, by = c("city", "site"))

write_csv(all_data, here("data", "covariates", "COUNTS_ALL_ENV_URB_SITES_1000m.csv"))
