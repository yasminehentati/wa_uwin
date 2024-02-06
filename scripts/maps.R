
### load packages 

# install.packages("pacman")
# remotes::install_github("walkerke/crsuggest") 
library(pacman)
pacman::p_load(extrafont, ggplot2, tidyr, dplyr, mapview, lme4, magrittr, lintr, sf, 
               raster, viridis, cowplot, markdown, sf, here, tidycensus,
               crsuggest, terra, spatialEco, readr, ggfortify, rgeos, usmap,
               rnaturalearth, rnaturalearthdata, maps, tools, stringr,
               rmapshaper, cowplot, ggrepel, ggspatial)

library(RColorBrewer)

#### load in camera locations
points <- st_read(here("data", "cameras", "points_wa.shp"))
points <- st_as_sf(points)

#### load in state county and census tract shapefile for study site maps 

wa_map <- st_read(here("data", "figure_data", "tl_2016_53_cousub.shp")) 
wa_tract <- st_read(here("data", "figure_data", "tract20.shp"))

wa_map <- wa_map %>%  st_transform(crs = 4326) %>% 
  st_crop(c(xmin = -125.5, xmax = -121, ymin = 46.5, ymax = 49))

wa_tract <- wa_tract %>%  st_transform(crs = 4326) %>% 
  st_crop(c(xmin = -125.5, xmax = -121, ymin = 46.5, ymax = 49))

# add water
water <- st_read(here("data", "figure_data", 
                      "DNR_Hydrography_-_Water_Bodies_-_Forest_Practices_Regulation.shp")) 

sf_use_s2(FALSE)

# crop to our map area 
water <-  water %>% st_transform(crs = 4326) %>% 
  st_crop(c(xmin = -125.5, xmax = -121, ymin = 46.5, ymax = 49))



# let's put everything in the same proj 

points <- points %>% st_transform(st_crs(wa_map))
st_crs(water) == st_crs(wa_map)

# clip counties shapefile to water 

wa_crop <- ms_erase(wa_map, water)

mapview(wa_crop)

 # for now we'll be lazy and manually erase the lines in the 
# water (that line up with canadian borders) later


# add cities 
# wacities <- data.frame(state = rep("Washington", 5), 
#                    city = c("Seattle", "Tacoma", 
#                             "Olympia", "Port Angeles", "Everett"), 
#                    lat = c(25.7616798, 

# make the big map
washington <- ggplot(data = wa_crop) +
  geom_sf(fill = "antiquewhite4", color = NA) +
  #      geom_sf(data = water, fill = "alice blue", color = gray(.2)) + 
  #      geom_sf(data = points_tawa, size = 1, shape = 23, fill = "darkred") +
  #     annotate(geom = "text", x = -85.5, y = 27.5, label = "Gulf of Mexico", 
  #           color = "grey22", size = 4.5) +
  #     geom_sf(data = wacities) +
  # #     geom_text_repel(data = flcities, aes(x = lng, y = lat, label = city), 
  #                    fontface = "bold", nudge_x = c(1, -1.5, 2, 2, -1), nudge_y = c(0.25, 
  #                                                                                   -0.25, 0.5, 0.5, -0.5)) +
  coord_sf(xlim = c(-123.3, -121.6), ylim = c(46.6, 48.5), expand = FALSE) +
  xlab("Longitude")+ ylab("Latitude")+
  theme(panel.grid.major = element_blank(), panel.background = element_rect(fill = "#DCF0F5"), 
        panel.border = element_rect(fill = NA)) +     
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
                         style = north_arrow_fancy_orienteering) + 
  
  # add bounding boxes for study areas
  # tacoma 
  geom_rect(
    xmin = -122.6,
    ymin = 47.17,
    xmax = -122.3,
    ymax = 47.35,
    fill = NA, 
    colour = "red",
    linewidth = 1
  ) +
  
  # seattle 
  geom_rect(
    xmin = -122.54,
    ymin = 47.36,
    xmax = -121.85,
    ymax = 47.77,
    fill = NA, 
    colour = "red",
    linewidth = 1
  ) + 
  
  # eatonville sites
  geom_rect(
    xmin = -122.2,
    ymin = 46.89,
    xmax = -122.3,
    ymax = 46.95,
    fill = NA, 
    colour = "red",
    linewidth = 1
  ) 


washington
# clip tract shapefile to water 
tract_crop <- ms_erase(wa_tract, water)

# read in shapefiles 

env_WA_sp <- st_read(here("data", "env_health_data", "env_health_all_KP.shp"))


colnames(env_WA_sp)
#now lets make one big pollutant dataframe so we can create percentiles for our map 


#create percentile for each metric. we'll use this to create the pollution burden score later

pollutants <- st_as_sf(env_WA_sp) %>%
  mutate(LEAD_PCT = ntile(env_WA_sp$Pct_U_L, 100)) %>%
  mutate(PM25_PCT = ntile(env_WA_sp$PM2_5_C, 100)) %>%
  mutate(HAZARD_PCT = ntile(env_WA_sp$A_PTSDF, 100)) %>%
  mutate(TRAFFIC_PCT = ntile(env_WA_sp$P_H_T_R, 100)) %>%
  mutate(PNPL_PCT = ntile(env_WA_sp$Av_PNPL, 100)) %>% 
  mutate(TOXIC_PCT = ntile(env_WA_sp$A_RSEI_, 100)) %>% 
  mutate(PWDIS_PCT = ntile(env_WA_sp$A_PWDIS, 100)) %>% 
  mutate(DIESEL_PCT = ntile(env_WA_sp$ATK2DNO, 100)) 


#CALCULATE OUR OWN POLLUTION BURDEN. We will follow WA Env Health Map Methods

#first we need to average the exposures (pm25, diesel, toxic releases)
# and environmental effects (cleanupsites, lead, gw threat, and hazards for us)

pollutants <- pollutants %>%
  mutate(EXPOSURE = (PM25_PCT + DIESEL_PCT +  TOXIC_PCT) / 3) %>%
  # removing groundwater because therea re lots of NAs
  mutate(EFFECT = (LEAD_PCT + HAZARD_PCT + PNPL_PCT) / 3) %>% 
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

class(pollutants)
pollutants$BURDEN_PERCENTILE

## read in points 
points_wa <- st_read(here("data", "cameras", "points_wa.shp"))

###########
# make study site map - tacoma 
TAWA <- ggplot(data = pollutants) + 
  geom_sf(
    aes(fill = BURDEN_PERCENTILE), 
    lwd = 0,
    colour = "lightgrey") +
  scale_fill_distiller(
    type = "seq",
    aesthetics = "fill",
    palette = "Reds",
     direction = 1,
    na.value = "grey80",
  #    limits = factor(c(1, 100)),
    #   oob = scales::squish,
    #   labels = scales::percent,
    name = "Environmental effects \nburden percentile") +
  geom_sf(data = st_as_sf(points_wa), size = 2, shape = 23, fill = "black") +
  coord_sf(xlim = c(-122.58,-122.25), ylim= c(47.15,47.35), expand = FALSE) +
  annotate("text", x = -122.48, y = 47.32, label= "Tacoma", fontface = "bold",
           family = "Futura-Bold", size = 6) + 
  annotation_scale(location = "bl", width_hint = 0.4) +
  theme_void() + 
  theme(panel.grid.major = element_blank(), panel.background = element_rect(fill = "#DCF0F5"), 
        panel.border = element_rect(fill = NA)) + 
  #  theme(
  # legend.justification defines the edge of the legend that the legend.position coordinates refer to
  #    legend.justification = c(0, 1),
  # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
  #   legend.position = c(0, .95)
  #   ) +     
  theme(
    text = element_text(family = "Futura-Bold"),
    legend.title = element_text(family = "Futura-Bold", size = 10),
    legend.text = element_text(family = "Futura-Medium", size = 10)
  ) + theme(legend.position = "none")

TAWA


?scale_color_brewer
# make study site map - seattle 


SEWA <- pollutants %>% 
  ggplot() +
  geom_sf(
    aes(fill = BURDEN_PERCENTILE), 
    lwd = 0,
    colour = "lightgrey") +
  scale_fill_distiller(
    type = "seq",
    aesthetics = "fill",
    palette = "Reds",
    direction = 1,
    na.value = "grey80",
    #   oob = scales::squish,
    #   labels = scales::percent,
    name = "Environmental health \nexposures and effects") + 
  geom_sf(data = st_as_sf(points_wa), size =2, shape = 23, fill = "black") +
  coord_sf(xlim = c(-121.85,-122.54), ylim= c(47.36,47.77), expand = FALSE) + 
  annotate("text", x = -122.44, y = 47.61, label= "Seattle", 
           fontface = "bold", family = "Futura-Bold", size = 6) + 
  annotation_scale(location = "bl", width_hint = 0.4) +
  #    xlab("Longitude")+ ylab("Latitude") + 
  theme_void() + 
  theme(panel.grid.major = element_blank(), panel.background = element_rect(fill = "#DCF0F5"), 
        panel.border = element_rect(fill = NA))  + 
  theme(legend.position = "none")

SEWA

# create arrows for bigger map 
arrowA <- data.frame(x1 = 11, x2 = 16, y1 = 10.5, y2 = 14.5)
arrowB <- data.frame(x1 = 8.5, x2 = 15, y1 = 7.5, y2 = 5.5)



# get the legend 

# extract legend from plot1
legend <- get_legend(
  TAWA +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom") +     
    theme(
      text = element_text(family = "Futura-Medium"),
      legend.title = element_text(family = "Futura-Bold", size = 10),
      legend.text = element_text(family = "Futura-Medium", size = 10)) + 
    theme(
      legend.direction="vertical")
) 


set_null_device("png")

ggdraw(xlim = c(0, 28), ylim = c(0, 20)) +
  draw_plot(washington, x = 0, y = 0, width = 13, height = 20) +
  draw_plot(SEWA, x = 13, y = 10, width = 12, height = 10) +
  draw_plot(TAWA, x = 13, y = 0, width = 12, height = 10) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowA, 
               arrow = arrow(), lineend = "round") +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowB, 
               arrow = arrow(), lineend = "round") +  
  draw_plot(legend, x = 25, y = 5, width = 2, height = 10) 


?scale_color_brewer

#############################################################################################
### GRAVEYARD - RIP  ### 

#### load in regional map for inset using rnaturalearth and maps

world <- ne_countries(scale='medium',returnclass = 'sf')
class(world)

states_all <- ne_states(
  country = c("canada", "united states of america"),
  returnclass = "sf"
)

states_all <- states_all %>%
  filter(name_en == "Washington" | 
           name_en == "British Columbia")


# bring in county borders 
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))


# keep only WA counties 
counties <- subset(counties, grepl("washington", counties$ID))



# suggest top CRS 
suggest_top_crs(wa_map) # 6597 is projected CRS 

head(wa_map)

wa_map <- wa_map %>% dplyr::filter(substr(GEOID20, 1, 5),
                                   %in% c("53033", "53053"))

mapview(wa_map)
# re-project and crop 
tractsKP <- st_transform(tractsKP, crs=4326)
tractsKP_crop <- st_crop(tractsKP, c(xmin= -121.7, ymin = 46.7, xmax = -122.8, ymax = 47.8))





















#### 
mapview(mapview(breweries, zcol = "founded", at = seq(1400, 2200, 200), legend = TRUE))