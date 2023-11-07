# This function just tells you which sites need to get removed or merged.
qaqc_sites <- function(
    x,
    my_coords,
    sites,
    cities,
    my_crs,
    combine_distance = 150,
    distance_between_sites = 900
){
  # These are the longitudes of each city in this analysis. We will use this
  #  to determine the correct UTM for each city.
  
  
  # for the data summarise down to the unique sites
  x <- x[,c(cities, sites, my_coords)]
  if(any(duplicated(x[,sites]))){
    x <- x[-which(duplicated(x[,sites])),]
  }
  
  # add UTM zone
  x$Zone <- long_to_zone(x[,my_coords[1]])
  
  # get just the zone for the city, and then
  #  sort by city code.
  city_zone <- x[,c(cities, "Zone")] %>% 
    dplyr::distinct()
  city_zone <- city_zone[order(city_zone[,cities]),]
  
  
  # split the data by city, make lowercase just in case.
  x <- split(x, x[,cities])
  
  dist_issue <- vector(
    "list",
    length = length(x)
  )
  for(i in 1:length(x)){
    
    td <- sf::st_as_sf(
      x[[i]], 
      coords = my_coords,
      crs = my_crs
    )
    td_utm <- sf::st_transform(
      td,
      crs = as.numeric(
        paste0(
          "326",
          city_zone$Zone[i]
        )
      )
    )
    # calculate distance among sites
    dist_between <- sf::st_distance(
      td_utm
    )
    # convert this to a matrix
    dist_between <- matrix(
      dist_between,
      ncol = ncol(dist_between),
      nrow = nrow(dist_between)
    )
    # We need to determine sites to combine and sites to remove.
    #  We'll NA values that are outside of what we are interested in
    #
    # NA distance of site to itself 
    diag(dist_between) <- NA
    # NA anything greater than {distance_between_sites}
    dist_between[dist_between > distance_between_sites] <- NA
    # NA lower triangle so we don't double this up
    dist_between[lower.tri(dist_between)] <- NA
    
    
    # check which ones are too close
    if(
      any(
        dist_between < distance_between_sites,
        na.rm = TRUE
      )
    ){
      # these are the indexes of those sites
      site_idx <- which(
        dist_between < distance_between_sites,
        arr.ind = TRUE
      )
      
      tmp_dist <- rep(
        NA,
        nrow(site_idx)
      )
      for(j in 1:nrow(site_idx)){
        tmp_dist[j] <- dist_between[site_idx[j,1], site_idx[j,2]]
      }
      
      dist_issue[[i]] <- data.frame(
        City = names(x)[[i]],
        Site1 = td$Site[site_idx[,1]],
        Site2 = td$Site[site_idx[,2]],
        Meters = tmp_dist
      )
      # determine if sites should be merged or removed
      dist_issue[[i]]$Process <- ifelse(
        floor(dist_issue[[i]]$Meters) <= combine_distance,
        "merge",
        "remove"
      )
      
      
    }
  }
  dist_issue <- dplyr::bind_rows(
    dist_issue
  )
  return(dist_issue)
}
