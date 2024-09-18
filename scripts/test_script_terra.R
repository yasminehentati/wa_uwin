

template <- rast(ext(vect(envdatSP)), resolution=1000, crs="EPSG:32610")

head(envdatSP)



template <- rast(vect(envdatSP), resolution = 100, crs = "EPSG:32610")
lead_rast <- terra::rasterize(vect(envdatSP), template, field = "Pct_Units_Lead")

plot(lead_rast)

# Load raster data
raster_data_df <- as.data.frame(lead_rast, xy = TRUE, na.rm = TRUE)
raster_data_df

# Plot for raster
p2 <- ggplot(raster_data_df, aes(x = x, y = y, fill = Pct_Units_Lead)) + 
  geom_raster() + 
#  color_scale +
  theme_minimal() +
  ggtitle("Raster Plot")

# Arrange plots side by side
grid.arrange(p1, p2, ncol = 2)



# need to give it a bit extra extent 
buff_ext <- ext(buff_terra) 
buff_ext <- ext(c(
  xmin = buff_ext$xmin - 1000,
  xmax = buff_ext$xmax + 1000,
  ymin = buff_ext$ymin - 1000,
  ymax = buff_ext$ymax + 1000))
buff_terra <- crop(buff_terra, buff_ext)

lead_rast
site_index <- 10
pt <- sites[site_index, ]
buffer_radius <- 500
buff_terra <- vect(pt) %>% terra::buffer(width = buffer_radius) 

# crop and mask the raster 
lead_rast_cropped <- crop(lead_rast, buff_terra, snap = "out")
lead_rast_masked <- terra::mask(lead_rast_cropped, buff_terra)

# extract values and compute mean
extracted_value <- exact_extract(lead_rast_cropped, st_as_sf(buff_terra), fun = "mean", 
                                 weights = "area")
# also tried: 
extracted_value <- terra::extract(lead_rast_masked, vect(buff_terra), fun = "mean")


# Set the extent to match buff_terra
ext_buff <- ext(543000, 543800, 5225800, 5226800)

# Plot the raster with the specific extent
plot(lead_rast, ext = ext_buff, main = "Raster and Buffer")

plot(lead_rast)

# Define breaks and colors for the legend
breaks <- c(0, 100,200,300,400,500)  # Define the breaks
colors <- c("blue", "green", "yellow", "orange", "red")  #


plot(lead_rast)
plot(lead_rast,  col = colors, breaks = breaks, main = "Raster with Custom Legend")

# Add a legend
legend("topright", 
       legend = paste0(breaks[-length(breaks)], " - ", breaks[-1]), 
       fill = colors, 
       title = "Value Range",
       cex = 0.8,  # Adjust legend text size
       box.col = "black",  # Border color for legend box
       bg = "white")  # Background color for legend box

# Add the buffer layer

plot(buff_terra, add = TRUE, border = "red", col = NA)
lead_dat$site
lead_rast

############
for (i in 1:nrow(sites)) {
  pt <- sites[i, ]  # iterate through sites 
  
  # create buffer
  buff <- vect(st_transform(pt, crs(lead_rast)))
  buff_terra <- terra::buffer(buff, width = buffer_radius) 
  
  # need to give it a bit extra extent 
  buff_ext <- ext(buff_terra) 
  buff_ext <- ext(c(
    xmin = buff_ext$xmin - 1000,
    xmax = buff_ext$xmax + 1000,
    ymin = buff_ext$ymin - 1000,
    ymax = buff_ext$ymax + 1000))
  buff_terra <- crop(buff_terra, buff_ext)
  
  # crop and mask the raster 
  lead_rast_cropped <- crop(lead_rast, buff_terra)
  lead_rast_masked <- terra::mask(lead_rast_cropped, buff_terra)
  
  # Extract values within the buffer and compute the mean
  extracted_value <- exact_extract(lead_rast_masked, st_as_sf(buff_terra), fun = "mean", 
                                   weights = "area")
  lead_dat$Pct_Units_Lead[i] <- extracted_value
  
  
  
  # Convert the raster to a data frame for ggplot2
  lead_rast_df <- as.data.frame(lead_rast_masked, xy = TRUE)
  
  # Plot the results
  rast_col <- lead_rast[[]]
  p <- ggplot() +
    geom_raster(lead_rast_cropped, aes(x = x, y = y, fill = name)) +
    scale_fill_manual() +
    geom_sf(data = st_as_sf(buff_terra), fill = NA, color = "red", size = 1) +
    geom_sf(data = sites[i,], color = "blue", size = 3) +
    coord_sf(crs = st_crs(lead_rast)) +
    labs(title = paste("Site:", i),
         x = "Easting (m)", y = "Northing (m)",
         fill = "Raster Value") +
    theme_minimal()
  
  # Save the plot
  ggsave(filename = paste0("plots/plot_site_", i, ".png"), plot = p, width = 8, height = 6)
  
  Sys.sleep(2)
  # print(p)  # Uncomment to plot
  
  print(i)
}