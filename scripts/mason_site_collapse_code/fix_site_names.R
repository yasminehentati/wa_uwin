fix_site_names <- function(x){
  # this function is going to figure out what sites all need to be combined
  #  with one another. It's going to assume we are working with the
  #  sites to merge object that we created.
  
  # determine if a site is listed more than once
  
  # split by city
  x$new_name <- NA
  xl <- split(x, x$City)
  
  for(i in 1:length(xl)){
    # figure out the unique sites
    usi <- unique(unlist(xl[[i]][,c("Site1", "Site2")]))
    # figure out if a site is too close to multiple sites.
    tmp_mat <- diag(0, length(usi))
    row.names(tmp_mat) <- usi
    colnames(tmp_mat) <- usi
    for(j in 1:nrow(xl[[i]])){
      # determine row and column index
      row_idx <- which(usi == xl[[i]]$Site1[j])
      col_idx <- which(usi == xl[[i]]$Site2[j])
      # put a 1 if these sites are the same
      tmp_mat[row_idx, col_idx] <- 1
    }
    # figure out what the site should be called. We will go with the 
    #  name along the row.
    
    site_network <- igraph::graph.adjacency(tmp_mat)
    site_clusters <- igraph::clusters(site_network)
    for(k in 1:site_clusters$no){
      tmp_sites <- names(
        igraph::V(site_network)[site_clusters$membership == k]
      )
      
      xl[[i]]$new_name[
        which(xl[[i]]$Site1 %in% tmp_sites | xl[[i]]$Site2 %in% tmp_sites) 
      ]<- tmp_sites[1]
    }
  }
  return(dplyr::bind_rows(xl))
}
