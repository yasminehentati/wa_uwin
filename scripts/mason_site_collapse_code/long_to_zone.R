# This gives us the UTM zone of a given longitude (you'll have to know
#  if it's North or South), also assumes longitude is in [-180 - 180).
long_to_zone <- function(longitude){
  if(any(longitude < -180 | longitude > 180)){
    stop("longitude outside of bounds")
  }
  as.integer(unlist(floor((longitude + 180) / 6) + 1))
}
