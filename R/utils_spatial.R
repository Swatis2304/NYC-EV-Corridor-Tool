prepare_corridor_geometry <- function(sf_obj) {
  sf::st_transform(sf_obj, 4326)
}

calc_corridor_length <- function(sf_obj) {
  length_m <- sum(sf::st_length(sf_obj))
  
  list(
    feet = as.numeric(length_m * 3.28084),
    miles = as.numeric(length_m / 1609.34)
  )
}

create_corridor_buffer <- function(sf_obj, buffer_m = 500) {
  sf_obj_proj <- sf::st_transform(sf_obj, 3857)
  buffer <- sf::st_buffer(sf_obj_proj, buffer_m)
  sf::st_transform(buffer, 4326)
}