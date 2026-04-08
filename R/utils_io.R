read_corridor_upload <- function(path) {
  
  sf::st_read(path, quiet = TRUE)
  
}