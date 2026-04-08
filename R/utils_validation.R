validate_corridor <- function(sf_obj) {
  
  messages <- c()
  
  if (!inherits(sf_obj, "sf")) {
    return(list(valid = FALSE, messages = "Not a valid spatial file", data = NULL))
  }
  
  if (nrow(sf_obj) == 0) {
    messages <- c(messages, "No features found")
  }
  
  geom_type <- unique(sf::st_geometry_type(sf_obj))
  
  if (!any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
    messages <- c(messages, "Geometry must be a line")
  }
  
  valid <- length(messages) == 0
  
  if (valid) {
    messages <- "Valid corridor file"
  }
  
  list(
    valid = valid,
    messages = messages,
    data = sf_obj
  )
}