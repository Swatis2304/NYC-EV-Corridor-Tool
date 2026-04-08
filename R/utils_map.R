make_base_map <- function() {
  leaflet::leaflet() %>%
    leaflet::addTiles()
}

add_corridor_layer <- function(map, corridor) {
  map %>%
    leaflet::addPolylines(
      data = corridor,
      color = "blue",
      weight = 4
    )
}