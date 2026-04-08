estimate_ev_demand <- function(corridor_length_miles) {
  traffic_per_mile <- 20000
  ev_share <- 0.1
  charging_rate <- 0.05
  
  total_traffic <- corridor_length_miles * traffic_per_mile
  ev_vehicles <- total_traffic * ev_share
  charging_demand <- ev_vehicles * charging_rate
  
  round(charging_demand)
}