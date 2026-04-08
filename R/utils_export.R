export_results_workbook <- function(path, corridor_sf, metrics_df, cost_df) {
  
  wb <- openxlsx2::wb_workbook()
  
  wb$add_worksheet("Corridor_Metadata")
  wb$add_data("Corridor_Metadata", metrics_df)
  
  wb$add_worksheet("Scenario_Economics")
  wb$add_data("Scenario_Economics", cost_df)
  
  corridor_attr <- sf::st_drop_geometry(corridor_sf)
  if (ncol(corridor_attr) == 0) {
    corridor_attr <- data.frame(Note = "No non-spatial attributes in uploaded file")
  }
  
  wb$add_worksheet("Uploaded_Corridor_Attributes")
  wb$add_data("Uploaded_Corridor_Attributes", corridor_attr)
  
  wb$add_worksheet("Assumptions")
  assumptions_df <- data.frame(
    Parameter = c(
      "Traffic per mile",
      "EV share",
      "Charging rate",
      "Discount rate",
      "Analysis period (years)"
    ),
    Value = c(
      20000,
      0.10,
      0.05,
      0.07,
      10
    )
  )
  wb$add_data("Assumptions", assumptions_df)
  
  wb$save(path)
}