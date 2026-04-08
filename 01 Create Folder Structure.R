# Set project path to CURRENT directory
project_path <- "."

# Create folders
dir.create("R", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", showWarnings = FALSE)
dir.create("data/lookup", showWarnings = FALSE)
dir.create("templates", showWarnings = FALSE)
dir.create("www", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

# Create main files
file.create("app.R")
file.create("global.R")
file.create("DESCRIPTION")
file.create("README.md")

# Create utility scripts
utils_files <- c(
  "utils_io.R",
  "utils_validation.R",
  "utils_spatial.R",
  "utils_demand.R",
  "utils_finance.R",
  "utils_export.R",
  "utils_map.R"
)

for (f in utils_files) {
  file.create(file.path("R", f))
}

# Create placeholders
file.create("templates/ev_corridor_template.xlsx")
file.create("www/custom.css")

cat("✅ NYC EV Corridor Tool structure created!")