# Install the R packages required to run the app.
# Usage (from the project root): Rscript scripts/install_packages.R

packages <- c("shiny", "bslib", "ggplot2", "dplyr", "tidyr", "janitor", "randomForest", "scales")

missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All required packages are already installed.")
}
