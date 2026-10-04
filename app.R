# Diabetes Risk Monitor
#
# Interactive Shiny dashboard exploring the CDC BRFSS 2015 diabetes health
# indicators and estimating diabetes status with a random forest.
#
# Run with: shiny::runApp()
# Files in R/ are sourced automatically by Shiny before this script runs.

library(shiny)
library(bslib)
library(ggplot2)
library(randomForest)

health_data <- load_health_data()
model       <- load_or_train_model(health_data)

shinyApp(ui = app_ui(), server = app_server(health_data, model))
