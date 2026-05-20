# =============================================================================
# global.R - Shiny app startup
# =============================================================================
# This file runs once when the app starts. Keep it as an orchestrator; put
# data loading, constants, formatters, and data source assembly in global_aux/.
# =============================================================================

library(shiny)
library(dplyr)
library(tidyr)
library(plotly)
library(reactable)
library(readxl)
library(shinyjs)
library(shinycssloaders)

source(file.path("global_aux", "load_data.R"), local = TRUE)

# US -> US4 display rename (notation only; canonical files unchanged).
# Must run BEFORE constants.R / data_sources.R so derived objects (COUNTRIES_LIST,
# LABOR_DATA_SOURCES_*) inherit the renamed country column.
.rename_us_to_us4 <- function(obj) {
  if (is.data.frame(obj) && "country" %in% names(obj)) {
    obj$country[obj$country == "US"] <- "US4"
    return(obj)
  }
  if (is.list(obj) && !is.data.frame(obj)) {
    return(lapply(obj, .rename_us_to_us4))
  }
  obj
}
for (.var in ls()) {
  if (startsWith(.var, "DATA_")) {
    assign(.var, .rename_us_to_us4(get(.var)))
  }
}
rm(.rename_us_to_us4, .var)

source(file.path("global_aux", "constants.R"), local = TRUE)
source(file.path("global_aux", "formatters.R"), local = TRUE)
source(file.path("global_aux", "data_sources.R"), local = TRUE)

message("Data loaded successfully")
message(sprintf("  - %d countries available", length(COUNTRIES_LIST) - 1))
message(sprintf("  - %d Excel sheets loaded", length(EXCEL_TABLES)))
