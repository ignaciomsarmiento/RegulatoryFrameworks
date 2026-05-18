# Regulatory Frameworks Explorer

A Shiny app for exploring regulatory frameworks. This is intended for review and testing.

## Requirements

- **R version**: 4.2.0 or higher (tested on 4.3+)
- **RStudio** (recommended) for opening the `.Rproj` file

### R packages

Install the required packages before running the app:

```r
install.packages(c(
  "shiny",
  "dplyr",
  "tidyr",
  "plotly",
  "reactable",
  "readxl",
  "shinyjs",
  "shinycssloaders"
))
```

## Download

Clone the repository with Git:

```bash
git clone https://github.com/ignaciomsarmiento/RegulatoryFrameworks.git
```

Or download the ZIP from GitHub and unzip it locally.

## Running the app

1. Open `_RegulatoryFrameworks.Rproj` in RStudio.
2. Open `global.R`, `ui.r`, or `server.r`.
3. Click **Run App** (or run `shiny::runApp()` from the R console at the project root).

On first launch, the console should display:

```
Datos cargados exitosamente
  - N paises disponibles
  - N tablas de Excel cargadas
```

## Project structure

```
_RegulatoryFrameworks/
├── global.R              # App startup: libraries, data loading
├── ui.r                  # Main UI definition
├── server.r              # Main server logic
├── global_aux/           # Data loading, constants, formatters
├── tabs/                 # UI and server modules per tab
├── landing_labor_costs/  # Landing page module
├── data/                 # Source Excel files and processed data
├── www/                  # CSS, JS, images, fonts
└── documents/            # Reference documents
```

## Feedback

Please report issues, suggestions, or comments directly to Ignacio.
