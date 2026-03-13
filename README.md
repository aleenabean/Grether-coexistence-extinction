# Coexistence & Extinction Heatmap App

An interactive Shiny app for exploring simulation results from the species coexistence model described in [Grether & Okamoto (2022)](https://sites.lifesci.ucla.edu/eeb-gretherlab/wp-content/uploads/sites/146/2022/09/Grether-Okamoto-2022.pdf).

## Overview

The app visualizes how the probability of coexistence, extinction, and evolutionary outcomes vary across key model parameters:

- **FracRefuge** — fraction of habitat serving as refuge for Species 2
- **ResourceOverlap** — degree of dietary/resource overlap between species
- **FightingDiff** — difference in fighting ability between species
- **InitialDiff** — scenario setting for initial phenotypic difference

Results are displayed as heatmaps with a fixed 0–1 color scale, allowing direct comparison across parameter slices.

## Features

- Coexistence probability heatmap
- Species 1 and Species 2 extinction probability heatmaps
- Outcome category probability heatmap (stasis, divergence, character displacement, etc.)
- Animated FracRefuge slider to explore parameter space
- Outcome category descriptions drawn from Grether & Okamoto (2022)

## Running the App

The app requires R with the following packages: `shiny`, `dplyr`, `ggplot2`, `readr`, `bslib`, `tidyr`.

Place `app.R`, `Simulations_summary10.csv`, and `Simulation_results_24.csv` in the same folder, then run:

```r
shiny::runApp()
```

## Live Version

The app is available on Grether Lab website and at https://aleenamunshi.shinyapps.io/coexistence_app/

## Author

Aleena Munshi — for questions or comments, contact [aleenamunshi001@g.ucla.edu](mailto:aleenamunshi001@g.ucla.edu)
