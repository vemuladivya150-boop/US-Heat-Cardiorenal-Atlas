# U.S. Heat-Cardiorenal Vulnerability Atlas

An interactive county-level spatial epidemiology atlas developed to examine geographic patterns in extreme heat exposure, cardiometabolic susceptibility, social vulnerability, and age-adjusted heart-disease mortality across U.S. counties.

## Live Interactive Dashboard

Explore the dashboard here:

**https://vemuladivya150-boop.github.io/US-Heat-Cardiorenal-Atlas/**

---

## Project Overview

The **U.S. Heat-Cardiorenal Vulnerability Atlas** integrates county-level heat exposure, cardiometabolic risk indicators, social vulnerability, and age-adjusted heart-disease mortality to identify geographic patterns of combined vulnerability across the United States.

The project combines epidemiology, statistical scoring, GIS, and interactive 3D visualization.

## Dashboard Features

- Interactive U.S. county-level map
- 3D county extrusion using heart-disease mortality
- Overall Heat-Cardiorenal Vulnerability (HCV) score
- Extreme heat exposure layer
- Clinical susceptibility layer
- Social vulnerability layer
- Heart-disease mortality layer
- County search and state filtering
- County-level epidemiologic profiles
- U.S. and state vulnerability rankings
- Highest-priority county summaries
- Globe, 3D, and top-down map views
- Public GitHub Pages deployment

---

## Study Period

**2016-2020**

---

## Data Sources

### CDC Environmental Public Health Tracking Network

Extreme heat exposure was represented using county-level historical extreme heat data from CDC's National Environmental Public Health Tracking Network.

- CDC Tracking climate-change data overview: https://www.cdc.gov/environmental-health-tracking/php/data-research/climate-change.html
- CDC Tracking Data Explorer: https://ephtracking.cdc.gov/DataExplorer/
- CDC EPHTrackR package / API examples: https://github.com/CDCgov/EPHTrackR

The heat measure used in this project was based on annual extreme heat days during May-September and a relative temperature threshold.

### CDC PLACES — 2022 County Release

County-level cardiometabolic indicators were obtained from the CDC PLACES 2022 county release.

- PLACES 2022 county dataset used in this project: https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-County-Data-20/duw2-7jbt/about_data
- CDC PLACES data portal: https://www.cdc.gov/places/tools/data-portal.html
- PLACES measure definitions: https://www.cdc.gov/places/measure-definitions/index.html

Indicators used:

- Hypertension
- Diabetes
- Obesity
- Coronary heart disease

### CDC/ATSDR Social Vulnerability Index

County-level social vulnerability was incorporated using CDC/ATSDR SVI data.

- SVI data and documentation: https://www.atsdr.cdc.gov/place-health/php/svi/svi-data-documentation-download.html

### CDC WONDER — Underlying Cause of Death

Age-adjusted heart-disease mortality was obtained from CDC WONDER and used as the epidemiologic outcome and the 3D extrusion height.

- CDC WONDER mortality datasets: https://wonder.cdc.gov/deaths-by-underlying-cause.html
- CDC WONDER dataset descriptions: https://wonder.cdc.gov/datasets.html

The analysis used heart-disease mortality for 2016-2020.

---

## Heat-Cardiorenal Vulnerability Score

The project-specific HCV score combines three equally weighted components:

**HCV Score = Mean(Heat Percentile, Clinical Susceptibility, Social Vulnerability) × 100**

Heart-disease mortality is **not** included in the HCV score. It is displayed separately as the 3D extrusion height and epidemiologic outcome.

### Clinical Susceptibility

Clinical susceptibility is based on percentile-ranked county prevalence of:

- Hypertension
- Diabetes
- Obesity
- Coronary heart disease

At least three of the four indicators are required for the clinical susceptibility score.

### Vulnerability Categories

Counties are grouped by percentile into:

- Lowest
- Low
- Moderate
- High
- Highest
- Data unavailable

> These are project-defined analytic categories and are not clinical risk categories or official classifications from CDC or other government agencies.

---

## 3D Visualization

Map **color** represents the selected epidemiologic vulnerability layer.

Map **height** represents age-adjusted heart-disease mortality.

A selected county is highlighted in **gold**, elevated above surrounding counties, and shown in the County Intelligence panel.

---

## Technologies Used

- **R**
- **Shiny**
- **sf**
- **dplyr**
- **stringr**
- **scales**
- **MapLibre / mapgl**
- **GeoJSON**
- **HTML / CSS / JavaScript**
- **GitHub Pages**
- **GIS / spatial epidemiology**

The public GitHub Pages version uses exported GeoJSON files and browser-side MapLibre rendering.

---

## Software, Documentation, and Project Links

### R and Spatial Analysis

- R: https://www.r-project.org/
- Shiny: https://shiny.posit.co/
- sf: https://cran.r-project.org/package=sf
- dplyr: https://dplyr.tidyverse.org/
- stringr: https://stringr.tidyverse.org/
- scales: https://scales.r-lib.org/
- tigris: https://walker-data.com/tigris/

### Interactive Mapping

- mapgl: https://walker-data.com/mapgl/
- MapLibre GL JS: https://maplibre.org/maplibre-gl-js/docs/
- CARTO basemaps: https://carto.com/basemaps/

### Publishing and Version Control

- GitHub: https://github.com/
- GitHub Pages documentation: https://docs.github.com/en/pages
- Live atlas: https://vemuladivya150-boop.github.io/US-Heat-Cardiorenal-Atlas/
- Project repository: https://github.com/vemuladivya150-boop/US-Heat-Cardiorenal-Atlas

---

## Repository Files

```text
US-Heat-Cardiorenal-Atlas/
|-- app.R
|-- export_for_web.R
|-- README.md
|-- CITATION.cff
|-- COPYRIGHT.md
|-- .gitignore
`-- docs/
    |-- index.html
    |-- counties.geojson
    `-- states.geojson
```

### app.R

Contains the complete R Shiny atlas interface, server logic, scoring framework, county selection behavior, rankings, and interactive 3D mapping.

### export_for_web.R

Processes the spatial analysis dataset and exports simplified county and state GeoJSON files for the static web version.

### docs/index.html

Contains the GitHub Pages version of the interactive atlas.

### docs/counties.geojson

Contains simplified county geometries and county-level epidemiologic attributes used by the public web atlas.

### docs/states.geojson

Contains state boundary geometries used by the public web atlas.

---

## Author

**Divya Vemula**

Project work includes data integration, epidemiologic analysis, vulnerability scoring, GIS visualization, interactive dashboard development, and interpretation.

---

## Citation

**Vemula, D. (2026). _U.S. Heat-Cardiorenal Vulnerability Atlas_ [Interactive spatial epidemiology dashboard].**

GitHub also provides a **Cite this repository** option using the included `CITATION.cff` file.

---

## Copyright

© 2026 Divya Vemula. All rights reserved.

The original dashboard design, code, scoring framework, analyses, visualizations, and written interpretation are attributed to the author. Underlying public datasets remain subject to the ownership, citation requirements, licenses, and terms of their original providers.

---

## Disclaimer

This atlas was developed for public health analysis, education, research demonstration, and professional portfolio use.

The HCV score and vulnerability categories are project-specific analytic measures and should not be interpreted as clinical recommendations or official classifications from CDC or other government agencies.

---

## Live Project

**Interactive dashboard:**

https://vemuladivya150-boop.github.io/US-Heat-Cardiorenal-Atlas/

**GitHub repository:**

https://github.com/vemuladivya150-boop/US-Heat-Cardiorenal-Atlas
