# ============================================================
# U.S. HEAT-CARDIORENAL VULNERABILITY ATLAS
# COUNTY FOCUS VERSION
#
# Author: Divya Vemula
# Copyright © 2026 Divya Vemula
# All rights reserved.
#
# Suggested citation:
# Vemula, D. (2026).
# U.S. Heat-Cardiorenal Vulnerability Atlas.
#
# Original dashboard design, scoring framework, analyses,
# visualizations, and application code © Divya Vemula.
#
# Underlying public datasets remain subject to the ownership,
# citation requirements, and terms of their original providers.
#
# COLOR  = epidemiologic vulnerability layer
# HEIGHT = age-adjusted heart-disease mortality
#
# SELECTED COUNTY:
#   - county zoom
#   - GOLD selected color
#   - elevated above neighbors
#   - cream-gold border
#   - surrounding counties dimmed
#
# CONTROLS:
#   - Globe
#   - 3D
#   - Top
#
# Study period: 2016-2020
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

library(shiny)
library(sf)
library(dplyr)
library(stringr)
library(scales)
library(mapgl)

options(scipen = 999)


# ============================================================
# 2. PROJECT PATH
# ============================================================

project_dir <- "/Users/divya/US_Heat_Cardiorenal_Atlas"

data_path <- file.path(
  project_dir,
  "spatial",
  "us_master_final.rds"
)

if (!file.exists(data_path)) {
  
  stop(
    paste0(
      "\nCannot find:\n",
      data_path,
      "\n\nMake sure us_master_final.rds is inside the spatial folder."
    )
  )
}


# ============================================================
# 3. LOAD DATA
# ============================================================

us_master <- readRDS(
  data_path
)

if (!inherits(
  us_master,
  "sf"
)) {
  
  stop(
    "us_master_final.rds must be an sf object."
  )
}


# ============================================================
# 4. REQUIRED VARIABLES
# ============================================================

required_variables <- c(
  "FIPS",
  "State_FIPS",
  "Hypertension",
  "Diabetes",
  "Obesity",
  "CHD",
  "Extreme_Heat_Days",
  "SVI",
  "Heart_Mortality"
)


missing_variables <- setdiff(
  required_variables,
  names(
    us_master
  )
)


if (
  length(
    missing_variables
  ) > 0
) {
  
  stop(
    paste0(
      "Missing variables: ",
      paste(
        missing_variables,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# 5. HELPER FUNCTIONS
# ============================================================

first_existing_column <- function(
    data,
    candidates
) {
  
  available <- candidates[
    candidates %in%
      names(
        data
      )
  ]
  
  
  if (
    length(
      available
    ) == 0
  ) {
    
    return(
      rep(
        NA_character_,
        nrow(
          data
        )
      )
    )
  }
  
  
  result <- rep(
    NA_character_,
    nrow(
      data
    )
  )
  
  
  for (
    column in available
  ) {
    
    values <- as.character(
      data[
        [
          column
        ]
      ]
    )
    
    
    idx <-
      is.na(
        result
      ) |
      result == ""
    
    
    result[
      idx
    ] <- values[
      idx
    ]
  }
  
  
  result
}


format1 <- function(x) {
  
  ifelse(
    
    is.na(
      x
    ),
    
    "Unavailable",
    
    formatC(
      x,
      format = "f",
      digits = 1,
      big.mark = ","
    )
  )
}


format_pct <- function(x) {
  
  ifelse(
    
    is.na(
      x
    ),
    
    "Unavailable",
    
    paste0(
      
      formatC(
        x,
        format = "f",
        digits = 1
      ),
      
      "%"
    )
  )
}


format_rank <- function(x) {
  
  ifelse(
    
    is.na(
      x
    ),
    
    "Unavailable",
    
    paste0(
      "#",
      as.integer(
        x
      )
    )
  )
}


percentile_category <- function(x) {
  
  case_when(
    
    is.na(
      x
    ) ~
      "Data unavailable",
    
    x <= 0.20 ~
      "Lowest",
    
    x <= 0.40 ~
      "Low",
    
    x <= 0.60 ~
      "Moderate",
    
    x <= 0.80 ~
      "High",
    
    TRUE ~
      "Highest"
  )
}


# ============================================================
# 6. STANDARDIZE IDS
# ============================================================

us_master$FIPS <- str_pad(
  
  as.character(
    us_master$FIPS
  ),
  
  width = 5,
  side = "left",
  pad = "0"
)


us_master$State_FIPS <- str_pad(
  
  as.character(
    us_master$State_FIPS
  ),
  
  width = 2,
  side = "left",
  pad = "0"
)


# ============================================================
# 7. DISPLAY NAMES
# ============================================================

us_master$State_Display <- first_existing_column(
  
  us_master,
  
  c(
    "State",
    "State_SVI",
    "State_Heat"
  )
)


us_master$State_Abbr_Display <- first_existing_column(
  
  us_master,
  
  c(
    "State_Abbreviation",
    "State_Abbreviation_SVI"
  )
)


us_master$County_Display <- first_existing_column(
  
  us_master,
  
  c(
    "County",
    "County_SVI",
    "County_Heat",
    "County_Name_GIS",
    "County_Mortality"
  )
)


# ============================================================
# 8. STATE ABBREVIATIONS
# ============================================================

state_name_to_abbr <- c(
  
  setNames(
    state.abb,
    state.name
  ),
  
  "District of Columbia" =
    "DC"
)


missing_abbr <-
  
  is.na(
    us_master$State_Abbr_Display
  ) |
  
  us_master$State_Abbr_Display ==
  ""


us_master$State_Abbr_Display[
  missing_abbr
] <- unname(
  
  state_name_to_abbr[
    
    us_master$State_Display[
      missing_abbr
    ]
  ]
)


# ============================================================
# 9. NUMERIC VARIABLES
# ============================================================

numeric_variables <- c(
  "Hypertension",
  "Diabetes",
  "Obesity",
  "CHD",
  "Extreme_Heat_Days",
  "SVI",
  "Heart_Mortality"
)


for (
  variable in numeric_variables
) {
  
  us_master[
    [
      variable
    ]
  ] <-
    
    suppressWarnings(
      
      as.numeric(
        
        us_master[
          [
            variable
          ]
        ]
      )
    )
}


us_master$SVI[
  us_master$SVI < 0
] <- NA_real_


# ============================================================
# 10. CLINICAL SUSCEPTIBILITY
# ============================================================

us_master <- us_master |>
  
  mutate(
    
    HTN_Pctl =
      percent_rank(
        Hypertension
      ),
    
    Diabetes_Pctl =
      percent_rank(
        Diabetes
      ),
    
    Obesity_Pctl =
      percent_rank(
        Obesity
      ),
    
    CHD_Pctl =
      percent_rank(
        CHD
      )
  )


clinical_matrix <- cbind(
  us_master$HTN_Pctl,
  us_master$Diabetes_Pctl,
  us_master$Obesity_Pctl,
  us_master$CHD_Pctl
)


us_master$Clinical_N <-
  
  rowSums(
    
    !is.na(
      clinical_matrix
    )
  )


clinical_average <-
  
  rowMeans(
    clinical_matrix,
    na.rm = TRUE
  )


us_master$Clinical_Susceptibility <-
  
  ifelse(
    
    us_master$Clinical_N >=
      3,
    
    clinical_average,
    
    NA_real_
  )


us_master$Clinical_Score <-
  
  us_master$Clinical_Susceptibility *
  100


# ============================================================
# 11. HEAT SCORE
# ============================================================

us_master$Heat_Percentile <-
  
  percent_rank(
    us_master$Extreme_Heat_Days
  )


us_master$Heat_Score <-
  
  us_master$Heat_Percentile *
  100


# ============================================================
# 12. HCV SCORE
#
# Heat + Clinical + SVI
# Mortality excluded.
# ============================================================

us_master$HCV_Score <- ifelse(
  
  !is.na(
    us_master$Heat_Percentile
  ) &
    
    !is.na(
      us_master$Clinical_Susceptibility
    ) &
    
    !is.na(
      us_master$SVI
    ),
  
  
  (
    us_master$Heat_Percentile +
      us_master$Clinical_Susceptibility +
      us_master$SVI
  ) / 3 *
    100,
  
  
  NA_real_
)


# ============================================================
# 13. PERCENTILES
# ============================================================

us_master$HCV_Percentile <-
  
  percent_rank(
    us_master$HCV_Score
  )


us_master$Clinical_Percentile <-
  
  percent_rank(
    us_master$Clinical_Score
  )


us_master$Mortality_Percentile <-
  
  percent_rank(
    us_master$Heart_Mortality
  )


# ============================================================
# 14. CATEGORIES
# ============================================================

us_master <- us_master |>
  
  mutate(
    
    HCV_Category =
      percentile_category(
        HCV_Percentile
      ),
    
    Heat_Category =
      percentile_category(
        Heat_Percentile
      ),
    
    Clinical_Category =
      percentile_category(
        Clinical_Percentile
      ),
    
    SVI_Category =
      percentile_category(
        SVI
      ),
    
    Mortality_Category =
      percentile_category(
        Mortality_Percentile
      )
  )


# ============================================================
# 15. RANKINGS
# ============================================================

us_master <- us_master |>
  
  mutate(
    
    US_Rank =
      min_rank(
        
        desc(
          HCV_Score
        )
      )
  ) |>
  
  group_by(
    State_Display
  ) |>
  
  mutate(
    
    State_Rank =
      min_rank(
        
        desc(
          HCV_Score
        )
      )
  ) |>
  
  ungroup()


# ============================================================
# 16. NORMAL 3D MORTALITY HEIGHT
# ============================================================

mortality_values <-
  us_master$Heart_Mortality


mortality_limits <- quantile(
  
  mortality_values,
  
  probs = c(
    0.02,
    0.98
  ),
  
  na.rm = TRUE
)


mortality_visual <- pmin(
  
  pmax(
    mortality_values,
    mortality_limits[
      1
    ]
  ),
  
  mortality_limits[
    2
  ]
)


us_master$Mortality_3D_Height <- ifelse(
  
  is.na(
    mortality_values
  ),
  
  1000,
  
  rescale(
    
    mortality_visual,
    
    to = c(
      4000,
      24000
    ),
    
    from =
      mortality_limits
  )
)


# ============================================================
# 17. POPUP HTML
# ============================================================

us_master$Popup_HTML <- paste0(
  
  "<div style='font-family:Arial;min-width:225px;'>",
  
  "<div style='font-size:16px;font-weight:800;color:#12232D;'>",
  
  us_master$County_Display,
  ", ",
  us_master$State_Abbr_Display,
  
  "</div>",
  
  
  "<div style='font-size:10px;color:#6A7B84;margin:5px 0 10px 0;'>",
  
  "County spatial epidemiology profile",
  
  "</div>",
  
  
  "<hr style='border:0;border-top:1px solid #E2E8EC;'>",
  
  
  "<b>HCV score:</b> ",
  
  format1(
    us_master$HCV_Score
  ),
  
  
  "<br><b>Risk level:</b> ",
  
  us_master$HCV_Category,
  
  
  "<br><b>U.S. rank:</b> ",
  
  format_rank(
    us_master$US_Rank
  ),
  
  
  "<br><br><b>Heart mortality:</b> ",
  
  format1(
    us_master$Heart_Mortality
  ),
  
  " /100k",
  
  
  "<br><b>Extreme heat:</b> ",
  
  format1(
    us_master$Extreme_Heat_Days
  ),
  
  " days",
  
  
  "<br><b>Clinical score:</b> ",
  
  format1(
    us_master$Clinical_Score
  ),
  
  
  "<br><b>Social vulnerability:</b> ",
  
  format1(
    us_master$SVI *
      100
  ),
  
  " /100",
  
  
  "</div>"
)


# ============================================================
# 18. MAP DATA
# ============================================================

map_all <- us_master |>
  
  select(
    
    FIPS,
    State_FIPS,
    
    State_Display,
    State_Abbr_Display,
    County_Display,
    
    Hypertension,
    Diabetes,
    Obesity,
    CHD,
    
    Extreme_Heat_Days,
    SVI,
    Heart_Mortality,
    
    Clinical_Score,
    
    Heat_Score,
    Heat_Percentile,
    
    HCV_Score,
    HCV_Percentile,
    
    Clinical_Percentile,
    Mortality_Percentile,
    
    HCV_Category,
    Heat_Category,
    Clinical_Category,
    SVI_Category,
    Mortality_Category,
    
    US_Rank,
    State_Rank,
    
    Mortality_3D_Height,
    
    Popup_HTML,
    
    geometry
  ) |>
  
  mutate(
    
    across(
      where(
        is.factor
      ),
      as.character
    )
  ) |>
  
  mutate(
    
    across(
      
      where(
        is.character
      ),
      
      ~ iconv(
        .x,
        from = "",
        to = "UTF-8",
        sub = ""
      )
    )
  )


# ============================================================
# 19. CLEAN GEOMETRY
# ============================================================

map_all <- map_all |>
  
  st_make_valid() |>
  
  st_transform(
    5070
  ) |>
  
  st_simplify(
    dTolerance = 100,
    preserveTopology = TRUE
  ) |>
  
  st_transform(
    4326
  )


# ============================================================
# 20. STATE BOUNDARIES
# ============================================================

state_boundaries <- map_all |>
  
  select(
    State_FIPS,
    State_Display,
    State_Abbr_Display,
    geometry
  ) |>
  
  group_by(
    State_FIPS,
    State_Display,
    State_Abbr_Display
  ) |>
  
  summarise(
    .groups =
      "drop"
  ) |>
  
  st_make_valid()


# ============================================================
# 21. COLORS
# ============================================================

category_levels <- c(
  "Lowest",
  "Low",
  "Moderate",
  "High",
  "Highest",
  "Data unavailable"
)


category_colors <- c(
  
  "Lowest" =
    "#147A70",
  
  "Low" =
    "#297E9D",
  
  "Moderate" =
    "#B88D31",
  
  "High" =
    "#BC6234",
  
  "Highest" =
    "#BC3F57",
  
  "Data unavailable" =
    "#283844"
)


selected_gold <-
  "#FFD166"


selected_gold_edge <-
  "#FFF2B5"


# ============================================================
# 22. COLOR EXPRESSIONS
# ============================================================

category_column <- function(
    layer
) {
  
  switch(
    
    layer,
    
    "HCV" =
      "HCV_Category",
    
    "HEAT" =
      "Heat_Category",
    
    "CLINICAL" =
      "Clinical_Category",
    
    "SVI" =
      "SVI_Category",
    
    "MORTALITY" =
      "Mortality_Category",
    
    "HCV_Category"
  )
}


base_color_expression <- function(
    layer
) {
  
  match_expr(
    
    column =
      category_column(
        layer
      ),
    
    values =
      category_levels,
    
    stops =
      unname(
        
        category_colors[
          category_levels
        ]
      ),
    
    default =
      "#283844"
  )
}


# ============================================================
# 23. STATE CHOICES
# ============================================================

state_names <- state_boundaries |>
  
  st_drop_geometry() |>
  
  filter(
    
    !is.na(
      State_Display
    )
  ) |>
  
  arrange(
    State_Display
  ) |>
  
  pull(
    State_Display
  )


state_choices <- c(
  
  "Contiguous United States" =
    "CONUS",
  
  "All U.S. - Globe View" =
    "ALL",
  
  setNames(
    state_names,
    state_names
  )
)


# ============================================================
# 24. COUNTY FOCUS BOUNDS
# ============================================================

county_focus_bbox <- function(
    county_sf,
    expansion = 0.30
) {
  
  county_4326 <-
    
    county_sf |>
    
    st_transform(
      4326
    )
  
  
  bbox <-
    
    st_bbox(
      county_4326
    )
  
  
  xmin <-
    
    as.numeric(
      bbox[
        "xmin"
      ]
    )
  
  
  ymin <-
    
    as.numeric(
      bbox[
        "ymin"
      ]
    )
  
  
  xmax <-
    
    as.numeric(
      bbox[
        "xmax"
      ]
    )
  
  
  ymax <-
    
    as.numeric(
      bbox[
        "ymax"
      ]
    )
  
  
  width <-
    xmax -
    xmin
  
  
  height <-
    ymax -
    ymin
  
  
  if (
    !is.finite(
      width
    ) ||
    width <= 0
  ) {
    
    width <-
      0.20
  }
  
  
  if (
    !is.finite(
      height
    ) ||
    height <= 0
  ) {
    
    height <-
      0.20
  }
  
  
  c(
    
    xmin -
      width *
      expansion,
    
    ymin -
      height *
      expansion,
    
    xmax +
      width *
      expansion,
    
    ymax +
      height *
      expansion
  )
}


# ============================================================
# 25. CSS
# ============================================================

app_css <- "

html,
body {

  width:100%;
  height:100%;

  margin:0;
  padding:0;

  overflow:hidden;

  background:#020B12;

  color:#F4F7F8;

  font-family:
    -apple-system,
    BlinkMacSystemFont,
    'Segoe UI',
    Arial,
    sans-serif;
}


.container-fluid {

  padding:0 !important;
  margin:0 !important;
}


.atlas-shell {

  width:100vw;
  height:100vh;

  position:relative;

  overflow:hidden;

  background:#020B12;
}


.map-stage {

  position:absolute;

  inset:0;

  z-index:1;
}


#map {

  width:100% !important;

  height:100vh !important;
}


.map-atmosphere {

  position:absolute;

  inset:0;

  z-index:2;

  pointer-events:none;

  background:

    radial-gradient(
      circle at 50% 46%,
      rgba(8,43,68,.025) 0%,
      rgba(2,14,24,.015) 55%,
      rgba(1,7,12,.08) 100%
    );
}


/* ==========================================================
   TITLE
   ========================================================== */

.hero-title {

  position:absolute;

  z-index:20;

  top:23px;
  left:24px;

  width:590px;

  pointer-events:none;
}


.eyebrow {

  display:inline-block;

  padding:7px 12px;

  margin-bottom:10px;

  border:
    1px solid rgba(42,184,200,.50);

  border-radius:999px;

  background:
    rgba(2,16,25,.80);

  color:#57CAD2;

  font-size:9px;

  font-weight:800;

  letter-spacing:1.55px;
}


.hero-heading {

  margin:0;

  color:#FFFFFF;

  font-size:31px;

  line-height:1.03;

  font-weight:850;

  letter-spacing:-1.15px;

  text-shadow:
    0 3px 22px rgba(0,0,0,.55);
}


.hero-subtitle {

  margin-top:8px;

  width:540px;

  color:
    rgba(224,235,240,.72);

  font-size:10px;

  line-height:1.48;
}


/* ==========================================================
   GLASS PANELS
   ========================================================== */

.glass {

  background:
    rgba(2,17,27,.92);

  backdrop-filter:
    blur(20px);

  -webkit-backdrop-filter:
    blur(20px);

  border:
    1px solid rgba(113,170,192,.22);

  box-shadow:
    0 18px 48px rgba(0,0,0,.34),
    inset 0 1px 0 rgba(255,255,255,.035);
}


/* ==========================================================
   LEFT PANEL
   ========================================================== */

.control-deck {

  position:absolute;

  z-index:20;

  left:23px;
  top:160px;

  width:290px;

  padding:21px;

  border-radius:16px;

  box-sizing:border-box;
}


.panel-kicker {

  margin-bottom:16px;

  color:#B9CAD2;

  font-size:11px;

  font-weight:850;

  letter-spacing:1.5px;
}


.intel-panel .panel-kicker {

  margin-bottom:18px;

  font-size:13px;

  letter-spacing:2px;
}


.control-deck label {

  color:#C2D0D6;

  font-size:11px;

  font-weight:800;

  margin-bottom:7px;

  letter-spacing:.25px;
}


.control-deck .form-group {

  margin-bottom:18px;
}


.form-control,
.selectize-input {

  min-height:46px !important;

  padding-top:10px !important;

  padding-bottom:10px !important;

  background:
    rgba(255,255,255,.055) !important;

  border:
    1px solid rgba(150,190,207,.25) !important;

  color:#F4F7F9 !important;

  box-shadow:none !important;

  border-radius:9px !important;

  font-size:13px !important;

  font-weight:650 !important;
}


.selectize-input input {

  color:#FFFFFF !important;

  font-size:13px !important;
}


.selectize-input .item {

  color:#F4F7F9 !important;

  font-size:13px !important;
}


.selectize-dropdown {

  background:#071B28 !important;

  color:#F4F7F9 !important;

  border:
    1px solid #264553 !important;

  border-radius:8px !important;

  font-size:13px !important;
}


.selectize-dropdown-content .option {

  color:#E7EEF2 !important;

  padding:10px 12px !important;

  font-size:13px !important;
}


.selectize-dropdown-content .active {

  background:#163847 !important;
}


/* ==========================================================
   VIEW BUTTONS
   ========================================================== */

.camera-heading {

  margin-top:10px;

  margin-bottom:10px;

  color:#B4C4CB;

  font-size:11px;

  font-weight:850;

  letter-spacing:1.2px;
}


.camera-grid {

  display:grid;

  grid-template-columns:
    repeat(3,1fr);

  gap:8px;
}


.camera-grid .btn {

  min-height:42px;

  padding:8px 5px;

  background:
    rgba(255,255,255,.055);

  border:
    1px solid rgba(158,190,203,.20);

  border-radius:8px;

  color:#D9E3E7;

  font-size:11px;

  font-weight:800;
}


.camera-grid .btn:hover {

  background:
    rgba(32,118,138,.30);

  color:#FFFFFF;

  border-color:
    rgba(73,167,185,.45);
}


#globe_view {

  background:
    rgba(20,111,125,.30);
}


/* ==========================================================
   RIGHT COUNTY INTELLIGENCE PANEL
   ========================================================== */

.intel-panel {

  position:absolute;

  z-index:20;

  top:18px;
  right:18px;

  width:390px;

  max-height:
    calc(100vh - 36px);

  overflow-y:auto;

  padding:22px;

  border-radius:18px;

  box-sizing:border-box;
}


.intel-panel::-webkit-scrollbar {

  width:4px;
}


.intel-panel::-webkit-scrollbar-thumb {

  background:
    rgba(180,205,216,.20);

  border-radius:12px;
}


.intel-heading {

  display:flex;

  justify-content:space-between;

  align-items:center;

  margin-bottom:13px;
}


.intel-title {

  color:#F0F5F7;

  font-size:12px;

  font-weight:850;

  letter-spacing:1.6px;
}


.study-years {

  color:#728995;

  font-size:11px;

  font-weight:750;
}


.selected-name {

  margin-top:4px;

  color:#FFFFFF;

  font-size:30px;

  font-weight:850;

  line-height:1.08;

  letter-spacing:-.5px;
}


.selected-sub {

  margin-top:8px;

  color:#FFD166;

  font-size:10px;

  font-weight:850;

  letter-spacing:1px;
}


.hero-score {

  display:flex;

  align-items:flex-end;

  justify-content:space-between;

  margin-top:18px;

  margin-bottom:14px;
}


.score-number {

  color:#FFFFFF;

  font-size:58px;

  line-height:.95;

  font-weight:850;

  letter-spacing:-1.4px;
}


.score-label {

  margin-top:8px;

  color:#718994;

  font-size:10px;

  font-weight:800;

  letter-spacing:1.2px;
}


.risk-badge {

  padding:7px 12px;

  border-radius:999px;

  font-size:10px;

  font-weight:850;
}


/* ==========================================================
   METRICS
   ========================================================== */

.metric-grid {

  display:grid;

  grid-template-columns:
    1fr 1fr;

  gap:12px;

  margin-top:8px;
}


.metric-card {

  min-height:88px;

  padding:14px;

  border-radius:14px;

  background:
    rgba(255,255,255,.035);

  border:
    1px solid rgba(159,193,206,.12);

  box-sizing:border-box;
}


.metric-name {

  color:#78909B;

  font-size:10px;

  font-weight:800;

  letter-spacing:.8px;
}


.metric-big {

  margin-top:8px;

  color:#F2F6F8;

  font-size:20px;

  line-height:1.2;

  font-weight:800;
}


.divider {

  height:1px;

  margin:18px 0;

  background:
    rgba(190,213,223,.10);
}


.rank-line {

  display:flex;

  justify-content:space-between;

  align-items:center;

  padding:11px 0;

  border-top:
    1px solid rgba(184,208,218,.09);

  font-size:12px;
}


.rank-label {

  color:#7F959F;

  font-size:12px;
}


.rank-value {

  color:#F2F6F8;

  font-weight:800;

  font-size:12px;
}


/* ==========================================================
   SUMMARY
   ========================================================== */

.summary-grid {

  display:grid;

  grid-template-columns:
    1fr 1fr;

  gap:10px;
}


.summary-box {

  min-height:72px;

  padding:14px;

  border-radius:12px;

  background:
    rgba(255,255,255,.035);

  border:
    1px solid rgba(171,202,214,.10);
}


.summary-label {

  color:#718995;

  font-size:10px;

  font-weight:800;

  text-transform:uppercase;

  letter-spacing:.8px;
}


.summary-value {

  margin-top:8px;

  color:#EDF3F5;

  font-size:20px;

  font-weight:800;
}


/* ==========================================================
   PRIORITY TABLE
   ========================================================== */

.priority-table {

  width:100%;

  border-collapse:collapse;

  font-size:12px;
}


.priority-table td {

  padding:11px 2px;

  border-top:
    1px solid rgba(183,208,218,.08);
}


.priority-index {

  width:32px;

  color:#E05A69;

  font-weight:850;

  font-size:13px;
}


.priority-score {

  text-align:right;

  color:#FFFFFF;

  font-weight:800;

  font-size:12px;
}


/* ==========================================================
   BOTTOM KEY
   ========================================================== */

.encoding-chip {

  position:absolute;

  z-index:20;

  bottom:16px;
  left:50%;

  transform:
    translateX(-50%);

  padding:8px 14px;

  border-radius:999px;

  background:
    rgba(2,16,25,.91);

  border:
    1px solid rgba(117,172,193,.18);

  color:#A9BBC3;

  font-size:8px;

  font-weight:750;

  white-space:nowrap;
}


.key-color {

  color:#D4A73E;
}


.key-height {

  color:#4DA1BF;
}


.key-selected {

  color:#FFD166;
}


/* ==========================================================
   AUTHOR / COPYRIGHT CREDIT
   ========================================================== */

.project-credit {

  position:absolute;

  z-index:25;

  right:430px;
  bottom:14px;

  max-width:350px;

  padding:8px 11px;

  border-radius:9px;

  background:
    rgba(2,16,25,.91);

  border:
    1px solid rgba(117,172,193,.18);

  box-shadow:
    0 8px 22px rgba(0,0,0,.28);

  color:
    rgba(220,231,236,.82);

  font-size:9px;

  line-height:1.45;

  text-align:right;
}


.project-author {

  color:#FFFFFF;

  font-weight:800;

  letter-spacing:.2px;
}


.project-rights {

  margin-top:2px;

  color:#91A8B2;

  font-size:8px;
}


.project-citation {

  margin-top:4px;

  color:#7FC6CF;

  font-size:8px;
}


/* ==========================================================
   MAP POPUP + CONTROLS
   ========================================================== */

.maplibregl-popup-content {

  background:
    rgba(248,251,252,.98) !important;

  color:#14232B !important;

  border-radius:11px !important;
}


.maplibregl-ctrl-group {

  background:
    rgba(3,18,28,.90) !important;

  border:
    1px solid rgba(115,166,186,.18) !important;
}


.shiny-input-container {

  width:100% !important;
}

"


# ============================================================
# 26. UI
# ============================================================

ui <- fluidPage(
  
  tags$head(
    
    # ========================================================
    # AUTHORSHIP / COPYRIGHT METADATA
    # ========================================================
    
    tags$title(
      "U.S. Heat-Cardiorenal Vulnerability Atlas | Divya Vemula"
    ),
    
    
    tags$meta(
      name =
        "author",
      content =
        "Divya Vemula"
    ),
    
    
    tags$meta(
      name =
        "copyright",
      content =
        "© 2026 Divya Vemula. All rights reserved."
    ),
    
    
    tags$meta(
      name =
        "description",
      content =
        paste0(
          "U.S. Heat-Cardiorenal Vulnerability Atlas: ",
          "an interactive county-level spatial epidemiology atlas ",
          "developed by Divya Vemula."
        )
    ),
    
    
    tags$style(
      
      HTML(
        app_css
      )
    )
  ),
  
  
  tags$div(
    
    class =
      "atlas-shell",
    
    
    # ========================================================
    # MAP
    # ========================================================
    
    tags$div(
      
      class =
        "map-stage",
      
      maplibreOutput(
        
        "map",
        
        height =
          "100vh"
      )
    ),
    
    
    tags$div(
      
      class =
        "map-atmosphere"
    ),
    
    
    # ========================================================
    # TITLE
    # ========================================================
    
    tags$div(
      
      class =
        "hero-title",
      
      
      tags$div(
        
        class =
          "eyebrow",
        
        "3D SPATIAL EPIDEMIOLOGY | 2016-2020"
      ),
      
      
      tags$h1(
        
        class =
          "hero-heading",
        
        "U.S. Heat-Cardiorenal Vulnerability Atlas"
      ),
      
      
      tags$div(
        
        class =
          "hero-subtitle",
        
        paste0(
          "Interactive county epidemiology of extreme heat, ",
          "cardiometabolic susceptibility, social vulnerability and ",
          "age-adjusted heart-disease mortality."
        )
      )
    ),
    
    
    # ========================================================
    # LEFT PANEL
    # ========================================================
    
    tags$div(
      
      class =
        "control-deck glass",
      
      
      tags$div(
        
        class =
          "panel-kicker",
        
        "EXPLORE THE LANDSCAPE"
      ),
      
      
      selectInput(
        
        "state_filter",
        
        "GEOGRAPHY",
        
        choices =
          state_choices,
        
        selected =
          "CONUS"
      ),
      
      
      selectInput(
        
        "map_layer",
        
        "COLOR LAYER",
        
        choices =
          c(
            
            "Overall HCV Vulnerability" =
              "HCV",
            
            "Extreme Heat Exposure" =
              "HEAT",
            
            "Clinical Susceptibility" =
              "CLINICAL",
            
            "Social Vulnerability" =
              "SVI",
            
            "Heart-Disease Mortality" =
              "MORTALITY"
          ),
        
        selected =
          "HCV"
      ),
      
      
      selectizeInput(
        
        "county_search",
        
        "COUNTY SEARCH",
        
        choices =
          NULL,
        
        selected =
          NULL,
        
        options =
          list(
            
            placeholder =
              "Search county..."
          )
      ),
      
      
      tags$div(
        
        class =
          "camera-heading",
        
        "VIEW"
      ),
      
      
      tags$div(
        
        class =
          "camera-grid",
        
        
        actionButton(
          
          "globe_view",
          
          "Globe"
        ),
        
        
        actionButton(
          
          "three_d_view",
          
          "3D"
        ),
        
        
        actionButton(
          
          "top_view",
          
          "Top"
        )
      )
    ),
    
    
    # ========================================================
    # RIGHT PANEL
    # ========================================================
    
    tags$div(
      
      class =
        "intel-panel glass",
      
      
      tags$div(
        
        class =
          "intel-heading",
        
        
        tags$div(
          
          class =
            "intel-title",
          
          "COUNTY INTELLIGENCE"
        ),
        
        
        tags$div(
          
          class =
            "study-years",
          
          "2016-2020"
        )
      ),
      
      
      uiOutput(
        "county_details"
      ),
      
      
      tags$div(
        
        class =
          "divider"
      ),
      
      
      tags$div(
        
        class =
          "panel-kicker",
        
        "CURRENT AREA"
      ),
      
      
      uiOutput(
        "area_summary"
      ),
      
      
      tags$div(
        
        class =
          "divider"
      ),
      
      
      tags$div(
        
        class =
          "panel-kicker",
        
        "HIGHEST PRIORITY COUNTIES"
      ),
      
      
      uiOutput(
        "top_counties"
      )
    ),
    
    
    # ========================================================
    # BOTTOM ENCODING KEY
    # ========================================================
    
    tags$div(
      
      class =
        "encoding-chip",
      
      
      tags$span(
        
        class =
          "key-color",
        
        "COLOR"
      ),
      
      
      " = epidemiologic layer",
      
      "   |   ",
      
      
      tags$span(
        
        class =
          "key-height",
        
        "HEIGHT"
      ),
      
      
      " = mortality",
      
      "   |   ",
      
      
      tags$span(
        
        class =
          "key-selected",
        
        "GOLD"
      ),
      
      
      " = selected county"
    ),
    
    
    # ========================================================
    # AUTHOR / COPYRIGHT / CITATION
    # ========================================================
    
    tags$div(
      
      class =
        "project-credit",
      
      
      tags$div(
        
        class =
          "project-author",
        
        HTML(
          "&copy; 2026 Divya Vemula"
        )
      ),
      
      
      tags$div(
        
        class =
          "project-rights",
        
        "All rights reserved."
      ),
      
      
      tags$div(
        
        class =
          "project-citation",
        
        "Suggested citation: Vemula, D. (2026). U.S. Heat-Cardiorenal Vulnerability Atlas."
      )
    )
  )
)


# ============================================================
# 27. SERVER
# ============================================================

server <- function(
    input,
    output,
    session
) {
  
  
  # ==========================================================
  # REACTIVE VALUES
  # ==========================================================
  
  selected_fips <-
    
    reactiveVal(
      NULL
    )
  
  
  # ==========================================================
  # AREA DATA
  # ==========================================================
  
  area_data <- reactive({
    
    req(
      input$state_filter
    )
    
    
    if (
      input$state_filter ==
      "CONUS"
    ) {
      
      map_all |>
        
        filter(
          
          !State_FIPS %in%
            c(
              "02",
              "15"
            )
        )
      
      
    } else if (
      input$state_filter ==
      "ALL"
    ) {
      
      map_all
      
      
    } else {
      
      map_all |>
        
        filter(
          
          State_Display ==
            input$state_filter
        )
    }
  })
  
  
  # ==========================================================
  # STATE BOUNDARIES
  # ==========================================================
  
  area_states <- reactive({
    
    req(
      input$state_filter
    )
    
    
    if (
      input$state_filter ==
      "CONUS"
    ) {
      
      state_boundaries |>
        
        filter(
          
          !State_FIPS %in%
            c(
              "02",
              "15"
            )
        )
      
      
    } else if (
      input$state_filter ==
      "ALL"
    ) {
      
      state_boundaries
      
      
    } else {
      
      state_boundaries |>
        
        filter(
          
          State_Display ==
            input$state_filter
        )
    }
  })
  
  
  # ==========================================================
  # COUNTY SEARCH OPTIONS
  # ==========================================================
  
  observe({
    
    counties <-
      
      area_data() |>
      
      st_drop_geometry() |>
      
      arrange(
        County_Display
      ) |>
      
      mutate(
        
        Search_Label =
          
          paste0(
            County_Display,
            ", ",
            State_Abbr_Display
          )
      )
    
    
    county_choices <-
      
      setNames(
        counties$FIPS,
        counties$Search_Label
      )
    
    
    updateSelectizeInput(
      
      session,
      
      "county_search",
      
      choices =
        county_choices,
      
      selected =
        if (
          is.null(
            selected_fips()
          )
        ) {
          
          character(
            0
          )
          
        } else {
          
          selected_fips()
        },
      
      server =
        TRUE
    )
  })
  
  
  # ==========================================================
  # CLEAR COUNTY WHEN STATE CHANGES
  # ==========================================================
  
  observeEvent(
    
    input$state_filter,
    
    {
      
      selected_fips(
        NULL
      )
      
      
      updateSelectizeInput(
        
        session,
        
        "county_search",
        
        selected =
          character(
            0
          )
      )
    },
    
    ignoreInit =
      TRUE
  )
  
  
  # ==========================================================
  # LEGEND TITLE
  # ==========================================================
  
  legend_title <- reactive({
    
    switch(
      
      input$map_layer,
      
      "HCV" =
        "Overall HCV Vulnerability",
      
      "HEAT" =
        "Extreme Heat Exposure",
      
      "CLINICAL" =
        "Clinical Susceptibility",
      
      "SVI" =
        "Social Vulnerability",
      
      "MORTALITY" =
        "Heart-Disease Mortality",
      
      "County Level"
    )
  })
  
  
  # ==========================================================
  # MAIN MAP
  # ==========================================================
  
  output$map <- renderMaplibre({
    
    data <-
      area_data()
    
    
    states <-
      area_states()
    
    
    req(
      nrow(
        data
      ) > 0
    )
    
    
    base_color <-
      
      base_color_expression(
        input$map_layer
      )
    
    
    projection_mode <- if (
      
      input$state_filter %in%
      c(
        "CONUS",
        "ALL"
      )
      
    ) {
      
      "globe"
      
    } else {
      
      "mercator"
    }
    
    
    map <-
      
      maplibre(
        
        style =
          carto_style(
            "dark-matter"
          ),
        
        center =
          c(
            -99,
            38
          ),
        
        zoom =
          2.55,
        
        pitch =
          28,
        
        bearing =
          -5,
        
        projection =
          projection_mode,
        
        minZoom =
          1.3
      ) |>
      
      
      # ======================================================
    # COUNTY SOURCE
    # ======================================================
    
    add_source(
      
      id =
        "county_source",
      
      data =
        data
    ) |>
      
      
      # ======================================================
    # COUNTY BASE
    # ======================================================
    
    add_fill_layer(
      
      id =
        "county_base",
      
      source =
        "county_source",
      
      fill_color =
        base_color,
      
      fill_opacity =
        0.88,
      
      fill_outline_color =
        "#102731"
    ) |>
      
      
      # ======================================================
    # COUNTY 3D EXTRUSION
    # ======================================================
    
    add_fill_extrusion_layer(
      
      id =
        "county_extrusion",
      
      source =
        "county_source",
      
      fill_extrusion_color =
        base_color,
      
      fill_extrusion_height =
        get_column(
          "Mortality_3D_Height"
        ),
      
      fill_extrusion_base =
        0,
      
      fill_extrusion_opacity =
        0.94,
      
      fill_extrusion_vertical_gradient =
        TRUE,
      
      fill_extrusion_cutoff_fade_range =
        0,
      
      fill_extrusion_cast_shadows =
        TRUE,
      
      fill_extrusion_ambient_occlusion_intensity =
        0.28,
      
      fill_extrusion_ambient_occlusion_radius =
        2.5,
      
      tooltip =
        "{County_Display}, {State_Abbr_Display}",
      
      popup =
        "Popup_HTML",
      
      tooltip_style =
        "dark",
      
      popup_style =
        "light"
    ) |>
      
      
      # ======================================================
    # COUNTY LINES
    # ======================================================
    
    add_line_layer(
      
      id =
        "county_lines",
      
      source =
        "county_source",
      
      line_color =
        "#17323C",
      
      line_width =
        0.35,
      
      line_opacity =
        0.55
    ) |>
      
      
      # ======================================================
    # SELECTED COUNTY GLOW
    # ======================================================
    
    add_line_layer(
      
      id =
        "selected_glow",
      
      source =
        "county_source",
      
      line_color =
        selected_gold,
      
      line_width =
        9,
      
      line_opacity =
        0.24,
      
      line_blur =
        3,
      
      filter =
        list(
          
          "==",
          
          get_column(
            "FIPS"
          ),
          
          "__NONE__"
        )
    ) |>
      
      
      # ======================================================
    # SELECTED COUNTY OUTLINE
    # ======================================================
    
    add_line_layer(
      
      id =
        "selected_outline",
      
      source =
        "county_source",
      
      line_color =
        selected_gold_edge,
      
      line_width =
        3,
      
      line_opacity =
        1,
      
      filter =
        list(
          
          "==",
          
          get_column(
            "FIPS"
          ),
          
          "__NONE__"
        )
    ) |>
      
      
      # ======================================================
    # STATE SOURCE
    # ======================================================
    
    add_source(
      
      id =
        "state_source",
      
      data =
        states
    ) |>
      
      
      # ======================================================
    # STATE LINES
    # ======================================================
    
    add_line_layer(
      
      id =
        "state_lines",
      
      source =
        "state_source",
      
      line_color =
        "#7C919B",
      
      line_width =
        1.1,
      
      line_opacity =
        0.65
    ) |>
      
      
      # ======================================================
    # MAP CONTROLS
    # ======================================================
    
    add_navigation_control(
      
      position =
        "bottom-right",
      
      visualize_pitch =
        TRUE
    ) |>
      
      
      add_globe_control(
        
        position =
          "top-right"
      ) |>
      
      
      add_fullscreen_control(
        
        position =
          "top-right"
      ) |>
      
      
      # ======================================================
    # LEGEND
    # ======================================================
    
    add_categorical_legend(
      
      legend_title =
        legend_title(),
      
      values =
        category_levels,
      
      colors =
        unname(
          
          category_colors[
            category_levels
          ]
        ),
      
      position =
        "bottom-left",
      
      layer_id =
        "county_extrusion",
      
      width =
        185,
      
      style =
        list(
          
          background_color =
            "#03131E",
          
          background_opacity =
            0.96,
          
          border_color =
            "#244453",
          
          border_width =
            1,
          
          text_color =
            "#D8E3E8",
          
          title_font_weight =
            "bold"
        )
    )
    
    
    # ========================================================
    # INITIAL STATE VIEW
    # ========================================================
    
    if (
      
      !input$state_filter %in%
      c(
        "CONUS",
        "ALL"
      )
      
    ) {
      
      map <-
        
        map |>
        
        fit_bounds(
          
          data,
          
          animate =
            FALSE,
          
          padding =
            120,
          
          maxZoom =
            5.5,
          
          pitch =
            30,
          
          bearing =
            -5
        )
    }
    
    
    map
  })
  
  
  # ==========================================================
  # RESTORE NORMAL MAP STYLE
  # ==========================================================
  
  restore_map_style <- function() {
    
    base_color <-
      
      base_color_expression(
        input$map_layer
      )
    
    
    no_county <-
      
      list(
        
        "==",
        
        get_column(
          "FIPS"
        ),
        
        "__NONE__"
      )
    
    
    maplibre_proxy(
      
      "map",
      
      session =
        session
      
    ) |>
      
      set_paint_property(
        
        layer_id =
          "county_extrusion",
        
        name =
          "fill-extrusion-color",
        
        value =
          base_color
      ) |>
      
      
      set_paint_property(
        
        layer_id =
          "county_extrusion",
        
        name =
          "fill-extrusion-height",
        
        value =
          get_column(
            "Mortality_3D_Height"
          )
      ) |>
      
      
      set_paint_property(
        
        layer_id =
          "county_extrusion",
        
        name =
          "fill-extrusion-opacity",
        
        value =
          0.94
      ) |>
      
      
      set_paint_property(
        
        layer_id =
          "county_base",
        
        name =
          "fill-color",
        
        value =
          base_color
      ) |>
      
      
      set_paint_property(
        
        layer_id =
          "county_base",
        
        name =
          "fill-opacity",
        
        value =
          0.88
      ) |>
      
      
      set_filter(
        
        "selected_glow",
        
        no_county
      ) |>
      
      
      set_filter(
        
        "selected_outline",
        
        no_county
      )
    
    
    invisible(
      NULL
    )
  }
  
  
  # ==========================================================
  # SELECT COUNTY
  # ==========================================================
  
  apply_county_selection <- function(
    fips
  ) {
    
    if (
      
      is.null(
        fips
      ) ||
      
      fips ==
      ""
      
    ) {
      
      return(
        
        invisible(
          NULL
        )
      )
    }
    
    
    fips <-
      
      as.character(
        fips
      )
    
    
    county <-
      
      area_data() |>
      
      filter(
        
        FIPS ==
          fips
      )
    
    
    if (
      
      nrow(
        county
      ) != 1
      
    ) {
      
      return(
        
        invisible(
          NULL
        )
      )
    }
    
    
    base_color <-
      
      base_color_expression(
        input$map_layer
      )
    
    
    selected_condition <-
      
      list(
        
        "==",
        
        get_column(
          "FIPS"
        ),
        
        fips
      )
    
    
    selected_color_expression <-
      
      list(
        
        "case",
        
        selected_condition,
        
        selected_gold,
        
        base_color
      )
    
    
    selected_height_expression <-
      
      list(
        
        "case",
        
        selected_condition,
        
        list(
          
          "+",
          
          get_column(
            "Mortality_3D_Height"
          ),
          
          28000
        ),
        
        get_column(
          "Mortality_3D_Height"
        )
      )
    
    
    selected_opacity_expression <-
      
      list(
        
        "case",
        
        selected_condition,
        
        1.0,
        
        0.55
      )
    
    
    selected_base_opacity <-
      
      list(
        
        "case",
        
        selected_condition,
        
        1.0,
        
        0.56
      )
    
    
    maplibre_proxy(
      
      "map",
      
      session =
        session
      
    ) |>
      
      set_paint_property(
        
        layer_id =
          "county_extrusion",
        
        name =
          "fill-extrusion-color",
        
        value =
          selected_color_expression
      ) |>
      
      
      set_paint_property(
        
        layer_id =
          "county_extrusion",
        
        name =
          "fill-extrusion-height",
        
        value =
          selected_height_expression
      ) |>
      
      
      set_paint_property(
        
        layer_id =
          "county_extrusion",
        
        name =
          "fill-extrusion-opacity",
        
        value =
          selected_opacity_expression
      ) |>
      
      
      set_paint_property(
        
        layer_id =
          "county_base",
        
        name =
          "fill-color",
        
        value =
          selected_color_expression
      ) |>
      
      
      set_paint_property(
        
        layer_id =
          "county_base",
        
        name =
          "fill-opacity",
        
        value =
          selected_base_opacity
      ) |>
      
      
      set_filter(
        
        "selected_glow",
        
        selected_condition
      ) |>
      
      
      set_filter(
        
        "selected_outline",
        
        selected_condition
      )
    
    
    focus_bbox <-
      
      county_focus_bbox(
        
        county,
        
        expansion =
          0.30
      )
    
    
    later::later(
      
      function() {
        
        maplibre_proxy(
          
          "map",
          
          session =
            session
          
        ) |>
          
          fit_bounds(
            
            bbox =
              focus_bbox,
            
            animate =
              TRUE,
            
            padding =
              list(
                
                top =
                  100,
                
                bottom =
                  120,
                
                left =
                  310,
                
                right =
                  430
              ),
            
            maxZoom =
              9.0,
            
            pitch =
              60,
            
            bearing =
              -20,
            
            duration =
              1300
          )
      },
      
      delay =
        0.20
    )
    
    
    invisible(
      NULL
    )
  }
  
  
  # ==========================================================
  # COUNTY SEARCH
  # ==========================================================
  
  observeEvent(
    
    input$county_search,
    
    {
      
      req(
        input$county_search
      )
      
      
      fips <-
        
        as.character(
          input$county_search
        )
      
      
      selected_fips(
        fips
      )
      
      
      apply_county_selection(
        fips
      )
    },
    
    ignoreInit =
      TRUE
  )
  
  
  # ==========================================================
  # MAP CLICK
  # ==========================================================
  
  observeEvent(
    
    input$map_feature_click,
    
    {
      
      clicked <-
        input$map_feature_click
      
      
      req(
        clicked
      )
      
      
      fips <-
        clicked$properties$FIPS
      
      
      if (
        is.null(
          fips
        )
      ) {
        
        return()
      }
      
      
      fips <-
        
        as.character(
          fips
        )
      
      
      selected_fips(
        fips
      )
      
      
      updateSelectizeInput(
        
        session,
        
        "county_search",
        
        selected =
          fips
      )
      
      
      apply_county_selection(
        fips
      )
    }
  )
  
  
  # ==========================================================
  # COLOR LAYER CHANGE
  # ==========================================================
  
  observeEvent(
    
    input$map_layer,
    
    {
      
      selected <-
        selected_fips()
      
      
      if (
        is.null(
          selected
        )
      ) {
        
        restore_map_style()
        
      } else {
        
        apply_county_selection(
          selected
        )
      }
    },
    
    ignoreInit =
      TRUE
  )
  
  
  # ==========================================================
  # SELECTED COUNTY DATA
  # ==========================================================
  
  selected_county <- reactive({
    
    data <-
      area_data()
    
    
    selected <-
      selected_fips()
    
    
    if (
      !is.null(
        selected
      )
    ) {
      
      result <-
        
        data |>
        
        filter(
          
          FIPS ==
            selected
        )
      
      
      if (
        nrow(
          result
        ) > 0
      ) {
        
        return(
          result
        )
      }
    }
    
    
    data |>
      
      filter(
        
        !is.na(
          HCV_Score
        )
      ) |>
      
      arrange(
        
        desc(
          HCV_Score
        )
      ) |>
      
      slice(
        1
      )
  })
  
  
  # ==========================================================
  # COUNTY DETAILS
  # ==========================================================
  
  output$county_details <- renderUI({
    
    x <-
      selected_county()
    
    
    req(
      nrow(
        x
      ) > 0
    )
    
    
    risk_color <-
      
      unname(
        
        category_colors[
          x$HCV_Category
        ]
      )
    
    
    if (
      
      length(
        risk_color
      ) == 0 ||
      
      is.na(
        risk_color
      )
      
    ) {
      
      risk_color <-
        "#283844"
    }
    
    
    active <-
      
      !is.null(
        selected_fips()
      )
    
    
    tags$div(
      
      
      tags$div(
        
        class =
          "selected-name",
        
        paste0(
          x$County_Display,
          ", ",
          x$State_Abbr_Display
        )
      ),
      
      
      tags$div(
        
        class =
          "selected-sub",
        
        if (
          active
        ) {
          
          "SELECTED COUNTY | GOLD FOCUS + 3D ELEVATION"
          
        } else {
          
          "COUNTY-LEVEL EPIDEMIOLOGIC PROFILE"
        }
      ),
      
      
      tags$div(
        
        class =
          "hero-score",
        
        
        tags$div(
          
          tags$div(
            
            class =
              "score-number",
            
            format1(
              x$HCV_Score
            )
          ),
          
          
          tags$div(
            
            class =
              "score-label",
            
            "HCV SCORE / 100"
          )
        ),
        
        
        tags$div(
          
          class =
            "risk-badge",
          
          style =
            paste0(
              
              "color:",
              risk_color,
              
              ";background:",
              risk_color,
              "18;",
              
              "border:1px solid ",
              risk_color,
              "66;"
            ),
          
          x$HCV_Category
        )
      ),
      
      
      # ======================================================
      # METRICS
      # ======================================================
      
      tags$div(
        
        class =
          "metric-grid",
        
        
        tags$div(
          
          class =
            "metric-card",
          
          
          tags$div(
            
            class =
              "metric-name",
            
            "HEART MORTALITY"
          ),
          
          
          tags$div(
            
            class =
              "metric-big",
            
            paste0(
              
              format1(
                x$Heart_Mortality
              ),
              
              " /100k"
            )
          )
        ),
        
        
        tags$div(
          
          class =
            "metric-card",
          
          
          tags$div(
            
            class =
              "metric-name",
            
            "EXTREME HEAT"
          ),
          
          
          tags$div(
            
            class =
              "metric-big",
            
            paste0(
              
              format1(
                x$Extreme_Heat_Days
              ),
              
              " days"
            )
          )
        ),
        
        
        tags$div(
          
          class =
            "metric-card",
          
          
          tags$div(
            
            class =
              "metric-name",
            
            "CLINICAL SCORE"
          ),
          
          
          tags$div(
            
            class =
              "metric-big",
            
            format1(
              x$Clinical_Score
            )
          )
        ),
        
        
        tags$div(
          
          class =
            "metric-card",
          
          
          tags$div(
            
            class =
              "metric-name",
            
            "SOCIAL VULNERABILITY"
          ),
          
          
          tags$div(
            
            class =
              "metric-big",
            
            paste0(
              
              format1(
                x$SVI *
                  100
              ),
              
              " /100"
            )
          )
        )
      ),
      
      
      tags$div(
        
        style =
          "margin-top:11px;"
      ),
      
      
      # ======================================================
      # RANKS / INDICATORS
      # ======================================================
      
      tags$div(
        
        class =
          "rank-line",
        
        
        tags$span(
          
          class =
            "rank-label",
          
          "U.S. vulnerability rank"
        ),
        
        
        tags$span(
          
          class =
            "rank-value",
          
          format_rank(
            x$US_Rank
          )
        )
      ),
      
      
      tags$div(
        
        class =
          "rank-line",
        
        
        tags$span(
          
          class =
            "rank-label",
          
          "State vulnerability rank"
        ),
        
        
        tags$span(
          
          class =
            "rank-value",
          
          format_rank(
            x$State_Rank
          )
        )
      ),
      
      
      tags$div(
        
        class =
          "rank-line",
        
        
        tags$span(
          
          class =
            "rank-label",
          
          "Hypertension"
        ),
        
        
        tags$span(
          
          class =
            "rank-value",
          
          format_pct(
            x$Hypertension
          )
        )
      ),
      
      
      tags$div(
        
        class =
          "rank-line",
        
        
        tags$span(
          
          class =
            "rank-label",
          
          "Diabetes"
        ),
        
        
        tags$span(
          
          class =
            "rank-value",
          
          format_pct(
            x$Diabetes
          )
        )
      ),
      
      
      tags$div(
        
        class =
          "rank-line",
        
        
        tags$span(
          
          class =
            "rank-label",
          
          "Obesity"
        ),
        
        
        tags$span(
          
          class =
            "rank-value",
          
          format_pct(
            x$Obesity
          )
        )
      ),
      
      
      tags$div(
        
        class =
          "rank-line",
        
        
        tags$span(
          
          class =
            "rank-label",
          
          "Coronary heart disease"
        ),
        
        
        tags$span(
          
          class =
            "rank-value",
          
          format_pct(
            x$CHD
          )
        )
      )
    )
  })
  
  
  # ==========================================================
  # AREA SUMMARY
  # ==========================================================
  
  output$area_summary <- renderUI({
    
    data <-
      
      area_data() |>
      
      st_drop_geometry()
    
    
    valid <-
      
      data |>
      
      filter(
        
        !is.na(
          HCV_Score
        )
      )
    
    
    if (
      nrow(
        valid
      ) == 0
    ) {
      
      return(
        
        tags$div(
          "No available HCV data."
        )
      )
    }
    
    
    highest <-
      
      valid |>
      
      arrange(
        
        desc(
          HCV_Score
        )
      ) |>
      
      slice(
        1
      )
    
    
    tags$div(
      
      class =
        "summary-grid",
      
      
      tags$div(
        
        class =
          "summary-box",
        
        
        tags$div(
          
          class =
            "summary-label",
          
          "Counties"
        ),
        
        
        tags$div(
          
          class =
            "summary-value",
          
          comma(
            
            nrow(
              data
            )
          )
        )
      ),
      
      
      tags$div(
        
        class =
          "summary-box",
        
        
        tags$div(
          
          class =
            "summary-label",
          
          "Median HCV"
        ),
        
        
        tags$div(
          
          class =
            "summary-value",
          
          format1(
            
            median(
              valid$HCV_Score,
              na.rm =
                TRUE
            )
          )
        )
      ),
      
      
      tags$div(
        
        class =
          "summary-box",
        
        style =
          "grid-column:1 / span 2;",
        
        
        tags$div(
          
          class =
            "summary-label",
          
          "Highest vulnerability"
        ),
        
        
        tags$div(
          
          class =
            "summary-value",
          
          paste0(
            highest$County_Display,
            ", ",
            highest$State_Abbr_Display
          )
        )
      )
    )
  })
  
  
  # ==========================================================
  # TOP FIVE COUNTIES
  # ==========================================================
  
  output$top_counties <- renderUI({
    
    top5 <-
      
      area_data() |>
      
      st_drop_geometry() |>
      
      filter(
        
        !is.na(
          HCV_Score
        )
      ) |>
      
      arrange(
        
        desc(
          HCV_Score
        )
      ) |>
      
      slice_head(
        
        n =
          5
      )
    
    
    if (
      
      nrow(
        top5
      ) == 0
      
    ) {
      
      return(
        
        tags$div(
          "No available data."
        )
      )
    }
    
    
    tags$table(
      
      class =
        "priority-table",
      
      
      tags$tbody(
        
        lapply(
          
          seq_len(
            
            nrow(
              top5
            )
          ),
          
          
          function(i) {
            
            tags$tr(
              
              
              tags$td(
                
                class =
                  "priority-index",
                
                paste0(
                  "0",
                  i
                )
              ),
              
              
              tags$td(
                
                paste0(
                  top5$County_Display[
                    i
                  ],
                  ", ",
                  top5$State_Abbr_Display[
                    i
                  ]
                )
              ),
              
              
              tags$td(
                
                class =
                  "priority-score",
                
                format1(
                  top5$HCV_Score[
                    i
                  ]
                )
              )
            )
          }
        )
      )
    )
  })
  
  
  # ==========================================================
  # GLOBE VIEW
  # ==========================================================
  
  observeEvent(
    
    input$globe_view,
    
    {
      
      maplibre_proxy(
        
        "map",
        
        session =
          session
        
      ) |>
        
        fly_to(
          
          center =
            c(
              -99,
              38
            ),
          
          zoom =
            2.55,
          
          pitch =
            25,
          
          bearing =
            -5,
          
          duration =
            1000
        )
    }
  )
  
  
  # ==========================================================
  # 3D VIEW
  # ==========================================================
  
  observeEvent(
    
    input$three_d_view,
    
    {
      
      selected <-
        selected_fips()
      
      
      if (
        !is.null(
          selected
        )
      ) {
        
        county <-
          
          area_data() |>
          
          filter(
            
            FIPS ==
              selected
          )
        
        
        if (
          nrow(
            county
          ) == 1
        ) {
          
          focus_bbox <-
            
            county_focus_bbox(
              
              county,
              
              expansion =
                0.30
            )
          
          
          maplibre_proxy(
            
            "map",
            
            session =
              session
            
          ) |>
            
            fit_bounds(
              
              bbox =
                focus_bbox,
              
              animate =
                TRUE,
              
              padding =
                list(
                  
                  top =
                    100,
                  
                  bottom =
                    120,
                  
                  left =
                    310,
                  
                  right =
                    430
                ),
              
              maxZoom =
                9,
              
              pitch =
                65,
              
              bearing =
                -20,
              
              duration =
                900
            )
        }
        
        
      } else {
        
        maplibre_proxy(
          
          "map",
          
          session =
            session
          
        ) |>
          
          fit_bounds(
            
            bbox =
              area_data(),
            
            animate =
              TRUE,
            
            padding =
              120,
            
            maxZoom =
              5.5,
            
            pitch =
              46,
            
            bearing =
              -8,
            
            duration =
              900
          )
      }
    }
  )
  
  
  # ==========================================================
  # TOP VIEW
  # ==========================================================
  
  observeEvent(
    
    input$top_view,
    
    {
      
      selected <-
        selected_fips()
      
      
      if (
        !is.null(
          selected
        )
      ) {
        
        county <-
          
          area_data() |>
          
          filter(
            
            FIPS ==
              selected
          )
        
        
        if (
          nrow(
            county
          ) == 1
        ) {
          
          focus_bbox <-
            
            county_focus_bbox(
              
              county,
              
              expansion =
                0.20
            )
          
          
          maplibre_proxy(
            
            "map",
            
            session =
              session
            
          ) |>
            
            fit_bounds(
              
              bbox =
                focus_bbox,
              
              animate =
                TRUE,
              
              padding =
                list(
                  
                  top =
                    90,
                  
                  bottom =
                    100,
                  
                  left =
                    310,
                  
                  right =
                    430
                ),
              
              maxZoom =
                9.2,
              
              pitch =
                0,
              
              bearing =
                0,
              
              duration =
                800
            )
        }
        
        
      } else {
        
        maplibre_proxy(
          
          "map",
          
          session =
            session
          
        ) |>
          
          fit_bounds(
            
            bbox =
              area_data(),
            
            animate =
              TRUE,
            
            padding =
              120,
            
            maxZoom =
              5.5,
            
            pitch =
              0,
            
            bearing =
              0,
            
            duration =
              800
          )
      }
    }
  )
}


# ============================================================
# 28. RUN APP
# ============================================================

shiny::shinyApp(
  ui =
    ui,
  server =
    server
)