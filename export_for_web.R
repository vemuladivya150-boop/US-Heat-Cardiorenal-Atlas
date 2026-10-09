
library(sf)
library(dplyr)
library(stringr)
library(scales)

project_dir <- "/Users/divya/US_Heat_Cardiorenal_Atlas"
input_rds <- file.path(project_dir, "spatial", "us_master_final.rds")
docs_dir <- file.path(project_dir, "docs")
data_dir <- file.path(docs_dir, "data")

dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_rds)) {
  stop(paste0("Cannot find: ", input_rds))
}

us_master <- readRDS(input_rds)

if (!inherits(us_master, "sf")) {
  stop("us_master_final.rds must be an sf object.")
}

first_existing_column <- function(data, candidates) {
  available <- candidates[candidates %in% names(data)]
  if (length(available) == 0) {
    return(rep(NA_character_, nrow(data)))
  }

  result <- rep(NA_character_, nrow(data))

  for (column in available) {
    values <- as.character(data[[column]])
    idx <- is.na(result) | result == ""
    result[idx] <- values[idx]
  }

  result
}

percentile_category <- function(x) {
  case_when(
    is.na(x) ~ "Data unavailable",
    x <= 0.20 ~ "Lowest",
    x <= 0.40 ~ "Low",
    x <= 0.60 ~ "Moderate",
    x <= 0.80 ~ "High",
    TRUE ~ "Highest"
  )
}

us_master$FIPS <- str_pad(
  as.character(us_master$FIPS),
  width = 5,
  side = "left",
  pad = "0"
)

us_master$State_FIPS <- str_pad(
  as.character(us_master$State_FIPS),
  width = 2,
  side = "left",
  pad = "0"
)

if (!"State_Display" %in% names(us_master)) {
  us_master$State_Display <- first_existing_column(
    us_master,
    c("State", "State_SVI", "State_Heat")
  )
}

if (!"State_Abbr_Display" %in% names(us_master)) {
  us_master$State_Abbr_Display <- first_existing_column(
    us_master,
    c("State_Abbreviation", "State_Abbreviation_SVI")
  )
}

if (!"County_Display" %in% names(us_master)) {
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
}

state_name_to_abbr <- c(
  setNames(state.abb, state.name),
  "District of Columbia" = "DC"
)

missing_abbr <- is.na(us_master$State_Abbr_Display) |
  us_master$State_Abbr_Display == ""

us_master$State_Abbr_Display[missing_abbr] <- unname(
  state_name_to_abbr[
    us_master$State_Display[missing_abbr]
  ]
)

numeric_variables <- c(
  "Hypertension",
  "Diabetes",
  "Obesity",
  "CHD",
  "Extreme_Heat_Days",
  "SVI",
  "Heart_Mortality"
)

for (variable in numeric_variables) {
  if (!variable %in% names(us_master)) {
    stop(paste0("Missing variable: ", variable))
  }

  us_master[[variable]] <- suppressWarnings(
    as.numeric(us_master[[variable]])
  )
}

us_master$SVI[us_master$SVI < 0] <- NA_real_

us_master <- us_master |>
  mutate(
    HTN_Pctl = percent_rank(Hypertension),
    Diabetes_Pctl = percent_rank(Diabetes),
    Obesity_Pctl = percent_rank(Obesity),
    CHD_Pctl = percent_rank(CHD)
  )

clinical_matrix <- cbind(
  us_master$HTN_Pctl,
  us_master$Diabetes_Pctl,
  us_master$Obesity_Pctl,
  us_master$CHD_Pctl
)

us_master$Clinical_N <- rowSums(!is.na(clinical_matrix))
clinical_average <- rowMeans(clinical_matrix, na.rm = TRUE)

us_master$Clinical_Susceptibility <- ifelse(
  us_master$Clinical_N >= 3,
  clinical_average,
  NA_real_
)

us_master$Clinical_Score <- us_master$Clinical_Susceptibility * 100
us_master$Heat_Percentile <- percent_rank(us_master$Extreme_Heat_Days)
us_master$Heat_Score <- us_master$Heat_Percentile * 100

us_master$HCV_Score <- ifelse(
  !is.na(us_master$Heat_Percentile) &
    !is.na(us_master$Clinical_Susceptibility) &
    !is.na(us_master$SVI),
  (
    us_master$Heat_Percentile +
      us_master$Clinical_Susceptibility +
      us_master$SVI
  ) / 3 * 100,
  NA_real_
)

us_master$HCV_Percentile <- percent_rank(us_master$HCV_Score)
us_master$Clinical_Percentile <- percent_rank(us_master$Clinical_Score)
us_master$Mortality_Percentile <- percent_rank(us_master$Heart_Mortality)

us_master <- us_master |>
  mutate(
    HCV_Category = percentile_category(HCV_Percentile),
    Heat_Category = percentile_category(Heat_Percentile),
    Clinical_Category = percentile_category(Clinical_Percentile),
    SVI_Category = percentile_category(SVI),
    Mortality_Category = percentile_category(Mortality_Percentile)
  ) |>
  mutate(
    US_Rank = min_rank(desc(HCV_Score))
  ) |>
  group_by(State_Display) |>
  mutate(
    State_Rank = min_rank(desc(HCV_Score))
  ) |>
  ungroup()

mortality_values <- us_master$Heart_Mortality

mortality_limits <- quantile(
  mortality_values,
  probs = c(0.02, 0.98),
  na.rm = TRUE
)

mortality_visual <- pmin(
  pmax(mortality_values, mortality_limits[1]),
  mortality_limits[2]
)

us_master$Mortality_3D_Height <- ifelse(
  is.na(mortality_values),
  1000,
  rescale(
    mortality_visual,
    to = c(4000, 24000),
    from = mortality_limits
  )
)

web_counties <- us_master |>
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
    HCV_Score,
    HCV_Category,
    Heat_Category,
    Clinical_Category,
    SVI_Category,
    Mortality_Category,
    US_Rank,
    State_Rank,
    Mortality_3D_Height,
    geometry
  ) |>
  st_make_valid() |>
  st_transform(5070) |>
  st_simplify(
    dTolerance = 150,
    preserveTopology = TRUE
  ) |>
  st_transform(4326)

web_states <- web_counties |>
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
  summarise(.groups = "drop") |>
  st_make_valid()

counties_file <- file.path(data_dir, "counties.geojson")
states_file <- file.path(data_dir, "states.geojson")

if (file.exists(counties_file)) file.remove(counties_file)
if (file.exists(states_file)) file.remove(states_file)

st_write(
  web_counties,
  counties_file,
  driver = "GeoJSON",
  quiet = TRUE
)

st_write(
  web_states,
  states_file,
  driver = "GeoJSON",
  quiet = TRUE
)

cat(
  "\nSUCCESS\n",
  "Created:\n",
  counties_file,
  "\n",
  states_file,
  "\n",
  sep = ""
)