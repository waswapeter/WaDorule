# ==============================================================================
# WaDorule Package - User Guide
# Developed by Peter Wasswa-Duke University 14-02-2026
# Contact:peter.wasswa@duke.edu/waswapeter4@gmail.com
# ==============================================================================

# ------------------------------------------------------------------------------
# 1 Get WaDorule package
# ------------------------------------------------------------------------------

remotes::install_github("waswapeter/WaDorule") #if first time, run this code once; THIS IS IMPORTANT!
library(WaDorule) # then always run this code whevener you want to use WaDorule ofcourse, after installation

# ------------------------------------------------------------------------------
# 2 Upstream inflows Metadata information
# ------------------------------------------------------------------------------

# User can provide one or more USGS site numbers
#----# https://dashboard.waterdata.usgs.gov/app/nwd/en/
# -->Can use this link for USGS site codes, but this depends on user interest for a gauge directly upstream of the reservoir
# -->For reservoirs lacking direct upstream gauges, inflows from the nearest hydrologically comparable upstream stations can be used basing on drainage area similarity, upstream position, and climatic consistency.
# -->Also list of parameters particularly for discharge can be found here; https://waterdata.usgs.gov/reference-list/op/parameter-code/?namesOnly=true&dataOnly=true
#==================================================================================================================================================================================================================
sites <- c("02085500","02085000")   # multiple gauges; these are upstream gauges of Falls Lake Dam; #Flat River@ 02085500; #Eno River @ 02085000

#sites <- c("02085500") # single site

# ------------------------------------------------------------------------------
# ✅ Safety checks on gauges
# ------------------------------------------------------------------------------

stopifnot(
  exists("sites"),
  is.character(sites),
  length(sites) >= 1
)

message("Number of upstream gauges provided: ", length(sites))

start_date <- "1930-01-01" # note, if your option was multiple, all gauges should have the same start and end date
end_date   <- "2025-12-31"

stopifnot(as.Date(start_date) <= as.Date(end_date))

# ------------------------------------------------------------------------------
# 3 Download upstream inflows
# ------------------------------------------------------------------------------

upstream_inflows <- wad_get_upstream_inflows(
  sites = sites,
  start_date = start_date,
  end_date = end_date,
  units = "m3s"   # "m3s" or "cfs"
)

# ------------------------------------------------------------------------------
# ✅ Inform user what was done
# ------------------------------------------------------------------------------

if (length(sites) == 1) {
  message("Using inflow from single upstream gauge: ", sites)
} else {
  message(
    "Aggregated upstream inflows from ",
    length(sites),
    " gauges: ",
    paste(sites, collapse = ", ")
  )
}

head(upstream_inflows)

# ------------------------------------------------------------------------------
# 4 Compute seasonal inflow weights
# ------------------------------------------------------------------------------

seasonal_weights <- wad_seasonal_weights(
  inflow_df = upstream_inflows,
  inflow_col = "Q_upstream"
)

seasonal_weights  # named vector of monthly weights
#NB: Seasonal weights are normalized to unit mean rather than unit sum, ensuring that they act as multiplicative scalars on release magnitude without altering the annual mean release level.

# ------------------------------------------------------------------------------
# 5 Compute target storage
# ------------------------------------------------------------------------------

# Option 1: Use package default elevation CSV, but this is for Falls Lake Dam and it is in meters
storage_df <- wad_compute_target_storage(
  min_storage_acft = 25077, #this is for Falls Lake Dam
  max_storage_acft = 131395  #this is for Falls Lake Dam
)

# Option 2: Use a user-provided CSV (365 or 366 rows); please use this option
user_csv <- "C:/Users/pw174/Documents/Dam_Information/CONUS_DAMS/Master_file/Falls_Lake_Dam_Elevation.csv" # let this be your elevation target csv in meters

if (file.exists(user_csv)) {
  storage_df <- wad_compute_target_storage(
    min_storage_acft = 25077,
    max_storage_acft = 131395,
    elev_file = user_csv
  )
}

# ------------------------------------------------------------------------------
# ✅ Ensure Date column exists
# ------------------------------------------------------------------------------

if (!"Date" %in% names(storage_df)) {
  storage_df$Date <- seq.Date(
    from = as.Date("2020-01-01"),  # arbitrary year to set up the release policy profile
    by = "day",
    length.out = nrow(storage_df)
  )
}

head(storage_df)

# ------------------------------------------------------------------------------
# ✅ Rename storage column for wad_release_policy
# ------------------------------------------------------------------------------

# wad_release_policy expects column named "Storage"
release_input <- storage_df[, c("Date", "target_storage_MCM")]
names(release_input)[2] <- "Storage"

# ------------------------------------------------------------------------------
# 6 Compute daily release policy
# ------------------------------------------------------------------------------

release_df <- wad_release_policy(
  df = release_input,
  seasonal_weights = seasonal_weights,
  Smin = 30.9 * max(storage_df$target_storage_MCM),
  Smax = 162.0 * max(storage_df$target_storage_MCM),
  Qmin = 2.24,       # m^3/s
  Qmax = 8.5,        # m^3/s
  rule = "linear",   # "linear" or "logistic"
  plot = TRUE        # set FALSE to skip plotting
)

head(release_df)

# ------------------------------------------------------------------------------
# 7 Save CSV of Storage, Elevation, and Release Policy
# ------------------------------------------------------------------------------

export_df <- data.frame(
  Day = 1:nrow(release_df),                           # 1–365 or 1–366
  Storage_rule = release_input$Storage,               # storage rule (MCM)
  Elevation_rule = storage_df$Target_Elev,            # elevation rule (m)
  Release_policy = release_df$Release                 # computed release policy (m^3/s)
)

# ------------------------------------------------------------------------------
# 8 save to CSV to your preferred storage area for future use
# ------------------------------------------------------------------------------

csv_path <- "C:/Users/pw174/Documents/Dam_Information/CONUS_DAMS/Master_file/WaDorule_Output.csv" 
write.csv(export_df, csv_path, row.names = FALSE)

message("CSV file saved at: ", csv_path)

# ------------------------------------------------------------------------------
# 9 Notes for Users
# ------------------------------------------------------------------------------

# - This package is currently customized to USGS gauges only but further scaling to global gauges is in pipeline.
# - One or more upstream gauges are supported.
# - If multiple gauges are provided, inflows are summed by date.
# - If only one gauge is provided, its inflow is used directly.
# - User elevation CSV can have 365 or 366 rows; function handles both.
# - User is advised to use right and quality checked data.
# - Seasonal weights are normalized monthly fractions (mean = 1).
# - Release plot is optional; set plot = FALSE to skip plotting.
# - The saved CSV includes Storage_rule, Elevation_rule, and Release_policy columns.

#============================END-2026-==========================================
#===============================================================================
