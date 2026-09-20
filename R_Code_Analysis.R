# Code developed by Peter Wasswa @ Duke University.
#These codes do the analysis used in this paper
###############################################################################
library(readr)
library(dplyr)
library(ggplot2)
library(sf)
library(ggrepel)
library(scales)
library(viridis)

###############################################################################
# 1. File paths
###############################################################################

csv_file <- "C:/Users/pw174/Documents/Systematic_Paper/Selected_WaDorule_Dams.csv"

climate_file <- "C:/Users/pw174/Documents/Dam_Information/shapefile/CONUS_USA_Climezones.json"

nca_file <- "C:/Users/pw174/Documents/NWS River Basins/nca_7_regions.shp"

output_png <- "C:/Users/pw174/Documents/Systematic_Paper/selected_CONUS_reservoirs_map.png"

output_pdf <- "C:/Users/pw174/Documents/Systematic_Paper/selected_CONUS_reservoirs_map.pdf"

# Select the map background:
# "climate" = climate zones
#"nca"     = NCA-7 regions
#background_type <- "climate"
background_type <- "nca"
###############################################################################
# 2. Read reservoir data
###############################################################################

dams <- read_csv(
  csv_file,
  show_col_types = FALSE,
  trim_ws = TRUE
) %>%
  rename(
    Dam = `Dam Name`,
    Operational_Target = `Operational Target`,
    Primary_Purpose = `Main Purpose`,
    Other_Uses = `Other Uses`,
    Storage_AF = `Storage (AF)`
  ) %>%
  mutate(
    Dam = as.character(Dam),
    State = as.character(State),
    Agency = as.character(Agency),
    Operational_Target = as.character(Operational_Target),
    Primary_Purpose = as.character(Primary_Purpose),
    Other_Uses = as.character(Other_Uses),

    Latitude = parse_number(as.character(Latitude)),
    Longitude = parse_number(as.character(Longitude)),
    Storage_AF = parse_number(as.character(Storage_AF))
  ) %>%
  filter(
    !is.na(Dam),
    !is.na(Latitude),
    !is.na(Longitude),
    !is.na(Storage_AF),
    Latitude >= 24,
    Latitude <= 50,
    Longitude >= -125,
    Longitude <= -66
  )

###############################################################################
# 3. Create label text
###############################################################################

# Include reservoir name and other uses in the map labels
dams <- dams %>%
  mutate(
    Map_Label = if_else(
      is.na(Other_Uses) | Other_Uses == "",
      Dam,
      paste0(Dam, "\n", Other_Uses)
    )
  )

###############################################################################
# 4. Read climate zones or NCA-7 regions
###############################################################################

if (background_type == "nca") {

  background <- st_read(
    climate_file,
    quiet = TRUE
  )

  background_name <- "NCA-7 regions"

} else if (background_type == "climate") {

  background <- st_read(
    nca_file,
    quiet = TRUE
  )

  background_name <- "CONUS climate zones"

} else {
  stop("background_type must be either 'nca' or 'climate'.")
}

# Check that the spatial file has a CRS
if (is.na(st_crs(background))) {
  stop(
    "The selected background file has no coordinate reference system. ",
    "Assign its CRS before proceeding."
  )
}

# Convert the background to geographic coordinates, crop CONUS, and project
background <- background %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  st_crop(
    xmin = -125,
    xmax = -66,
    ymin = 24,
    ymax = 50
  ) %>%
  st_transform(5070)     # NAD83 / Conus Albers Equal Area

###############################################################################
# 5. Convert reservoir coordinates to spatial features
###############################################################################

dams_sf <- st_as_sf(
  dams,
  coords = c("Longitude", "Latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(5070)

# Coordinates for ggrepel labels
dam_coords <- dams_sf %>%
  cbind(st_coordinates(.)) %>%
  st_drop_geometry()

###############################################################################
# 6. Define visual categories
###############################################################################

# Primary purpose is represented by point shape
purpose_levels <- unique(dams$Primary_Purpose)

shape_values <- setNames(
  rep(21, length(purpose_levels)),
  purpose_levels
)

shape_values[
  grepl("hydro", names(shape_values), ignore.case = TRUE)
] <- 24

shape_values[
  grepl("flood", names(shape_values), ignore.case = TRUE)
] <- 21

# Operational target is represented by outline color
target_levels <- unique(dams$Operational_Target)

target_colors <- c(
  "Seasonal" = "#D55E00",
  "Uniform"  = "#0072B2",
  "Dynamic"  = "#009E73",
  "Variable" = "#CC79A7"
)

# Retain only colors corresponding to categories present in the data
target_colors <- target_colors[
  names(target_colors) %in% target_levels
]

# Add fallback colors for any unexpected categories
missing_targets <- setdiff(target_levels, names(target_colors))

if (length(missing_targets) > 0) {
  extra_colors <- setNames(
    hcl.colors(length(missing_targets), palette = "Dark 3"),
    missing_targets
  )
  target_colors <- c(target_colors, extra_colors)
}

###############################################################################
# 7. Create one map
###############################################################################

p <- ggplot() +

  # Climate-zone or NCA-7 background
  geom_sf(
    data = background,
    fill = "grey96",
    color = "grey55",
    linewidth = 0.35
  ) +

  # Reservoir symbols
  #
  # Fill       = managing agency
  # Size       = storage capacity
  # Shape      = primary purpose
  # Colour     = operational target
  geom_sf(
    data = dams_sf,
    aes(
      fill = Agency,
      size = Storage_AF,
      shape = Primary_Purpose,
      color = Operational_Target
    ),
    alpha = 0.90,
    stroke = 1.1
  ) +

  # Reservoir labels
  geom_label_repel(
    data = dam_coords,
    aes(
      x = X,
      y = Y,
      label = Map_Label
    ),
    size = 2.35,
    lineheight = 0.85,
    label.size = 0.15,
    label.padding = unit(0.10, "lines"),
    fill = alpha("white", 0.82),
    color = "black",
    box.padding = 0.35,
    point.padding = 0.20,
    min.segment.length = 0,
    max.overlaps = Inf,
    seed = 123,
    show.legend = FALSE
  ) +

  # Storage capacity
  scale_size_area(
    name = "Storage capacity (AF)",
    max_size = 10,
    breaks = c(3e6, 5e6, 1e7, 2e7, 3e7),
    labels = label_number(
      scale = 1e-6,
      suffix = " M",
      accuracy = 1
    )
  ) +

  # Managing agency
  scale_fill_viridis_d(
    name = "Managing agency",
    option = "D",
    end = 0.90,
    na.value = "grey60"
  ) +

  # Primary purpose
  scale_shape_manual(
    name = "Primary purpose",
    values = shape_values,
    na.value = 21
  ) +

  # Operational target
  scale_color_manual(
    name = "Operational target",
    values = target_colors,
    na.value = "black"
  ) +

  coord_sf(
    crs = 5070,
    expand = FALSE
  ) +

  guides(
    fill = guide_legend(
      order = 1,
      override.aes = list(shape = 21, size = 5, color = "black")
    ),
    color = guide_legend(
      order = 2,
      override.aes = list(shape = 21, size = 5, fill = "white")
    ),
    shape = guide_legend(
      order = 3,
      override.aes = list(size = 5, color = "black")
    ),
    size = guide_legend(
      order = 4
    )
  ) +

  theme_minimal(base_size = 10) +

  theme(
    panel.grid.major = element_line(
      color = "grey86",
      linewidth = 0.25
    ),
    panel.grid.minor = element_blank(),

    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 8),

    plot.title = element_text(
      face = "bold",
      size = 13
    ),

    plot.subtitle = element_text(
      size = 9
    ),

    plot.caption = element_text(
      hjust = 0,
      size = 8,
      color = "grey30"
    ),

    axis.text = element_text(size = 8)
  )

###############################################################################
# 8. Display the map
###############################################################################

print(p)

###############################################################################
# 9. Save the map
###############################################################################

ggsave(
  filename = output_png,
  plot = p,
  width = 11,
  height = 8,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = output_pdf,
  plot = p,
  width = 11,
  height = 8,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

cat("PNG map saved to:\n", output_png, "\n")
cat("PDF map saved to:\n", output_pdf, "\n")
#################################################################################

#############################################################################
library(tidyverse)
library(scales)

# ---------------------------------------------------------
# 1. Read the CSV file
# ---------------------------------------------------------

reservoir_data <- read_csv(
  "C:/Users/pw174/Documents/Dam_Information/AGU_Data/New_para_Data/Selected_4_Dams/Reserviors_25.csv",
  show_col_types = FALSE
)

# ---------------------------------------------------------
# 2. Check that there are 366 daily observations
# ---------------------------------------------------------

if (nrow(reservoir_data) != 366) {
  stop(
    paste0(
      "Expected 366 rows, but found ",
      nrow(reservoir_data),
      " rows."
    )
  )
}

# ---------------------------------------------------------
# 3. Add day of year and convert values to numeric
# ---------------------------------------------------------

reservoir_data <- reservoir_data %>%
  mutate(
    Day = seq_len(n())
  ) %>%
  relocate(Day) %>%
  mutate(
    across(
      .cols = -Day,
      .fns = ~ as.numeric(.x)
    )
  )

# ---------------------------------------------------------
# 4. Convert from wide to long format
# ---------------------------------------------------------

long_data <- reservoir_data %>%
  pivot_longer(
    cols = -Day,
    names_to = "Reservoir",
    values_to = "Release"
  ) %>%
  mutate(
    Reservoir = str_replace_all(Reservoir, "_", " "),
    Reservoir = str_squish(Reservoir),
    Reservoir = factor(Reservoir, levels = unique(Reservoir))
  )

# ---------------------------------------------------------
# 5. Define x-axis breaks
# ---------------------------------------------------------

day_breaks <- c(1, 31, 61, 91, 121, 151,
                181, 211, 241, 271, 301, 331, 366)

# ---------------------------------------------------------
# 6. Create publication-style plot
# ---------------------------------------------------------

reservoir_plot <- ggplot(
  long_data,
  aes(
    x = Day,
    y = Release,
    group = Reservoir
  )
) +
  geom_step(
    direction = "mid",
    color = "#1F4EAA",
    linewidth = 1,
    na.rm = TRUE
  ) +
  facet_wrap(
    ~ Reservoir,
    ncol = 5,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = day_breaks,
    labels = day_breaks,
    limits = c(1, 366),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = label_number(
      accuracy = 1,
      big.mark = ","
    ),
    expand = expansion(mult = c(0.04, 0.08))
  ) +
  labs(
    x = "Day of Year",
    y = expression("Derived Release Target (" * m^3 * s^{-1} * ")")
  ) +
  theme_minimal(
    base_size = 12,
    base_family = "sans"
  ) +
  theme(
    # Facet headers
    strip.text = element_text(
      face = "bold",
      size = 12,
      color = "black"
    ),
    strip.background = element_rect(
      fill = "grey92",
      color = "black",
      linewidth = 0.4
    ),
    
    # Axes
    axis.title.x = element_text(
      face = "bold",
      size = 12,
      margin = margin(t = 8)
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 12,
      margin = margin(r = 8)
    ),
    axis.text = element_text(
      color = "black",
      size = 8
    ),
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.35
    ),
    
    # Gridlines
    panel.grid.major.x = element_line(
      color = "grey85",
      linewidth = 0.35
    ),
    panel.grid.major.y = element_line(
      color = "grey85",
      linewidth = 0.35
    ),
    panel.grid.minor = element_blank(),
    
    # Panel borders
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.45
    ),
    
    # Panel spacing
    panel.spacing = unit(0.75, "lines"),
    
    # Figure margins
    plot.margin = margin(
      t = 8,
      r = 8,
      b = 8,
      l = 8
    )
  )

# Display the plot
print(reservoir_plot)
#####################################################################################################
# ============================================================
# Four-reservoir analysis:
#
# Mean monthly inflow volume
# Average monthly inflow flow
# Monthly target storage
# Monthly release policy
# Observed daily outflows
# CONUS/NCA reservoir-location map
#
# Reservoirs:
#   Hoover
#   Garrison
#   John Kerr
#   Sam Rayburn
#
# ------------------------------------------------------------
# Input inflow columns:
#   Date
#   Inflows_m/s^3
#
# Input outflow columns:
#   date
#   outflow
#
# Input target columns:
#   target_stor_final
#   target_rel_final
#
# ------------------------------------------------------------
# Important temporal treatment:
#
# Inflows:
#   Daily observations restricted to 1970–2020 for panel (a).
#
# Observed outflows:
#   Daily observations restricted to 1970–2020 for panel (d).
#   These are NOT treated as a 366-day seasonal series.
#
# Targets:
#   366-day seasonal target series.
#   Used to derive 12 monthly target values.
#
# ------------------------------------------------------------
# Units:
#   Inflow:           m3/s
#   Release policy:   m3/s
#   Target storage:   MCM
#   Observed outflow: m3/s
# ============================================================

# ============================================================
# Packages
# ============================================================

library(tidyverse)
library(lubridate)
library(sf)
library(patchwork)
library(ggrepel)
library(scales)

# ============================================================
# Directories
# ============================================================

data_dir <- paste0(
  "C:/Users/pw174/Documents/Dam_Information/",
  "AGU_Data/New_para_Data/Selected_4_Dams"
)

output_dir <- file.path(
  data_dir,
  "Journal_Figures"
)

dir.create(
  output_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

nca_file <- paste0(
  "C:/Users/pw174/Documents/NWS River Basins/",
  "nca_7_regions.shp"
)

# ============================================================
# Comparison period
# Panels (a) and (d) only
# ============================================================

comparison_start <- as.Date("1970-01-01")
comparison_end   <- as.Date("2020-12-31")

# ============================================================
# Reservoir information
# ============================================================

dam_info <- tibble(
  Dam_ID = c(
    "Hoover",
    "Garrison",
    "John_Kerr",
    "Sam_Rayburn"
  ),
  
  Dam_Label = c(
    "Hoover",
    "Garrison",
    "John Kerr",
    "Sam Rayburn"
  ),
  
  Inflow_File = c(
    "Hoover_inflows.csv",
    "Garrison_inflows.csv",
    "John_Kerr_inflows.csv",
    "Sam_Rayburn_inflows.csv"
  ),
  
  Outflow_File = c(
    "Hoover_outflow.csv",
    "Garrison_outflow.csv",
    "John_Kerr_outflow.csv",
    "Sam_Rayburn_outflow.csv"
  ),
  
  Target_File = c(
    "Hoover_targets.csv",
    "Garrison_targets.csv",
    "John_Kerr_targets.csv",
    "Sam_Rayburn_targets.csv"
  ),
  
  Longitude = c(
    -114.7370,
    -101.4320,
    -78.2954,
    -94.107917
  ),
  
  Latitude = c(
    36.01630,
    47.50613,
    36.59625,
    31.06625
  )
)

# ============================================================
# Date parser
# ============================================================

parse_dates <- function(x) {
  
  as.Date(
    parse_date_time(
      as.character(x),
      orders = c(
        "mdy",
        "m/d/Y",
        "m/d/y",
        "ymd",
        "Y-m-d"
      ),
      quiet = TRUE
    )
  )
}

# ============================================================
# Read inflows
# ============================================================

read_inflows <- function(file_name) {
  
  file_path <- file.path(
    data_dir,
    file_name
  )
  
  if (!file.exists(file_path)) {
    stop(
      "Inflow file not found:\n",
      file_path
    )
  }
  
  x <- read_csv(
    file_path,
    show_col_types = FALSE,
    name_repair = "minimal"
  )
  
  required_columns <- c(
    "Date",
    "Inflows_m/s^3"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(x)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing inflow column(s) in ",
      basename(file_path),
      ": ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  }
  
  inflows <- x %>%
    transmute(
      Date = parse_dates(
        .data[["Date"]]
      ),
      Inflow = as.numeric(
        .data[["Inflows_m/s^3"]]
      )
    ) %>%
    filter(
      !is.na(Date),
      !is.na(Inflow)
    ) %>%
    arrange(Date)
  
  if (nrow(inflows) == 0) {
    stop(
      "No valid inflow records found in ",
      basename(file_path)
    )
  }
  
  inflows
}

# ============================================================
# Read observed outflows
# ============================================================

read_outflows <- function(file_name) {
  
  file_path <- file.path(
    data_dir,
    file_name
  )
  
  if (!file.exists(file_path)) {
    stop(
      "Outflow file not found:\n",
      file_path
    )
  }
  
  x <- read_csv(
    file_path,
    show_col_types = FALSE,
    name_repair = "minimal"
  )
  
  required_columns <- c(
    "date",
    "outflow"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(x)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing outflow column(s) in ",
      basename(file_path),
      ": ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  }
  
  outflows <- x %>%
    transmute(
      Date = parse_dates(
        .data[["date"]]
      ),
      Outflow = as.numeric(
        .data[["outflow"]]
      )
    ) %>%
    filter(
      !is.na(Date),
      !is.na(Outflow)
    ) %>%
    arrange(Date)
  
  if (nrow(outflows) == 0) {
    stop(
      "No valid outflow records found in ",
      basename(file_path)
    )
  }
  
  outflows
}

# ============================================================
# Read 366-day target series
# ============================================================

read_targets <- function(file_name) {
  
  file_path <- file.path(
    data_dir,
    file_name
  )
  
  if (!file.exists(file_path)) {
    stop(
      "Target file not found:\n",
      file_path
    )
  }
  
  x <- read_csv(
    file_path,
    show_col_types = FALSE,
    name_repair = "minimal"
  )
  
  required_columns <- c(
    "target_stor_final",
    "target_rel_final"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(x)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing target column(s) in ",
      basename(file_path),
      ": ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  }
  
  if (nrow(x) != 366) {
    stop(
      basename(file_path),
      " contains ",
      nrow(x),
      " rows. Exactly 366 rows are required."
    )
  }
  
  targets <- x %>%
    transmute(
      Day = seq_len(n()),
      
      Target_Storage = as.numeric(
        .data[["target_stor_final"]]
      ),
      
      Target_Release = as.numeric(
        .data[["target_rel_final"]]
      )
    ) %>%
    mutate(
      
      Reference_Date =
        as.Date("2000-01-01") +
        days(Day - 1),
      
      Month_Number =
        month(Reference_Date),
      
      Month = factor(
        month(
          Reference_Date,
          label = TRUE,
          abbr = TRUE
        ),
        levels = month.abb
      ),
      
      Target_X =
        Month_Number +
        (day(Reference_Date) - 0.5) /
        days_in_month(Reference_Date)
    )
  
  targets
}

# ============================================================
# Process reservoir
# ============================================================

process_dam <- function(
    dam_id,
    dam_label,
    inflow_file,
    outflow_file,
    target_file
) {
  
  # ----------------------------------------------------------
  # Read complete source records
  # ----------------------------------------------------------
  
  inflows_all <- read_inflows(
    inflow_file
  )
  
  outflows_all <- read_outflows(
    outflow_file
  )
  
  targets <- read_targets(
    target_file
  )
  
  # ----------------------------------------------------------
  # Restrict inflows to 1970–2020 for panel (a)
  # ----------------------------------------------------------
  
  inflows <- inflows_all %>%
    filter(
      Date >= comparison_start,
      Date <= comparison_end
    )
  
  # ----------------------------------------------------------
  # Restrict observed releases to 1970–2020 for panel (d)
  # ----------------------------------------------------------
  
  outflows <- outflows_all %>%
    filter(
      Date >= comparison_start,
      Date <= comparison_end
    )
  
  # ----------------------------------------------------------
  # Check that both comparison datasets contain observations
  # ----------------------------------------------------------
  
  if (nrow(inflows) == 0) {
    stop(
      "No inflow records for ",
      dam_label,
      " within 1970–2020."
    )
  }
  
  if (nrow(outflows) == 0) {
    stop(
      "No observed release records for ",
      dam_label,
      " within 1970–2020."
    )
  }
  
  # ----------------------------------------------------------
  # Daily inflow data
  # ----------------------------------------------------------
  
  inflows_daily <- inflows %>%
    mutate(
      Year = year(Date),
      
      Month_Number =
        month(Date),
      
      Month = factor(
        month(
          Date,
          label = TRUE,
          abbr = TRUE
        ),
        levels = month.abb
      ),
      
      Month_Date =
        floor_date(
          Date,
          "month"
        )
    )
  
  # ----------------------------------------------------------
  # Daily observed release data
  # ----------------------------------------------------------
  
  outflows_daily <- outflows %>%
    mutate(
      Year = year(Date),
      
      Month_Number =
        month(Date),
      
      Month = factor(
        month(
          Date,
          label = TRUE,
          abbr = TRUE
        ),
        levels = month.abb
      ),
      
      Month_Date =
        floor_date(
          Date,
          "month"
        )
    )
  
  # ----------------------------------------------------------
  # Monthly inflow statistics
  # ----------------------------------------------------------
  
  monthly_inflows <- inflows_daily %>%
    group_by(
      Year,
      Month_Number,
      Month,
      Month_Date
    ) %>%
    summarise(
      
      Monthly_Total_MCM =
        sum(
          Inflow * 86400 / 1e6,
          na.rm = TRUE
        ),
      
      Monthly_Mean_Inflow =
        mean(
          Inflow,
          na.rm = TRUE
        ),
      
      Number_of_Days =
        sum(!is.na(Inflow)),
      
      .groups = "drop"
    ) %>%
    arrange(Month_Date)
  
  # ----------------------------------------------------------
  # Average monthly inflow statistics
  # ----------------------------------------------------------
  
  average_monthly_inflows <-
    monthly_inflows %>%
    
    group_by(
      Month_Number,
      Month
    ) %>%
    
    summarise(
      
      Mean_Monthly_Total_MCM =
        mean(
          Monthly_Total_MCM,
          na.rm = TRUE
        ),
      
      SD_Monthly_Total_MCM =
        sd(
          Monthly_Total_MCM,
          na.rm = TRUE
        ),
      
      Mean_Monthly_Flow =
        mean(
          Monthly_Mean_Inflow,
          na.rm = TRUE
        ),
      
      SD_Monthly_Flow =
        sd(
          Monthly_Mean_Inflow,
          na.rm = TRUE
        ),
      
      Number_of_Years =
        n(),
      
      .groups = "drop"
    ) %>%
    
    arrange(
      Month_Number
    )
  
  # ----------------------------------------------------------
  # Monthly storage target
  #
  # Target series remains unchanged and is not restricted
  # to 1970–2020.
  # ----------------------------------------------------------
  
  monthly_targets <- targets %>%
    
    group_by(
      Month_Number,
      Month
    ) %>%
    
    summarise(
      
      Target_Storage =
        mean(
          Target_Storage,
          na.rm = TRUE
        ),
      
      .groups = "drop"
    ) %>%
    
    arrange(
      Month_Number
    )
  
  # ----------------------------------------------------------
  # Return processed data
  # ----------------------------------------------------------
  
  list(
    
    Dam_ID =
      dam_id,
    
    Dam_Label =
      dam_label,
    
    Inflows =
      inflows,
    
    Monthly_Inflows =
      monthly_inflows,
    
    Average_Monthly_Inflows =
      average_monthly_inflows,
    
    Outflows =
      outflows_daily,
    
    Targets =
      targets,
    
    Monthly_Targets =
      monthly_targets,
    
    Inflow_Record_Start =
      min(
        inflows$Date,
        na.rm = TRUE
      ),
    
    Inflow_Record_End =
      max(
        inflows$Date,
        na.rm = TRUE
      ),
    
    Outflow_Record_Start =
      min(
        outflows$Date,
        na.rm = TRUE
      ),
    
    Outflow_Record_End =
      max(
        outflows$Date,
        na.rm = TRUE
      ),
    
    Inflow_N =
      nrow(inflows),
    
    Outflow_N =
      nrow(outflows)
  )
}

# ============================================================
# Process all reservoirs
# ============================================================

dam_results <- setNames(
  
  lapply(
    seq_len(
      nrow(dam_info)
    ),
    
    function(i) {
      
      process_dam(
        
        dam_id =
          dam_info$Dam_ID[i],
        
        dam_label =
          dam_info$Dam_Label[i],
        
        inflow_file =
          dam_info$Inflow_File[i],
        
        outflow_file =
          dam_info$Outflow_File[i],
        
        target_file =
          dam_info$Target_File[i]
      )
    }
  ),
  
  dam_info$Dam_ID
)

# ============================================================
# Read NCA shapefile
# ============================================================

if (!file.exists(nca_file)) {
  
  stop(
    "NCA shapefile not found:\n",
    nca_file
  )
}

nca_regions <- st_read(
  nca_file,
  quiet = TRUE
)

if (is.na(st_crs(nca_regions))) {
  
  stop(
    "The NCA shapefile has no CRS."
  )
}

nca_regions <- nca_regions %>%
  
  st_make_valid() %>%
  
  st_transform(4326)

conus <- st_crop(
  nca_regions,
  xmin = -125,
  xmax = -66,
  ymin = 24,
  ymax = 50
)

dam_points <- dam_info %>%
  
  st_as_sf(
    coords = c(
      "Longitude",
      "Latitude"
    ),
    crs = 4326,
    remove = FALSE
  )

# ============================================================
# CONUS map
# ============================================================

conus_map <- ggplot() +
  
  geom_sf(
    data = conus,
    fill = "grey94",
    color = "grey35",
    linewidth = 0.35
  ) +
  
  geom_sf(
    data = dam_points,
    shape = 21,
    fill = "#2166AC",
    color = "black",
    size = 3.5,
    stroke = 0.6
  ) +
  
  geom_text_repel(
    data = dam_info,
    
    aes(
      x = Longitude,
      y = Latitude,
      label = Dam_Label
    ),
    
    size = 3.1,
    fontface = "bold",
    color = "black",
    seed = 123,
    
    box.padding = 0.45,
    point.padding = 0.25,
    min.segment.length = 0,
    
    segment.color = "grey35",
    segment.size = 0.3
  ) +
  
  coord_sf(
    xlim = c(
      -125,
      -66
    ),
    
    ylim = c(
      24,
      50
    ),
    
    expand = FALSE
  ) +
  
  theme_void(
    base_size = 9
  ) +
  
  theme(
    
    panel.border =
      element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.5
      ),
    
    plot.margin =
      margin(
        5,
        5,
        5,
        5
      )
  )

# ============================================================
# Save CONUS map
# ============================================================

ggsave(
  
  filename = file.path(
    output_dir,
    "CONUS_four_dam_locations_NCA.tiff"
  ),
  
  plot = conus_map,
  
  width = 7,
  height = 4.5,
  
  units = "in",
  dpi = 600,
  
  compression = "lzw",
  
  bg = "white"
)

# ============================================================
# Journal theme
# ============================================================

journal_theme <- theme_minimal(
  
  base_size = 9,
  
  base_family = "sans"
  
) +
  
  theme(
    
    plot.title =
      element_text(
        face = "bold",
        size = 10,
        hjust = 0
      ),
    
    plot.subtitle =
      element_text(
        size = 8,
        color = "grey25",
        hjust = 0
      ),
    
    axis.title =
      element_text(
        face = "bold",
        size = 9
      ),
    
    axis.text =
      element_text(
        color = "black",
        size = 7.5
      ),
    
    axis.ticks =
      element_line(
        color = "black",
        linewidth = 0.3
      ),
    
    panel.grid.major =
      element_line(
        color = "grey85",
        linewidth = 0.3
      ),
    
    panel.grid.minor =
      element_blank(),
    
    panel.border =
      element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.45
      ),
    
    legend.position =
      "bottom",
    
    legend.title =
      element_blank(),
    
    legend.text =
      element_text(
        size = 7.5
      ),
    
    plot.margin =
      margin(
        4,
        6,
        4,
        6
      )
  )

# ============================================================
# Create four-panel reservoir figure
# ============================================================

create_dam_figure <- function(
    result,
    dam_id,
    dam_label
) {
  
  # ----------------------------------------------------------
  # Panel (a)
  # 1970–2020 inflows
  # ----------------------------------------------------------
  
  seasonal_inflows <-
    result$Average_Monthly_Inflows %>%
    
    arrange(
      Month_Number
    ) %>%
    
    mutate(
      Month_Number =
        as.numeric(
          Month_Number
        )
    )
  
  record_start <- format(
    result$Inflow_Record_Start,
    "%Y"
  )
  
  record_end <- format(
    result$Inflow_Record_End,
    "%Y"
  )
  
  total_values <-
    seasonal_inflows$
    Mean_Monthly_Total_MCM
  
  flow_values <-
    seasonal_inflows$
    Mean_Monthly_Flow
  
  total_range <- range(
    total_values,
    na.rm = TRUE
  )
  
  flow_range <- range(
    flow_values,
    na.rm = TRUE
  )
  
  total_span <-
    diff(total_range)
  
  flow_span <-
    diff(flow_range)
  
  if (
    !is.finite(total_span) ||
    total_span == 0
  ) {
    
    total_span <- 1
  }
  
  if (
    !is.finite(flow_span) ||
    flow_span == 0
  ) {
    
    flow_span <- 1
  }
  
  axis_multiplier <-
    total_span /
    flow_span
  
  axis_intercept <-
    total_range[1] -
    axis_multiplier *
    flow_range[1]
  
  seasonal_inflows <-
    seasonal_inflows %>%
    
    mutate(
      
      Flow_Primary_Scale =
        axis_intercept +
        axis_multiplier *
        Mean_Monthly_Flow
    )
  
  p_a <- ggplot(
    
    seasonal_inflows,
    
    aes(
      x = Month_Number
    )
    
  ) +
    
    geom_col(
      
      aes(
        y =
          Mean_Monthly_Total_MCM,
        
        fill =
          "Mean monthly inflow volume"
      ),
      
      width = 0.72,
      alpha = 0.62,
      
      color = "#2166AC",
      linewidth = 0.2
    ) +
    
    geom_line(
      
      aes(
        y =
          Flow_Primary_Scale,
        
        color =
          "Average monthly flow"
      ),
      
      linewidth = 1.0,
      
      lineend =
        "round"
    ) +
    
    geom_point(
      
      aes(
        y =
          Flow_Primary_Scale,
        
        color =
          "Average monthly flow"
      ),
      
      size = 1.7
    ) +
    
    scale_fill_manual(
      
      values = c(
        
        "Mean monthly inflow volume" =
          "#2166AC"
      )
    ) +
    
    scale_color_manual(
      
      values = c(
        
        "Average monthly flow" =
          "#009E73"
      )
    ) +
    
    scale_x_continuous(
      
      breaks = 1:12,
      
      labels =
        month.abb,
      
      limits =
        c(
          0.5,
          12.5
        ),
      
      expand =
        c(
          0,
          0
        )
    ) +
    
    scale_y_continuous(
      
      name =
        "Mean monthly inflow volume (MCM)",
      
      labels =
        label_number(
          accuracy = 1,
          big.mark = ","
        ),
      
      expand =
        expansion(
          mult =
            c(
              0.03,
              0.08
            )
        ),
      
      sec.axis =
        sec_axis(
          
          trans =
            ~ (
              . -
                axis_intercept
            ) /
            axis_multiplier,
          
          name =
            expression(
              "Average monthly flow (" *
                m^3 ~ s^{-1} *
                ")"
            ),
          
          labels =
            label_number(
              accuracy = 1,
              big.mark = ","
            )
        )
    ) +
    
    labs(
      
      title =
        "(a) Mean monthly inflow volume and flow",
      
      subtitle =
        "Inflow record: 1970–2020",
      
      x =
        "Month",
      
      fill = NULL,
      color = NULL
    ) +
    
    journal_theme +
    
    theme(
      
      axis.title.y.left =
        element_text(
          color = "#2166AC",
          face = "bold"
        ),
      
      axis.text.y.left =
        element_text(
          color = "#2166AC"
        ),
      
      axis.title.y.right =
        element_text(
          color = "#009E73",
          face = "bold"
        ),
      
      axis.text.y.right =
        element_text(
          color = "#009E73"
        )
    )
  
  # ----------------------------------------------------------
  # Panel (b)
  # Monthly storage target
  # ----------------------------------------------------------
  
  p_b <- ggplot(
    
    result$Monthly_Targets,
    
    aes(
      x = Month_Number,
      y = Target_Storage
    )
    
  ) +
    
    geom_line(
      
      color = "#762A83",
      
      linewidth = 1.0
    ) +
    
    geom_point(
      
      color = "#762A83",
      
      size = 1.5
    ) +
    
    scale_x_continuous(
      
      breaks = 1:12,
      
      labels =
        month.abb,
      
      limits =
        c(
          0.5,
          12.5
        ),
      
      expand =
        c(
          0,
          0
        )
    ) +
    
    scale_y_continuous(
      
      labels =
        label_number(
          accuracy = 0.1,
          big.mark = ","
        ),
      
      expand =
        expansion(
          mult =
            c(
              0.04,
              0.10
            )
        )
    ) +
    
    labs(
      
      title =
        "(b) Monthly storage target",
      
      x =
        "Month",
      
      y =
        "Storage target (MCM)"
    ) +
    
    journal_theme
  
  # ----------------------------------------------------------
  # Panel (c)
  # Daily release target
  # ----------------------------------------------------------
  
  p_c <- ggplot(
    
    result$Targets,
    
    aes(
      x = Reference_Date,
      y = Target_Release
    )
    
  ) +
    
    geom_line(
      
      color = "#D73027",
      
      linewidth = 0.8,
      
      na.rm = TRUE
    ) +
    
    scale_x_date(
      
      breaks =
        seq(
          as.Date("2000-01-01"),
          as.Date("2000-12-01"),
          by = "1 month"
        ),
      
      labels =
        month.abb,
      
      limits =
        c(
          as.Date("1999-12-20"),
          as.Date("2000-12-31")
        ),
      
      expand =
        c(
          0,
          0
        )
    ) +
    
    scale_y_continuous(
      
      labels =
        label_number(
          accuracy = 1,
          big.mark = ","
        ),
      
      expand =
        expansion(
          mult =
            c(
              0.04,
              0.10
            )
        )
    ) +
    
    labs(
      
      title =
        "(c) Daily release target",
      
      x =
        "Month",
      
      y =
        expression(
          "Release target (" *
            m^3 ~ s^{-1} *
            ")"
        )
    ) +
    
    journal_theme
  
  # ----------------------------------------------------------
  # Target profile for panel (d)
  # ----------------------------------------------------------
  
  target_release_plot <-
    result$Targets %>%
    
    filter(
      
      !is.na(
        Target_Release
      ),
      
      !is.na(
        Target_X
      )
    ) %>%
    
    arrange(
      Day
    )
  
  # ----------------------------------------------------------
  # Panel (d)
  # 1970–2020 observed releases
  # ----------------------------------------------------------
  
  outflow_start <- format(
    result$Outflow_Record_Start,
    "%Y"
  )
  
  outflow_end <- format(
    result$Outflow_Record_End,
    "%Y"
  )
  
  p_d <- ggplot(
    
    result$Outflows,
    
    aes(
      x = Month_Number,
      y = Outflow
    )
    
  ) +
    
    geom_boxplot(
      
      aes(
        group =
          Month_Number,
        
        fill =
          "Observed daily releases"
      ),
      
      width = 0.58,
      
      color = "grey25",
      
      linewidth = 0.45,
      
      outlier.size = 0.7,
      
      outlier.alpha = 0.40
    ) +
    
    geom_line(
      
      data =
        target_release_plot,
      
      aes(
        
        x =
          Target_X,
        
        y =
          Target_Release,
        
        color =
          "Daily release target"
      ),
      
      linewidth = 0.9,
      
      inherit.aes = FALSE
    ) +
    
    scale_fill_manual(
      
      name = NULL,
      
      values = c(
        
        "Observed daily releases" =
          "grey80"
      )
    ) +
    
    scale_color_manual(
      
      name = NULL,
      
      values = c(
        
        "Daily release target" =
          "#D73027"
      )
    ) +
    
    scale_x_continuous(
      
      breaks = 1:12,
      
      labels =
        month.abb,
      
      limits =
        c(
          0.5,
          12.5
        ),
      
      expand =
        c(
          0,
          0
        )
    ) +
    
    scale_y_continuous(
      
      labels =
        label_number(
          accuracy = 1,
          big.mark = ","
        ),
      
      expand =
        expansion(
          mult =
            c(
              0.04,
              0.10
            )
        )
    ) +
    
    labs(
      
      title =
        "(d) Observed releases and target profile",
      
      subtitle =
        paste0(
          "Observed record: 1970–2020",
          " | n = ",
          comma(
            result$Outflow_N
          )
        ),
      
      x =
        "Month",
      
      y =
        expression(
          "Release (" *
            m^3 ~ s^{-1} *
            ")"
        )
    ) +
    
    journal_theme +
    
    theme(
      
      legend.position =
        "bottom",
      
      legend.direction =
        "horizontal",
      
      legend.box =
        "horizontal",
      
      legend.margin =
        margin(
          t = 2,
          r = 0,
          b = 0,
          l = 0
        ),
      
      legend.text =
        element_text(
          size = 8
        ),
      
      legend.key =
        element_rect(
          fill = "white",
          color = NA
        )
    )
  
  # ----------------------------------------------------------
  # Highlight selected reservoir on map
  # ----------------------------------------------------------
  
  selected_dam <-
    dam_points %>%
    
    filter(
      Dam_ID == dam_id
    )
  
  map_for_dam <-
    conus_map +
    
    geom_sf(
      
      data =
        selected_dam,
      
      shape = 21,
      
      fill =
        "#D73027",
      
      color =
        "black",
      
      size =
        5,
      
      stroke =
        0.8
    )
  
  # ----------------------------------------------------------
  # Combine analytical panels
  # ----------------------------------------------------------
  
  analytical_panels <- (
    
    p_a | p_b
    
  ) / (
    
    p_c | p_d
    
  )
  
  # ----------------------------------------------------------
  # Final figure
  # ----------------------------------------------------------
  
  final_figure <-
    
    (
      
      map_for_dam |
        analytical_panels
      
    ) +
    
    plot_layout(
      
      widths =
        c(
          0.85,
          2.60
        )
    )
  
  final_figure
}

# ============================================================
# Generate figures for all four reservoirs
# ============================================================

dam_figures <- setNames(
  
  lapply(
    
    seq_len(
      nrow(dam_info)
    ),
    
    function(i) {
      
      dam_id <-
        dam_info$Dam_ID[i]
      
      dam_label <-
        dam_info$Dam_Label[i]
      
      figure <-
        create_dam_figure(
          
          result =
            dam_results[[dam_id]],
          
          dam_id =
            dam_id,
          
          dam_label =
            dam_label
        )
      
      file_stub <-
        paste0(
          dam_id,
          "_inflows_storage_release_outflows"
        )
      
      # ------------------------------------------------------
      # TIFF
      # ------------------------------------------------------
      
      ggsave(
        
        filename =
          file.path(
            output_dir,
            paste0(
              file_stub,
              ".tiff"
            )
          ),
        
        plot =
          figure,
        
        width =
          12,
        
        height =
          8,
        
        units =
          "in",
        
        dpi =
          600,
        
        compression =
          "lzw",
        
        bg =
          "white"
      )
      
      # ------------------------------------------------------
      # PDF
      # ------------------------------------------------------
      
      ggsave(
        
        filename =
          file.path(
            output_dir,
            paste0(
              file_stub,
              ".pdf"
            )
          ),
        
        plot =
          figure,
        
        width =
          12,
        
        height =
          8,
        
        units =
          "in",
        
        device =
          cairo_pdf,
        
        bg =
          "white"
      )
      
      figure
    }
  ),
  
  dam_info$Dam_ID
)

# ============================================================
# Combined PDF containing all four reservoirs
# ============================================================

all_dams_pdf <- file.path(
  
  output_dir,
  
  "All_four_dams_inflows_storage_release_outflows.pdf"
)

pdf(
  
  file =
    all_dams_pdf,
  
  width =
    12,
  
  height =
    8,
  
  onefile =
    TRUE
)

for (
  figure in dam_figures
) {
  
  print(figure)
}

dev.off()

# ============================================================
# Final message
# ============================================================

message(
  "Figures saved to: ",
  output_dir
)

message(
  "Panels (a) and (d) use daily records from 1970–2020."
)

message(
  "Panels (b) and (c) retain the 366-day WaDorule target series."
)
