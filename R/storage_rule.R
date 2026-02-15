#' Compute target storage from elevation rule curve or daily CSV
#'
#' Converts elevation (ft) to storage (MCM). Accepts user daily CSV (365/366 rows)
#' or package default elevation curve.
#'
#' @param elev_df Data frame with column `Target_Elev` (optional)
#' @param min_storage_acft Minimum conservation storage (ac-ft)
#' @param max_storage_acft Maximum conservation storage (ac-ft)
#' @param elev_file Optional path to user elevation CSV (overrides default)
#' @return Data frame with columns:
#'   - Target_Elev (ft)
#'   - target_storage_MCM (MCM)
#' @export
wad_compute_target_storage <- function(
    elev_df = NULL,
    min_storage_acft,
    max_storage_acft,
    elev_file = NULL
) {

  ## ---- 1️⃣ Load CSV if provided ----
  if (!is.null(elev_file)) {
    if (!file.exists(elev_file)) stop("User CSV file not found: ", elev_file)
    elev_df <- utils::read.csv(elev_file)
  }

  ## ---- 2️⃣ Use package default CSV if elev_df still NULL ----
  if (is.null(elev_df)) {
    default_csv <- system.file(
      "extdata",
      "Falls_Lake_Dam_Elevation.csv",
      package = "WaDorule"
    )
    if (default_csv == "") stop("Default elevation CSV not found in package")
    elev_df <- utils::read.csv(default_csv)
  }

  ## ---- 3️⃣ Sanity checks ----
  stopifnot("Target_Elev" %in% names(elev_df))
  if (!is.numeric(elev_df$Target_Elev)) elev_df$Target_Elev <- as.numeric(elev_df$Target_Elev)

  n_rows <- nrow(elev_df)

  ## ---- 4️⃣ Map elevation → storage ----
  if (n_rows %in% c(365, 366)) {
    # User-supplied daily CSV: map min/max storage to min/max Target_Elev
    min_elev <- min(elev_df$Target_Elev, na.rm = TRUE)
    max_elev <- max(elev_df$Target_Elev, na.rm = TRUE)
    if (min_elev == max_elev) stop("Min and max elevations are equal; cannot map to storage.")

    stor <- (elev_df$Target_Elev - min_elev) /
      (max_elev - min_elev) *
      (max_storage_acft - min_storage_acft) +
      min_storage_acft

  } else {
    # Standard curve: assume sorted Target_Elev for linear mapping
    min_elev <- min(elev_df$Target_Elev, na.rm = TRUE)
    max_elev <- max(elev_df$Target_Elev, na.rm = TRUE)
    if (min_elev == max_elev) stop("Min and max elevations are equal; cannot map to storage.")

    stor <- (elev_df$Target_Elev - min_elev) /
      (max_elev - min_elev) *
      (max_storage_acft - min_storage_acft) +
      min_storage_acft
  }

  ## ---- 5️⃣ Convert ac-ft → MCM ----
  elev_df$target_storage_MCM <- stor * 0.00123348

  ## ---- 6️⃣ Return only relevant columns ----
  elev_df[, c("Target_Elev", "target_storage_MCM")]
}
