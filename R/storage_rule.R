
# Import base functions to avoid "no visible function definition" notes
#' @importFrom utils read.csv
#' @importFrom stats setNames
NULL

#' Compute target storage from elevation rule curve
#'
#' @param elev_df Data frame with Target_Elev (ft), optional
#' @param min_storage_acft Minimum conservation storage (ac-ft)
#' @param max_storage_acft Maximum conservation storage (ac-ft)
#' @param elev_file Optional path to user elevation CSV (overrides default)
#' @return Data frame with target storage (MCM)
#' @export
wad_compute_target_storage <- function(
    elev_df = NULL,
    min_storage_acft,
    max_storage_acft,
    elev_file = NULL
) {

  # 1️⃣ User CSV overrides everything
  if (!is.null(elev_file)) {
    if (!file.exists(elev_file)) stop("User CSV file not found: ", elev_file)
    elev_df <- read.csv(elev_file)
  }

  # 2️⃣ If elev_df is still NULL, use package default CSV
  if (is.null(elev_df)) {
    default_csv <- system.file(
      "extdata",
      "Falls_Lake_Dam_Elevation.csv",
      package = "WaDorule"
    )
    if (default_csv == "") stop("Default elevation CSV not found in package")
    elev_df <- read.csv(default_csv)
  }

  # 3️⃣ Sanity check
  stopifnot("Target_Elev" %in% names(elev_df))

  # Linear mapping elevation → storage
  min_elev <- min(elev_df$Target_Elev, na.rm = TRUE)
  max_elev <- max(elev_df$Target_Elev, na.rm = TRUE)

  stor <- (elev_df$Target_Elev - min_elev) /
    (max_elev - min_elev) *
    (max_storage_acft - min_storage_acft) +
    min_storage_acft

  elev_df$target_storage_MCM <- stor * 0.00123348

  return(elev_df)
}
