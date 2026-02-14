# Declare global variables for R CMD check
utils::globalVariables(c(".data", "Date", "Month", "Qm", "Release"))

# Import base functions to avoid "no visible function definition" notes
#' @importFrom utils read.csv
#' @importFrom stats setNames
NULL

#' Get aggregated upstream inflows
#'
#' Downloads daily discharge from one or more upstream USGS gauges and
#' aggregates them into a single upstream inflow time series.
#'
#' @param sites Character vector of USGS site numbers (one or more)
#' @param start_date Start date (YYYY-MM-DD)
#' @param end_date End date (YYYY-MM-DD)
#' @param units Output units ("m3s" or "cfs")
#' @param na_rm Logical; remove NA values when aggregating
#'
#' @details
#' Daily upstream inflows are computed as the sum of all available upstream
#' tributary discharges, obtained from USGS daily-value gauges. When multiple
#' tributaries exist, inflows are aggregated by date to form a single composite
#' upstream inflow time series. If only one gauge is provided, the inflow equals
#' the discharge at that gauge.
#'
#' @return Data frame with columns Date and Q_upstream
#' @export
#' @examples
#' elev_file <- system.file("extdata", "Falls_Lake_Dam_Elevation.csv", package = "WaDorule")
#' storage_df <- wad_compute_target_storage(
#'   elev_df = read.csv(elev_file),
#'   min_storage_acft = 50000,
#'   max_storage_acft = 200000
#' )

wad_get_upstream_inflows <- function(
    sites,
    start_date,
    end_date,
    units = "m3s",
    na_rm = TRUE
) {

  stopifnot(is.character(sites), length(sites) >= 1)

  inflows <- lapply(sites, function(site) {

    df <- dataRetrieval::readNWISdv(
      siteNumbers = site,
      parameterCd = "00060",
      startDate   = start_date,
      endDate     = end_date
    )

    # Keep only numeric discharge column (drop *_cd qualifier columns)
    qcol <- grep("^X_[0-9]+_[0-9]+$", names(df), value = TRUE)

    if (length(qcol) != 1) {
      stop("Could not uniquely identify discharge column for site ", site)
    }

    df <- df[, c("Date", qcol)]
    names(df)[2] <- site
    df
  })

  # Merge all tributaries by Date
  merged <- Reduce(
    function(x, y) merge(x, y, by = "Date", all = TRUE),
    inflows
  )

  # Force numeric (safety)
  merged[-1] <- lapply(merged[-1], as.numeric)

  # Unit conversion
  if (units == "m3s") {
    merged[-1] <- merged[-1] * 0.0283168
  }

  # Sum all tributaries → single upstream inflow
  merged$Q_upstream <- rowSums(merged[-1], na.rm = na_rm)

  merged[, c("Date", "Q_upstream")]
}
