# Declare global variables for R CMD check
utils::globalVariables(c("Date"))

#' Get aggregated upstream inflows from USGS gauges (daily)
#'
#' Downloads daily discharge data from one or more upstream USGS gauges and
#' aggregates them into a single daily upstream inflow time series over a
#' user-defined period.
#'
#' @param sites Character vector of USGS site numbers (e.g., "02087183").
#' @param start_date Start date (YYYY-MM-DD)
#' @param end_date End date (YYYY-MM-DD)
#' @param units Output units. Either "m3s" (default) or "cfs"
#' @param na_rm Logical; if TRUE (default), missing values are ignored when
#'   summing multiple tributaries
#'
#' @return A data frame with columns:
#'   - Date: Date
#'   - Q_upstream: Daily upstream inflow in requested units
#'
#' @export
#'
#' @examples
#' \dontrun{
#' qin <- wad_get_upstream_inflows(
#'   sites = c("02087183", "02087275"),
#'   start_date = "1990-01-01",
#'   end_date   = "2020-12-31",
#'   units = "m3s"
#' )
#' }

wad_get_upstream_inflows <- function(
    sites,
    start_date,
    end_date,
    units = c("m3s", "cfs"),
    na_rm = TRUE
) {
  ## ---- Input validation ----
  stopifnot(
    is.character(sites),
    length(sites) >= 1,
    is.character(start_date),
    is.character(end_date)
  )

  units <- match.arg(units)
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)

  if (is.na(start_date) || is.na(end_date)) stop("Invalid dates.")
  if (start_date > end_date) stop("start_date must be <= end_date.")

  ## ---- Download inflows for each site ----
  inflows <- lapply(sites, function(site) {

    # Use read_waterdata_daily if exported, else fallback to readNWISdv
    if ("read_waterdata_daily" %in% getNamespaceExports("dataRetrieval")) {
      read_func <- getExportedValue("dataRetrieval", "read_waterdata_daily")
      df <- read_func(
        siteNumbers = site,
        startDate   = start_date,
        endDate     = end_date,
        parameterCd = "00060"
      )
    } else {
      df <- dataRetrieval::readNWISdv(
        siteNumbers = site,
        parameterCd = "00060",
        startDate   = start_date,
        endDate     = end_date
      )
    }

    if (nrow(df) == 0) stop("No data returned for site ", site)

    # Identify discharge column: X_###_### or Flow
    qcol <- grep("^X_[0-9]+_[0-9]+$|^Flow$", names(df), value = TRUE)
    if (length(qcol) != 1) stop("Cannot identify discharge column for site ", site)

    out <- df[, c("Date", qcol)]
    names(out)[2] <- site
    out
  })

  ## ---- Merge tributaries ----
  merged <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), inflows)
  merged[-1] <- lapply(merged[-1], as.numeric)

  # Convert to m3/s if requested
  if (units == "m3s") merged[-1] <- merged[-1] * 0.0283168

  # Sum tributaries; set NA if all NA
  merged$Q_upstream <- ifelse(
    rowSums(!is.na(merged[-1])) == 0,
    NA,
    rowSums(merged[-1], na.rm = na_rm)
  )

  merged[, c("Date", "Q_upstream")]
}
