# Declare global variables for R CMD check
# Declare global variables for R CMD check
utils::globalVariables(c("Date", "Month", "Qm", "Weight"))

#' Compute seasonal inflow weights
#'
#' Computes relative monthly inflow weights from daily discharge data.
#' The average weight across the 12 calendar months is normalized to 1.
#'
#' The function is provider-independent and can use inflow data from USGS,
#' CDEC, or any other country or data source, provided the input contains a
#' date column and an inflow column.
#'
#' @param inflow_df Data frame containing daily inflow data.
#' @param inflow_col Name of the inflow column. Default is `"Q_upstream"`.
#' @param date_col Name of the date column. Default is `"Date"`.
#' @param missing_month How to handle a month with no valid inflow data.
#'   `"zero"` assigns a weight of zero. `"error"` stops the function.
#' @param na_rm Logical. If TRUE, missing inflow values are ignored when
#'   calculating monthly means.
#'
#' @return A named numeric vector of 12 monthly seasonal weights. The names are
#'   the standard three-letter month abbreviations from `month.abb`.
#'
#' @details
#' For each month, the function calculates the mean daily inflow:
#'
#' \deqn{
#' Q_m = mean(daily\ inflow\ for\ month\ m)
#' }
#'
#' The seasonal weight is then calculated as:
#'
#' \deqn{
#' w_m = Q_m / mean(Q_1, ..., Q_{12})
#' }
#'
#' Therefore, the mean of the 12 monthly weights is 1. A weight greater than 1
#' represents a month with above-average inflow, while a weight less than 1
#' represents a month with below-average inflow.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' inflow_df <- data.frame(
#'   Date = seq(
#'     as.Date("2020-01-01"),
#'     as.Date("2020-12-31"),
#'     by = "day"
#'   ),
#'   Q_upstream = runif(366, 50, 200)
#' )
#'
#' wad_seasonal_weights(inflow_df)
#' }
wad_seasonal_weights <- function(
    inflow_df,
    inflow_col = "Q_upstream",
    date_col = "Date",
    missing_month = c("zero", "error"),
    na_rm = TRUE
) {
  ## ---- Match arguments ----
  missing_month <- match.arg(missing_month)

  ## ---- Validate input ----
  if (!is.data.frame(inflow_df)) {
    stop("inflow_df must be a data frame.")
  }

  if (!date_col %in% names(inflow_df)) {
    stop("The date column '", date_col, "' was not found in inflow_df.")
  }

  if (!inflow_col %in% names(inflow_df)) {
    stop(
      "The inflow column '", inflow_col,
      "' was not found in inflow_df."
    )
  }

  if (!is.logical(na_rm) ||
      length(na_rm) != 1 ||
      is.na(na_rm)) {
    stop("na_rm must be TRUE or FALSE.")
  }

  ## ---- Convert and validate dates ----
  dates <- as.Date(inflow_df[[date_col]])

  if (all(is.na(dates))) {
    stop("The date column contains no valid dates.")
  }

  ## ---- Convert and validate inflows ----
  inflow <- suppressWarnings(
    as.numeric(inflow_df[[inflow_col]])
  )

  if (all(is.na(inflow))) {
    stop("The inflow column contains no valid numeric values.")
  }

  ## ---- Keep valid observations ----
  valid <- !is.na(dates)

  if (na_rm) {
    valid <- valid & !is.na(inflow)
  }

  dates <- dates[valid]
  inflow <- inflow[valid]

  if (length(dates) == 0) {
    stop("No valid date and inflow observations are available.")
  }

  ## ---- Extract calendar month ----
  month_number <- as.integer(format(dates, "%m"))

  ## ---- Calculate monthly mean inflows ----
  monthly_mean <- rep(NA_real_, 12)

  for (month_index in 1:12) {
    values <- inflow[month_number == month_index]

    if (length(values) == 0) {
      monthly_mean[month_index] <- NA_real_
    } else if (all(is.na(values))) {
      monthly_mean[month_index] <- NA_real_
    } else {
      monthly_mean[month_index] <- mean(
        values,
        na.rm = TRUE
      )
    }
  }

  names(monthly_mean) <- month.abb

  ## ---- Handle missing months ----
  missing_indices <- which(is.na(monthly_mean))

  if (length(missing_indices) > 0 &&
      missing_month == "error") {
    stop(
      "No valid inflow data were available for: ",
      paste(month.abb[missing_indices], collapse = ", ")
    )
  }

  if (length(missing_indices) > 0 &&
      missing_month == "zero") {
    monthly_mean[missing_indices] <- 0
  }

  ## ---- Check for valid seasonal information ----
  if (all(monthly_mean == 0, na.rm = TRUE)) {
    stop(
      "All monthly mean inflows are zero. Seasonal weights cannot be computed."
    )
  }

  reference_mean <- mean(
    monthly_mean,
    na.rm = TRUE
  )

  if (!is.finite(reference_mean) || reference_mean <= 0) {
    stop(
      "The mean monthly inflow is not positive. ",
      "Seasonal weights cannot be computed."
    )
  }

  ## ---- Calculate normalized weights ----
  weights <- monthly_mean / reference_mean

  names(weights) <- month.abb

  weights
}
