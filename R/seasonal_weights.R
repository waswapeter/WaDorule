# Declare global variables for R CMD check
utils::globalVariables(c(".data", "Date", "Month", "Qm"))

#' Compute seasonal inflow weights
#'
#' Computes relative monthly inflow weights from daily discharge data.
#'
#' @param inflow_df Data frame with Date and inflow column
#' @param inflow_col Name of inflow column (default "Q_upstream")
#' @return Named numeric vector of 12 monthly seasonal weights
#'
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' inflow_df <- data.frame(
#'   Date = seq(as.Date("2020-01-01"), by = "day", length.out = 365),
#'   Q_upstream = runif(365, 50, 200)
#' )
#' wad_seasonal_weights(inflow_df)

wad_seasonal_weights <- function(
    inflow_df,
    inflow_col = "Q_upstream"
) {
  stopifnot(all(c("Date", inflow_col) %in% names(inflow_df)))
  stopifnot(inherits(inflow_df$Date, "Date"))

  # Compute monthly mean inflows
  w <- inflow_df |>
    dplyr::mutate(Month = lubridate::month(Date)) |>
    dplyr::group_by(Month) |>
    dplyr::summarise(Qm = mean(.data[[inflow_col]], na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(w = ifelse(is.na(Qm), 0, Qm / mean(Qm, na.rm = TRUE))) |>
    dplyr::arrange(Month)

  # Ensure all 12 months are present
  w_full <- data.frame(Month = 1:12) |>
    dplyr::left_join(w[, c("Month", "w")], by = "Month") |>
    dplyr::mutate(w = ifelse(is.na(w), 0, w))

  # Explicit import fixes R CMD check note
  stats::setNames(w_full$w, month.abb)
}
