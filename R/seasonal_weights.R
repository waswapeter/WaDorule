# Declare global variables for R CMD check
utils::globalVariables(c(".data","Date","Month","Qm"))

# Import base functions to avoid "no visible function definition" notes
#' @importFrom utils read.csv
#' @importFrom stats setNames
NULL

#' Compute seasonal inflow weights
#'
#' @param inflow_df Data frame with Date and inflow
#' @param inflow_col Column name of inflow
#' @return Named vector of monthly weights
#' #' @examples
#' elev_file <- system.file("extdata", "Falls_Lake_Dam_Elevation.csv", package = "WaDorule")
#' storage_df <- wad_compute_target_storage(
#'   elev_df = read.csv(elev_file),
#'   min_storage_acft = 50000,
#'   max_storage_acft = 200000
#' )
#' @export

wad_seasonal_weights <- function(
    inflow_df,
    inflow_col = "Q_upstream"
) {

  stopifnot(all(c("Date", inflow_col) %in% names(inflow_df)))

  w <- inflow_df |>
    dplyr::mutate(Month = lubridate::month(Date)) |>
    dplyr::group_by(Month) |>
    dplyr::summarise(Qm = mean(.data[[inflow_col]], na.rm = TRUE)) |>
    dplyr::mutate(w = Qm / mean(Qm)) |>
    dplyr::pull(w)

  setNames(w, month.abb)
}
