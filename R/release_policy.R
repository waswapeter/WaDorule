
# Declare global variables for R CMD check
utils::globalVariables(c("Date","Release"))

# Import base functions to avoid "no visible function definition" notes
#' @importFrom utils read.csv
#' @importFrom stats setNames
NULL

#' WaDorule release policy with optional release plot
#'
#' Computes daily releases from a storage rule curve and monthly seasonal weights.
#'
#' @param df Data frame with Date and Storage (MCM)
#' @param seasonal_weights Named vector of monthly inflow weights
#' @param Smin Minimum storage (MCM)
#' @param Smax Maximum storage (MCM)
#' @param Qmin Minimum release (m3/s)
#' @param Qmax Maximum release (m3/s)
#' @param rule linear or logistic
#' @param plot Logical; if TRUE, generates a daily release plot
#' @return Data frame with Date, Storage, Month, Release
#' #' #' @examples
#' elev_file <- system.file("extdata", "Falls_Lake_Dam_Elevation.csv", package = "WaDorule")
#' storage_df <- wad_compute_target_storage(
#'   elev_df = read.csv(elev_file),
#'   min_storage_acft = 50000,
#'   max_storage_acft = 200000
#' )
#' @export

wad_release_policy <- function(
    df,
    seasonal_weights,
    Smin,
    Smax,
    Qmin,
    Qmax,
    rule = "linear",
    plot = FALSE
) {

  # Check inputs
  stopifnot(all(c("Date", "Storage") %in% names(df)))
  stopifnot(is.numeric(Smin), is.numeric(Smax), Smin < Smax)
  stopifnot(is.numeric(Qmin), is.numeric(Qmax), Qmin <= Qmax)
  stopifnot(rule %in% c("linear", "logistic"))

  # Add Month column
  df$Month <- format(df$Date, "%b")

  # Compute storage index (0–1)
  S_idx <- pmin(1, pmax(0, (df$Storage - Smin) / (Smax - Smin)))

  # Compute release fraction
  rel_frac <- if (rule == "logistic") {
    1 / (1 + exp(-8 * (S_idx - 0.55)))
  } else {
    S_idx
  }

  # Compute daily release
  df$Release <- (Qmin + rel_frac * (Qmax - Qmin)) * seasonal_weights[df$Month]

  # Optional plot: daily release
  if (plot) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      warning("ggplot2 required for plotting. Install it with install.packages('ggplot2')")
    } else {
      p <- ggplot2::ggplot(df, ggplot2::aes(x = Date, y = Release)) +
        ggplot2::geom_line(color = "steelblue", linewidth = 1) +
        ggplot2::labs(
          title = "Daily Release from WaDorule Policy",
          x = "Date",
          y = "Release (m3/s)"
        ) +
        ggplot2::theme_minimal()

      print(p)  # force plot to display inside function
    }
  }

  return(df)
}


