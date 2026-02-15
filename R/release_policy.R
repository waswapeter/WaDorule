#' WaDorule release policy with optional release plot
#'
#' Computes daily releases from a storage rule curve and monthly seasonal weights.
#'
#' @param df Data frame with columns:
#'   - Date (class Date)
#'   - Storage (numeric, in MCM)
#' @param seasonal_weights Named numeric vector of 12 monthly weights (Jan–Dec)
#' @param Smin Minimum storage (MCM)
#' @param Smax Maximum storage (MCM)
#' @param Qmin Minimum release (m3/s)
#' @param Qmax Maximum release (m3/s)
#' @param rule Character; either "linear" or "logistic"
#' @param plot Logical; if TRUE, generates a daily release plot
#' @return Data frame with columns: Date, Storage, Month, Release
#' @importFrom rlang .data
#' @export
wad_release_policy <- function(
    df,
    seasonal_weights,
    Smin,
    Smax,
    Qmin,
    Qmax,
    rule = c("linear", "logistic"),
    plot = FALSE
) {

  ## ---- Input validation ----
  stopifnot(
    inherits(df$Date, "Date"),
    is.numeric(df$Storage),
    length(seasonal_weights) == 12,
    all(month.abb %in% names(seasonal_weights)),
    Smin < Smax,
    Qmin <= Qmax
  )

  rule <- match.arg(rule)

  # Ensure numeric storage
  df$Storage <- as.numeric(df$Storage)

  # Extract month abbreviation
  df$Month <- format(df$Date, "%b")

  # Normalize storage to 0–1
  S_idx <- pmin(1, pmax(0, (df$Storage - Smin) / (Smax - Smin)))

  # Compute fractional release
  rel_frac <- if (rule == "logistic") {
    1 / (1 + exp(-8 * (S_idx - 0.55)))
  } else {
    S_idx
  }

  # Apply seasonal weights; fallback to 1 if missing
  weights <- seasonal_weights[df$Month]
  weights[is.na(weights)] <- 1

  df$Release <- (Qmin + rel_frac * (Qmax - Qmin)) * weights

  ## ---- Optional plot ----
  if (plot) {
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Date, y = .data$Release)) +
        ggplot2::geom_line(color = "steelblue", linewidth = 1) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = "Daily Reservoir Release",
          x = "Date",
          y = "Release (m3/s)"
        )
      print(p)
    } else {
      warning("Install ggplot2 to generate plot", call. = FALSE)
    }
  }

  # Return only relevant columns
  df[, c("Date", "Storage", "Month", "Release")]
}
