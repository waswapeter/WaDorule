#' Compute target storage from an elevation or storage rule curve
#'
#' Uses a user-supplied daily elevation rule, a user-supplied daily storage
#' rule, or both. If storage data are supplied, they are used directly. If
#' storage data are not supplied, storage is calculated from elevation using
#' user-supplied storage and elevation limits.
#'
#' No default reservoir elevation or storage curve is used. Each reservoir must
#' provide its own data.
#'
#' @param elev_df Optional data frame containing user-supplied elevation and/or
#'   storage data. Either `elev_df` or `elev_file` must be supplied.
#' @param min_storage_acft Minimum operational or conservation storage in
#'   acre-feet. Required when storage is calculated from elevation.
#' @param max_storage_acft Maximum operational or conservation storage in
#'   acre-feet. Required when storage is calculated from elevation.
#' @param elev_file Optional path to a user-supplied CSV file. If supplied, it
#'   takes precedence over `elev_df`.
#' @param min_elev_ft Minimum operational elevation in feet. If NULL, the
#'   minimum elevation in the input data is used.
#' @param max_elev_ft Maximum operational elevation in feet. If NULL, the
#'   maximum elevation in the input data is used.
#' @param date_col Optional name of the date column. Common names such as
#'   `Date`, `date`, or `datetime` are detected automatically if this is NULL.
#'
#' @return A data frame containing `Date`, if supplied, `Target_Elev`, if
#'   supplied, and `target_storage_MCM`.
#'
#' @details
#' The function recognizes the following storage columns:
#'
#' \describe{
#'   \item{MCM storage}{
#'     \code{target_storage_MCM}, \code{Target_Storage_MCM},
#'     \code{storage_MCM}, or \code{Storage_MCM}.
#'   }
#'   \item{Acre-feet storage}{
#'     \code{target_storage_acft}, \code{Target_Storage_acft},
#'     \code{storage_acft}, \code{Storage_acft}, or \code{Storage}.
#'   }
#' }
#'
#' One acre-foot equals approximately 0.00123348 million cubic metres (MCM).
#'
#' @export
wad_compute_target_storage <- function(
    elev_df = NULL,
    min_storage_acft = NULL,
    max_storage_acft = NULL,
    elev_file = NULL,
    min_elev_ft = NULL,
    max_elev_ft = NULL,
    date_col = NULL
) {
  ## ---- Conversion constant ----
  acft_to_mcm <- 0.00123348

  ## ---- Require user-specific reservoir data ----
  if (is.null(elev_df) && is.null(elev_file)) {
    stop(
      "No reservoir elevation or storage data were supplied. ",
      "Provide either elev_df or elev_file. The package does not use ",
      "a default elevation curve."
    )
  }

  ## ---- Load user CSV ----
  if (!is.null(elev_file)) {
    if (!is.character(elev_file) ||
        length(elev_file) != 1 ||
        is.na(elev_file)) {
      stop("elev_file must be a single valid file path.")
    }

    if (!file.exists(elev_file)) {
      stop("User elevation/storage CSV file not found: ", elev_file)
    }

    elev_df <- utils::read.csv(
      elev_file,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  if (!is.data.frame(elev_df)) {
    stop("elev_df must be a data frame.")
  }

  if (nrow(elev_df) == 0) {
    stop("The supplied elevation/storage data contain no rows.")
  }

  ## ---- Identify date column ----
  if (is.null(date_col)) {
    possible_date_cols <- c("Date", "date", "datetime")

    date_matches <- possible_date_cols[
      possible_date_cols %in% names(elev_df)
    ]

    if (length(date_matches) > 0) {
      date_col <- date_matches[1]
    }
  }

  if (!is.null(date_col)) {
    if (!date_col %in% names(elev_df)) {
      stop("date_col '", date_col, "' is not present in the data.")
    }

    parsed_dates <- as.Date(elev_df[[date_col]])

    if (anyNA(parsed_dates)) {
      stop("The date column contains invalid dates.")
    }

    elev_df[[date_col]] <- parsed_dates
  }

  ## ---- Identify elevation column ----
  elevation_candidates <- c(
    "Target_Elev",
    "target_elev",
    "Elevation",
    "elevation",
    "TargetElevation"
  )

  elevation_matches <- elevation_candidates[
    elevation_candidates %in% names(elev_df)
  ]

  elev_col <- if (length(elevation_matches) > 0) {
    elevation_matches[1]
  } else {
    NULL
  }

  ## ---- Identify direct storage columns ----
  storage_mcm_candidates <- c(
    "target_storage_MCM",
    "Target_Storage_MCM",
    "storage_MCM",
    "Storage_MCM",
    "target_storage_mcm",
    "storage_mcm"
  )

  storage_acft_candidates <- c(
    "target_storage_acft",
    "Target_Storage_acft",
    "storage_acft",
    "Storage_acft",
    "target_storage_ACFT",
    "Storage"
  )

  storage_mcm_matches <- storage_mcm_candidates[
    storage_mcm_candidates %in% names(elev_df)
  ]

  storage_acft_matches <- storage_acft_candidates[
    storage_acft_candidates %in% names(elev_df)
  ]

  ## ---- Use directly supplied storage rule ----
  if (length(storage_mcm_matches) > 0 ||
      length(storage_acft_matches) > 0) {

    if (length(storage_mcm_matches) > 0) {
      storage_col <- storage_mcm_matches[1]

      target_storage_mcm <- suppressWarnings(
        as.numeric(elev_df[[storage_col]])
      )
    } else {
      storage_col <- storage_acft_matches[1]

      target_storage_acft <- suppressWarnings(
        as.numeric(elev_df[[storage_col]])
      )

      target_storage_mcm <- target_storage_acft * acft_to_mcm
    }

    if (all(is.na(target_storage_mcm))) {
      stop("The supplied storage column contains no valid numeric values.")
    }

    output <- data.frame(
      stringsAsFactors = FALSE
    )

    if (!is.null(date_col)) {
      output$Date <- elev_df[[date_col]]
    }

    if (!is.null(elev_col)) {
      output$Target_Elev <- suppressWarnings(
        as.numeric(elev_df[[elev_col]])
      )
    }

    output$target_storage_MCM <- target_storage_mcm

    return(output)
  }

  ## ---- Compute storage from elevation ----
  if (is.null(elev_col)) {
    stop(
      "The input does not contain a recognized elevation or storage column. ",
      "Provide 'Target_Elev' or a storage column."
    )
  }

  ## ---- Validate storage limits ----
  if (is.null(min_storage_acft) ||
      is.null(max_storage_acft) ||
      !is.numeric(min_storage_acft) ||
      !is.numeric(max_storage_acft) ||
      length(min_storage_acft) != 1 ||
      length(max_storage_acft) != 1 ||
      is.na(min_storage_acft) ||
      is.na(max_storage_acft)) {
    stop(
      "min_storage_acft and max_storage_acft must be supplied when ",
      "storage data are not included in the input. Obtain these values ",
      "from the reservoir's operational description or another reliable source."
    )
  }

  if (min_storage_acft > max_storage_acft) {
    stop("min_storage_acft must be less than max_storage_acft.")
  }

  ## ---- Convert elevation to numeric ----
  target_elev <- suppressWarnings(
    as.numeric(elev_df[[elev_col]])
  )

  if (all(is.na(target_elev))) {
    stop("The elevation column contains no valid numeric values.")
  }

  ## ---- Determine elevation limits ----
  if (is.null(min_elev_ft)) {
    min_elev_ft <- min(target_elev, na.rm = TRUE)
  }

  if (is.null(max_elev_ft)) {
    max_elev_ft <- max(target_elev, na.rm = TRUE)
  }

  if (!is.numeric(min_elev_ft) ||
      !is.numeric(max_elev_ft) ||
      length(min_elev_ft) != 1 ||
      length(max_elev_ft) != 1 ||
      is.na(min_elev_ft) ||
      is.na(max_elev_ft)) {
    stop("min_elev_ft and max_elev_ft must be single numeric values.")
  }

  if (min_elev_ft >= max_elev_ft) {
    stop("min_elev_ft must be less than max_elev_ft.")
  }

  ## ---- Calculate storage from elevation ----
  target_storage_acft <-
    min_storage_acft +
    (
      (target_elev - min_elev_ft) /
        (max_elev_ft - min_elev_ft)
    ) *
    (max_storage_acft - min_storage_acft)

  target_storage_mcm <- target_storage_acft * acft_to_mcm

  ## ---- Return result ----
  output <- data.frame(
    stringsAsFactors = FALSE
  )

  if (!is.null(date_col)) {
    output$Date <- elev_df[[date_col]]
  }

  output$Target_Elev <- target_elev
  output$target_storage_MCM <- target_storage_mcm

  output
}
