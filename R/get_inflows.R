# Declare global variables for R CMD check
utils::globalVariables(c("Date"))

#' Aggregate upstream inflows from USGS, CDEC, and user-defined gages
#'
#' Downloads daily discharge data from one or more USGS and/or CDEC gages,
#' combines these data with user-supplied inflow data, and returns one daily
#' aggregated upstream inflow time series.
#'
#' User-defined data can represent gages from any country. International data
#' can either be supplied directly as a data frame or downloaded through a
#' user-defined reader function.
#'
#' @param usgs_sites Character vector of USGS site numbers.
#' @param cdec_sites Character vector of CDEC station codes.
#' @param cdec_sensor CDEC sensor code. Default is `"76"`.
#' @param cdec_dur_code CDEC duration code. Default is `"D"` for daily data.
#' @param cdec_units Units of the CDEC data. Either `"cfs"` or `"m3s"`.
#' @param inflow_data Optional data frame containing user-provided gage data.
#'   It must contain a `Date` column and one or more flow columns.
#' @param user_flow_cols Optional character vector identifying the flow columns
#'   in `inflow_data`. If NULL, all numeric columns except `Date` are used.
#' @param inflow_units Units of `inflow_data`. Either `"cfs"` or `"m3s"`.
#' @param other_data Optional data frame containing non-US or international
#'   gage data. It must contain a `Date` column and one or more flow columns.
#' @param other_flow_cols Optional character vector identifying the flow
#'   columns in `other_data`. If NULL, all numeric columns except `Date` are
#'   used.
#' @param other_units Units of `other_data`. Either `"cfs"` or `"m3s"`.
#' @param other_sites Optional character vector of non-US gage or station
#'   identifiers. These identifiers are passed to `other_reader`.
#' @param other_reader Optional user-defined function for downloading data from
#'   another country or data provider. The function must accept the arguments
#'   `site`, `start_date`, and `end_date`, and must return a data frame with a
#'   `Date` column and one or more numeric flow columns.
#' @param start_date Start date in `"YYYY-MM-DD"` format.
#' @param end_date End date in `"YYYY-MM-DD"` format.
#' @param units Output units. Either `"m3s"` or `"cfs"`.
#' @param na_rm Logical. If TRUE, missing values are ignored when summing gages.
#'   If FALSE, a missing value from any source causes the daily total to be NA.
#'
#' @return A data frame with two columns:
#'   \describe{
#'     \item{Date}{Date of the inflow record.}
#'     \item{Q_upstream}{Aggregated upstream inflow.}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # USGS gages
#' qin <- wad_get_upstream_inflows(
#'   usgs_sites = c("12175500", "12178100"),
#'   start_date = "2000-01-01",
#'   end_date = "2020-12-31",
#'   units = "m3s"
#' )
#'
#' # CDEC gages
#' qin <- wad_get_upstream_inflows(
#'   cdec_sites = c("ISB", "ABC"),
#'   cdec_sensor = "76",
#'   cdec_dur_code = "D",
#'   cdec_units = "cfs",
#'   start_date = "1994-01-01",
#'   end_date = "2020-12-31",
#'   units = "m3s"
#' )
#'
#' # Multiple user-provided gages
#' user_data <- data.frame(
#'   Date = as.Date(c("2020-01-01", "2020-01-02")),
#'   Gage_A = c(100, 125),
#'   Gage_B = c(50, 75)
#' )
#'
#' qin <- wad_get_upstream_inflows(
#'   inflow_data = user_data,
#'   user_flow_cols = c("Gage_A", "Gage_B"),
#'   inflow_units = "cfs",
#'   start_date = "2020-01-01",
#'   end_date = "2020-01-02",
#'   units = "m3s"
#' )
#' }
wad_get_upstream_inflows <- function(
    usgs_sites = NULL,
    cdec_sites = NULL,
    cdec_sensor = "76",
    cdec_dur_code = "D",
    cdec_units = c("cfs", "m3s"),
    inflow_data = NULL,
    user_flow_cols = NULL,
    inflow_units = c("cfs", "m3s"),
    other_data = NULL,
    other_flow_cols = NULL,
    other_units = c("cfs", "m3s"),
    other_sites = NULL,
    other_reader = NULL,
    start_date,
    end_date,
    units = c("m3s", "cfs"),
    na_rm = TRUE
) {
  ## ---- Match arguments ----
  units <- match.arg(units)
  cdec_units <- match.arg(cdec_units)
  inflow_units <- match.arg(inflow_units)
  other_units <- match.arg(other_units)

  ## ---- Input validation ----
  if (is.null(usgs_sites)) usgs_sites <- character(0)
  if (is.null(cdec_sites)) cdec_sites <- character(0)
  if (is.null(other_sites)) other_sites <- character(0)

  if (!is.character(usgs_sites)) {
    stop("usgs_sites must be a character vector.")
  }

  if (!is.character(cdec_sites)) {
    stop("cdec_sites must be a character vector.")
  }

  if (!is.character(other_sites)) {
    stop("other_sites must be a character vector.")
  }

  if (!is.character(start_date) ||
      length(start_date) != 1 ||
      !is.character(end_date) ||
      length(end_date) != 1) {
    stop("start_date and end_date must each be one character value.")
  }

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if (is.na(start_date) || is.na(end_date)) {
    stop("Invalid start_date or end_date.")
  }

  if (start_date > end_date) {
    stop("start_date must be less than or equal to end_date.")
  }

  if (!is.logical(na_rm) || length(na_rm) != 1 || is.na(na_rm)) {
    stop("na_rm must be TRUE or FALSE.")
  }

  if (length(usgs_sites) == 0 &&
      length(cdec_sites) == 0 &&
      is.null(inflow_data) &&
      is.null(other_data) &&
      is.null(other_reader)) {
    stop(
      "Provide at least one USGS site, CDEC site, inflow_data, ",
      "other_data, or other_reader."
    )
  }

  ## ---- Helper functions ----

  convert_to_m3s <- function(x, input_units) {
    if (input_units == "cfs") {
      return(x * 0.0283168)
    }

    x
  }

  convert_from_m3s <- function(x, output_units) {
    if (output_units == "cfs") {
      return(x / 0.0283168)
    }

    x
  }

  # Aggregate duplicate records from the same source and date.
  # Mean is used when duplicate records occur for a single source.
  aggregate_daily <- function(x) {
    date_groups <- split(x$Q, as.character(x$Date))

    result <- lapply(date_groups, function(values) {
      if (all(is.na(values))) {
        return(NA_real_)
      }

      mean(values, na.rm = TRUE)
    })

    data.frame(
      Date = as.Date(names(result)),
      Q = as.numeric(result),
      stringsAsFactors = FALSE
    )
  }

  get_flow_columns <- function(data, flow_cols, data_name) {
    if (!is.data.frame(data)) {
      stop(data_name, " must be a data frame.")
    }

    if (!"Date" %in% names(data)) {
      stop(data_name, " must contain a column named 'Date'.")
    }

    if (is.null(flow_cols)) {
      flow_cols <- names(data)[
        vapply(data, is.numeric, logical(1))
      ]

      flow_cols <- setdiff(flow_cols, "Date")

      if (length(flow_cols) == 0) {
        stop(
          "No numeric flow columns were found in ", data_name, ". ",
          "Specify the columns explicitly."
        )
      }
    }

    if (!is.character(flow_cols) || length(flow_cols) == 0) {
      stop("Flow column names must be supplied as a character vector.")
    }

    missing_cols <- setdiff(flow_cols, names(data))

    if (length(missing_cols) > 0) {
      stop(
        "The following flow columns are missing from ", data_name, ": ",
        paste(missing_cols, collapse = ", ")
      )
    }

    flow_cols
  }

  standardize_source_data <- function(
    data,
    flow_cols,
    source_name,
    input_units
  ) {
    flow_cols <- get_flow_columns(
      data = data,
      flow_cols = flow_cols,
      data_name = source_name
    )

    source_results <- list()

    for (flow_col in flow_cols) {
      out <- data.frame(
        Date = as.Date(data$Date),
        Q = suppressWarnings(as.numeric(data[[flow_col]])),
        stringsAsFactors = FALSE
      )

      out <- out[
        !is.na(out$Date) &
          out$Date >= start_date &
          out$Date <= end_date,
        ,
        drop = FALSE
      ]

      if (nrow(out) == 0) {
        warning(
          "No data available for ", source_name,
          " flow column '", flow_col, "'."
        )
        next
      }

      out <- aggregate_daily(out)
      out$Q <- convert_to_m3s(out$Q, input_units)

      source_results[[paste0(source_name, "_", flow_col)]] <- out
    }

    source_results
  }

  inflow_list <- list()

  ## ---- Download USGS data ----
  if (length(usgs_sites) > 0) {
    if (!requireNamespace("dataRetrieval", quietly = TRUE)) {
      stop(
        "The dataRetrieval package is required for USGS data. ",
        "Install it with install.packages('dataRetrieval')."
      )
    }

    for (site in usgs_sites) {
      message("Downloading USGS data for site ", site)

      df <- dataRetrieval::readNWISdv(
        siteNumbers = site,
        parameterCd = "00060",
        startDate = start_date,
        endDate = end_date
      )

      if (nrow(df) == 0) {
        warning("No data returned for USGS site ", site)
        next
      }

      if (!"Date" %in% names(df)) {
        warning("No Date column returned for USGS site ", site)
        next
      }

      # USGS discharge columns commonly begin with X_00060.
      qcols <- grep("^X_00060", names(df), value = TRUE)

      # Prefer daily mean discharge, whose statistic code is commonly 00003.
      daily_mean_col <- grep("_00003$", qcols, value = TRUE)

      if (length(daily_mean_col) == 1) {
        qcol <- daily_mean_col
      } else if (length(qcols) >= 1) {
        qcol <- qcols[1]
      } else {
        qcols <- grep("^Flow$|^Q$", names(df), value = TRUE)

        if (length(qcols) == 0) {
          warning(
            "Could not identify a discharge column for USGS site ",
            site
          )
          next
        }

        qcol <- qcols[1]
      }

      usgs_df <- data.frame(
        Date = df$Date,
        USGS_flow = df[[qcol]],
        stringsAsFactors = FALSE
      )

      inflow_list <- c(
        inflow_list,
        standardize_source_data(
          data = usgs_df,
          flow_cols = "USGS_flow",
          source_name = paste0("USGS_", site),
          input_units = "cfs"
        )
      )
    }
  }

  ## ---- Download CDEC data ----
  if (length(cdec_sites) > 0) {
    if (!requireNamespace("CDECRetrieve", quietly = TRUE)) {
      stop(
        "The CDECRetrieve package is required for CDEC data. ",
        "Install it with remotes::install_github(",
        "'FlowWest/CDECRetrieve')."
      )
    }

    for (station in cdec_sites) {
      message("Downloading CDEC data for station ", station)

      # Positional arguments are used because CDECRetrieve versions may
      # use slightly different argument names.
      df <- CDECRetrieve::cdec_query(
        station,
        cdec_sensor,
        cdec_dur_code,
        start_date,
        end_date
      )

      if (is.null(df) || nrow(df) == 0) {
        warning("No data returned for CDEC station ", station)
        next
      }

      if (!all(c("datetime", "parameter_value") %in% names(df))) {
        warning(
          "CDEC data for station ", station,
          " must contain datetime and parameter_value columns."
        )
        next
      }

      cdec_df <- data.frame(
        Date = as.Date(df$datetime),
        CDEC_flow = df$parameter_value,
        stringsAsFactors = FALSE
      )

      inflow_list <- c(
        inflow_list,
        standardize_source_data(
          data = cdec_df,
          flow_cols = "CDEC_flow",
          source_name = paste0("CDEC_", station),
          input_units = cdec_units
        )
      )
    }
  }

  ## ---- Add user-provided data ----
  if (!is.null(inflow_data)) {
    inflow_list <- c(
      inflow_list,
      standardize_source_data(
        data = inflow_data,
        flow_cols = user_flow_cols,
        source_name = "USER",
        input_units = inflow_units
      )
    )
  }

  ## ---- Download or add international/non-US data ----

  # A custom reader must return a data frame with Date and one or more
  # numeric flow columns.
  if (!is.null(other_reader)) {
    if (!is.function(other_reader)) {
      stop("other_reader must be a function.")
    }

    reader_sites <- other_sites

    # If no site identifiers are supplied, call the reader once with site = NULL.
    if (length(reader_sites) == 0) {
      reader_sites <- NA_character_
    }

    for (site in reader_sites) {
      message(
        "Downloading non-US data",
        if (!is.na(site)) paste0(" for site ", site) else ""
      )

      other_result <- other_reader(
        site = if (is.na(site)) NULL else site,
        start_date = start_date,
        end_date = end_date
      )

      if (is.null(other_result) || nrow(other_result) == 0) {
        warning(
          "No data returned by other_reader",
          if (!is.na(site)) paste0(" for site ", site) else ""
        )
        next
      }

      site_name <- if (is.na(site)) {
        "OTHER"
      } else {
        paste0("OTHER_", site)
      }

      inflow_list <- c(
        inflow_list,
        standardize_source_data(
          data = other_result,
          flow_cols = other_flow_cols,
          source_name = site_name,
          input_units = other_units
        )
      )
    }
  }

  # Add non-US data supplied directly as a data frame.
  if (!is.null(other_data)) {
    inflow_list <- c(
      inflow_list,
      standardize_source_data(
        data = other_data,
        flow_cols = other_flow_cols,
        source_name = "OTHER",
        input_units = other_units
      )
    )
  }

  if (length(inflow_list) == 0) {
    stop("No inflow data were available for the requested period.")
  }

  ## ---- Combine all sources ----
  all_inflows <- do.call(
    rbind,
    inflow_list
  )

  # Create the complete requested daily date sequence.
  result <- data.frame(
    Date = seq.Date(
      from = start_date,
      to = end_date,
      by = "day"
    )
  )

  # Split all source flows by date.
  values_by_date <- split(
    all_inflows$Q,
    as.character(all_inflows$Date)
  )

  result$Q_upstream <- vapply(
    as.character(result$Date),
    FUN = function(day) {
      values <- values_by_date[[day]]

      if (is.null(values) || length(values) == 0) {
        return(NA_real_)
      }

      if (na_rm) {
        if (all(is.na(values))) {
          return(NA_real_)
        }

        return(sum(values, na.rm = TRUE))
      }

      if (anyNA(values)) {
        return(NA_real_)
      }

      sum(values)
    },
    FUN.VALUE = numeric(1)
  )

  ## ---- Convert output units ----
  result$Q_upstream <- convert_from_m3s(
    result$Q_upstream,
    units
  )

  result
}
