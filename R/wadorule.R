utils::globalVariables(c(
  "Date", "Storage", "Elevation", "Storage_Target",
  "Inflow", "n_records", "Month", "Mean_Inflow",
  "Storage_Target_Change", "Storage_Operational",
  "Storage_Normalized", "Storage_Level_Signal",
  "Storage_Change", "Storage_Level_Adjustment",
  "Release_Target_Raw", "Release_Target",
  "Elevation_Target", "Abrupt_Transition",
  "Release_Target_Limited", "Release_Change",
  "outflow", "ObservedRelease", "Month_Position"
))

#' WaDorule water-balance release-target algorithm
#'
#' Derives daily reservoir release targets from daily inflows,
#' storage targets, elevation targets, and reservoir-specific constraints.
#'
#' @param inflows Data frame with `Date` and `inflow` columns.
#' @param targets Data frame with `Date`, `Storage`, and `Elevation` columns.
#' @param Smin Minimum physical storage.
#' @param Smax Maximum physical storage.
#' @param Rmin Minimum release.
#' @param Rmax Maximum release.
#' @param alpha_storage Maximum daily storage movement as a fraction of active storage.
#' @param minimum_transition_days Minimum duration for abrupt storage-target changes.
#' @param alpha_release Maximum daily release movement as a fraction of release range.
#' @param beta_storage Storage-level feedback strength.
#' @param storage_deadband Central deadband for storage-level feedback.
#' @param transition_threshold_fraction Fraction of active storage used to identify abrupt changes.
#' @param flow_to_storage Daily conversion factor from m3/s to MCM/day.
#' @param output_file Optional path for the final four-column CSV.
#' @param diagnostic_file Optional path for the diagnostic CSV.
#' @param observed Optional data frame with `date` and `outflow` columns for contextual plots.
#' @param release_plot Optional path for the observed-release comparison plot.
#' @param inflow_plot Optional path for the inflow-release comparison plot.
#'
#' @return A data frame containing `Date`, `Storage_Target`,
#' `Elevation_Target`, and `Release_Target`.
#'
#' @export

wad_wadorule <- function(
    inflows,
    targets,
    Smin,
    Smax,
    Rmin,
    Rmax,
    alpha_storage = 0.00025,
    minimum_transition_days = 7,
    alpha_release = 0.03,
    beta_storage = 0.10,
    storage_deadband = 0.10,
    transition_threshold_fraction = 0.01,
    flow_to_storage = 0.0864,
    output_file = NULL,
    diagnostic_file = NULL,
    observed = NULL,
    release_plot = NULL,
    inflow_plot = NULL
) {

  if (!is.finite(Smin) ||
      !is.finite(Smax) ||
      Smax <= Smin) {
    stop("Smax must be greater than Smin.")
  }

  if (!is.finite(Rmin) ||
      !is.finite(Rmax) ||
      Rmax <= Rmin) {
    stop("Rmax must be greater than Rmin.")
  }

  if (!is.finite(flow_to_storage) ||
      flow_to_storage <= 0) {
    stop("flow_to_storage must be greater than zero.")
  }

  active_storage <- Smax - Smin
  release_range <- Rmax - Rmin

  max_storage_change_per_day <-
    alpha_storage * active_storage

  max_release_change_per_day <-
    alpha_release * release_range

  transition_threshold <-
    transition_threshold_fraction * active_storage

  parse_dates <- function(x) {
    as.Date(
      lubridate::parse_date_time(
        as.character(x),
        orders = c(
          "ymd",
          "ymd HMS",
          "ymd HM",
          "mdy",
          "mdy HMS",
          "mdy HM",
          "dmy",
          "dmy HMS",
          "dmy HM"
        ),
        quiet = TRUE
      )
    )
  }

  required_target_columns <- c(
    "Date",
    "Storage",
    "Elevation"
  )

  missing_target_columns <- setdiff(
    required_target_columns,
    names(targets)
  )

  if (length(missing_target_columns) > 0) {
    stop(
      "Missing target columns: ",
      paste(missing_target_columns, collapse = ", ")
    )
  }

  targets <- targets |>
    dplyr::transmute(
      Date = parse_dates(Date),
      Storage_Target = as.numeric(Storage),
      Elevation_Target = as.numeric(Elevation)
    ) |>
    dplyr::filter(
      !is.na(Date),
      is.finite(Storage_Target)
    ) |>
    dplyr::arrange(Date) |>
    dplyr::mutate(
      Storage_Target = pmin(
        pmax(Storage_Target, Smin),
        Smax
      )
    )

  if (nrow(targets) == 0) {
    stop("No valid storage-target records were found.")
  }

  if (anyDuplicated(targets$Date) > 0) {
    stop("Duplicate dates found in the target data.")
  }

  required_inflow_columns <- c(
    "Date",
    "inflow"
  )

  missing_inflow_columns <- setdiff(
    required_inflow_columns,
    names(inflows)
  )

  if (length(missing_inflow_columns) > 0) {
    stop(
      "Missing inflow columns: ",
      paste(missing_inflow_columns, collapse = ", ")
    )
  }

  inflow <- inflows |>
    dplyr::transmute(
      Date = parse_dates(Date),
      Inflow = as.numeric(inflow)
    ) |>
    dplyr::filter(
      !is.na(Date),
      is.finite(Inflow)
    ) |>
    dplyr::arrange(Date)

  duplicate_inflow_dates <- inflow |>
    dplyr::count(Date, name = "n_records") |>
    dplyr::filter(n_records > 1)

  if (nrow(duplicate_inflow_dates) > 0) {
    warning(
      "Duplicate dates found in the inflow data. ",
      "Duplicate values will be averaged by date."
    )

    inflow <- inflow |>
      dplyr::group_by(Date) |>
      dplyr::summarise(
        Inflow = mean(Inflow, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(Date)
  }

  monthly_inflow <- inflow |>
    dplyr::mutate(
      Month = lubridate::month(Date)
    ) |>
    dplyr::group_by(Month) |>
    dplyr::summarise(
      Mean_Inflow = mean(Inflow, na.rm = TRUE),
      .groups = "drop"
    )

  target <- targets |>
    dplyr::left_join(
      inflow,
      by = "Date"
    ) |>
    dplyr::mutate(
      Inflow_Original = Inflow,
      Month = lubridate::month(Date)
    ) |>
    dplyr::left_join(
      monthly_inflow,
      by = "Month"
    ) |>
    dplyr::mutate(
      Inflow = dplyr::coalesce(
        Inflow,
        Mean_Inflow
      )
    ) |>
    dplyr::select(
      -Month,
      -Mean_Inflow
    ) |>
    dplyr::arrange(Date)

  if (any(!is.finite(target$Inflow))) {
    stop("Missing inflows remain after monthly-mean replacement.")
  }

  target <- target |>
    dplyr::mutate(
      Storage_Target_Change =
        Storage_Target - dplyr::lag(Storage_Target),

      Abrupt_Transition =
        abs(Storage_Target_Change) >
        transition_threshold
    )

  storage_operational <- numeric(nrow(target))

  storage_operational[1] <-
    target$Storage_Target[1]

  if (nrow(target) > 1) {

    for (i in 2:nrow(target)) {

      previous_storage <-
        storage_operational[i - 1]

      current_target <-
        target$Storage_Target[i]

      target_change <-
        target$Storage_Target[i] -
        target$Storage_Target[i - 1]

      daily_storage_rate <-
        max_storage_change_per_day

      if (
        is.finite(target_change) &&
        abs(target_change) > transition_threshold
      ) {
        daily_storage_rate <- min(
          max_storage_change_per_day,
          abs(target_change) / minimum_transition_days
        )
      }

      storage_difference <-
        current_target - previous_storage

      storage_step <-
        sign(storage_difference) *
        min(
          abs(storage_difference),
          daily_storage_rate
        )

      storage_operational[i] <-
        previous_storage + storage_step
    }
  }

  target$Storage_Operational <-
    storage_operational

  target$Storage_Change <- c(
    diff(target$Storage_Operational),
    0
  )

  target <- target |>
    dplyr::mutate(
      Storage_Normalized =
        (Storage_Operational - Smin) /
        active_storage,

      Storage_Normalized =
        pmin(
          pmax(Storage_Normalized, 0),
          1
        ),

      Storage_Level_Signal =
        1 - 2 * Storage_Normalized,

      Storage_Level_Signal =
        dplyr::if_else(
          abs(Storage_Level_Signal) <
            storage_deadband,
          0,
          Storage_Level_Signal
        ),

      Storage_Level_Adjustment =
        beta_storage *
        release_range *
        Storage_Level_Signal
    )

  target <- target |>
    dplyr::mutate(
      Release_Target_Raw =
        Inflow -
        Storage_Change / flow_to_storage +
        Storage_Level_Adjustment
    )

  target <- target |>
    dplyr::mutate(
      Release_Target_Limited =
        pmin(
          pmax(
            Release_Target_Raw,
            Rmin
          ),
          Rmax
        )
    )

  release_target <- numeric(nrow(target))

  release_target[1] <-
    target$Release_Target_Limited[1]

  if (nrow(target) > 1) {

    for (i in 2:nrow(target)) {

      previous_release <-
        release_target[i - 1]

      requested_release <-
        target$Release_Target_Limited[i]

      requested_change <-
        requested_release - previous_release

      release_step <-
        sign(requested_change) *
        min(
          abs(requested_change),
          max_release_change_per_day
        )

      release_target[i] <-
        previous_release + release_step

      release_target[i] <-
        pmin(
          pmax(
            release_target[i],
            Rmin
          ),
          Rmax
        )
    }
  }

  target$Release_Target <-
    release_target

  target <- target |>
    dplyr::mutate(
      Release_Change =
        Release_Target - dplyr::lag(Release_Target),

      Drawdown_Release_Component =
        pmax(
          -Storage_Change / flow_to_storage,
          0
        )
    )

  output <- target |>
    dplyr::select(
      Date,
      Storage_Target,
      Elevation_Target,
      Release_Target
    )

  if (!is.null(output_file)) {
    readr::write_csv(
      output,
      output_file
    )
  }

  if (!is.null(diagnostic_file)) {

    diagnostic_output <- target |>
      dplyr::select(
        Date,
        Storage_Target,
        Storage_Operational,
        Storage_Target_Change,
        Abrupt_Transition,
        Inflow,
        Storage_Change,
        Storage_Level_Adjustment,
        Release_Target_Raw,
        Release_Target_Limited,
        Release_Target,
        Release_Change
      )

    readr::write_csv(
      diagnostic_output,
      diagnostic_file
    )
  }

  if (!is.null(observed)) {

    required_observed_columns <- c(
      "date",
      "outflow"
    )

    if (all(
      required_observed_columns %in%
        names(observed)
    )) {

      observed <- observed |>
        dplyr::transmute(
          Date = parse_dates(date),
          ObservedRelease = as.numeric(outflow)
        ) |>
        dplyr::filter(
          !is.na(Date),
          is.finite(ObservedRelease)
        ) |>
        dplyr::mutate(
          Month = lubridate::month(Date)
        )

      target_plot <- target |>
        dplyr::mutate(
          Month = lubridate::month(Date),

          Month_Position =
            Month -
            0.45 +
            (
              (lubridate::day(Date) - 0.5) /
                lubridate::days_in_month(Date)
            ) * 0.90
        ) |>
        dplyr::filter(
          is.finite(Release_Target),
          is.finite(Inflow)
        )

      if (!is.null(release_plot)) {

        p_release <- ggplot2::ggplot() +
          ggplot2::geom_boxplot(
            data = observed,
            ggplot2::aes(
              x = Month,
              y = ObservedRelease,
              group = Month
            ),
            width = 0.65,
            fill = "grey80",
            colour = "grey25",
            outlier.alpha = 0.25
          ) +
          ggplot2::geom_line(
            data = target_plot,
            ggplot2::aes(
              x = Month_Position,
              y = Release_Target
            ),
            colour = "red",
            linewidth = 0.8
          ) +
          ggplot2::scale_x_continuous(
            breaks = 1:12,
            labels = month.abb,
            limits = c(0.5, 12.5)
          ) +
          ggplot2::labs(
            title = "Observed and WaDorule-Derived Release Targets",
            x = "Month",
            y = expression("Release (m"^3 * "/s)")
          ) +
          ggplot2::theme_bw()

        ggplot2::ggsave(
          filename = release_plot,
          plot = p_release,
          width = 12,
          height = 8,
          dpi = 300
        )
      }

      if (!is.null(inflow_plot)) {

        p_inflow <- ggplot2::ggplot() +
          ggplot2::geom_boxplot(
            data = target_plot,
            ggplot2::aes(
              x = Month,
              y = Inflow,
              group = Month
            ),
            width = 0.65,
            fill = "grey85",
            colour = "grey30",
            outlier.alpha = 0.25
          ) +
          ggplot2::geom_line(
            data = target_plot,
            ggplot2::aes(
              x = Month_Position,
              y = Release_Target
            ),
            colour = "red",
            linewidth = 0.8
          ) +
          ggplot2::scale_x_continuous(
            breaks = 1:12,
            labels = month.abb,
            limits = c(0.5, 12.5)
          ) +
          ggplot2::labs(
            title = "Inflow and WaDorule-Derived Release Targets",
            x = "Month",
            y = expression("Flow (m"^3 * "/s)")
          ) +
          ggplot2::theme_bw()

        ggplot2::ggsave(
          filename = inflow_plot,
          plot = p_inflow,
          width = 12,
          height = 8,
          dpi = 300
        )
      }
    }
  }

  return(output)
}
