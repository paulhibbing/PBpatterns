res_missing <- function(look_for, calculate_from, d) {
  !exists(look_for, d) &
  exists(calculate_from, d)
}

residual_prepare <- function(d, verbose = FALSE) {

  #* Wear time (hr/day)

    if (res_missing(
      "weartime_hr_day", c("total_weartime_min", "n_days"), d
    )) {
      if (verbose) print("Calculating wear time (hours/day)")
      d$weartime_hr_day <- d$total_weartime_min / 60 / d$n_days
      d %<>% PAutilities::df_reorder("weartime_hr_day", "n_days")
    }

  #* SB time (hr/day)

    if (res_missing(
      "SB_hr_day", c("total_SB_min", "n_days"), d
    )) {
      if (verbose) print("Calculating sedentary time (hours/day)")
      d$SB_hr_day <- d$total_SB_min / 60 / d$n_days
      d %<>% PAutilities::df_reorder("SB_hr_day", "total_SB_min")
    }

  #* MVPA time (min/day)

    if (res_missing(
      "MVPA_min_day", c("total_MVPA_min", "n_days"), d
    )) {
      if (verbose) print("Calculating MVPA time (minutes/day)")
      d$MVPA_min_day <- d$total_MVPA_min / d$n_days
      d %<>% PAutilities::df_reorder("MVPA_min_day", "total_MVPA_min")
    }

  #* Done

    d

}
