res_missing <- function(look_for, calculate_from, d) {
  !exists(look_for, d) &
  all(sapply(calculate_from, exists, d, USE.NAMES = FALSE))
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

  #* Total SB time (hr/day)

    if (res_missing(
      "SB_hr_day", c("total_SB_min", "n_days"), d
    )) {
      if (verbose) print("Calculating sedentary time (hours/day)")
      d$SB_hr_day <- d$total_SB_min / 60 / d$n_days
      d %<>% PAutilities::df_reorder("SB_hr_day", "total_SB_min")
    }

  #* SB time in bouts of < 15 min (hr/day)

    if (res_missing(
      "sb_0_14_hr_day", c("sb_0_14_hr", "n_days"), d
    )) {
      if (verbose) print(
        "Calculating sedentary time (hours/day) in bouts of < 15 min"
      )
      d$sb_0_14_hr_day <- d$sb_0_14_hr / d$n_days
      d %<>% PAutilities::df_reorder("sb_0_14_hr_day", "sb_0_14_hr")
    }

  #* SB time in bouts of 15-29 min (hr/day)

    if (res_missing(
      "sb_15_29_hr_day", c("sb_15_29_hr", "n_days"), d
    )) {
      if (verbose) print(
        "Calculating sedentary time (hours/day) in bouts of 15-29.9 min"
      )
      d$sb_15_29_hr_day <- d$sb_15_29_hr / d$n_days
      d %<>% PAutilities::df_reorder("sb_15_29_hr_day", "sb_15_29_hr")
    }

  #* SB time in bouts of >= 30 min (hr/day)

    if (res_missing(
      "sb_30_Inf_hr_day", c("sb_30_Inf_hr", "n_days"), d
    )) {
      if (verbose) print(
        "Calculating sedentary time (hours/day) in bouts of >= 30 min"
      )
      d$sb_30_Inf_hr_day <- d$sb_30_Inf_hr / d$n_days
      d %<>% PAutilities::df_reorder("sb_30_Inf_hr_day", "sb_30_Inf_hr")
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
