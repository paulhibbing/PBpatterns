# Documentation -----------------------------------------------------------

#' Retrieve summary metrics for bouts of sedentary behavior and/or MVPA
#'
#' @param d data frame of accelerometer data
#' @param is_sb character scalar. Name of column in which each epoch is
#'   classified as sedentary (\code{TRUE}) or non-sedentary (\code{FALSE})
#' @param is_mvpa character scalar. Name of column in which each epoch is
#'   classified as MVPA (\code{TRUE}) or non-MVPA (\code{FALSE})
#' @param is_wear character scalar. Name of column in which each epoch is
#'   classified as wear (\code{TRUE}) or non-wear (\code{FALSE}). Sleep is
#'   considered non-wear for this function. If \code{NULL} is passed (the
#'   default), function will assume all values are \code{TRUE} (i.e., no
#'   filtering will take place to exclude invalid minutes/bouts).
#' @param time_var character scalar. Name of column giving POSIX-formatted
#'   timestamps for the epochs of accelerometer data
#' @param min_bout_minutes the minimum bout length, in minutes
#' @param valid_indices user-determined row numbers of valid data, used to
#'   filter out bouts that overlap with invalid periods (e.g., on partial days
#'   or days with insufficient wear time). By default, all rows are assumed to
#'   be valid
#' @param epoch_length_sec numeric. Number of seconds represented by each row of
#'   data in \code{d}
#' @param other_info optional. A one-row data frame of other information to add
#'   to the results of sedentary bout analysis
#' @param probs bout duration quantiles to return. See \code{?stats::quantile}
#'
#' @return
#'   For \code{sb_bout_summary}, a one-row data frame containing sedentary bout
#'   analysis variables. Quantile \emph{names} take the form \code{Q*_bout},
#'   where the star is replaced with the numeric quantile, e.g. \code{Q50_bout}
#'   for median bout duration. Quantile \emph{values} are expressed in minutes,
#'   with reference to \code{epoch_length_sec}. Other time-relative sedentary
#'   variables are generally given in minutes as well (by the same reference),
#'   except for the following: \code{bout_frequency} (bouts per hour),
#'   \code{fragmentation_index} (breaks per hour), any \code{*hrs_day} variables
#'   (hours per day), and time in bouts of 0-14, 15-29, and 30+ minutes
#'   (\code{sb_0_14}, \code{sb_15_29}, and \code{sb_30_Inf}, respectively; hours
#'   per day). Percent sedentary time (called \code{SB_perc} in the output)
#'   reflects the total time of all qualifying bouts divided by the total wear
#'   time. This may differ from overall percent of time spent in sedentary
#'   behavior, e.g., if \code{min_bout_minutes} is set to a value > 1.
#'
#'   For \code{mvpa_bout_summary}
#'
#' @examples
#'
#' data(example_data, package = "PBpatterns")
#'
#' example_data$is_sb <- example_data$PAXINTEN <= 100
#' example_data$is_mvpa <- example_data$PAXINTEN >= 760
#' example_data$Timestamp <- seq(
#'   as.POSIXct("00:00:00", "UTC", format = "%H:%M:%S"),
#'   by = "1 min",
#'   length.out = nrow(example_data)
#' )
#'
#' valid_indices <- c(
#'   654:1454, 1917:2837, 3499:4266, 5216:5632,
#'   6340:7119, 7704:8555, 9118:10077
#' )
#'
#' sb_bouts <- sb_bout_summary(example_data, valid_indices = valid_indices)
#'
#' mvpa_bout_summary(
#'   example_data, valid_indices = valid_indices,
#'   other_info = sb_bouts
#' )
#'
#' @name rle_bouts_exported
NULL

# Sedentary ---------------------------------------------------------------

#' @export
#' @rdname rle_bouts_exported
sb_bout_summary <- function(
  d, is_sb = "is_sb", is_wear = NULL, time_var = "Timestamp",
  min_bout_minutes = 0, valid_indices = 1:nrow(d),
  epoch_length_sec = 60, other_info = NULL,
  probs = c(0.1, 0.2, 0.25, seq(0.3, 0.7, 0.1), 0.75, 0.8, 0.9)
) {

  ## Construct and inspect starting variables/objects

    validate_rle_bout_input(d, is_sb, is_wear, time_var)

    bouts <- rle_bouts(
      d, is_sb, is_wear, epoch_length_sec,
      min_bout_minutes, valid_indices
    )

    other_info %<>% other_info_weartime(
      d, is_wear, time_var, valid_indices, epoch_length_sec
    )

  ## Assemble the output

    bouts$lengths %>%
    stats::quantile(probs = probs) %>%
    t(.) %>%
    data.frame(.) %>%
    stats::setNames(
      ., gsub("\\.$", "_bout", names(.))
    ) %>%
    stats::setNames(
      ., gsub("^X", "Q", names(.))
    ) %>%
    {data.frame(
      n_SB_bouts = nrow(bouts),
      min_bout_threshold = min_bout_minutes,
      total_SB_min = sum(bouts$lengths),
      .,
      IQR = .$Q75_bout - .$Q25_bout,
      IDR = .$Q90_bout - .$Q10_bout,
      stringsAsFactors = FALSE
    )} %>%
    merge(other_info, .) %>%
    within({
      bout_frequency = n_SB_bouts / total_weartime_min * 60
      mean_SB_bout_min = total_SB_min / n_SB_bouts
      SB_perc = total_SB_min / total_weartime_min
      SB_hr_day = total_SB_min / n_days / 60
    }) %>%
    PAutilities::df_reorder(
      c("SB_hr_day", "mean_SB_bout_min", "SB_perc"),
      "total_SB_min"
    ) %>%
    sb_range_bouts(bouts) %>%
    usual_bout_duration(bouts) %>%
    fragmentation_index(nrow(d), bouts) %>%
    gini(bouts) %>%
    alpha(bouts)

}

# MVPA --------------------------------------------------------------------

#' @export
#' @rdname rle_bouts_exported
mvpa_bout_summary <- function(
  d, is_mvpa = "is_mvpa", is_wear = NULL, time_var = "Timestamp",
  min_bout_minutes = 0, valid_indices = 1:nrow(d),
  epoch_length_sec = 60, other_info = NULL
) {

  ## Construct and inspect starting variables/objects

    validate_rle_bout_input(d, is_mvpa, is_wear, time_var)

    other_info %<>% other_info_weartime(
      d, is_wear, time_var, valid_indices, epoch_length_sec
    )

  ## Assemble the output

    rle_bouts(
      d, is_mvpa, is_wear, epoch_length_sec,
      min_bout_minutes, valid_indices
    ) %>%
    {sum(.$lengths)} %>%
    data.frame(other_info, total_MVPA_raw = ., stringsAsFactors = FALSE) %>%
    within({
      MVPA_perc = total_MVPA_raw / total_weartime_min
      MVPA_min_day = total_MVPA_raw / n_days
    })

}
