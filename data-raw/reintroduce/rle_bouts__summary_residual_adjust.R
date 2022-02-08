#' Apply residual adjustment to the output of \code{\link{sb_bout_summary}}
#' and/or \code{\link{mvpa_bout_summary}}
#'
#' @param d the output of \code{\link{sb_bout_summary}} and/or
#'   \code{\link{mvpa_bout_summary}}
#' @param verbose logical. Print updates to console
#'
#' @return The original \code{d}, with extra columns (\code{adj_*}) reflecting
#'   residual adjustments on relevant variables
#' @export
#'
#' @examples
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
#' bout_summary_residual_adjust(sb_bouts)
#'
bout_summary_residual_adjust <- function(d, verbose = FALSE) {

  d %>%
  PAutilities::residual_adjust(
    "SB_hr_day", "weartime_hr_day",
    "adj_total_SB", verbose
  ) %>%
  PAutilities::residual_adjust(
    "mean_SB_bout_min", "weartime_hr_day",
    "adj_mean_SB_bout", verbose
  ) %>%
  PAutilities::residual_adjust(
    "sb_0_14", "weartime_hr_day",
    "adj_sb_0_14", verbose
  ) %>%
  PAutilities::residual_adjust(
    "sb_15_29", "weartime_hr_day",
    "adj_sb_15_29", verbose
  ) %>%
  PAutilities::residual_adjust(
    "sb_30_Inf", "weartime_hr_day",
    "adj_sb_30_Inf", verbose
  ) %>%
  PAutilities::residual_adjust(
    "Q50_bout", "weartime_hr_day",
    "adj_median_sb_bout", verbose
  ) %>%
  PAutilities::residual_adjust(
    "MVPA_min_day", "weartime_hr_day",
    "adj_MVPA", verbose
  )

}
