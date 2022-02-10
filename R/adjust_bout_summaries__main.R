#' Apply wear time residual adjustment to the output of bout summary methods
#'
#' Applies to output from the \code{*_summary} methods of
#' \code{\link{analyze_bouts}}, in conjunction with information from
#' \code{\link{summarize_weartime}}
#'
#' @param d the output of to adjust
#' @param verbose logical. Print updates to console
#'
#' @return The original \code{d}, with extra columns (\code{adj_*}) reflecting
#'   residual adjustments on relevant variables
#' @export
#'
#' @examples
#'
#' ## Set up the data
#'
#'   data(example_data, package = "PBpatterns")
#'
#'   valid_indices <- c(
#'     654:1454, 1917:2837, 3499:4266, 5216:5632,
#'     6340:7119, 7704:8555, 9118:10077
#'   )
#'
#'   example_data$Timestamp <- seq(
#'     as.POSIXlt(Sys.Date()), by = "1 min", length.out = nrow(example_data)
#'   )
#'
#'   example_data$is_wear <- TRUE
#'
#'   example_data$intensity <- cut(
#'     example_data$PAXINTEN,
#'     c(-Inf, 101, 760, Inf),
#'     c("SB", "LPA", "MVPA"),
#'     right = FALSE
#'   )
#'
#'   example_data$valid_index <- 1:nrow(example_data) %in% valid_indices
#'
#' ## Set up the analysis (It needs to have more than one data
#' ## point, so we will stratify by PAXDAY for illustration)
#'
#'   weartime_info <- purrr::map_df(
#'     split(example_data, example_data$PAXDAY),
#'     ~ summarize_weartime(.x, "is_wear", "Timestamp", .x$valid_index)
#'   )
#'
#'   sb_bouts <- purrr::map_df(
#'     split(example_data, example_data$PAXDAY),
#'      ~ analyze_bouts(
#'        .x$intensity, "SB", "SB_summary",
#'        is_wear = .x$is_wear,
#'        valid_indices = .x$valid_index,
#'        epoch_length_sec = 60
#'      )
#'   )
#'
#'   mvpa_bouts <- purrr::map_df(
#'     split(example_data, example_data$PAXDAY),
#'     ~ analyze_bouts(
#'       .x$intensity, "MVPA", "MVPA_summary",
#'       is_wear = .x$is_wear,
#'       valid_indices = .x$valid_index,
#'       epoch_length_sec = 60
#'       )
#'   )
#'
#'   d <- merge(weartime_info, sb_bouts)
#'
#'   d <- merge(d, mvpa_bouts)
#'
#' ## Run the analysis
#'
#'   adjust_bout_summaries(d)
#'
adjust_bout_summaries <- function(d, verbose = FALSE) {

  residual_prepare(d, verbose) %>%
  PAutilities::residual_adjust(
    "SB_hr_day", "weartime_hr_day",
    "adj_total_SB", verbose
  ) %>%
  PAutilities::residual_adjust(
    "mean_SB_bout_min", "weartime_hr_day",
    "adj_mean_SB_bout", verbose
  ) %>%
  PAutilities::residual_adjust(
    "sb_0_14_hr_day", "weartime_hr_day",
    "adj_sb_0_14", verbose
  ) %>%
  PAutilities::residual_adjust(
    "sb_15_29_hr_day", "weartime_hr_day",
    "adj_sb_15_29", verbose
  ) %>%
  PAutilities::residual_adjust(
    "sb_30_Inf_hr_day", "weartime_hr_day",
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
