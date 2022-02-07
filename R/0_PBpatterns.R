if(getRversion() >= "2.15.1") utils::globalVariables(c(
  ".", "end_index", "fragmentation_index", "Freq", "n_days",
  "n_SB_bouts", "total_mvpa_raw", "total_MVPA_raw", "total_SB_min",
  "total_weartime_min", "valid_index", "counts", "id",
  "valid_indices", "wear", "sb"
))

#' Analyze Patterns of Physical Behavior
#'
#' @description Useful for assessing sedentary patterns and profiles, bouts of
#'   physical activity or sleep, and more.
#'
#' @seealso https://github.com/paulhibbing/PBpatterns
#'
#' @importFrom magrittr %>% %T>% %<>% %$%
#' @docType package
#' @name PBpatterns
NULL
