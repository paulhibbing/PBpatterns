if(getRversion() >= "2.15.1") utils::globalVariables(c(
  ".", "x", "y", "end_index", "epoch", "fragmentation_index", "Freq", "n_days",
  "n_SB_bouts", "timestamp", "total_MVPA_min", "total_SB_min", "total_weartime_min",
  "valid_index", "counts", "id", "values", "valid_indices", "value", "wear", "sb",
  "length_value", "length_total", "bouts"
))

#' Analyze Patterns of Physical Behavior
#'
#' @description Useful for assessing sedentary patterns and profiles, bouts of
#'   physical activity or sleep, and more.
#'
#' @seealso https://github.com/paulhibbing/PBpatterns
#'
#' @importFrom magrittr %>% %T>% %<>% %$%
#' @import ggplot2
#' @docType package
#' @name PBpatterns
NULL

#' Data for use in code examples throughout the \code{PBpatterns} package
#'
#' This is a minute-by-minute data set for one participant in the 2003-2006
#' National Health and Nutrition Examination Survey (NHANES). For details, see the
#' \href{https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/PAXRAW_D.htm}{NHANES documentation}.
#' (Link is to the 2005-2006 documentation, as an additional variable (PAXSTEP)
#' was included in that cycle.)
"example_data"
