#' Internal functions providing information about sedentary bout patterns
#'
#' These functions are invoked in the \code{\link{analyze_bouts}} when
#' \code{method == "SB_summary"} unless suppressed with an additional
#' \code{patterns = FALSE} argument.
#'
#' @param d one-row data frame with bout information
#' @param bouts data frame with information for each bout, based on
#'   \code{PAutilities::index_runs}
#' @param x original data stream
#' @param epoch_length_sec numeric. The epoch length of \code{x}
#'
#' @keywords internal
#' @name SB_patterns
NULL
