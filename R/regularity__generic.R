#' Calculate regularity indices for analyzing time-of-day consistency of
#' recurring behaviors
#'
#' @param values a vector of values to analyze
#' @param timestamps a vector of \code{POSIX-formatted} timestamps. Must have
#'   the same length as \code{values}, and it must have element-wise, one-to-one
#'   correspondence with the elements of \code{values}
#' @param ... further arguments passed to methods
#'
#' @param min_days the minimum number of days that are required to ensure proper
#'   analysis for each time of day. A warning will issue for any daytimes (i.e.,
#'   epochs) that fall below this threshold.
#' @param numeric logical. Return the score as a numeric object (default)? If
#'   \code{FALSE}, a \code{regularity} object will be returned instead.
#' @param target character scalar. When \code{values} is a factor, which level
#'   should be emphasized? All other levels will be collapsed into a single
#'   'other' category.
#' @param n_bands the number of difference bands that will be examined for a
#'   continuous input
#' @param band_size the size of each difference band that will be examined for a
#'   continuous input
#'
#' @return A regularity index whose value lies between -100 and +100.
#' @details When setting \code{numeric = TRUE}, only the numeric value is
#' returned. Otherwise, a \code{regularity} object is returned, whose value
#' equals the numeric value, and whose attributes include additional information
#' from the analysis (potentially useful for debugging).
#'
#' @export
#'
#' @examples
#' set.seed(1919)
#' values <- factor(
#'   sample(c("Awake", "Asleep"), 6, TRUE)
#' )
#' timestamp_offsets <- 43200 * 0:5
#' regularity_index(values, as.POSIXlt(Sys.Date()) + timestamp_offsets, min_days = 2)
#' regularity_index(values, as.POSIXlt(Sys.Date()) + timestamp_offsets, min_days = 2, numeric = FALSE)
regularity_index <- function(values, timestamps, ...) {

  stopifnot(
    !anyNA(values),
    inherits(timestamps, "POSIXt"),
    length(timestamps) == length(values)
  )

  UseMethod("regularity_index", values)

}
