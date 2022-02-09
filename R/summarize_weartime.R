#' Generate a summary of accelerometer wear time
#'
#' @param d data frame of accelerometer data
#' @param is_wear character. Column name of a logical variable indicating
#'   whether each epoch is wear time (\code{TRUE}) or not (\code{FALSE})
#' @param time_var character. Column name of a timestamp variable
#' @param valid_indices optional vector (logical or integer/numeric) indicating
#'   which rows in \code{d} correspond with valid wear days. If no value is
#'   provided, all rows will be considered valid
#' @param other_info optional one-row data frame containing additional
#'   information to bind with the results of wear time summarizing
#'
#' @return A one-row data frame containing the wear time summary and anything
#'   else passed in through \code{other_info}
#'
#' @export
#'
#' @examples
#' set.seed(610)
#' data(example_data, package = "PBpatterns")
#'
#' example_data$timestamp <- seq(
#'   Sys.time(), by = "1 min", length.out = nrow(example_data)
#' )
#' example_data$is_wear <- sample(c(FALSE, TRUE), nrow(example_data), TRUE)
#' example_data$intensity <- cut(
#'   example_data$PAXINTEN,
#'   c(-Inf, 101, 760, Inf),
#'   c("SB", "LPA", "MVPA"),
#'   right = FALSE
#' )
#'
#' extra_info <- analyze_bouts(
#'   example_data$intensity, "SB", "SB_summary",
#'   is_wear = example_data$is_wear
#' )
#'
#' ## Standalone usage
#' summarize_weartime(example_data, "is_wear", "timestamp")
#'
#' ## In conjunction with extra information
#' summarize_weartime(
#'   example_data, "is_wear", "timestamp",
#'   other_info = extra_info
#' )
summarize_weartime <- function(
  d, is_wear, time_var, valid_indices, other_info = NULL
) {

  ## Initial tests

    stopifnot(
      is.data.frame(d),
      exists(is_wear, d), is.logical(d[ ,is_wear]),
      exists(time_var, d), inherits(d[ ,time_var], "POSIXt")
    )

    if (missing(valid_indices)) {
      valid_indices <- rep(TRUE, nrow(d))
    } else {
      stopifnot(
        inherits(valid_indices, c("integer", "numeric", "logical"))
      )
    }

  ## Epoch length

    epoch_length_sec <-
      d[ ,time_var] %>%
      utils::head(nrow(d) * 0.1) %>%
      diff(.) %>%
      as.numeric("secs") %>%
      unique(.) %T>%
      {if (length(.) != 1) stop(
        "`d` does not have a consistent epoch length", call. = FALSE
      )}

  ## Total wear time

    total_weartime_min <-
      d[valid_indices,is_wear] %>%
      sum(.) %>%
      {. * (epoch_length_sec / 60)}

    if (is.null(other_info)) {

      other_info <- data.frame(
        epoch_length_sec = epoch_length_sec,
        total_weartime_min = total_weartime_min
      )

    } else {

      stopifnot(is.data.frame(other_info), nrow(other_info) == 1)

      if (!exists("epoch_length_sec", other_info)) {
        other_info %<>% data.frame(
          epoch_length_sec = epoch_length_sec, .
        )
      }

      if (!exists("total_weartime_min", other_info)) {
        other_info %<>% data.frame(
          total_weartime_min = total_weartime_min, .
        )
      }

    }

  ## Number of days

    if (!exists("n_days", other_info)) {
      other_info$n_days <-
        d[valid_indices ,time_var] %>%
        as.Date(.) %>%
        unique(.) %>%
        length(.)
    }

  ## Wear time (hr/day)

    if (!exists("weartime_hr_day", other_info)) {
      other_info %<>% within({
        weartime_hr_day = total_weartime_min / 60 / n_days
      })
    }

  ## Finish up

    c("epoch_length_sec", "total_weartime_min", "n_days", "weartime_hr_day") %>%
    {c(., setdiff(names(other_info), .))} %>%
    other_info[ ,.]

}
