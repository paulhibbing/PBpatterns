#' Internal functions for summarizing bouts
#'
#' @inheritParams rle_bouts
#'
#' @keywords internal
#' @name rle_bouts_internal
validate_rle_bout_input <- function(d, is_activity, is_wear, time_var) {

  if (is.null(is_wear)) {
    if (exists("is_wear", d)) warning(
      "A variable called is_wear exists in the dataset, but it",
      " was not\npassed in using `is_wear = \"is_wear\"`. It",
      " will be overwritten with TRUE."
    )
    is_wear <- "is_wear"
    assign("is_wear", is_wear, envir = parent.frame())
    d$is_wear <- TRUE
  }

  stopifnot(
    inherits(d, "data.frame", TRUE) == 1,
    nrow(d) > 0,
    exists(is_activity, d), is.logical(d[ ,is_activity]),
    exists(is_wear, d), is.logical(d[ ,is_wear]),
    exists(time_var, d), inherits(d[ ,time_var], "POSIXt")
  )

  assign("d", d, envir = parent.frame())

  invisible()

}

#' @rdname rle_bouts_internal
#' @keywords internal
other_info_weartime <- function(
  other_info, d, is_wear, time_var,
  valid_indices, epoch_length_sec
) {

  ## Total wear time

    total_weartime_min <-
      d[valid_indices,is_wear] %>%
      sum(.) %>%
      {. * (epoch_length_sec / 60)}

    if (is.null(other_info)) {

      other_info <- data.frame(
        total_weartime_min = total_weartime_min
      )

    } else {

      if (!exists("total_weartime_min", other_info)) {
        other_info %<>% data.frame(total_weartime_min = total_weartime_min)
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

    other_info

}
