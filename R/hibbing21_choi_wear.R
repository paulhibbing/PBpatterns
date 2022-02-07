#' Retrieve wear information for an NHANES accelerometer data file
#'
#' This is a wrapper around \code{PhysicalActivity::wearingMarking} that takes
#' into account the non-POSIX formatting of NHANES timestamps
#'
#' @inheritParams sb_profile
#' @inheritParams sb_profile_Hibbing2021
#'
#' @return a vector of wear time information (\code{TRUE} means wearing and
#'   \code{FALSE} means not wearing)
#' @export
#'
#' @examples
#' data(example_data, package = "PBpatterns")
#' choi_wear(example_data$PAXINTEN)
choi_wear <- function(counts) {

  if (!requireNamespace("PhysicalActivity", quietly = TRUE)) {
    stop("Run `install.packages(\"PhysicalActivity\") and try again")
  }

  invisible(utils::capture.output(

    result <-
      as.POSIXct("2000-01-01", "UTC") %>%
      seq(by = "1 min", length.out = length(counts)) %>%
      as.character(.) %>%
      data.frame(TimeStamp = ., counts = counts) %>%
      PhysicalActivity::wearingMarking(
        perMinuteCts = 1, cts = "counts",
        getMinuteMarking = TRUE
      ) %>%
      {.$wearing %in% "w"}

  ))

  result

}
