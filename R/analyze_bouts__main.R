#' Analyze bouts of various physical behaviors
#'
#' This is a universal function that serves as a wrapper for several bout
#' classifying methods.
#'
#' @param x factor. Epoch-by-epoch behavior classifications, e.g., \code{SB},
#'   \code{LPA}, and \code{MVPA}
#' @param target the level of \code{x} for which to examine bouts
#' @param method character. The bout classifying method to use. See details.
#' @param ... arguments passed to the method functions being wrapped
#' @param minimum_bout_length numeric filtering criterion (\code{rle_standard},
#'   \code{CRIB}, \code{SB_summary}). Bouts will be discarded if
#'   \code{length_value} (see below) is less than this amount.
#' @param is_wear logical vector (\code{rle_standard}, \code{SB_summary}) having
#'   the same length as \code{x}, indicating whether each epoch corresponds to
#'   wear time. (Default value of \code{TRUE} reflects assumption that all
#'   epochs are wear epochs)
#' @param valid_indices numeric/integer vector (\code{rle_standard},
#'   \code{SB_summary}) specifying which elements of \code{x} occurred on a
#'   valid day. (Default value assumes all elements are valid)
#' @param activation_window numeric (\code{Troiano_MVPA}). Size of window
#'   (number of epochs) to use when searching for a bout activation
#' @param activation_min numeric (\code{Troiano_MVPA}). Number of epochs in the
#'   activation window that must equal \code{target} for an activation to be
#'   detected
#' @param termination_min numeric (\code{Troiano_MVPA}). Number of consecutive
#'   non-\code{target} epochs required to terminate a bout
#' @param target_buffer numeric (\code{CRIB}). Maximum separation between runs
#'   of \code{target}, beyond which they will not be clustered together
#' @param longest_allowable_interruption numeric (\code{CRIB}). The maximum
#'   length for any single interruption in a valid bout
#' @param required_percent numeric (1-100; \code{CRIB}). The minimum percentage
#'   of the full bout period that must be spent engaging in the target behavior.
#'   Stated differently, this threshold stipulates that interruptions can
#'   compose no more than \code{100-required_percent} of the bout
#' @param max_n_interruptions numeric (\code{CRIB}). The maximum number of
#'   interruption events that are allowed before a bout will be considered
#'   invalid
#'
#' @note Users should note that these functions (input, code, and output)
#'   operate by index, not duration. That is, the functions cannot tell if each
#'   data point represents a 1-s period, a 1-min period, or anything else. Users
#'   need to take this into consideration when deciding which settings to use
#'   (e.g. \code{longest_allowable_interruption = 12} to allow for 1-min
#'   interruptions if input data are in 5-s epochs) and how to interpret the
#'   output (e.g. \code{length_value == 12} corresponds to one minute if data
#'   are in 5-s epochs).
#'
#' @details Currently, the following methods are supported:
#'   \code{"rle_standard"}, \code{"Troiano_MVPA"}, \code{"CRIB"}, and
#'   \code{"SB_summary"}. More can easily be added over time, including those
#'   designed for specific behaviors or activity monitors.
#'
#'   \code{CRIB} returns a data frame formatted as follows:
#'
#'   \describe{ \item{start_index}{The start index of the bout period}
#'   \item{end_index}{The end index of the bout period} \item{values}{The target
#'   behavior (the name is a vestige of \code{PAutilities::index_runs}})
#'   \item{n_total_events}{The number of distinct behavior runs in the bout
#'   period. Equal to the sum of \code{n_value_events} and
#'   \code{n_interruption_events}} \item{n_value_events}{The number of distinct
#'   occurrences of the target behavior} \item{n_interruption events}{The number
#'   of distinct occurrences of interruptive behavior} \item{length_total}{The
#'   total number of indices comprising the bout period} \item{length_value}{The
#'   number of indices spent engaged in the target behavior}
#'   \item{length_interruption}{The number of indices spent engaged in
#'   interruptive behavior} \item{longest_interruption_event}{The number of
#'   indices comprising the longest interruption event}
#'   \item{percent_time_engaged}{The percentage (0-100) of \code{length_total}
#'   that was spent engaging in \code{target}, equal to \code{length_value /
#'   length_total * 100}} }
#'
#' @seealso
#' \href{https://journals.lww.com/acsm-msse/pages/articleviewer.aspx?year=2008&issue=01000&article=00025&type=Fulltext}{Troiano
#' et al. (2008)} \code{\link{expand_bouts}}
#'
#' @examples
#' data(example_data, package = "PBpatterns")
#'
#' x <- cut(
#'   example_data$PAXINTEN,
#'   c(-Inf, 101, 760, Inf),
#'   c("SB", "LPA", "MVPA"),
#'   right = FALSE
#' )
#'
#' analyze_bouts(x, "MVPA", "rle_standard")[1:6, ]
#' \donttest{
#' analyze_bouts(x, "MVPA", "CRIB", 20, 5, 50, 3, 10)
#' }
#' analyze_bouts(x, "MVPA", "Troiano_MVPA")
#' \donttest{
#' analyze_bouts(x, "SB", "SB_summary", is_wear = TRUE)
#' }
#'
#' @export
analyze_bouts <- function(
  x, target, method = c("rle_standard", "CRIB", "Troiano_MVPA", "SB_summary"), ...
) {

  #* Initial tests and formatting

    ex <- substitute(x)
    tar <- substitute(target)

    if (is.character(x)) {
      warning("Casting ", deparse(ex), " to factor", call. = FALSE)
      x <- as.factor(x)
    } else {
      stopifnot(is.factor(x))
    }

    if (!target %in% levels(x)) stop(
      deparse(tar), " is not a level in `", deparse(ex),
      "` -- is this an error, or are there no bouts in this dataset?",
      call. = FALSE
    )

    method <- match.arg(method)

  #* "Dispatch"

    switch(
      method,
      "rle_standard" = rle_bouts(x, target, ...),
      "CRIB" = crib_bouts(x, target, ...),
      "Troiano_MVPA" = troiano_mvpa_bouts(x, target, ...),
      "SB_summary" = sb_summary_bouts(x, target, ...),
      stop(
        "Could not find appropriate function to analyze bouts based",
        " on `method = ", method, "`", call. = FALSE
      )
    ) %>%
    structure(., class = append(class(.), "bouts", 0))

}
