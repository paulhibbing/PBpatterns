#' Analyze bouts of various physical behaviors
#'
#' \code{analyze_bouts} is a universal function that serves as a wrapper for
#' several bout classifying methods (included below in the \code{usage}
#' section).
#'
#' @param x factor. Epoch-by-epoch behavior classifications, e.g., \code{SB},
#'   \code{LPA}, and \code{MVPA}
#' @param target the level of \code{x} for which to examine bouts
#' @param method character. The bout classifying method to use. See details.
#' @param ... arguments passed to the method function being wrapped (see
#'   \code{usage} section, above)
#'
#' @param is_wear \emph{[optional argument available for all methods]} a logical
#'   scalar (or vector, as long as its length equals the length of \code{x}),
#'   indicating whether each epoch corresponds to wear time. For length 1, the
#'   provided value is assigned for all epochs. Thus, the default value of
#'   \code{TRUE} reflects assumption that all epochs are wear epochs
#' @param valid_indices \emph{[optional argument available for all methods]} a
#'   numeric/integer/logical vector specifying which elements of \code{x}
#'   occurred on a valid day. The default (\code{NULL}) assumes all elements are
#'   valid
#' @param minimum_bout_duration_minutes \emph{[optional argument available for
#'   all methods]} a numeric filtering criterion. Bouts will be discarded if the
#'   length (in epochs) is less than this amount
#'
#' @param epoch_length_sec \emph{[REQUIRED argument for all methods]} a numeric
#'   scalar giving the epoch length of \code{x}, in seconds
#'
#' @param target_buffer_mins numeric (\code{CRIB}). Maximum separation (in
#'   minutes) between runs of \code{target}, beyond which they will not be
#'   clustered together
#' @param longest_allowable_interruption_mins numeric (\code{CRIB}). The maximum
#'   length (in minutes) for any single interruption in a valid bout
#' @param required_percent numeric (1-100; \code{CRIB}). The minimum percentage
#'   of the full bout period that must be spent engaging in the target behavior.
#'   Stated differently, this threshold stipulates that interruptions can
#'   compose no more than \code{100-required_percent} of the bout
#' @param max_n_interruptions numeric (\code{CRIB}). The maximum number of
#'   interruption events that are allowed before a bout will be considered
#'   invalid
#'
#' @param activation_window_min numeric (\code{Troiano_MVPA}). Size of window
#'   (in minutes) to use when searching for a bout activation
#' @param activation_min numeric (\code{Troiano_MVPA}). Number of minutes in
#'   \code{activation_window} that must equal \code{target} for an activation to
#'   be detected
#' @param termination_min numeric (\code{Troiano_MVPA}). Number of consecutive
#'   non-\code{target} minutes required to terminate a bout
#'
#' @param probs quantile values to return (\code{SB_summary})
#' @param patterns logical (\code{SB_summary}). Append the output with extra
#'   sedentary pattern variables?
#'
#' @details Currently, the following methods are supported:
#'   \code{"rle_standard"}, \code{"CRIB"}, \code{"Troiano_MVPA"},
#'   \code{"SB_summary"}, and \code{"MVPA_summary"}. More can easily be added
#'   over time, including more \code{*_summary} methods or others that are
#'   designed for specific behaviors or activity monitors.
#'
#' @section CRIB:
#'
#'   For help understanding output when \code{method == "CRIB"}, see
#'   \code{\link{CRIB_output}}
#'
#' @section *_summary:
#'
#'   These functions return a one-row data frame with bout summary information,
#'   rather than a data frame with one row for each individual bout
#'
#' @seealso
#' \href{https://journals.lww.com/acsm-msse/pages/articleviewer.aspx?year=2008&issue=01000&article=00025&type=Fulltext}{Troiano
#' et al. (2008)}
#' \code{\link{expand_bouts}}
#' \code{\link{plot.bouts}}
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
#' analyze_bouts(x, "MVPA", "rle_standard", epoch_length_sec = 60)[1:6, ]
#' \donttest{
#' analyze_bouts(x, "MVPA", "CRIB", 20, 5, 50, 3, 10, 60)
#' }
#' analyze_bouts(x, "MVPA", "Troiano_MVPA", epoch_length_sec = 60)
#' \donttest{
#' analyze_bouts(x, "SB", "SB_summary", is_wear = TRUE, epoch_length_sec = 60)
#' }
#' analyze_bouts(x, "MVPA", "MVPA_summary", is_wear = TRUE, epoch_length_sec = 60)
#'
#' @export
analyze_bouts <- function(
  x, target, method = c(
    "rle_standard", "CRIB", "Troiano_MVPA",
    "SB_summary", "MVPA_summary"
  ), ...
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
      "rle_standard" = rle_standard_bouts(x, target, ...),
      "CRIB" = crib_bouts(x, target, ...),
      "Troiano_MVPA" = troiano_mvpa_bouts(x, target, ...),
      "SB_summary" = sb_summary_bouts(x, target, ...),
      "MVPA_summary" = mvpa_summary_bouts(x, target, ...),
      stop(
        "Could not find appropriate function to analyze bouts based",
        " on `method = ", method, "`", call. = FALSE
      )
    ) %>%
    structure(., class = append(class(.), "bouts", 0))

}
