#' Extract a summary of bout duration distribution for sedentary profiles
#'
#' @param df optional data frame input (possibly with data for multiple strata,
#'   e.g., different participants)
#' @param is_sb logical vector indicating epoch-by-epoch classifications of
#'   sedentary behavior (\code{TRUE}) or non-sedentary (\code{FALSE})
#' @param is_wear logical vector indicating epoch-by-epoch classifications of
#'   wear time (\code{TRUE}) or non-wear  (\code{FALSE})
#' @inheritParams sb_profile_Hibbing2021
#' @param minimum_bout_duration_minutes numeric. Bouts shorter than this
#'   duration will be excluded from processing
#' @param simplify logical. Collapse output for each stratum into a single data
#'   frame?
#' @param probs numeric vector. Quantiles to summarize in the output
#' @param epoch_length_sec numeric. The epoch_length of \code{x}
#'
#' @details \code{profile_describe_sb} will "dispatch" to
#'   \code{profile_describe_sb_df} if a value is provided for \code{df}.
#'   Arguments can be used accordingly, but should always be named.
#'
#' @export
#'
#' @examples
#' data(example_data, package = "PBpatterns")
#' example_data$is_wear <- TRUE
#' profile_describe_sb(
#'   example_data, counts = "PAXINTEN",
#'   wear = "is_wear", id = "PAXDAY",
#'   epoch_length_sec = 60
#' )
profile_describe_sb <- function(
  df = NULL, is_sb, is_wear, minimum_bout_duration_minutes = 5,
  valid_indices = NULL, id = NULL, counts = NULL,
  wear = NULL, sb = 100, simplify = TRUE,
  probs = c(0.1, 0.2, 0.25, seq(0.3, 0.7, 0.1), 0.75, 0.8, 0.9),
  epoch_length_sec
) {

  if (is.null(df)) {

    stopifnot(!missing(is_sb), !missing(is_wear))

    analyze_bouts(
      factor(as.character(is_sb)), "TRUE", "SB_summary",
      minimum_bout_duration_minutes, probs, FALSE,
      epoch_length_sec, is_wear, valid_indices
    )

  } else {

    profile_describe_sb_df(
      df, counts, wear, id, sb, minimum_bout_duration_minutes,
      valid_indices, simplify, probs, epoch_length_sec
    )

  }

}

#' @rdname profile_describe_sb
#' @keywords internal
profile_describe_sb_df <- function(
  df, counts = NULL, wear = NULL, id = NULL, sb = 100,
  minimum_bout_duration_minutes = 5, valid_indices = NULL, simplify = TRUE,
  probs = c(0.1, 0.2, 0.25, seq(0.3, 0.7, 0.1), 0.75, 0.8, 0.9),
  epoch_length_sec
) {

  df %>%
  profile_df_check(counts, valid_indices, id, wear) %>%
  lapply(
    function(x, sb, minimum_bout_duration_minutes, probs, epoch_length_sec) {
      profile_describe_sb(
        is_sb = x$counts <= sb,
        is_wear = x$is_wear,
        minimum_bout_duration_minutes = minimum_bout_duration_minutes,
        valid_indices = x$valid_index,
        probs = probs,
        epoch_length_sec = epoch_length_sec
      )
    }, sb, minimum_bout_duration_minutes, probs, epoch_length_sec
  ) %>%
  profile_id_bind(id, simplify)

}
