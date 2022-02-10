#' Re-vectorize run length encoded data
#'
#' @param bouts output from \code{\link{analyze_bouts}}
#' @param ... arguments passed to methods
#'
#' @return a vector of individual values comprising the runs in \code{bouts}
#' @export
#'
#' @examples
#' data(example_data, package = "PBpatterns")
#' intensity <- cut(
#'   example_data$PAXINTEN,
#'   c(-Inf, 101, 760, Inf),
#'   c("SB", "LPA", "MVPA"),
#'   right = FALSE
#' )
#' bouts <- analyze_bouts(
#'   intensity, "MVPA", "Troiano_MVPA", epoch_length_sec = 60
#' )
#' expand_bouts(bouts)[910:940]
expand_bouts <- function(bouts, ...) {

  UseMethod("expand_bouts", bouts)

}

#' @rdname expand_bouts
#' @export
expand_bouts.default <- function(bouts, ...) {

  if (!is.data.frame(bouts)) stop(
    "Don\'t know how to handle data frames in `expand_bouts`",
    call. = FALSE
  )

  no_methods <- c("rle_standard", "SB_summary", "MVPA_summary")
  if (attr(bouts, "method") %in% no_methods) stop(
    "No method exists (yet?) for expanding bouts when method == ",
    attr(bouts, "method"), call. = FALSE
  )

  if ("other" %in% bouts$values) warning(
    "`expand_bouts` may behave oddly when",
    " the input includes 'other' in its values",
    call. = FALSE
  )

  target <-
    attr(bouts, "target") %T>%
    {if (!all(bouts$values == .)) stop(
      "Expecting all(bouts$values == attr(bouts, \"target\")) == TRUE",
      call. = FALSE
    )}

  anyBouts <-
    attr(bouts, "anyBouts") %T>%
    {if ((!is.logical(.)) | is.na(.)) stop(
      "attr(bouts, \"anyBouts\") is not logical and non-missing",
      call. = FALSE
    )}

  result <-
    attr(bouts, "input_length") %>%
    rep("other", .)

  if (!anyBouts) return(
    factor(result, c("other", target, "interruption"))
  )

  for (i in seq(nrow(bouts))) {

    result[bouts$start_index[i]:bouts$end_index[i]] <- target

  }

  attr(bouts, "x") %>%
  {. != target} %>%
  {. & result == target} %>%
  ifelse("interruption", result) %>%
  factor(c("other", target, "interruption"))

}
