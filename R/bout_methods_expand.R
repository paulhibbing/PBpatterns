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
#' \donttest{
#' bouts <- analyze_bouts(intensity, target = "MVPA", target_buffer = 1)
#' tail(bout_expand(bouts), 40)
#' }
bout_expand <- function(bouts, ...) {

  UseMethod("bout_expand", bouts)

}

#' @rdname bout_expand
#' @export
bout_expand.default <- function(bouts, ...) {

  if ("other" %in% bouts$values) warning(
    "`bout_expand` may behave oddly when",
    " the input includes 'other' in its values"
  )

  target <-
    attr(bouts, "target") %T>%
    {stopifnot(all(bouts$values == .))}

  anyBouts <- attr(bouts, "anyBouts")

  result <-
    attr(bouts, "input_length") %>%
    rep("other", .)

  if (!anyBouts) return(result)

  for (i in seq(nrow(bouts))) {

    result[bouts$start_index[i]:bouts$end_index[i]] <- target

  }

  attr(bouts, "x") %>%
  {. != target} %>%
  {. & result == target} %>%
  ifelse("interruption", result) %>%
  factor(c("other", target, "interruption"))

}
