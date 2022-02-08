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
#'
#' @details Currently, two methods are supported, namely \code{"CRIB"} and
#'   \code{"Troiano_MVPA"}. More can easily be added over time, including
#'   those designed for specific behaviors or activity monitors.
#'
#' @seealso
#'   \href{https://journals.lww.com/acsm-msse/pages/articleviewer.aspx?year=2008&issue=01000&article=00025&type=Fulltext}{Troiano et al. (2008)}
#'
#' @export
#'
#' @examples
#' data(ex_data, package = "PAutilities")
analyze_bouts <- function(x, target, method = c("CRIB", "Troiano_MVPA"), ...) {

  if (is.character(x)) {
    warning("Casting ", substitute(x), " to factor", call. = FALSE)
    x <- as.factor(x)
  }

  stopifnot(is.factor(x), target %in% levels(x))

  method <- match.arg(method)

  switch(
    method,
    "CRIB" = crib_bouts(x, target, ...),
    "Troiano_MVPA" = troiano_mvpa_bouts(x, target, ...),
    stop(
      "Could not find appropriate function to analyze bouts based",
      " on `method = ", method, "`", call. = FALSE
    )
  )

}
