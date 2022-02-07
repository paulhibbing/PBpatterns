#' Implement the sedentary profiles
#'
#' @param object input (either data frame or output from
#'   \code{\link{sb_bout_dist}})
#' @param method character. The profile method to use (currently only supports
#'   \code{Hibbing_2021}).
#' @param ... further arguments passed to \code{\link{sb_bout_dist}} (used only
#'   in data frame method)
#'
#' @return one or more profile assignments
#' @export
#'
#' @examples
#' data(example_data, package = "PBpatterns")
#' sb_profile(example_data, counts = "PAXINTEN")
#' sb_profile(
#'   example_data, counts = "PAXINTEN", model = "randomForest"
#' )
#' sb_profile(example_data, id = "PAXDAY", counts = "PAXINTEN")
#' sb_profile(
#'   example_data, id = "PAXDAY",
#'   counts = "PAXINTEN", model = "decisionTree"
#' )
#'
#' @seealso
#'   \code{\link{sb_profile_Hibbing2021}}
#'   \href{https://journals.lww.com/acsm-msse/Abstract/9000/Sedentary_Profiles__A_New_Perspective_on.95921.aspx}{Hibbing et al. (2021)}
sb_profile <- function(object, method = "Hibbing_2021", ...) {

  method <- match.arg(method)

  if (method == "Hibbing_2021") {

    requireNamespace("tree", quietly = TRUE)
    requireNamespace("randomForest", quietly = TRUE)

    UseMethod("sb_profile_Hibbing2021", object)

  } else {
    stop("No sb_profile framework exists for method == ", method, call. = FALSE)
  }

}

#' Apply the SB profiles of Hibbing et al. (2021)
#'
#' @inheritParams sb_profile
#' @param model character. The model(s) to use for the prediction
sb_profile_Hibbing2021 <- function(object, method = "Hibbing_2021", ...) {
  UseMethod("sb_profile_Hibbing2021", object)
}

#' @rdname sb_profile_Hibbing2021
#' @export
sb_profile_Hibbing2021.bout <- function(
  object, method = "Hibbing_2021",
  model = c("both", "decisionTree", "randomForest"), ...
) {

  model <- match.arg(model)

  if (inherits(object, "bout1")) {

    dt <- stats::predict(.$tree1_Hibbing2021, object, "class")
    rf <-
      stats::predict(.$forest1_Hibbing2021, object) %>%
      unname(.)

  } else if (inherits(object, "bout5")) {

    dt <- stats::predict(.$tree5_Hibbing2021, object, "class")
    rf <-
      stats::predict(.$forest5_Hibbing2021, object) %>%
      unname(.)

  } else {

    stop(
      "cannot predict SB profile because object does",
      " not have `bout1` or `bout5` class!", call. = FALSE
    )

  }

  both <-
    list(dt, rf) %>%
    stats::setNames(c("decisionTree", "randomForest"))

  switch(
    model,
    "both" = both,
    "decisionTree" = dt,
    "randomForest" = rf,
    NULL
  )

}

#' @rdname sb_profile_Hibbing2021
#' @param id character scalar (optional). Column name on which to divide
#'   \code{object} (if a data frame) into a list of separate objects
#' @param counts character scalar. Column name of the variable to use when
#'   classifying sedentary behavior (and wear time, depending on the function)
#' @param wear character scalar [optional]. Column name of the variable to use
#'   for determining wear time (logical vector with \code{TRUE} for wear time
#'   minutes). If no value is provided, \code{\link{choi_wear}} is invoked on
#'   the \code{counts} column
#' @param sb integer. The cut point to use for classifying sedentary behavior
#' @param valid_indices integer vector (optional) specifying which indices of
#'   \code{is_sb} and {is_wear} correspond to valid measurement days (e.g. with
#'   10+ hours of wear time on 4+ days)
#' @export
sb_profile_Hibbing2021.data.frame <- function(
  object, method = "Hibbing_2021", model = c("both", "decisionTree", "randomForest"),
  id = NULL, counts = NULL, wear = NULL, sb = 100, valid_indices = NULL, ...
) {

  model <- match.arg(model)

  object %>%
  df_check_format(counts, valid_indices, id, wear) %>%
  lapply(
    function(x, sb, ...) {sb_bout_dist(
      df = NULL,
      is_sb = x$counts <= sb,
      is_wear = x$is_wear,
      valid_indices = which(x$valid_index),
      ...
    )},
    sb, ...
  ) %>%
  lapply(sb_profile, method, model) %>%
  lapply(function(x, model) {
    if (length(x) > 1)
      do.call(data.frame, x)
    else
      stats::setNames(data.frame(x), model)
  }, model) %>%
  id_bind(id)

}

#' @rdname sb_profile_Hibbing2021
#' @export
sb_profile_Hibbing2021.list <- function(
  object, method = "Hibbing_2021",
  model = c("both", "decisionTree", "randomForest"), ...
) {

  stopifnot(sapply(object, inherits, c("bout1", "bout5")))
  model <- match.arg(model)

  object %<>% lapply(sb_profile_Hibbing2021, method = method, model = model)

  if (model == "both") {
    object %>%
    lapply(c, stringsAsFactors = FALSE) %>%
    lapply(do.call, what = data.frame) %>%
    do.call(rbind, .) %>%
    as.list(.)
  } else {
    unlist(object, use.names = FALSE)
  }

}
