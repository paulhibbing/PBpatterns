#' Apply the SB profiles of Hibbing et al. (2021)
#'
#' @inheritParams sb_profile
#' @param model character. The model(s) to use for the prediction
#' @param id character scalar (optional). Column name on which to divide
#'   \code{object} (if a data frame) into a list of separate objects
#' @param counts character scalar. Column name of the variable to use when
#'   classifying sedentary behavior (and wear time, depending on the function)
#' @param wear character scalar [optional]. Column name of the variable to use
#'   for determining wear time (logical vector with \code{TRUE} for wear time
#'   minutes). If no value is provided, \code{\link{profile_choi_wear}} is invoked on
#'   the \code{counts} column
#' @param sb numeric. The cut point to use for classifying sedentary behavior
#' @param valid_indices [optional] numeric/integer/logical vector specifying
#'   which indices of \code{is_sb} and {is_wear} correspond to valid measurement
#'   days (e.g. with 10+ hours of wear time on 4+ days). The default
#'   (\code{NULL}) assumes all elements are valid
#' @seealso
#'   \href{https://journals.lww.com/acsm-msse/Abstract/9000/Sedentary_Profiles__A_New_Perspective_on.95921.aspx}{Hibbing et al. (2021)}
sb_profile_Hibbing2021 <- function(object, method = "Hibbing_2021", ...) {

  UseMethod("sb_profile_Hibbing2021", object)

}

#' @rdname sb_profile_Hibbing2021
#' @export
sb_profile_Hibbing2021.bouts <- function(
  object, method = "Hibbing_2021",
  model = c("both", "decisionTree", "randomForest"), ...
) {

  model <- match.arg(model)

  tree_fail <- !requireNamespace("tree", quietly = TRUE)
  rf_fail <- !requireNamespace("randomForest", quietly = TRUE)
  both_fail <- c(tree = tree_fail, randomForest = rf_fail)

  if (model == "decisionTree" & tree_fail) stop(
    "You must install the `tree` package to use the SB profiles of ",
    " Hibbing et al. (2021) with model==\"decisionTree\"",
    call. = FALSE
  )

  if (model == "randomForest" & rf_fail) stop(
    "You must install the `randomForest` package to use the SB profiles of ",
    " Hibbing et al. (2021) with model==\"randomForest\"",
    call. = FALSE
  )

  if (model == "both" & any(both_fail)) {
    names(both_fail) %>%
    .[both_fail] %>%
    paste(collapse = ", ") %>%
    {stop(
      "You must install this/these missing package(s) to use the SB profiles of",
      "\nHibbing et al. (2021) with model==\"both\": ", ., call. = FALSE
    )}
  }


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
      " not have `bout1` or `bout5` subclass!", call. = FALSE
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
#' @export
sb_profile_Hibbing2021.data.frame <- function(
  object, method = "Hibbing_2021", model = c("both", "decisionTree", "randomForest"),
  id = NULL, counts = NULL, wear = NULL, sb = 100, valid_indices = NULL, ...
) {

  model <- match.arg(model)

  object %>%
  profile_df_check(counts, valid_indices, id, wear) %>%
  lapply(
    function(x, sb, ...) {profile_describe_sb(
      df = NULL,
      is_sb = x$counts <= sb,
      is_wear = x$is_wear,
      valid_indices = x$valid_index,
      ...
    )},
    sb, ...
  ) %>%
  lapply(sb_profile_Hibbing2021, method, model) %>%
  lapply(function(x, model) {
    if (length(x) > 1)
      do.call(data.frame, x)
    else
      stats::setNames(data.frame(x), model)
  }, model) %>%
  profile_id_bind(id)

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
    purrr::map(c, stringsAsFactors = FALSE) %>%
    purrr::map_dfr(do.call, what = data.frame) %>%
    as.list(.)
  } else {
    unlist(object, use.names = FALSE)
  }

}
