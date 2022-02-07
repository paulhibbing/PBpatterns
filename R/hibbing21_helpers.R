#' @name Hibbing_2021_internal
#' @title Functions used internally in the package
#'
#' @inheritParams sb_profile
#' @inheritParams sb_bout_dist
#' @param result output that may need data frame formatting (possibly ID-based)
#'
#' @details The purpose of \code{id_wear_time_verify} is to determine if the Choi
#'   wear time algorithm needs to be run and, if so, to run it (via
#'   \code{\link{choi_wear}}, with a message). To bypass,
#'   run \code{df$is_wear <- TRUE} prior to executing
#'   \code{id_wear_time_verify(df, id, wear = "is_wear")}.
#'
#'   \code{df_check_format} is a wrapper that encompasses
#'
#' @keywords internal
NULL

#' @rdname Hibbing_2021_internal
#' @keywords internal
df_check_format <- function(df, counts, valid_indices, id, wear) {

  df %>%
  counts_verify(counts) %>%
  valid_indices_verify(valid_indices) %>%
  id_wear_time_verify(id, wear)

}

#' @rdname Hibbing_2021_internal
#' @keywords internal
counts_verify <- function(df, counts) {

  if (is.null(counts) | missing(counts)) stop(
    "To run this operation on a data frame, you must pass a value for",
    "\nthe `counts` argument (character scalar giving the name of the",
    "\ncolumn on which to operate)", call. = FALSE
  )

  if (!counts %in% names(df)) {

    stop(
      "`counts` must be a column name in `df`",
      call. = FALSE
    )

  } else {

    stopifnot(sum(names(df) == counts) == 1)

    names(df) %<>% {ifelse(. == counts, "counts", .)}

  }

  df

}

#' @rdname Hibbing_2021_internal
#' @keywords internal
valid_indices_verify <- function(df, valid_indices) {

  df %>%
  {within(., {
    valid_index = if (is.null(valid_indices)) {
      TRUE
    } else {
      seq(nrow(.)) %in% valid_indices
    }
  })}

}

#' @rdname Hibbing_2021_internal
#' @keywords internal
id_wear_time_verify <- function(df, id, wear) {

  if (is.null(wear) & "is_wear" %in% names(df)) {
    stop(
      "Detected a column called `is_wear` with no value passed for ",
      "the `wear` argument.\nThis is not allowed. To fix, either rename ",
      "the current `is_wear` variable\nor rerun the ",
      "call adding this: wear = \"non-wear\"",
      call. = FALSE
    )
  }

  ## Odd logic -- if `wear` is specified, deal with it up front, otherwise,
  ## after conversion to a list via `id_verify`

  if (!is.null(wear)) {

    stopifnot(wear %in% names(df))
    location_of_wear_variable <- which(names(df) == wear)
    stopifnot(length(location_of_wear_variable) == 1)
    names(df)[location_of_wear_variable] <- "is_wear"

  }

  df %<>% id_verify(id)

  if (is.null(wear)) {

    message(
      "Applying Choi non-wear algorithm (separately for",
      " each chunk specified by `id`, if applicable)"
    )

    df %<>%
      lapply("[[", "counts") %>%
      lapply(choi_wear) %>%
      {mapply(data.frame, df, is_wear = ., SIMPLIFY = FALSE)}

  }

  df

}

#' @rdname Hibbing_2021_internal
#' @keywords internal
id_verify <- function(df, id) {

  if (!is.null(id)) {

    if (!all(
      is.character(id),
      length(id) == 1,
      id %in% names(df)
    )) {
      stop(
        "id must be a character scalar corresponding",
        " to a column name in `df`",
        call. = FALSE
      )
    }

    df %<>%
      split(df[ ,id]) %>%
      stats::setNames(
        ., sapply(., function(x, id) unique(x[ ,id]), id)
      )

  } else {

    df %<>% list(.)

  }

  df

}

#' @rdname Hibbing_2021_internal
#' @keywords internal
id_bind <- function(result, id, simplify = TRUE) {

  ## Step 1 (Stops here if no formatting desired, i.e., simplify = FALSE)

    if (!simplify) {

      return(result)

    } else {

      result %<>% do.call(rbind, .)

    }

  ## Step 2

    if (!is.null(id)) {

      data.frame(
        variable = row.names(result),
        result,
        stringsAsFactors = FALSE,
        row.names = NULL
      ) %>%
      stats::setNames(., gsub("^variable$", id, names(.)))

    } else {

      result

    }

}
