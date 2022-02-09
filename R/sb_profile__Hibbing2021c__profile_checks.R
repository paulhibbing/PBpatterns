# profile_df_check (main) -------------------------------------------------

#' @rdname Hibbing_2021_internal
#' @keywords internal
profile_df_check <- function(df, counts, valid_indices, id, wear) {

  df %>%
  profile_counts_check(counts) %>%
  profile_indices_check(valid_indices) %>%
  profile_id_wear_check(id, wear)

}

# profile_df_check (primary helpers) --------------------------------------

#' @rdname Hibbing_2021_internal
#' @keywords internal
profile_counts_check <- function(df, counts) {

  if (is.null(counts) | missing(counts)) stop(
    "To run this operation on a data frame, you must pass a value for",
    "\nthe `counts` argument (character scalar giving the name of the",
    "\ncolumn on which to operate)", call. = FALSE
  )

  if (!exists(counts, df)) {

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
profile_indices_check <- function(df, valid_indices) {

  valid_indices %>%
  valid_valid_indices(df, TRUE) %>%
  {within( df, {valid_index = .} )}

}

#' @rdname Hibbing_2021_internal
#' @keywords internal
profile_id_wear_check <- function(df, id, wear) {

  if (is.null(wear) & exists("is_wear", df)) {
    stop(
      "Detected a column called `is_wear` with no value passed for ",
      "the `wear` argument.\nThis is not allowed. To fix, either rename ",
      "the current `is_wear` variable\nor rerun the ",
      "call adding this: wear = \"is_wear\"",
      call. = FALSE
    )
  }

  ## Odd logic -- if `wear` is specified, deal with it up front, otherwise,
  ## after conversion to a list via `id_verify`

  if (!is.null(wear)) {

    stopifnot(exists(wear, df))
    location_of_wear_variable <- which(names(df) == wear)
    stopifnot(length(location_of_wear_variable) == 1)
    names(df)[location_of_wear_variable] <- "is_wear"

  }

  df %<>% profile_id_verify(id)

  if (is.null(wear)) {

    message(
      "Applying Choi non-wear algorithm (separately for",
      " each chunk specified by `id`, if applicable)"
    )

    df %<>%
      lapply("[[", "counts") %>%
      lapply(profile_choi_wear) %>%
      {mapply(data.frame, df, is_wear = ., SIMPLIFY = FALSE)}

  }

  df

}

# profile_df_check (secondary helpers) -----------------------------------

#' @rdname Hibbing_2021_internal
#' @keywords internal
profile_id_verify <- function(df, id) {

  if (!is.null(id)) {

    if (!all(
      is.character(id),
      length(id) == 1,
      exists(id, df)
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
profile_choi_wear <- function(counts) {

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
