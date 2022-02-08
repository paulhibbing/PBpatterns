#' @rdname Hibbing_2021_internal
#' @keywords internal
profile_id_bind <- function(result, id, simplify = TRUE) {

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
