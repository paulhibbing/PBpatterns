#' @rdname regularity_index
#' @export
regularity_index.factor <- function(
  values, timestamps, target,
  min_days = 3, numeric = TRUE, ...
) {

  ## Setup

    n_levels <- dplyr::n_distinct(values)

    if (!missing(target)) {
      stopifnot(target %in% values)
      target %<>% as.character(.)
    }


  ## Preliminary checks

    # 1) Is there variability?
    if (n_levels == 1) {
      warning("All elements of `values` are the same. Returning 100.")
      return(as_regularity(100, values, timestamps, numeric = numeric))
    }

    # 2) Are there excess levels?
    if (n_levels != 2) {

      if (missing(target)) {
        target <-
          dplyr::first(values) %>%
          as.character(.)
        warning(
          "More than 2 levels exist in `values`. Dichotomizing as '", target,
          "' and 'other'.\n  Suppress this warning by setting a value for `target`."
        )
      }

      values <-
        dichotomize_behavior(values, target) %>%
        factor(c(target, "other"))

    }


  ## Execute the main process

    info <-
      epoch_number(timestamps) %>%
      regularity_info(values, timestamps, .)

    epoch_results <-
      info %>%
      dplyr::arrange(epoch, timestamp) %>%
      dplyr::group_by(epoch) %>%
      dplyr::summarise(
        matches =
          as.numeric(value) %>%
          diff(.) %>%
          {. == 0} %>%
          sum(.),
        pairs = dplyr::n() - 1
      ) %T>%
      check_sparse("pairs", min_days)


  ## Format the output

    as_regularity(
      200 * sum(epoch_results$matches) / sum(epoch_results$pairs) - 100,
      info = list(
        raw = info,
        epoch = epoch_results
      ),
      numeric = numeric
    )

}
