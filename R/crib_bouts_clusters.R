#' @keywords internal
#' @rdname CRIB_internal
crib_clusters <- function(
  x, target, target_buffer, required_percent,
  longest_allowable_interruption, max_n_interruptions
) {

  x %<>% dichotomize_intensity(target)

  PAutilities::index_runs(x) %>%
  within({
    group = cumsum(values == "other" & lengths >= target_buffer)
  }) %>%
  split(., .$group) %>%

  lapply(function(df, target) df[df$values == target, ], target) %>%
  .[sapply(., function(df) nrow(df) > 0)] %>%

  purrr::map_df(
    crib_stratum, x, target, required_percent,
    longest_allowable_interruption, max_n_interruptions
  ) %>%

  structure(., h = .$h) %>%
  within({
    h = NULL
    group = NULL
  })

}

#' @keywords internal
#' @rdname CRIB_internal
crib_stratum <- function(
  runs, x, target, required_percent, longest_allowable_interruption,
  max_n_interruptions
) {

  if (nrow(runs) == 1) {
    result <-
      collapse_runs(runs, target) %>%
      cbind(h = NA, run_info(runs), .)
    return(result)
  }

  if (nrow(runs) > 100) message(
    "\nDetected more than 100 potential bouts",
    " (exact n = ", nrow(runs), ").",
    "\nAlgorithm will take a long time to run.",
    "\nTo circumvent this, consider splitting `x`",
    " into subsets,\n  e.g. by analyzing ",
    "each day of data separately, or by",
    "\n  setting `target_buffer` to a smaller value."
  )

  tree <-
    ## Calculate distance between each event, i.e.,
    ## time elapsed from the end of one event to
    ## the start of another. Keep in mind the end
    ## index represents the interval [end, end + 1),
    ## so the time elapsed formula is
    ## `start_index - (end_index + 1)`.
    ## This call to mapply is a little convoluted
    ## because it populates a full square matrix when
    ## we're only interested in the lower triangle.
    ## (The upper triangle calculations represent
    ## reverse distances that are off by 2 because
    ## the `- 1` operation should be a `+ 1` when
    ## going in reverse.) This is not a problem
    ## because `as.dist` will cut out all but the
    ## lower triangle.
    mapply(
      function(end, starts) starts - end - 1,
      runs$end_index,
      MoreArgs = list(starts = runs$start_index)
    ) %>%
    structure(dimnames = list(
      row.names(runs), row.names(runs)
    )) %>%
    stats::as.dist(.) %>%
    stats::hclust("single")

  results <- mapply(
    crib_test_stratum,
    h = c(
      0,
      unique(tree$height) ## Ties produce redundant values
    ),
    MoreArgs = list(
      x = x, target = target, runs = runs, tree = tree,
      required_percent = required_percent,
      longest_allowable_interruption = longest_allowable_interruption,
      max_n_interruptions = max_n_interruptions
    ),
    SIMPLIFY = FALSE
  )

  sapply(results, "[[", "meets_requirements") %>%
  which(.) %>%
  max(.) %>%
  results[[.]] %>%
  .$result

}

#' @keywords internal
#' @rdname CRIB_internal
crib_test_stratum <- function(
  x, target, runs, tree, h, required_percent,
  max_n_interruptions, longest_allowable_interruption
) {

  result <-
    stats::cutree(tree, h = h) %T>%
    {stopifnot(length(.) == nrow(runs))} %>%
    {within(runs, {group = .})} %>%
    split(., .$group) %>%
    lapply(function(run, x, target) {

      info <- run_info(run)

      seq(info$start_index, info$end_index) %>%
      x[.] %>%
      PAutilities::index_runs(.) %>%
      within({group = 1}) %>%
      collapse_runs(target) %>%
      cbind(info, .)

    }, x = x, target = target) %>%
    do.call(rbind, .) %>%
    cbind(h = h, .)

  meets_requirements <- all(
    result$percent_time_engaged >= required_percent,
    result$longest_interruption_event <= longest_allowable_interruption,
    result$n_interruption_events <= max_n_interruptions
  )

  list(
    meets_requirements = meets_requirements,
    result = result
  )

}
