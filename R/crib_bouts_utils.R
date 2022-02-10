#' @keywords internal
#' @rdname CRIB_internal
run_info <- function(run) {
  data.frame(
    start_index = run$start_index[1],
    end_index = run$end_index[nrow(run)],
    group = unique(run$group),
    values = unique(run$values),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' @keywords internal
#' @rdname CRIB_internal
collapse_runs <- function(x, target) {

  split(x, x$group) %>%
  purrr::map_df(function(df, target) {

    cases <- df$values == target

    data.frame(

      n_total_events = nrow(df),
      n_value_events = sum(cases),
      n_interruption_events = sum(!cases),

      length_total = sum(df$lengths),
      length_value = sum(
        ifelse(cases, df$lengths, 0)
      ),
      length_interruption = sum(
        ifelse(cases, 0, df$lengths)
      ),
      longest_interruption_event = max(
        ifelse(cases, 0, df$lengths)
      )

    ) %>%

    within({percent_time_engaged = length_value / length_total * 100})

  }, target)

}

# Format Checks -----------------------------------------------------------

#' @keywords internal
#' @rdname CRIB_internal
crib_check_input <- function(
  x, target = NULL, required_percent, target_buffer
) {

  if (is.null(target)) stop(
    "Value must be supplied for `target` argument (CRIB).", call. = FALSE
  )

  if (is.null(target_buffer)) warning(
    "No value specified for `target_buffer` (CRIB).\nDefaulting to Inf, ",
    "which may dramatically increase runtime.", call. = FALSE
  )

  stopifnot(length(target) == 1)
  stopifnot(required_percent <= 100)

  if (required_percent < 1) {
    warning(
      "required_percent should be a percentage,",
      " not a proportion.\nDid you mean to pass ",
      round(required_percent * 100, 1), " rather than ",
      required_percent, "?", call. = FALSE
    )
  }

}
