#' @keywords internal
#' @rdname analyze_bouts
ostendorf_mvpa_bouts <- function(
  x, target, required_percent = 80, termination_min = 3,
  minimum_bout_duration_minutes = 10,
  epoch_length_sec, is_wear = TRUE, valid_indices = NULL
) {

  crib_check_input(x, target, required_percent, NA)
  PCT <- if (required_percent > 1) required_percent / 100 else required_percent

  twoclass_runs(x, target, FALSE) %>%
  {if (nrow(.) == 0) {
    data.frame(., is_start = logical(), is_end = logical())
  } else {
    data.frame(
      .,
      is_start = TRUE,
      is_end = run_end(
        .$start_index, .$end_index,
        termination_min, epoch_length_sec
      )
    )
  }} %>%
  troiano_bout_summarize(
    x, target, epoch_length_sec,
    minimum_bout_duration_minutes
  ) %>%
  ostendorf_bout_summarize(target, minimum_bout_duration_minutes, PCT) %>%
  valid_bouts(x, valid_wear(is_wear, x)) %>%
  valid_bouts(x, valid_indices) %>%
  check_no_bouts(target, mvpa_min = 0, mvpa_pct = 0) %>%
  structure(
    ., row.names = seq(nrow(.)), x = x, target = target,
    input_length = length(x), required_percent = required_percent,
    PCT = PCT, termination_min = termination_min,
    minimum_bout_duration_minutes = minimum_bout_duration_minutes,
    method = "ostendorf_MVPA"
  )

}

ostendorf_bout_summarize <- function(
  runs, target, minimum_bout_duration_minutes, PCT
) {

  if (nrow(runs) == 0) {

    data.frame(
      start_index = NA_integer_, end_index = NA_integer_,
      values = target, mvpa_min = 0, mvpa_pct = 0
    )[-1, ]

  } else {

    runs %>%
    purrr::pmap_df(
      function(start_index, end_index, values, mvpa_min) {
        seq(start_index, end_index) %>%
        {mvpa_min / length(.)} %>%
        data.frame(
          start_index = start_index,
          end_index = end_index,
          values = values,
          mvpa_min = mvpa_min,
          mvpa_pct = .
        ) %>%
        {if (
          .$mvpa_min >= minimum_bout_duration_minutes &
          .$mvpa_pct >= PCT
        ) . else NULL}
      }
    )

  }

}
