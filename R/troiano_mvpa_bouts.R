#' @keywords internal
#' @rdname analyze_bouts
troiano_mvpa_bouts <- function(
  x, target, activation_window_min = 10,
  activation_min = 8, termination_min = 3,
  minimum_bout_duration_minutes = 10,
  epoch_length_sec, is_wear = TRUE, valid_indices = NULL
) {

  twoclass_runs(x, target, FALSE) %>%
  data.frame(
    .,
    is_start = sapply(
      .$start_index,
      function(index, x, target, activation_window, activation) {
        x[seq(index, index + activation_window - 1)] %>%
        {sum(. %in% target)} %>%
        {. >= activation}
      },
      x,
      target,
      n_epochs(activation_window_min, epoch_length_sec),
      n_epochs(activation_min, epoch_length_sec)
    ),
    is_end = c(
      utils::tail(.$start_index, -1) - utils::head(.$end_index, -1) - 1 >=
      n_epochs(termination_min, epoch_length_sec),
      TRUE
    )
  ) %>%
  troiano_bout_summarize(
    x, target, epoch_length_sec,
    minimum_bout_duration_minutes
  ) %>%
  valid_bouts(x, valid_wear(is_wear, x)) %>%
  valid_bouts(x, valid_indices) %>%
  check_no_bouts(target, mvpa_min = 0) %>%
  structure(
    ., row.names = seq(nrow(.)), x = x, target = target,
    input_length = length(x), activation_window_min = activation_window_min,
    activation_min = activation_min, termination_min = termination_min,
    minimum_bout_duration_minutes = minimum_bout_duration_minutes,
    method = "Troiano_MVPA"
  )

}

troiano_bout_summarize <- function(
  runs, x, target, epoch_length_sec,
  minimum_bout_duration_minutes
) {

  if (!any(runs$is_start)) {

    data.frame(
      start_index = NA_integer_, end_index = NA_integer_,
      values = target, mvpa_min = 0
    )[-1, ]

  } else {

    which(runs$is_start) %>%
    purrr::map_df(
      function(start, end, runs) {
        which(end >= start) %>%
        .[1] %>%
        end[.] %>%
        {data.frame(
          start_index = runs$start_index[start],
          end_index = runs$end_index[.],
          values = runs$values[start]
        )}
      },
      which(runs$is_end),
      runs
    ) %>%
    split(., .$end_index) %>%
    purrr::map_df(
      function(
        info, x, target, epoch_length_sec,
        minimum_bout_duration_minutes
      ) {
        seq(info$start_index[1], info$end_index[nrow(info)]) %>%
        x[.] %>%
        {data.frame(
          start_index = info$start_index[1],
          end_index = info$end_index[nrow(info)],
          values = target,
          mvpa_min = n_minutes(sum(. %in% target), epoch_length_sec)
        )} %>%
        {if (.$mvpa_min >= minimum_bout_duration_minutes) . else NULL}
      },
      x, target, epoch_length_sec, minimum_bout_duration_minutes
    )

  }

}
