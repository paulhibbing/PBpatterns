#' @keywords internal
#' @rdname analyze_bouts
troiano_mvpa_bouts <- function(
  x, target, activation_window = 10,
  activation_min = 8, termination_min = 3
) {

  dichotomize_intensity(x, target) %>%
  PAutilities::index_runs(.) %>%
  {.[.$values == target, ]} %>%
  data.frame(
    .,
    is_start = sapply(
      .$start_index,
      function(index, x, target, activation_window, activation_min) {
        x[seq(index, index + activation_window - 1)] %>%
        {sum(. %in% target)} %>%
        {. >= 8}
      },
      x, target, activation_window, activation_min
    ),
    is_end = c(
      utils::tail(.$start_index, -1) - utils::head(.$end_index, -1) - 1 >= 3,
      TRUE
    )
  ) %>%
  troiano_bout_summarize(x, target) %>%
  structure(
    x = x, target = target, input_length = length(x),
    activation_window = activation_window,
    activation_min = activation_min, termination_min = termination_min,
    method = "Troiano_MVPA"
  )

}

troiano_bout_summarize <- function(runs, x, target) {

  if (!any(runs$is_start)) {

    data.frame(
      start_index = NA_integer_, end_index = NA_integer_,
      values = target, mvpa_min = 0
    ) %>%
    structure(anyBouts = FALSE)

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
      function(info, x, target) {
        seq(info$start_index[1], info$end_index[nrow(info)]) %>%
        x[.] %>%
        {data.frame(
          start_index = info$start_index[1],
          end_index = info$end_index[nrow(info)],
          values = target,
          mvpa_min = sum(. %in% target)
        )} %>%
        {if (.$mvpa_min >= 10) . else NULL}
      },
      x,
      target
    ) %>%
    structure(anyBouts = TRUE)

  }

}
