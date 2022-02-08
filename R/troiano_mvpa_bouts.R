#' @param activation_window size of window (number of epochs) to use when
#'   searching for a bout activation
#' @param activation_min number of epochs in the activation window that must
#'   equal \code{target} for an activation to be detected
#' @param termination_min number of consecutive non-\code{target} epochs
#'   required to terminate a bout
#' @keywords internal
#' @rdname analyze_bouts
troiano_mvpa_bouts <- function(
  x, target, activation_window = 10,
  activation_min = 8, termination_min = 3
) {

  as.character(x) %>%
  {. %in% target} %>%
  PAutilities::index_runs(.) %>%
  {.[.$values, ]} %>%
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
      tail(.$start_index, -1) - head(.$end_index, -1) - 1 >= 3,
      TRUE
    )
  ) %>%
  troiano_bout_summarize(target) %>%
  structure(
    x = x, target = target, activation_window = activation_window,
    activation_min = activation_min, termination_min = termination_min,
    method = "Troiano_MVPA"
  )



}

troiano_bout_summarize <- function(runs, target) {

  if (!any(runs$is_start)) {

    data.frame(start = NA_integer_, end = NA_integer_, mvpa_min = 0)

  } else {

    which(runs$is_start) %>%
    purrr::map_df(
      function(start, end, runs) {
        which(end >= start) %>%
        .[1] %>%
        end[.] %>%
        {data.frame(
          start = runs$start_index[start],
          end = runs$end_index[.]
        )}
      },
      which(runs$is_end),
      runs
    ) %>%
    split(., .$end) %>%
    purrr::map_df(
      function(info, x, target) {
        seq(info$start[1], info$end[nrow(info)]) %>%
        x[.] %>%
        {data.frame(
          start = info$start[1], end = info$end[nrow(info)],
          mvpa_min = sum(. %in% target)
        )} %>%
        {if (.$mvpa_min >= 10) . else NULL}
      },
      x,
      target
    )

  }

}
