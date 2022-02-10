#' @keywords internal
#' @rdname analyze_bouts
rle_standard_bouts <- function(
  x, target, minimum_bout_duration_minutes = 0,
  epoch_length_sec, is_wear = TRUE, valid_indices = NULL
) {

  minimum_bout_duration_minutes %>%
  n_epochs(epoch_length_sec) %>%
  logic_runs(x, target, valid_wear(is_wear, x), .) %>%
  within({
    duration_minutes = n_minutes(lengths, epoch_length_sec)
    lengths = NULL
  }) %>%
  valid_bouts(x, valid_indices) %>%
  check_no_bouts(target, duration_minutes = 0) %>%
  structure(
    ., row.names = seq(nrow(.)), x = x,
    target = target, input_length = length(x),
    minimum_bout_duration_minutes = minimum_bout_duration_minutes,
    is_wear = is_wear, valid_indices = valid_indices, method = "rle_standard"
  )

}
