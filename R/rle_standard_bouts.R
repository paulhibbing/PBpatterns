#' @keywords internal
#' @rdname analyze_bouts
rle_standard_bouts <- function(
  x, target, minimum_bout_length = 0,
  is_wear = TRUE, valid_indices = NULL
) {

  valid_wear(is_wear, x) %>%
  logic_runs(x, target, ., minimum_bout_length) %>%
  valid_bouts(valid_indices) %>%
  within({values = target}) %>%
  structure(
    x = x, target = target, input_length = length(x),
    minimum_bout_length = minimum_bout_length, is_wear = is_wear,
    valid_indices = valid_indices, method = "rle_standard"
  )

}
