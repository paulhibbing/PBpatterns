#' @keywords internal
#' @rdname analyze_bouts
rle_bouts <- function(
  x, target, minimum_bout_length = 0,
  is_wear = TRUE, valid_indices = seq(x)
) {

  logic_runs(x, target, is_wear, minimum_bout_length) %>%
  validate_rle_bouts(valid_indices) %>%
  within({values = target}) %>%
  structure(
    x = x, target = target, input_length = length(x),
    minimum_bout_length = minimum_bout_length, is_wear = is_wear,
    valid_indices = valid_indices, method = "rle_standard"
  )

}

validate_rle_bouts <- function(bouts, valid_indices) {

  if (nrow(bouts) > 0){

    bouts %>%
    purrr::pmap_lgl(
      function(start_index, end_index, valid_indices, ...) {
        seq(start_index, end_index) %>%
        {. %in% valid_indices} %>%
        all(.)
      },
      valid_indices
    ) %>%
    bouts[., ]

  } else {

    bouts

  }

}
