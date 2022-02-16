dichotomize_behavior <- function(x, target) {

  as.character(x) %>%
  {. %in% target} %>%
  ifelse(target, "other")

}

twoclass_runs <- function(x, target, return_everything = TRUE) {

  dichotomize_behavior(x, target) %>%
  PAutilities::index_runs(.) %>%
  {.[.$values == target | return_everything, ]}

}

logic_runs <- function(x, target, is_wear, minimum_bout_epochs = 0) {

  stopifnot(length(is_wear) == length(x))

  {x == target} %>%
  paste(is_wear) %>%
  PAutilities::index_runs(.) %>%
  within({values = as.character(values)}) %>%
  .[.$values == "TRUE TRUE", ] %>%
  .[.$lengths >= minimum_bout_epochs, ]

}

run_end <- function(start_index, end_index, termination_min, epoch_length_sec) {
  {utils::tail(start_index, -1) - utils::head(end_index, -1) - 1} %>%
  {. >= n_epochs(termination_min, epoch_length_sec)} %>%
  c(TRUE)
}

valid_bouts <- function(bouts, x, valid_indices = NULL) {

  if (!is.null(valid_indices) & nrow(bouts) > 0){

    bouts %>%
    purrr::pmap_lgl(
      function(start_index, end_index, valid_indices, ...) {
        seq(start_index, end_index) %>%
        {. %in% valid_indices} %>%
        all(.)
      },
      valid_valid_indices(valid_indices, x)
    ) %>%
    bouts[., ] %>%
    structure(., anyBouts = nrow(.) > 0)

  } else {

    bouts %>%
    structure(., anyBouts = nrow(.) > 0)

  }

}

check_no_bouts <- function(results, target, ...) {

  if (nrow(results) > 0) {

    results %>%
    within({values = target})

  } else {

    data.frame(
      start_index = NA_integer_,
      end_index = NA_integer_,
      values = target,
      ...
    ) %>%
    structure(anyBouts = FALSE)

  }

}
