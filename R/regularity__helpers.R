epoch_number <- function(timestamps) {

  lubridate::floor_date(timestamps, "days") %>%
  difftime(timestamps, .) %>%
  as.numeric(.)

}

regularity_info <- function(values, timestamps, epochs = NA_real_, info) {

  if (!missing(info)) return(info)

  data.frame(
    value = values,
    timestamp = timestamps,
    epoch = epochs
  ) %>%
  dplyr::arrange(epoch, timestamp)

}

as_regularity <- function(score, values, timestamps, ..., numeric = FALSE) {

  stopifnot(is.numeric(score))

  if (numeric) return(score)

  structure(
    score,
    class = append(class(score), "regularity", 0),
    info = regularity_info(values, timestamps, ...)
  )

}

check_sparse <- function(epoch_results, count_var, min_value) {

  sparse_epochs <- epoch_results[[count_var]] < min_value

  if (any(sparse_epochs)) warning(
    "The minimum number of comparisons (n = ", min_value, ")",
    " could not be obtained for ", sum(sparse_epochs), " time(s) of day"
  )

  invisible()

}
