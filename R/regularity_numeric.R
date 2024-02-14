#' @rdname regularity_index
#' @export
regularity_index.numeric <- function(
  values, timestamps, n_bands, band_size,
  min_days = 3, numeric = TRUE, ...
) {

  ## Preliminary checks

    stopifnot(
      is.numeric(n_bands),
      is.numeric(band_size),
      length(n_bands) == 1,
      length(band_size) == 1,
      n_bands > 0,
      band_size > 0
    )


  ## Execute the main process

    info <-
      epoch_number(timestamps) %>%
      regularity_info(values, timestamps, .)

    epoch_results <-
      info %>%
      ## continuous data do not need a preliminary call to dplyr::arrange
      dplyr::group_by(epoch) %>%
      dplyr::summarise(
        deviations =
          mean(value) %>%
          {abs(value - .) / band_size} %>%
          pmin(n_bands) %>%
          sum(.),
        epochs = dplyr::n()
      ) %T>%
      check_sparse("epochs", min_days)


  ## Format the output

    as_regularity(
      200 * sum(epoch_results$deviations) / sum(epoch_results$epochs) / n_bands - 100,
      info = list(
        raw = info,
        epoch = epoch_results
      ),
      numeric = numeric
    )

}
