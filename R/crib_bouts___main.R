#' @keywords internal
#' @rdname analyze_bouts
crib_bouts <- function(
  x, target, target_buffer,
  longest_allowable_interruption = Inf, required_percent = 100,
  max_n_interruptions = Inf, minimum_bout_epochs = 0
) {

  #* Initial setup and testing

    if (missing(target)) target <- NULL
    if (missing(target_buffer)) target_buffer <- NULL
    crib_check_input(x, target, required_percent, target_buffer)
    if (is.null(target_buffer)) target_buffer <- Inf

  #* Run the algorithm

    crib_clusters(
      x, target, target_buffer, required_percent,
      longest_allowable_interruption, max_n_interruptions
    ) %>%
    .[.$length_value >= minimum_bout_epochs, ] %>%
    check_no_bouts(x, target) %>%
    structure(
      ., row.names = seq(nrow(.)),
      x = x, target = target,
      input_length = length(x),
      longest_allowable_interruption = longest_allowable_interruption,
      required_percent = required_percent,
      max_n_interruptions = max_n_interruptions,
      minimum_bout_epochs = minimum_bout_epochs,
      target_buffer = target_buffer,
      method = "CRIB"
    )

}
