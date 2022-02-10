#' @keywords internal
#' @rdname analyze_bouts
crib_bouts <- function(
  x, target, target_buffer_mins,
  longest_allowable_interruption_mins = Inf, required_percent = 100,
  max_n_interruptions = Inf, minimum_bout_duration_minutes = 0,
  epoch_length_sec, is_wear = TRUE, valid_indices = NULL
) {

  #* Initial setup and testing

    if (missing(target)) target <- NULL
    if (missing(target_buffer_mins)) target_buffer_mins <- NULL
    crib_check_input(x, target, required_percent, target_buffer_mins)
    if (is.null(target_buffer_mins)) target_buffer_mins <- Inf

  #* Run the algorithm

    crib_clusters(
      x, target, n_epochs(target_buffer_mins, epoch_length_sec),
      required_percent,
      n_epochs(longest_allowable_interruption_mins, epoch_length_sec),
      max_n_interruptions
    ) %>%
    within({

      longest_interruption_minutes = n_minutes(
        longest_interruption_event, epoch_length_sec
      )
      longest_interruption_event = NULL

      total_interruption_minutes = n_minutes(
        length_interruption, epoch_length_sec
      )
      length_interruption = NULL

      engaged_minutes = n_minutes(length_value, epoch_length_sec)
      length_value = NULL

      overall_minutes = n_minutes(length_total, epoch_length_sec)
      length_total = NULL

    }) %>%
    PAutilities::df_reorder("percent_time_engaged", "engaged_minutes") %>%
    .[.$engaged_minutes >= minimum_bout_duration_minutes, ] %>%
    valid_bouts(x, valid_wear(is_wear, x)) %>%
    valid_bouts(x, valid_indices) %>%
    check_no_bouts(
      target,
      n_total_events = 0,
      n_value_events = 0,
      n_interruption_events = 0,
      overall_minutes = length(x),
      engaged_minutes = 0,
      percent_time_engaged = 0,
      total_interruption_minutes = 0,
      longest_interruption_minutes = 0
    ) %>%
    structure(
      ., row.names = seq(nrow(.)),
      x = x, target = target, input_length = length(x),
      longest_allowable_interruption_mins = longest_allowable_interruption_mins,
      required_percent = required_percent,
      max_n_interruptions = max_n_interruptions,
      minimum_bout_duration_minutes = minimum_bout_duration_minutes,
      target_buffer_mins = target_buffer_mins,
      is_wear = is_wear, method = "CRIB"
    )

}
