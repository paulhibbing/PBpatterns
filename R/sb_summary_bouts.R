#' @keywords internal
#' @rdname analyze_bouts
sb_summary_bouts <- function(
  x, target, is_wear, minimum_bout_length = 5, valid_indices = NULL,
  probs = c(
    0.1, 0.2, 0.25,
    seq(0.3, 0.7, 0.1),
    0.75, 0.8, 0.9
  )
) {

  ## Determine all SB bouts

  bouts <- logic_runs(x, target, is_wear, minimum_bout_length)

  ## If applicable, exclude bouts that overlap with invalid days
  ## Also calculate total wear time

  total_weartime_min <- sum(is_wear)

  if (!is.null(valid_indices) & nrow(bouts) > 0) {

    bouts %<>%
      nrow(.) %>%
      seq(.) %>%
      split(bouts, .) %>%
      sapply(function(x, valid_indices) {
        seq(x$start_index, x$end_index) %>%
          {. %in% valid_indices} %>%
          all(.)
      }, valid_indices = valid_indices) %>%
      bouts[., ]

    total_weartime_min <-
      which(is_wear) %>%
      intersect(valid_indices) %>%
      length(.)

  }

  ## Assemble features

  bouts$lengths %>%
  stats::quantile(probs = probs) %>%
  t(.) %>%
  data.frame(.) %>%
  stats::setNames(
    ., gsub("\\.$", "_bout", names(.))
  ) %>%
  stats::setNames(
    ., gsub("^X", "Q", names(.))
  ) %>%
  data.frame(
    total_weartime_min = total_weartime_min,
    n_SB_bouts = nrow(bouts),
    minimum_bout_length_threshold = minimum_bout_length,
    total_SB_min = sum(bouts$lengths),
    .,
    IQR = .$Q75_bout - .$Q25_bout,
    IDR = .$Q90_bout - .$Q10_bout,
    stringsAsFactors = FALSE
  ) %>%
  within({
    bout_frequency = n_SB_bouts / total_weartime_min * 60
    SB_perc = total_SB_min / total_weartime_min
  }) %>%
  structure(
    ., call = match.call(), method = "SB_summary",
    class = append(
      class(.),
      paste0("bout", minimum_bout_length),
      0
    )
  )

}
