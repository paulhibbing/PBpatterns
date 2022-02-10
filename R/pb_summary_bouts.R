# Setup helper ------------------------------------------------------------

  initialize_summary_bouts <- function(
    x, target, is_wear, valid_indices,
    minimum_bout_duration_minutes, epoch_length_sec
  ) {

    ## Test input

      is_wear %<>% valid_wear(x)

      valid_indices %<>% valid_valid_indices(x, FALSE)

    ## Determine all bouts (exclude any that overlap with invalid indices)

      bouts <-
        minimum_bout_duration_minutes %>%
        n_epochs(epoch_length_sec) %>%
        logic_runs(x, target, is_wear, .) %>%
        valid_bouts(x, valid_indices) %>%
        within({
          lengths_min = n_minutes(lengths, epoch_length_sec)
          lengths = NULL
        })

    ## Calculate total wear time

      total_weartime_min <-
        which(is_wear) %>%
        intersect(valid_indices) %>%
        length(.) %>%
        n_minutes(epoch_length_sec)

    ## Assign results to parent frame

      assign("is_wear", is_wear, parent.frame())
      assign("valid_indices", valid_indices, parent.frame())
      assign("bouts", bouts, parent.frame())
      assign("total_weartime_min", total_weartime_min, parent.frame())

  }

# Method-alikes for `analyze_bouts` ---------------------------------------

  #' @keywords internal
  #' @rdname analyze_bouts
  sb_summary_bouts <- function(
    x, target, minimum_bout_duration_minutes = 0,
    probs = c(0.1, 0.2, 0.25, seq(0.3, 0.7, 0.1), 0.75, 0.8, 0.9),
    patterns = TRUE, epoch_length_sec, is_wear = TRUE,
    valid_indices = NULL
  ) {

    initialize_summary_bouts(
      x, target, is_wear, valid_indices,
      minimum_bout_duration_minutes, epoch_length_sec
    )

    ## Assemble the summary

      bouts$lengths_min %>%
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
        epoch_length = epoch_length_sec,
        total_weartime_min = total_weartime_min,
        SB_bout_exclusion_threshold_minutes = minimum_bout_duration_minutes,
        n_SB_bouts = nrow(bouts),
        total_SB_min = sum(bouts$lengths_min),
        .,
        IQR = .$Q75_bout - .$Q25_bout,
        IDR = .$Q90_bout - .$Q10_bout,
        stringsAsFactors = FALSE
      ) %>%
      within({
        bout_frequency = n_SB_bouts / total_weartime_min * 60
        SB_perc = total_SB_min / total_weartime_min
      }) %>%
      {if (patterns) sb_patterns(., bouts, x) else .} %>%
      structure(
        ., call = match.call(), method = "SB_summary",
        class = append(
          class(.),
          paste0("bout", minimum_bout_duration_minutes),
          0
        )
      )

  }

  #' @keywords internal
  #' @rdname analyze_bouts
  mvpa_summary_bouts <- function(
    x, target, minimum_bout_duration_minutes = 0,
    epoch_length_sec, is_wear = TRUE, valid_indices = NULL
  ) {

    initialize_summary_bouts(
      x, target, is_wear, valid_indices,
      minimum_bout_duration_minutes, epoch_length_sec
    )

    data.frame(
      epoch_length = epoch_length_sec,
      total_weartime_min = total_weartime_min,
      MVPA_bout_exclusion_threshold_minutes = minimum_bout_duration_minutes,
      n_MVPA_bouts = nrow(bouts),
      total_MVPA_min = sum(bouts$lengths_min)
    ) %>%
    within({
      MVPA_perc = total_MVPA_min / total_weartime_min
    }) %>%
    structure(
      ., call = match.call(), method = "MVPA_summary",
      class = append(
        class(.),
        paste0("bout", minimum_bout_duration_minutes),
        0
      )
    )

  }
