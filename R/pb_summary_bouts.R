# Setup helper ------------------------------------------------------------

  initialize_summary_bouts <- function(
    x, target, is_wear, valid_indices,
    minimum_bout_length, epoch_length_sec
  ) {

    ## Test input

      is_wear %<>% valid_wear(x)

      valid_indices %<>% valid_valid_indices(x, FALSE)

    ## Determine all bouts (exclude any that overlap with invalid indices)

      bouts <-
        logic_runs(x, target, is_wear, minimum_bout_length) %>%
        valid_bouts(x, valid_indices)

    ## Calculate total wear time

      total_weartime_min <-
        which(is_wear) %>%
        intersect(valid_indices) %>%
        length(.) %>%
        {. * epoch_length_sec / 60}

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
    x, target, is_wear = TRUE, minimum_bout_length = 0, valid_indices = NULL,
    probs = c(0.1, 0.2, 0.25, seq(0.3, 0.7, 0.1), 0.75, 0.8, 0.9),
    patterns = TRUE, epoch_length_sec
  ) {

    initialize_summary_bouts(
      x, target, is_wear, valid_indices,
      minimum_bout_length, epoch_length_sec
    )

    ## Assemble the summary

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
        epoch_length = epoch_length_sec,
        total_weartime_min = total_weartime_min,
        minimum_SB_bout_length_threshold = minimum_bout_length,
        n_SB_bouts = nrow(bouts),
        total_SB_min = sum(bouts$lengths) * epoch_length_sec / 60,
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
          paste0("bout", minimum_bout_length),
          0
        )
      )

  }

  #' @keywords internal
  #' @rdname analyze_bouts
  mvpa_summary_bouts <- function(
    x, target, is_wear = TRUE, minimum_bout_length = 0,
    valid_indices = NULL, epoch_length_sec
  ) {

    initialize_summary_bouts(
      x, target, is_wear, valid_indices,
      minimum_bout_length, epoch_length_sec
    )

    data.frame(
      epoch_length = epoch_length_sec,
      total_weartime_min = total_weartime_min,
      minimum_MVPA_bout_length_threshold = minimum_bout_length,
      n_MVPA_bouts = nrow(bouts),
      total_MVPA_min = sum(bouts$lengths) * epoch_length_sec / 60
    ) %>%
    within({
      MVPA_perc = total_MVPA_min / total_weartime_min
    }) %>%
    structure(
      ., call = match.call(), method = "MVPA_summary"
    )

  }
