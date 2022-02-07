#' Extract features to summarize a participant's bout duration distribution
#'
#' @param df data frame of accelerometer input
#' @param is_sb logical vector reflecting minute-by-minute classifications
#'   (\code{TRUE} for sedentary behavior and \code{FALSE} for non-sedentary
#'   behavior)
#' @param is_wear logical vector reflecting minute-by-minute wear
#'   classifications (\code{TRUE} for wearing and \code{FALSE} for not wearing)
#' @param min_bout integer scalar. Threshold for a run to qualify as a bout
#' @param probs numeric vector. Percentiles to calculate (all values must be
#'   between 0 and 1)
#' @param simplify logical. If passing a data frame, setting to \code{FALSE}
#'   will return separate bout objects that can be passed into
#'   \code{\link{sb_profile}}). Setting to \code{TRUE} will return an
#'   aggregated data frame through \code{rbind} and \code{\link{id_bind}}.
#'
#' @return A bout object (data frame of distribution features)
#' @inheritParams sb_profile
#' @inheritParams sb_profile_Hibbing2021
#'
#' @details This function can be used in one of two ways, either 1) by directly
#'   providing values for \code{is_sb} and \code{is_wear} (with \code{df =
#'   NULL}), or 2) by providing values for \code{df} and \code{counts}
#'   (optionally with \code{wear} as well).
#'
#' @export
#'
#' @examples
#' data(example_data, package = "PBpatterns")
#' sb_bout_dist(
#'   is_sb = example_data$PAXINTEN <= 100,
#'   is_wear = choi_wear(example_data$PAXINTEN)
#' )
#' sb_bout_dist(
#'   example_data, id = "PAXDAY", counts = "PAXINTEN"
#' )
sb_bout_dist <- function(
  df = NULL, is_sb, is_wear, min_bout = 5,
  valid_indices = NULL, id = NULL, counts = NULL,
  wear = NULL, sb = 100, simplify = TRUE,
  probs = c(
    0.1, 0.2, 0.25,
    seq(0.3, 0.7, 0.1),
    0.75, 0.8, 0.9
  )
) {

  if (is.null(df)) {

    stopifnot(!missing(is_sb), !missing(is_wear))

    sb_bout_dist_default(
      is_sb, is_wear, min_bout, valid_indices, probs
    )

  } else {

    sb_bout_dist_df(
      df, counts, wear, id, sb, min_bout,
      valid_indices, simplify, probs
    )

  }

}

#' @rdname Hibbing_2021_internal
#' @inheritParams sb_bout_dist
#' @keywords internal
sb_bout_dist_default <- function(
  is_sb, is_wear, min_bout = 5, valid_indices = NULL,
  probs = c(
    0.1, 0.2, 0.25,
    seq(0.3, 0.7, 0.1),
    0.75, 0.8, 0.9
  )
) {

  ## Determine all SB bouts

    bouts <-
      paste(is_sb, is_wear) %>%
      PAutilities::index_runs(.) %>%
      within({values = as.character(values)}) %>%
      .[.$values=="TRUE TRUE", ] %>%
      .[.$lengths >= min_bout, ]

  ## For interactive use:
    # if (nrow(bouts) > 0) bouts %<>% structure(., row.names = seq(nrow(.)))

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
      min_bout_threshold = min_bout,
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
      .,
      class = append(
        class(.),
        c("bout", paste0("bout", min_bout)),
        0
      )
    )

}

#' @rdname Hibbing_2021_internal
#' @inheritParams sb_bout_dist
#' @keywords internal
sb_bout_dist_df <- function(
  df, counts = NULL, wear = NULL, id = NULL, sb = 100,
  min_bout = 5, valid_indices = NULL, simplify = TRUE,
  probs = c(
    0.1, 0.2, 0.25,
    seq(0.3, 0.7, 0.1),
    0.75, 0.8, 0.9
  )
) {

  df %>%
  df_check_format(counts, valid_indices, id, wear) %>%
  lapply(
    function(x, sb, min_bout, probs) {
      sb_bout_dist_default(
        is_sb = x$counts <= sb,
        is_wear = x$is_wear,
        min_bout = min_bout,
        valid_indices = which(x$valid_index),
        probs = probs
      )
    }, sb, min_bout, probs
  ) %>%
  id_bind(id, simplify)

}
