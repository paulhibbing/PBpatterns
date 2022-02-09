# Wrapper -----------------------------------------------------------------

#' @keywords internal
#' @name SB_patterns
sb_patterns <- function(d, bouts, x) {
  sb_range_bouts(d, bouts) %>%
  usual_bout_duration(bouts) %>%
  fragmentation_index(bouts, x) %>%
  gini(bouts) %>%
  alpha(bouts)
}

# Duration Encoding -------------------------------------------------------

#' @keywords internal
#' @name SB_patterns
sb_range_bouts <- function(d, bouts) {
  data.frame(
    d,
    sb_0_14 = sum(ifelse(
      bouts$lengths < 15, bouts$lengths, 0
    )),
    sb_15_29 = sum(ifelse(
      bouts$lengths >= 15 & bouts$lengths < 30, bouts$lengths, 0
    )),
    sb_30_Inf = sum(ifelse(
      bouts$lengths >= 30, bouts$lengths, 0
    ))
  ) %T>%
  {stopifnot(isTRUE(all.equal(
    sum(rev(.)[ ,1:3]), sum(bouts$lengths),
    scale = 1, tolerance = 1/60/10
  )))}
}

# Duration Summarizing ----------------------------------------------------

#' @keywords internal
#' @name SB_patterns
usual_bout_duration <- function(d, bouts) {

  if (nrow(bouts) == 0) {
    return(
      data.frame(d, ubd_empirical = NA_real_, ubd_predicted = NA_real_)
    )
  }

  df <-
    table(bouts$lengths) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    stats::setNames(., gsub("^Var1$", "l", names(.))) %>%
    within({
      l = as.numeric(as.character(l))
      cumprop = 0
      prop = 0
      tot.l = l * Freq
      total_time = sum(tot.l)
      prop = tot.l/total_time
      total_time = NULL
      cumprop = cumsum(prop)
    }) %T>%
    {stopifnot(identical(order(.$l), seq(nrow(.))))}

  empirical <-
    abs(df$cumprop - 0.5) %>%
    which.min(.) %>%
    df$l[.]

  predicted <- try(stats::nls(
    cumprop ~ l^n/(l^n+W50^n),
    data = df,
    start = c(n=1,W50=empirical)
  ), TRUE)

  if (inherits(predicted, "try-error")) {
    warning(
      "Error fitting model for predicted usual bout",
      " duration -- returning NA", call. = FALSE
    )
    predicted <- NA
  } else {
    predicted %<>%
      stats::coef(.) %>%
      .["W50"] %>%
      unname(.)
  }

  data.frame(d, ubd_empirical = empirical, ubd_predicted = predicted)

}

# SB Breaks ---------------------------------------------------------------

#' @keywords internal
#' @name SB_patterns
fragmentation_index <- function(d, bouts, x) {
  bouts %>%
  within({
    `break` = ifelse(end_index == length(x), 0, 1)
  }) %>%
  {sum(.$`break`) / sum(.$lengths) * 60} %>%
  data.frame(d, fragmentation_index = .)
}

# Complex Metrics ---------------------------------------------------------

#' @keywords internal
#' @name SB_patterns
gini <- function(d, bouts) {
  data.frame(d, gini = DescTools::Gini(bouts$lengths))
}

#' @keywords internal
#' @name SB_patterns
alpha <- function(d, bouts) {

  if (nrow(bouts) == 0) {
    data.frame(d, alpha = NA_real_, alpha_se = NA_real_)
  } else {
    bouts$lengths %>%
    sapply(., function(xi, xm) log(xi/xm), xm = min(.)) %>%
    sum(.) %>%
    {data.frame(
      alpha = 1 + nrow(bouts)/.
    )} %>%
    within({
      alpha_se = (alpha - 1) / sqrt(nrow(bouts))
    }) %>%
    data.frame(d, .)
  }

}
