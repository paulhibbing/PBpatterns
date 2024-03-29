% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adjust_bout_summaries__main.R
\name{adjust_bout_summaries}
\alias{adjust_bout_summaries}
\title{Apply wear time residual adjustment to the output of bout summary methods}
\usage{
adjust_bout_summaries(d, verbose = FALSE)
}
\arguments{
\item{d}{the output of to adjust}

\item{verbose}{logical. Print updates to console}
}
\value{
The original \code{d}, with extra columns (\code{adj_*}) reflecting
  residual adjustments on relevant variables
}
\description{
Applies to output from the \code{*_summary} methods of
\code{\link{analyze_bouts}}, in conjunction with information from
\code{\link{summarize_weartime}}
}
\examples{

## Set up the data

  data(example_data, package = "PBpatterns")

  valid_indices <- c(
    654:1454, 1917:2837, 3499:4266, 5216:5632,
    6340:7119, 7704:8555, 9118:10077
  )

  example_data$Timestamp <- seq(
    as.POSIXlt(Sys.Date()), by = "1 min", length.out = nrow(example_data)
  )

  example_data$is_wear <- TRUE

  example_data$intensity <- cut(
    example_data$PAXINTEN,
    c(-Inf, 101, 760, Inf),
    c("SB", "LPA", "MVPA"),
    right = FALSE
  )

  example_data$valid_index <- 1:nrow(example_data) \%in\% valid_indices

## Set up the analysis (It needs to have more than one data
## point, so we will stratify by PAXDAY for illustration)

  weartime_info <- purrr::map_df(
    split(example_data, example_data$PAXDAY),
    ~ summarize_weartime(.x, "is_wear", "Timestamp", .x$valid_index)
  )

  sb_bouts <- purrr::map_df(
    split(example_data, example_data$PAXDAY),
     ~ analyze_bouts(
       .x$intensity, "SB", "SB_summary",
       is_wear = .x$is_wear,
       valid_indices = .x$valid_index,
       epoch_length_sec = 60
     )
  )

  mvpa_bouts <- purrr::map_df(
    split(example_data, example_data$PAXDAY),
    ~ analyze_bouts(
      .x$intensity, "MVPA", "MVPA_summary",
      is_wear = .x$is_wear,
      valid_indices = .x$valid_index,
      epoch_length_sec = 60
      )
  )

  d <- merge(weartime_info, sb_bouts)

  d <- merge(d, mvpa_bouts)

## Run the analysis

  adjust_bout_summaries(d)

}
