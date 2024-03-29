% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crib_bouts__internal_doc.R,
%   R/crib_bouts_clusters.R, R/crib_bouts_utils.R
\name{CRIB_internal}
\alias{CRIB_internal}
\alias{crib_clusters}
\alias{crib_stratum}
\alias{crib_test_stratum}
\alias{run_info}
\alias{collapse_runs}
\alias{crib_check_input}
\title{Internal functions for the CRIB method}
\usage{
crib_clusters(
  x,
  target,
  target_buffer,
  required_percent,
  longest_allowable_interruption,
  max_n_interruptions
)

crib_stratum(
  runs,
  x,
  target,
  required_percent,
  longest_allowable_interruption,
  max_n_interruptions
)

crib_test_stratum(
  x,
  target,
  runs,
  tree,
  h,
  required_percent,
  max_n_interruptions,
  longest_allowable_interruption
)

run_info(run)

collapse_runs(x, target)

crib_check_input(x, target = NULL, required_percent, target_buffer)
}
\arguments{
\item{x}{factor. Epoch-by-epoch behavior classifications, e.g., \code{SB},
\code{LPA}, and \code{MVPA}}

\item{target}{the level of \code{x} for which to examine bouts}

\item{required_percent}{numeric (1-100; \code{CRIB} and
\code{Ostendorf_MVPA}). The minimum percentage of the full bout period that
must be spent engaging in the target behavior. Stated differently, this
threshold stipulates that interruptions can compose no more than
\code{100-required_percent} of the bout}

\item{max_n_interruptions}{numeric (\code{CRIB}). The maximum number of
interruption events that are allowed before a bout will be considered
invalid}

\item{runs}{pre-processed input (mostly run length encoded)}

\item{run}{a candidate run for which to retrieve basic information}

\item{results}{output from \code{crib_clusters}}
}
\description{
Internal functions for the CRIB method
}
\keyword{internal}
