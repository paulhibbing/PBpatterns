
# PBpatterns

<!-- badges: start -->
[![Project Status: Active ? The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![License](https://img.shields.io/badge/licence-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

PBpatterns is a package to assist with calculating physical behavior variables
in temporally-focused analyses. Examples include sedentary pattern variables
(e.g., fragmentation index, Gini index, and alpha), sedentary profiles, and
bouts of moderate-to-vigorous physical activity.

## Installation

The package is under development and will not be released on CRAN for quite
awhile (if ever). But you can install from GitHub with the following:

``` r
remotes::install_github("paulhibbing/PBpatterns")
```

## Current Capabilities: Bout Pattern Analysis

``` r

library(PBpatterns)
# remotes::install_github("paulhibbing/PAutilities")

## Set up data

  data(example_data, package = "PBpatterns")

  example_data$is_sb <- example_data$PAXINTEN <= 100
  example_data$is_mvpa <- example_data$PAXINTEN >= 760
  example_data$Timestamp <- seq(
    as.POSIXct("00:00:00", "UTC", format = "%H:%M:%S"),
    by = "1 min",
    length.out = nrow(example_data)
  )

  valid_indices <- c(
    654:1454, 1917:2837, 3499:4266, 5216:5632,
    6340:7119, 7704:8555, 9118:10077
  )

## Analyze bouts

  sb_bouts <- sb_bout_summary(
    example_data, valid_indices = valid_indices
  )
  
  with_mvpa <- mvpa_bout_summary(
    example_data, valid_indices = valid_indices, other_info = sb_bouts
  )

  adjusted_output <- bout_summary_residual_adjust(with_mvpa)
  
## View output

  sb_bouts
  with_mvpa
  adjusted_output

```
