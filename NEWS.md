# PBpatterns 0.2.2

* Tweaked the way `example_data$valid_index` is calculated in the `Bout Analysis
and Sedentary Patterns` vignette


# PBpatterns 0.2.2

* Tweaked `analyze_bouts` to ensure the `CRIB` and `Troiano_MVPA` methods are
  able to account for empty data


# PBpatterns 0.2.1

* Edited vignettes, primarily to avoid predicted UBD error in the bout analysis
  and sedentary patterns vignette


# PBpatterns 0.2.0

* Funnelled a lot of code through `analyze_bouts`
* Introduced semi-functional S3.bouts methods for `expand_bouts` and `plot`
* Incorporated sedentary profiles into the package
* Added `summarize_weartime` and `adjust_bout_summaries`
* Attempted to ensure all arguments and output are specified in minutes (or
  similar) rather than epochs
* Overhauled the package-wide documentation
* Expanded the vignette selection


# PBpatterns 0.1.0

* Initial version
