dichotomize_behavior <- function(x, target) {

  as.character(x) %>%
  {. %in% target} %>%
  ifelse(target, "other")

}

twoclass_runs <- function(x, target, return_everything = TRUE) {

  dichotomize_behavior(x, target) %>%
  PAutilities::index_runs(.) %>%
  {.[.$values == target | return_everything, ]}

}

logic_runs <- function(x, target, is_wear, minimum_bout_length = 0) {

  {x == target} %>%
  paste(is_wear) %>%
  PAutilities::index_runs(.) %>%
  within({values = as.character(values)}) %>%
  .[.$values == "TRUE TRUE", ] %>%
  .[.$lengths >= minimum_bout_length, ]

}
