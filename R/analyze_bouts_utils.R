dichotomize_intensity <- function(x, target) {

  as.character(x) %>%
  {. %in% target} %>%
  ifelse(target, "other")

}
