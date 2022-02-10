#' Visualize the result of a bout analysis
#'
#' @param x the object to plot
#' @param ... unused
#'
#' @return A \code{ggplot2} object that visualizes the bout analysis
#' @export
#'
#' @examples
#' data(example_data, package = "PBpatterns")
#' intensity <- cut(
#'   example_data$PAXINTEN,
#'   c(-Inf, 101, 760, Inf),
#'   c("SB", "LPA", "MVPA"),
#'   right = FALSE
#' )
#' bouts <- analyze_bouts(
#'   intensity, "MVPA", "Troiano_MVPA", epoch_length_sec = 60
#' )
#' # plot(bouts)
plot.bouts <- function(x, ...) {

  df <-
    expand_bouts(x) %>%
    data.frame(x = seq(.), y = .)

  ggplot(df, aes(x, as.numeric(y))) +
  geom_line() +
  scale_y_continuous(
    "State", breaks = seq(levels(df$y)), labels = levels(df$y)
  ) +
  scale_x_continuous("Time (epochs)") +
  theme_classic() +
  theme(
    axis.line = element_line(size = 0.5),
    axis.title = element_text(face = "bold")
  )

}
