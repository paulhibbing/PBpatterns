#' @name CRIB_output
#'
#' @title
#' Understanding the output of \code{\link{analyze_bouts}}
#' when \code{method == "CRIB"}
#'
#' @description
#' \code{CRIB} returns a data frame formatted as follows:
#'
#' \tabular{ll}{
#' \strong{start_index} \tab The start index of the bout period \cr
#' \strong{end_index} \tab The end index of the bout period \cr
#' \strong{values} \tab The target behavior \cr
#' \strong{n_total_events} \tab The total number of behavioral events (target or
#' interruptive) \cr
#' \strong{n_value_events} \tab The number of target behavior events \cr
#' \strong{n_interruption_events} \tab The number of interruptive events \cr
#' \strong{overall_minutes} \tab The full duration of the bout period, in
#' minutes \cr
#' \strong{engaged_minutes} \tab The portion of \code{overall_minutes} that was
#' spent in the target behavior \cr
#' \strong{percent_time_engaged} \tab The percentage of \code{overall_minutes}
#' comprising \code{engaged_minutes} \cr
#' \strong{total_interruption_minutes} \tab The portion of
#' \code{overall_minutes} that was spent in interruptive behavior \cr
#' \strong{longest_interruption_minutes} \tab The duration (in minutes) of the
#' longest interruption within the \code{overall_minutes} period \cr
#' }
#'
NULL
