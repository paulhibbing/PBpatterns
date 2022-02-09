#' Implement sedentary profiles
#'
#' @param object input (either data frame or output from
#'   \code{\link{profile_describe_sb}})
#' @param method character. The profile method to use (currently only supports
#'   \code{"Hibbing_2021"}).
#' @param ... further arguments passed to \code{\link{profile_describe_sb}}
#'   (used only in data frame method)
#'
#' @return one or more profile assignments
#' @export
#'
#' @examples
#' data(example_data, package = "PBpatterns")
#' sb_profile(example_data, counts = "PAXINTEN", epoch_length_sec = 60)
#' sb_profile(
#'   example_data, counts = "PAXINTEN",
#'   model = "randomForest", epoch_length_sec = 60
#' )
#' sb_profile(
#'   example_data, id = "PAXDAY",
#'   counts = "PAXINTEN", epoch_length_sec = 60
#' )
#' sb_profile(
#'   example_data, id = "PAXDAY", counts = "PAXINTEN",
#'   model = "decisionTree", epoch_length_sec = 60
#' )
#'
#' @seealso
#'   \code{\link{sb_profile_Hibbing2021}}
#'   \href{https://journals.lww.com/acsm-msse/Abstract/9000/Sedentary_Profiles__A_New_Perspective_on.95921.aspx}{Hibbing et al. (2021)}
sb_profile <- function(object, method = "Hibbing_2021", ...) {

  method <- match.arg(method)

  if (method == "Hibbing_2021") {

    UseMethod("sb_profile_Hibbing2021", object)

  } else {

    stop("No sb_profile framework exists for method == ", method, call. = FALSE)

  }

}
