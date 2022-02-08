#' @name Hibbing_2021_internal
#' @title Functions used internally for the sedentary profiles of \href{https://journals.lww.com/acsm-msse/Abstract/9000/Sedentary_Profiles__A_New_Perspective_on.95921.aspx}{Hibbing et al. (2021)}
#'
#' @inheritParams sb_profile
#' @inheritParams sb_profile_Hibbing2021
#' @param result output that may need data frame formatting (possibly ID-based)
#'
#' @details The purpose of \code{profile_id_wear_check} is to determine if the Choi
#'   wear time algorithm needs to be run and, if so, to run it (via
#'   \code{profile_choi_wear}, with a message). To bypass,
#'   run \code{df$is_wear <- TRUE} prior to executing
#'   \code{profile_id_wear_check(df, id, wear = "is_wear")}.
#'
#'   \code{profile_df_check} is a wrapper that encompasses all the checks on this
#'   page
#'
#' @keywords internal
NULL
