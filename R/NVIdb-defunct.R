#' @title Defunct Functions in Package NVIdb
#' @description The functions listed in "Usage" are no longer part of NVIdb.
#'     When possible, alternative functions are mentioned. The original help
#'     pages for defunct functions are available at \code{help("<function>-defunct")}.
#' @details \code{set_PAT}, \code{get_PAT}, and \code{remove_PAT} were defunct from
#'     v0.14.1 released 2025-10-29 and had been deprecated from v0.11.0 released
#'     2024-01-24. The functions were never taken into use. Functions from the
#'     much better package \code{gitcreds} should be used instead.
#'
#' The wrapper function \code{set_credentials_EOS} was defunct from v0.14.1
#'     released 2025-10-29 and had been deprecated from v0.11.2 released
#'     2024-05-03. The function was rarely used and was an unnecessary wrapper
#'     around \code{set_credentials}. \code{\link{set_credentials}} with the
#'     argument \code{dbservice} = "EOS" should be used instead.
#'
#' The wrapper functions \code{login_EOS}, \code{login_by_input_EOS}, and
#'     \code{login_by_credentials_EOS} were defunct from v0.14.1 released
#'     2025-10-29 and had been deprecated from v0.11.2 released
#'     2024-05-03. These functions were rarely used and were unnecessary
#'     wrappers around the login-functions. Use the corresponding
#'     login-functions with the argument \code{dbservice = "EOS"} instead.
#'
#' The wrapper functions \code{login_PJS}, \code{login_by_input_PJS}, and
#'     \code{login_by_credentials_PJS} were defunct from
#'     v0.14.1 released 2025-10-29 and had been deprecated from v0.13.0 released
#'     2024-12-13. These functions were unnecessary wrappers around the
#'     login-functions. Use the corresponding login-functions with the argument
#'     \code{dbservice = "PJS"} instead.
#'
#' The functions \code{read_eos_data} and \code{standardize_eos_data} have been
#'     moved to \code{NVIpjsr}. These were defunct from v0.14.1 released
#'     2025-10-29 and had been deprecated from v0.13.0 released 2024-12-13. Use
#'     the corresponding functions in \code{NVIpjsr}.
#'
#' @param \dots (arguments)
#' @return (results)
#' @name NVIdb-defunct
#' @keywords internal
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#'
#' @examples
#' \dontrun{
#' set_PAT(...) ### -- use gitcreds::gitcreds_set() instead.
#' get_PAT(...) ### -- use gitcreds::gitcreds_get() instead.
#' remove_PAT(...) ### -- use gitcreds::gitcreds_delete() instead.
#' set_credentials_EOS(...) ### -- use set_credentials("EOS") instead.
#' login_EOS(...) ### -- use login("EOS") instead.
#' login_by_input_EOS(...) ### -- use login_by_input("EOS") instead.
#' login_by_credentials_EOS(...) ### -- use login_by_credentials("EOS") instead.
#' login_PJS(...) ### -- use login("PJS") instead.
#' login_by_input_PJS(...) ### -- use login_by_input("PJS") instead.
#' login_by_credentials_PJS(...) ### -- use login_by_credentials("PJS") instead
#' read_eos_data(...) ### -- use NVIpjsr::read_eos_data() instead
#' standardize_eos_data(...) ### -- use NVIpjsr::standardize_eos_data() instead
#' }
#'
NULL
