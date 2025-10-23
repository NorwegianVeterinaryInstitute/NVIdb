#' @title Defunct Functions in Package NVIdb
#' @description These functions are no longer part of NVIdb.
#'     When possible, alternative functions are mentioned. The original help
#'     pages for defunct functions are available at \code{help("<function>-defunct")}.
#' @details \code{set_PAT}, \code{get_PAT}, and \code{remove_PAT} was defunct from
#'     v0.14.1 released 2025-10-## and had been deprecated from v0.11.0 released
#'     2024-01-24. The functions were never taken into use. Functions from the
#'     much better package \code{gitcreds} should be used instead.
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
#' }
#'
NULL
