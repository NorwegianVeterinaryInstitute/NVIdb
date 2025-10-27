#' @title Manage username and password (credentials) for database services at NVI
#' @description Save or remove the current user's username and password for the data base services at the Norwegian Veterinary Institute
#'  in the  the user profile.
#' @details The Norwegian Veterinary Institute has access to various database services. To simplify the access to the database services
#'     when using R, the function \code{set_credentials} makes it possible to save the username and password (credentials) in the
#'     user profile at the current machine. When the username and password have been saved in the user profile, the functions \code{login}
#'     or \code{login_by_credentials} will automatically log in to the database services without any need of new input of username and password.
#'
#'     The user profile is not copied between computers. Consequently, if a user runs scripts with \code{login} on different computers,
#'     the credentials have to be saved at each computer separately.
#'
#'     \code{set_credentials(dbservice)} is used to set the username and password for a database service. The username and password are input
#'     using windows and saved in the users profile at the current computer. When the password for the database service have been changed,
#'     \code{set_credentials(dbservice)} can be used to update the password.
#'
#'     \code{set_credentials_PJS} is a wrapper for \code{set_credentials(dbservice)} used to set the username and password for journal_rapp/PJS.
#'     Journal_rapp has views to information in PJS and some other internal databases at NVI. The username and password are the same as for PJS.
#'     When the password for PJS have been changed, \code{set_credentials_PJS} can be used to update the password.
#'
#'     \code{set_credentials_EOS} is a wrapper for \code{set_credentials(dbservice)} used to set the username and password for EOS.
#'     EOS has tables with surveillance data reported to the Norwegian Food Safety Authority.
#'
#'     \code{remove_credentials(dbservice)} is used to delete the credentials for a database service from the user's profile.
#'
#' @param dbservice Name of the database service, for example "PJS" or "EOS". For database services where one don't use the premade
#'     wrappers, the name can be chosen freely, but must be the same as used in \code{login} and \code{login_by_credentials}
#' @return \code{set_credentials} The username and password for a database service are saved in the user profile at the current computer.
#'
#'     \code{remove_credentials} The username and password for a database service are deleted from the user profile at the current computer.
#'
#' @seealso \code{\link{login}} and \code{\link{login_by_credentials}}
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @name set_credentials_EOS-defunct
#' @keywords internal
#'
NULL


#' @export
#' @rdname NVIdb-defunct
#' @keywords internal
#'
set_credentials_EOS <- function(...) {

  .Defunct(new = "set_credentials('EOS')",
           package = "NVIdb",
           msg = paste("'set_credentials_EOS' is replaced by 'set_credentials('EOS')'",
                       "as the wrapper 'set_credentials_EOS'",
                       "was rarely used and therefore considered unnecessary."))

  # .Deprecated(new = "set_credentials('EOS')",
  #             package = "NVIdb",
  #             msg = paste("'set_credentials_EOS' is replaced by 'set_credentials('EOS')'",
  #                         "as the wrapper 'set_credentials_EOS'",
  #                         "was rarely used and therefore considered unnecessary."))
  #
  # # General function to input username and password for a database service
  # # Set database service to EOS
  # set_credentials(dbservice = "EOS")
}
