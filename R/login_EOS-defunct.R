#' @title Log in to data base services
#' @description  Log in to NVI's data base services, in particular journal_rapp/PJS and EOS.
#' @details The NVI has access to several database services. These functions log
#'     in to such services. The functions provides methods to either log in using
#'     credentials set in the user profile by \code{\link{set_credentials}} or use
#'     input windows for username and password. Thereby the hard coding of
#'     username and password can be avoided.
#'
#'     \code{login} is general functions where all necessary specifications like
#'     server name and database name of the database must be input. The database
#'     provider can give information on what specifications that has to be used.
#'     This can be used to log in to many different databases. In the case that
#'     one login to a database service for which the connection parameters have
#'     been predefined (i.e. PJS, EOS, sea_sites and Fallvilt), it will be sufficient
#'     to provide the parameter \code{dbservice =}.
#'
#'     Depending on whether username and password have been saved in the users
#'     profile at the current computer or not, the user is asked to input credentials.
#'
#'     \code{login_by_input} is general functions where all necessary
#'     specifications like server name and database name of the database must be
#'     input. In the case that one login to a database service for which the
#'     connection parameters have been predefined (i.e. PJS, EOS, sea_sites and
#'     Fallvilt), it will be sufficient to provide the parameter \code{dbservice =}.
#'     The user is always asked to input username and password.
#'
#'     \code{login_by_credentials} is general functions where all necessary
#'     specifications like server name and database name of the database must be
#'     input. In the case that one login to a database service for which the
#'     connection parameters have been predefined (i.e. PJS, EOS, sea_sites and
#'     Fallvilt), it will be sufficient to provide the parameter \code{dbservice =}.
#'     The user is never asked for username and password, and the function can
#'     only be used when the credentials previously have been set in the user's
#'     profile at the current computer.
#'
#'     \code{login_PJS}, \code{login_by_input_PJS}, and \code{login_by_credentials_PJS}
#'     are wrappers for the functions above where the specifications for the
#'     database journal_rapp/PJS have been preset. The user only need to input
#'     username and password. In the case that the username and password for
#'     journal_rapp/PJS have been stored in the user profile at the current
#'     computer, the user is automatically logged in to journal_rapp. If the
#'     password is no longer valid, an error occur. If so, the user must update
#'     the username and password by \code{\link{set_credentials_PJS}}.
#'
#'     \code{login_EOS}, \code{login_by_input_EOS}, and \code{login_by_credentials_EOS}
#'     are wrappers for the functions above where the specifications for the
#'     database EOS have been preset. The user only need to input username and
#'     password or if the credentials are saved in the users profile by
#'     \code{\link{set_credentials_EOS}}, no input is needed.
#'
#'     The login functions returns an open ODBC-channel to the database service.
#'     The database can then be queried by using functions in the package used for
#'     data base interface. The data base interface must be one of \code{odbc},
#'     \code{RODBC} or, \code{RPostgreSQL}. The default is given in NVIconfig and is
#'     \code{RODBC} for "SQL server" and \code{RPostgreSQL} for "PostgreSQL".
#'
#'     When the session is finished, the script shall close the ODBC-channel by
#'     \code{odbcClose("myodbcchannel")} or \code{odbcCloseAll}.
#'
#' @param dbservice Name of the database service, for example "PJS" or "EOS".
#'     For database services where one don't use the premade wrappers, the name
#'     can be chosen freely, but must be the same as used in \code{\link{set_credentials}}.
#' @param dbdriver Name of database engine.
#' @param db Name of database.
#' @param dbserver Name of database server.
#' @param dbport Port.
#' @param dbprotocol Protocol to be used.
#' @param dbinterface The R-package that is used for interface towards the data
#'     base.
#' @param dbtext used in login with input. Gives the possibility of showing
#'     another name than the dbservice in the windows asking for username and
#'     password.
#' @param \dots Other arguments to be passed from the wrappers to
#'     login_by_credentials or login_by_input
#' @return An open ODBC-channel to the database service.
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @name login_EOS-defunct
#' @keywords internal
#'
NULL

#' @name login_by_input_EOS-defunct
#' @rdname login_EOS-defunct
#' @keywords internal
#'
NULL

#' @name login_by_credentials_EOS-defunct
#' @rdname login_EOS-defunct
#' @keywords internal
#'
NULL


#' @export
#' @rdname NVIdb-defunct
#' @keywords internal
#'
login_EOS <- function(dbinterface = NULL, ...) {

  .Defunct(new = "set_PAT",
           package = "NVIdb",
           msg = paste("'login_EOS' is replaced by 'login('EOS')' as the wrapper 'login_EOS'",
                       "was rarely used and therefore considered unnecessary."))

  # .Deprecated(new = "login('EOS')",
  #             package = "NVIdb",
  #             msg = paste("'login_EOS' is replaced by 'login('EOS')' as the wrapper 'login_EOS'",
  #                         "was rarely used and therefore considered unnecessary."))
  #
  # # ARGUMENT CHECKING ----
  # # Object to store check-results
  # checks <- checkmate::makeAssertCollection()
  #
  # # dbinterface
  # checkmate::assert_choice(dbinterface, choices = c("odbc", "RPostgreSQL", "RODBC"), null.ok = TRUE, add = checks)
  #
  # # Report check-results
  # checkmate::reportAssertions(checks)
  #
  # # Set service to EOS
  # dbservice <- "EOS"
  #
  # # Use check for saved credentials to chose between login_by_input and login_by_credentials
  # if (isTRUE(NVIcheckmate::check_credentials(dbservice))) {
  #   # If credentials are saved for the user profile
  #   login_by_credentials(dbservice, dbinterface = dbinterface, ...)
  # } else {
  #   # If credentials are missing from the user profile
  #   login_by_input(dbservice, dbinterface = dbinterface, ...)
  # }
}


#' @export
#' @rdname NVIdb-defunct

login_by_input_EOS <- function(dbinterface = NULL, ...) {

  .Defunct(new = "set_PAT",
           package = "NVIdb",
           msg = paste("'login_by_input_EOS' is replaced by 'login_by_input('EOS')' as",
                       "the wrapper 'login_by_input_EOS'",
                       "was rarely used and therefore considered unnecessary."))

  # .Deprecated(new = "login_by_input('EOS')",
  #             package = "NVIdb",
  #             msg = paste("'login_by_input_EOS' is replaced by 'login_by_input('EOS')' as",
  #                         "the wrapper 'login_by_input_EOS'",
  #                         "was rarely used and therefore considered unnecessary."))
  #
  # # ARGUMENT CHECKING ----
  # # Object to store check-results
  # checks <- checkmate::makeAssertCollection()
  #
  # # dbinterface
  # checkmate::assert_choice(dbinterface, choices = c("odbc", "RPostgreSQL", "RODBC"), null.ok = TRUE, add = checks)
  #
  # # Report check-results
  # checkmate::reportAssertions(checks)
  #
  # # Oppretterknytning mot EOS
  # odbcConnection <- login_by_input(dbservice = "EOS", dbinterface = dbinterface, ...)
  #
  # return(odbcConnection)
}



#' @export
#' @rdname NVIdb-defunct

login_by_credentials_EOS <- function(dbinterface = NULL, ...) {

  .Defunct(new = "set_PAT",
           package = "NVIdb",
           msg = paste("'login_by_credentials_EOS' is replaced by 'login_by_credentials('EOS')'",
                       "as the wrapper 'login_by_credentials_EOS'",
                       "was rarely used and therefore considered unnecessary."))

  # .Deprecated(new = "login_by_credentials('EOS')",
  #             package = "NVIdb",
  #             msg = paste("'login_by_credentials_EOS' is replaced by 'login_by_credentials('EOS')'",
  #                         "as the wrapper 'login_by_credentials_EOS'",
  #                         "was rarely used and therefore considered unnecessary."))
  #
  # # ARGUMENT CHECKING ----
  # # Object to store check-results
  # checks <- checkmate::makeAssertCollection()
  #
  # # Identify if NVIconfig are installed.
  # NVIcheckmate::assert_package(x = "NVIconfig", add = checks)
  # # credentials
  # NVIcheckmate::assert_credentials(x = "EOS", add = checks)
  # # dbinterface
  # checkmate::assert_choice(dbinterface, choices = c("odbc", "RPostgreSQL", "RODBC"), null.ok = TRUE, add = checks)
  #
  # # Report check-results
  # checkmate::reportAssertions(checks)
  #
  # connection <- NVIdb::login_by_credentials(dbservice = "EOS", dbinterface = dbinterface, ...)
  #
  # return(connection)
}
