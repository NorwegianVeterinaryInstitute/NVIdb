#' @title NVIdb: A package to facilitate the use of the Norwegian Veterinary Institute's databases.
#'
#' @description The NVIdb package provides functions to facilitate downloading and processing of data from NVI's databases, in particular PJS and EOS.
#'     The package comprises four categories of functions:
#'     manage credentials, login, read and copy data from in-house data registers and translation of codes into descriptive text.
#'
#' @section Manage credentials:
#' Set and remove credentials (i.e. password and user name) in the user profile at the current machine. These functions makes it possible
#' to connect to database services automatically in scripts while avoiding hard coding of the password and the user name.
#'
#' @section Login:
#' These functions use the credentials set by functions for managing credentials to automatically login to database services.
#' If the credentials have not been set, there are also login function for interactive input of credentials in windows when running scripts.
#'
#' @section Read, copy and update R-data from in-house data registers:
#' The NVI has copies of several data registers like kommuneregister, fylkesregister, register for søknad om Produksjonstilskudd.
#' The aim of these functions are to make these registers easily accessible for R-scripts to ensure that one always uses the latest version
#' of data. These functions reads the registers from where they are saved at NVI internal file system. If necessary, there are function to
#' copy or update the registers to local directories when local versions are needed for example for shiny applications.
#'
#' @section Translate codes into descriptive text:
#' Data often only includes codes that should be translated into the description text. These functions perform the translation for
#' PJS-codes and codes like komnr.
#'
#' @keywords internal
"_PACKAGE"
# #' @docType package
#' @aliases NVIdb-package
# #' @name NVIdb
NULL
