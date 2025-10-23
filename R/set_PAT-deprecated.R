#' @title Manage personal access token (PAT) for internet services
#' @description Save or remove the current user's PAT for internet services
#'     in the  the user profile.
#' @details For internet services like GitHub, personal access tokens can
#'     replace username and password when accessing the service. To simplify
#'     the access to the internet services when using R, the function
#'     \code{set_PAT} makes it possible to save the personal access token
#'     (PAT) in the user profile at the current machine. When the PAT has
#'     been saved in the user profile, the functions \code{get_PAT} will
#'     automatically get the PAT for use in code accessing the internet service.
#'
#'     The user profile is not copied between computers. Consequently, if a user
#'     runs scripts with \code{get_PAT} on different computers,
#'     the PAT has to be saved at each computer separately.
#'
#'     \code{set_PAT(service)} is used to set the PAT for a internet service.
#'     The PAT are input using windows and saved in the users profile at
#'     the current computer. When the PAT for the service has been changed,
#'     \code{set_PAT(service)} can be used to update the PAT.
#'
#'     \code{get_PAT(service)} is used to get the PAT for a internet service
#'     that previously has been saved in the users profile at the current
#'     computer.
#'
#'     \code{remove_PAT(service)} is used to delete the PAT for a internet
#'     service from the user's profile.
#'
#' @param service Name of the internet service, for example "GitHub". For
#'     internet services where one don't use the premade wrappers, the name
#'     can be chosen freely, but must be the same as used in \code{get_PAT}
#' @return \code{set_PAT} The PAT for a internet service are saved in the
#'     user profile at the current computer.
#'
#'     \code{get_PAT} The PAT for a internet service are fetched from the
#'     user profile at the current computer to be used in R-scripts.
#'
#'     \code{remove_PAT} The PAT for a internet service are deleted from
#'     the user profile at the current computer.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @name set_PAT-deprecated
#' @usage set_PAT(service)
#' @usage get_PAT(service)
#' @usage remove_PAT(service)
#' @keywords internal
#' @examples
#' \dontrun{
#' set_PAT("GitHub")
#'
#' get_PAT("GitHub")
#'
#' remove_PAT("GitHub")
#' }
NULL

#' @name get_PAT-deprecated
#' @rdname set_PAT-deprecated
#' @keywords internal
#'
NULL

#' @name remove_PAT-deprecated
#' @rdname set_PAT-deprecated
#' @keywords internal
#'
NULL


#' @title \code{set_PAT}, \code{get_PAT}, and \code{remove_PAT} is deprecated
#' @description \code{set_PAT}, \code{get_PAT}, and \code{remove_PAT} was
#'     deprecated from NVIdb v0.11.0 released 2023-09-22. These functions
#'     should be replaced by corresponding functions in package 'gitcreds'
#'     that are better, more flexible and maintained at cran.
#' @details The old help pages can be found at \code{help("set_PAT-deprecated")}.
#'     Information on deprecated functions can be found at \code{help("NVIdb-deprecated")}.
#' @param service Name of the internet service, for example "GitHub". For
#'     internet services where one don't use the premade wrappers, the name
#'     can be chosen freely, but must be the same as used in \code{get_PAT}
#' @export
#' @rdname NVIdb-defunct
#' @keywords internal
#'
set_PAT <- function(service) {

  .Defunct(new = "set_PAT",
              package = "NVIdb",
              msg = paste("'set_PAT', 'get_PAT', and 'remove_PAT' should be replaced by",
                          "corresponding functions in package 'gitcreds'. These functions",
                          "are better, more flexible and maintained at cran."))
  
  # .Deprecated(new = "set_PAT",
  #             package = "NVIdb",
  #             msg = paste("'set_PAT', 'get_PAT', and 'remove_PAT' shouild be replaced by",
  #                         "corresponding functions in package 'gitcreds'. These functions",
  #                         "are better, more flexible and maintained at cran."))
  # 
  # # ARGUMENT CHECKING service ----
  # checkmate::assert_character(x = service, min.chars = 1, len = 1, any.missing = FALSE)
  # 
  # # Removes previously set PAT for the database service
  # remove_PAT(service)
  # 
  # # Open window for input of PAT to the given service and saves service and PAT in the user's profile
  # # Use the service name as input to username
  # keyring::key_set_with_value(service = service,
  #                             username = service,
  #                             # password = getPass::getPass(paste("Enter your PAT for", service)),
  #                             password = askpass::askpass(prompt = paste("Enter your PAT for", service)),
  #                             keyring = NULL)
}


#' @export
#' @rdname set_PAT

get_PAT <- function(service) {

  .Defunt(new = "get_PAT",
              package = "NVIdb",
              msg = paste("'set_PAT', 'get_PAT', and 'remove_PAT' should be replaced by",
                          "corresponding functions in package 'gitcreds'. These functions",
                          "are better, more flexible and maintained at cran."))

  # .Deprecated(new = "get_PAT",
  #             package = "NVIdb",
  #             msg = paste("'set_PAT', 'get_PAT', and 'remove_PAT' shouild be replaced by",
  #                         "corresponding functions in package 'gitcreds'. These functions",
  #                         "are better, more flexible and maintained at cran."))
  # 
  # # Error handling
  # # 1. keyring package is missing
  # # Use of require is avoided as loading packages should be avoided in package functions
  # # This implies that there is no check of keyring is correctly installed
  # if (!is.element("keyring", utils::installed.packages()[, 1])) {
  #   stop("Package keyring need to be installed for this function to work")
  # }
  # 
  # # 2. Credentials for service are missing from the user profile
  # if (!is.element(tolower(service), tolower(keyring::key_list()[, 1]))) {
  #   stop(paste("PAT for",
  #              service,
  #              "is not available for the current user on this computer"))
  # }
  # 
  # # Identifies the spelling of service with regard to lower and upper case
  # # This is used in Connect-statement below to ensure correct spelling when fetching User ID and Password
  # service <- keyring::key_list()[which(tolower(keyring::key_list()[, 1]) == tolower(service)), 1]
  # 
  # # fetch the PAT
  # PAT <- keyring::key_get(service, as.character(keyring::key_list(service)[2]))
  # 
  # return(PAT)
}


#' @export
#' @rdname set_PAT

remove_PAT <- function(service) {

  .Defunct(new = "remove_PAT",
              package = "NVIdb",
              msg = paste("'set_PAT', 'get_PAT', and 'remove_PAT' should be replaced by",
                          "corresponding functions in package 'gitcreds'. These functions",
                          "are better, more flexible and maintained at cran."))

  # .Deprecated(new = "remove_PAT",
  #             package = "NVIdb",
  #             msg = paste("'set_PAT', 'get_PAT', and 'remove_PAT' shouild be replaced by",
  #                         "corresponding functions in package 'gitcreds'. These functions",
  #                         "are better, more flexible and maintained at cran."))
  # 
  # # ARGUMENT CHECKING ----
  # checkmate::assert_character(x = service, min.chars = 1, len = 1, any.missing = FALSE)
  # 
  # # REMOVE ALL EXISTING CREDENTIALS FOR service
  # # Checks if there are registered PAT for the database service
  # # Removes the service until no more service are registered
  # while (is.element(tolower(service), tolower(keyring::key_list()[, 1]))) {
  #   # Identifies the spelling of service with regard to lower and upper case
  #   # This is used in Connect-statement below to ensure correct spelling when fetching User ID
  #   services <- keyring::key_list()[which(tolower(keyring::key_list()[, 1]) == tolower(service)), 1]
  #   usernames <- keyring::key_list()[which(tolower(keyring::key_list()[, 1]) == tolower(service)), 2]
  # 
  #   # Removes the key for all combinations of service and username
  #   for (i in 1:length(services)) {
  #     keyring::key_delete(service = services[i], username = usernames[i])
  #   }
  # }
}
