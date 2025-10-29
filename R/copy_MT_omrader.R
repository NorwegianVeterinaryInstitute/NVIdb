#' @export
#' @rdname add_MT_omrader

copy_MT_omrader <- function(filename = list("komnr_2_MT_avdeling.csv", "MT_omrader.csv", "komnr_2_MT_enhet.csv"),
                            from_path = base::paste0(set_dir_NVI("GrunndataLand"), "FormaterteData/"),
                            to_path = NULL) {

  # PREPARE ARGUMENT ----
  # Removing ending "/" and "\\" from pathnames
  from_path <- sub("/+$|\\\\+$", "", from_path)
  to_path <- sub("/+$|\\\\+$", "", to_path)

  # ARGUMENT CHECKING ----
  assert_copy_function(filename = filename,
                       from_path = from_path,
                       to_path = to_path)

  # COPY FILE ----
  for (i in c(1:length(filename))) {
    copy_file_if_updated(filename = filename[[i]], from_path = from_path, to_path = to_path)
  }

}
