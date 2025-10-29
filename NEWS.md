# NVIdb 0.14.0.9000 - (2025-##-##)

## New features:

- `add_MT_omrader` can now be used for finding MT-areas in accordance with the new organisation of MT from 2025. 


## Bug fixes:

- corrected set_dir_NVI for use on workbench.


## Other changes:

-


## BREAKING CHANGES:

- `set_PAT`, `get_PAT`, and `remove_PAT` are defunct. Functions from the package \code{gitcreds} should be used instead. See `NVIdb-defunct` (help("NVIdb-defunct")).

- The wrapper functions `login_EOS`, `login_by_input_EOS`, and `login_by_credentials_EOS` are defunct. Use the corresponding login-functions with the argument `dbservice` = "EOS" instead. See `NVIdb-defunct` (help("NVIdb-defunct")).

- The wrapper functions `login_PJS`, `login_by_input_PJS`, and `login_by_credentials_PJS` are defunct. Use the corresponding login-functions with the argument `dbservice` = "PJS" instead. See `NVIdb-defunct` (help("NVIdb-defunct")).

- The wrapper function `set_credentials_EOS` is defunct. Use `set_credentials(dbservice = "EOS")` instead. See `NVIdb-defunct` (help("NVIdb-defunct")).



# NVIdb 0.14.0 - (2025-09-19)

## New features:

- Created read_avlsgris to read the register of nucleus and multiplier pig herds.

- Created read_purkering, to read the register of sow pools herds, i.e. the central herd and the satellite herds.

- Created find_file_near_date that finds the file nearest to a wanted date based on the date included in the file name. 


## BREAKING CHANGES:

- `set_dir_NVI` is updated and require NVIconfig >= v0.10.0.


# NVIdb 0.13.2 - (2025-06-23)

## New features:

- `set_dir_NVI` is updated to give correct path also when working on Workbench


# NVIdb 0.13.1 - (2024-12-19)

## New features:

- `standardize_columns` now accepts list or filename and path as input to `standards`.


# NVIdb 0.13.0 - (2024-12-13)

## Bug fixes:

- Fixed `login_by_input` for log in when using `dbinterface = "RPostgreSQL"`.


## Other changes:

- Included a meaningful error message for `read_Prodtilskudd` if no versions of Produksjonstilskudd was available for the selected years and months.

- Improved help by standardising parameter description for functions.

- Changed pipe to internal pipe |> and updated dependencies to R 4.1.0.


## BREAKING CHANGES:

- Deprecated all functions that have been moved to NVIpjsr, i.e. `add_PJS_code_description`, `build_query_hensikt`, `build_query_one_disease`, `build_query_outbreak`, `build_sql_select_code`, `build_sql_select_year`, `choose_PJS_levels`, `copy_PJS_code_2_text`, `exclude_from_PJSdata`, `login_by_credentials_PJS`, `read_eos_data`, `read_PJS_code_2_text`, `retrieve_PJSdata`, `select_PJSdata_for_value`, `set_disease_parameters`, `standardize_eos_data`, `standardize_PJSdata`, `transform_code_combinations`.


# NVIdb 0.12.0 - (2024-05-22)

## New features:

- Added argument `year` to `add_kommune_fylke`. Kommune and fylke can now be translated to any year from 1977 to current year from previous years. It is not possible to translate from a later year to previous years.


# NVIdb 0.11.3 - (2024-05-03)

## Other changes:

- improved and styled help for most functions.


# NVIdb 0.11.2 - (2024-04-05)

## Bug fixes:

- `retrieve_PJSdata` now accepts input for `selection_parameters` that includes `FUN` and `selection_statement`.

- `set_disease_parameters` now accepts input for `selection_parameters` that includes `FUN`. 


## BREAKING CHANGES:

- For `retrieve_PJSdata`, the arguments `FUN` and `select_statement` are deprecated. These should instead be included in the list input to `selection_parameters`. See NVIdb-deprecated (help("NVIdb-deprecated")).

- The wrapper functions `login_EOS`, `login_by_input_EOS`, and `login_by_credentials_EOS` are deprecated. Use the corresponding login-functions with the argument `dbservice` = "EOS" instead. See NVIdb-deprecated (help("NVIdb-deprecated")).

- The wrapper function `set_credentials_EOS` is deprecated. Use `set_credentials("EOS")` instead. See NVIdb-deprecated (help("NVIdb-deprecated")).


# NVIdb 0.11.1 - (2024-02-05)

## New features:

- set_disease_parameters now accepts the arguments purpose, FUN and select_statement.


## Bug fixes:

- set_disease_parameters now accepts metodekode with 2 digits.


# NVIdb 0.11.0 - (2024-01-24)

## New features:

- Created `retrieve_PJS_data` to retrieve and standardise PJSdata.

- Created `transform_code_values` to easy transform few code combinations into other values.

- Created `select_PJSdata_for_values` to perform inclusion/exclusion of rows in PJSdata based on coded values. For codes ending with "%", sub levels are included.

- `read_Prodtilskudd` now accepts the argument `exctracted_date`.

- The PJS- and EOS-wrappers for the login functions `login`, `login_by_credentials` and `login_by_input` now accepts arguments to be passed to the parent functions. Thereby, one may change login information for the databases PJS and EOS by modifying the scripts until a new version of the package `NVIconfig` is available and installed.


## Bug fixes:

- Corrected `read_varekode` so accepts old source files in latin1.


## Other changes:

- Modified help for several functions.

- Modified `set_disease_parameters` to make the name of the input arguments easier to comprehend. New arguments are `analytt2delete`, `include_missing_art` and `selection_parameters`. The argument `selection_parameters` accepts both a "purpose" file and a list (as generated by as `set_disease_parameters`) as input.


## BREAKING CHANGES:

- In `set_disease_parameters` the arguments `missing_art` and `file` is deprecated. These are replaced by `include_missing_art` and `selection_parameters`, respectively. If using the old arguments, the input will be transferred to the new arguments. See NVIdb-deprecated (help("NVIdb-deprecated")).

- `set_PAT`, `get_PAT`, and `remove_PAT` was deprecated.These should be replaced by corresponding functions in package `gitcreds`. See NVIdb-deprecated (help("NVIdb-deprecated")).


# NVIdb 0.10.1 - (2023-05-31)

## New features:

- `add_PJS_code_description` is extended so that analytt and kjennelse at sakskonklusjon-level can be translated when using the arguments `PJS_variable_type` = "auto" and `new_column` = "auto". 


## Other changes:

- Improved help by standardising argument description for a few functions.


# NVIdb 0.10.0 - (2023-04-18)

## New features:

- Created `standardize_eos_data` for standardisation of column names etc. in eos-data.

- `set_dir_NVI` now accepts argument `slash`. set `slash = FALSE` to remove ending slash.

- `add_PJS_code_description` now accepts the argument `impute_old_when_missing`.


## Bug fixes:

- `add_PJS_code_description` now performs back translation also in the case that the code has expired ("utg\u00E5tt").


## Other changes:

- Minor updates of help.


# NVIdb 0.9.0 - (2023-01-16)

## New features:

- Created `read_eos_data` for reading one or more years from EOS data for one surveillance programme from exported csv-files.

- Created `read_varekode` for reading the formatted varekoderegister or the raw data to update the formatted varekoderegister.

- `login`, `login_by_credentials` and `login_by_input` now accept the argument `dbinterface` with the R-package names "RODBC", "odbc" or, "RPostgreSQL" as input.Thanks to @krdean for providing code for logging in by odbc.

- Created `cut_slash` to remove ending slash from path. This is useful if pathnames should be created using `base::file.path`.


## Bug fixes:

- Fixed `add_PJS_code_description` when argument `backwards` = "TRUE" for R > 4.2.


## Other changes:

- Corrected argument checking of `pkode_year` in `read_Prodtilskudd` and `copy_Prodtilskudd`.


# NVIdb 0.8.0 - (2022-10-25)

## New features:

- `build_query_outbreak` builds a query for retrieving outbreak data from PJS. Potential inputs arguments are `period`, `hensiktkode`, `utbrudd_id`, `analyttkode` and `metodekode`. 

- `set_disease_selection_parameters` creates a list object with selection parameters from input arguments or by sourcing an R script that sets the input values (a "purpose" file).

- `read_Pkode_2_text` now reads translation table for Pkoder from 1995 to current year as default. Column names are changed in accord with standard conventions. Use `keep_old_names` = "TRUE" to keep old names. 

- `add_PJS_code_description` can now perform backward translation from PJS description text to PJS-code. Use `backward` = "TRUE". Backward translation only works when the description text is unique per code. Case is ignored for backward translation.

- `add_PJS_code_description`, `choose_PJS_levels`, and `standardize_PJS_data` now handles the levels subundersokelse and subresultat. 

- `set_dir_NVI` now accepts abbreviated input for `datasource`. The abbreviation must be unique.

- add-functions (`add_kommune_fylke`, `add_MT_omrader`, `add_PJS_code_description`, `add_poststed` and `add_produsent_properties`) now accepts abbreviated input for `position`. The abbreviation must be unique.


## Bug fixes:

- `add_produsent_properties` replaces `add_produsent`. `add_produsent_properties` now correctly adds geo-coordinates also when information on geo-coordinates is missing.

- copy-functions (`copy_kommune_fylke`, `copy_MT_omrader`, `copy_PJS_code_2_text`, `copy_Pkode_2_text`, `copy_poststed`, `copy_prodnr_2_current_prodnr`, and `copy_Prodtilskudd`),  now accepts `from_path` and `to_path` that ends with "\\".

- read-functions (`read_kommune_fylke`, `read_leveransereg`, `read_MT_omrader`, `read_PJS_code_2_text`, `read_Pkode_2_text`, `read_poststed`, `read_prodnr_2_coordinates`, `read_prodnr_2_current_prodnr`, `read_Prodtilskudd`, and `read_sonetilhorighet`),  now accepts `from_path` that ends with "\\".

- `exclude_from_PJSdata` now returns correct data frame if `quality` = "include".


## Other changes:

- Created vignette "Retrieve and standardise PJS data". 

- Included NVIdb reference manual as a PDF-file in the vignettes.

- Updated `README` and corrected the install instruction. 

- Updated the vignette "Contribute_to_NVIdb".

- Updated and improved help for many functions.


## BREAKING CHANGES:

- `add_produsent` is deprecated, use `add_produsent_properties` instead. See NVIdb-deprecated (help("NVIdb-deprecated")).

- Default translation table read by `read_Pkode_2_text` has new default column names. Use `keep_old_names` = `TRUE` to keep old names. 


# NVIdb 0.7.1 - (2022-08-24)

## Bug fixes:

- fixed problem with installation when `build_vignette` = `TRUE`.


# NVIdb 0.7.0 - (2022-03-14)

## New features:

- `add_PJS_codes_2_text` now accepts `PJS_variable_type` = "auto" and `new_column` = "auto" if the input to `code_colname`  is a vector of standardized column names for PJS-columns.


## Bug fixes:

- `read_leveransereg` now correctly reads numeric values with comma as decimal separator.

- `standardize_columns` don't get an error when -`property` = "colclasses" and the filename has a space.

- `standardize_PJS` creates fagnr without a hyphen between fagkode and number.


## Other changes:

- Help for `standardize_PJSdata` is improved and styled.

- Improved argument checking of `add_PJS_code_description`.

- Improved help for `add_poststed`, `add_MT_omrader`, and `add_PJS_code_description`.

- `README` and the vignette "Contribute_to_NVIdb" are updated.


# NVIdb 0.6.0 - (2021-09-26)

## New features:

- Improved argument checking of `add_poststed`, `add_kommune_fylke`, `add_MT_omrader`, `add_produsent`, `add_lokalitet`, `set_credentials`, `remove_credentials`, `login_by_credentials`.


## Other changes:

- The vignette "Contribute to NVIdb" have been updated.


# NVIdb 0.5.4 - (2021-05-10)

## Bug fixes:

- `standardize_columns` now accepts arguments for `property` written with both lower and upper case.


# NVIdb 0.5.3 - (2021-05-07)

## New features:

- `add_PJS_code_description` now translates registertype (type of location or address) and konkl_type (conclusion type) to descriptive text.

- `standardize_PJSdata` now transforms sak_forst_avsluttet (from v_innsendelse) to date format.


## Bug fixes:

- There was a problem with `standardize_columns` and the function was rewritten to use `dplyr` in stead of `poorman`.


# NVIdb 0.5.2 - (2021-04-24)

## Bug fixes:

- The selection statement when selecing PJS-codes, for example hensiktkode, is now correct also in he case that only codes with sub-levels are included.

- Corrected `add_kommune_fylke` so that Møre og Romsdal and Nordland are added correctly to fylkenr 15 and 18, respectively, in the case that gjeldende_fylkenr and/or gjeldende_fylke are added to fylkenr


# NVIdb 0.5.1 - (2021-04-08)

## Other changes:

- Corrected licence, authors and copyright holder.


# NVIdb 0.4.0.9001 - (2021-04-07)

## New features:

- `exclude_from_PJSdata` excludes samples commonly excluded, i.e. samples from abroad and samples registered as quality assurance.

- `standardize_PJSdata` standardizes PJSdata as extraced from PJS when using `RODBC`. The unnecessary columns konkl_provenr and vet_distriktnr are removed, the column names are standardized using `standardize_columns`,  numeric variables are transformed to numbers, date variables are transformed to date format, character variables are trimmed for leading and trailing spaces, the variables saksnr and, if possible, fagnr are generated, test data, i.e. saker with ansvarlig_seksjon in c("14", "99") are deleted.

- `build_query_one_disease` replaces `build_selection_PJS_one_disease`. The year argument now accepts a year range and analytt accepts both analytter with or without sublevels.

- `build_query_hensikt` replaces `build_selection_PJS_hensikt`. The year argument now accepts a year range and hensikt accepts hensikter with sublevels.

- `build_sql_select_year` builds the code to select one or a range of years.

- `build_sql_select_code` builds the code to select by hierarchical codes in PJS, one or many with or without sublevels.


## Other changes:

- The package is made dependent on the package `NVIconfig`. `NVIconfig` is not imported automatically when installing `NVIdb` and you need to install it manually from GitHub.


## BREAKING CHANGES:

- The `data` argument in `standardize_columns` should now also be used for csv-file. Removed the argument `csvfile` from `standardize_columns`.

- `build_selection_PJS_one_disease` is replaced by `build_query_one_disease`

- `build_selection_PJS_hensikt` is replaced by `build_query_hensikt`


# NVIdb 0.4.0 - (2021-02-13)

## New features:

- `build_selection_PJS_one_disease` generates code for selection of all PJS data for one disease based on analytt (disease and agent) and specific hensikter and metoder in one year.

- `build_selection_PJS_hensikt` generates code for selection of all PJS data for one or more hensikt in one year.

- `choose_PJS_levels` makes a new data frame that includes the columns from the given PJS-levels in a data frame with PJS data. The column names in the data frame must have standardized name as when using `standardize_columns`. The function is currently made ready for the levels c("sak", "prove", "delprove", "undersokelse", "resultat", "konklusjon") as read from PJS using the view "v2_sak_m_res".

- `read_sonetilhorighet` reads the file "sonetilhorighet.txt" with production zones and geo-coordinates for aquaculture sites.

- `add_lokalitet` adds production zones and/or geo-coordinates for aquaculture sites based on aquaculture site number.


## Bug fixes:

- `read_Prodtilskudd` now accepts both character and numeric input to Pkode_year

- `standardize_columns` now translates all combinations of lower and upper case ae, oe, and aa to Norwegian characters for column labels

- `standardize_columns`, `property` = "colorder" is not stopping even if no column standard is given. `exclude` = `FALSE` is corrected.


# NVIdb 0.3.0 - (2020-11-14)

## New features:

- new predefined paths is available through `set_dir_NVI`

- `standardize_columns`: `standardize_columns` is now able to generate standard labels (`property` = "collabels" in Norwegian (language = "no") or Englsh (language = "en"). For column names that are not translated to English yet, the Norwegian column label will be given. Standard column widths for Excel (`property` = "colwidths_Excel) for use when generating Excel sheets with `openxlsx`. Standard column order (`property` = "colorder") to sort columns in data frames. The parameter `exclude` = `FALSE` will exclude columns with information of sort order to fast make standard Excel sheets.

- all add-functions includes the parameter `overwrite`. The parameter `overwrite` = `TRUE`/`FALSE` is included for the case that a column already exist in the data frame and one want to replace the column with new content. The `position` = "keep" is used to keep the column at the same position as the original column in the data frame. The add-functions are `add_kommune_fylke`, `add_MT_omrader`, `add_PJS_code_description`, `add_poststed` and `add_produsent`.

- `read_prodnr_2_coordinates`: Reads file with coordinates. The coordinates can be merged into a data frame by using `add_produsent`.


## Bug fixes:

- `read_Prodtilskudd`: The function don't give an error message in the case that JSON - files has been read into the source directory, but UTF8 files have not yet been generated.


# NVIdb 0.2.2 - (2020-11-04)

## Bug fixes:

- `read_Prodtilskudd`: parameter `Pkode_month` = "last" is now accepted for all input to `Pkode_year`.


# NVIdb 0.2.1 - (2020-11-04)

## Bug fixes:

- `set_dir_NVI`: predefined path to OKprogrammer was corrected.


# NVIdb 0.2.0 - (2020-11-03)

## New features:

- `read_leveransereg`: reads Leveranseregister for slakt into a data frame

- the parameter `position` = c("first", "left", "right", "last") was included for the functions `add_produsent`, `add_MT_omrader` and `add_PJS_code_description`, and is thereby included for all add-functions.


## Bug fixes:

- `login`, `login_by_input`, and `login_by_credentials` now accept the name of the data base service as the only parameter and finds the remaining parameters for all predefined data base services, not only PJS. Currently the data base services c("PJS", "EOS", "sea_sites", "fallvilt") have predefined parameters.

- `read_Prodtilskudd`: removed warning when two or more months where given as input to `pkode_month`.

- `standardize_columns`: corrected input for `property` = "colnames" to be in accordance with help.

- `read_Pkode_2_text`: corrected reading of Telledato so it is read as character.


## BREAKING CHANGES:

- Removed the wrapper functions `login_sea_sites`, `login_by_input_sea_sites`, `login_by_credentials_sea_sites`, and `set_credentials_sea_sites` as the functions were not used. The functions are replaced by `login`("sea_sites"), `login_by_input`("sea_sites"), `login_by_credentials`("sea_sites"), and `set_credentials`("sea_sites").


# NVIdb 0.1.7 - (2020-10-12)

## Bug fixes:

- `add_kommune_fylke`: In v0.1.5 an error was introduced if fylke and fylke only was added to fylkenr. Now the values of fylke is correct inserted instead of missing values.


# NVIdb 0.1.6 - (2020-10-07)

## New features:

- `standardize_columns`: function to transform variable names in database tables into standardized column names in data frames. It also can make a named vector to be used in `colclasses` = when reading a csv-file.


## Bug fixes:

- `add_kommune_fylke`: There was an error if gjeldende_komnr was renamed to komnr. This was a bug affecting all add-functions. The add-functions now accept changing the name of new variables to the code variable in the translation table.


## Other changes:

- provided vignette on how to "Contribute to NVIdb"


# NVIdb 0.1.5 - (2020-08-28)

## New features:

- `login`, `login_by_input` and `login_by_credentials` now accepts `dbservice` as the only parameter for database services where the connection parameters have been predefined. This means that `login_by_credentials`("PJS") will do the same as `login_by_credentials_PJS`.

- included predefined parameters for connecting to the database for fallvilt transferred from Naturdata


## Bug fixes:

- `login`: if parameter input `dbservice` was given with upper case letters, one would always be asked for credentials even if the credentials were stored in the user profile. The dbservice parameter now accepts both lower and upper case letters.

- `read_poststed`: ensured that column postnr and komnr is read as character, not number

- `add_kommune_fylke` duplicated rows when translating from fylkenr to gjeldende_fylkenr. Ensured translation to only valid gjeldende_fylkenr.


# NVIdb 0.1.4 - (2020-08-20)

## Bug fixes:

- `read_Prodtilskudd`: fixed error introduced in v-0.1.3 when correcting for column names in the source files that had been changed.


# NVIdb 0.1.3 - (2020-08-19)

## New features:

- introduced `copy_poststed` and `read_poststed` to manage translation table for poststed.

- introduced `add_poststed` to translate postnr to poststed and komnr.

- introduced `copy_Prodtilskudd` to copy the latest versions of the Prodtilskudd register for selected years ans months.


## Bug fixes:

- `read_Prodtilskudd`: fixed error because the column names in the source files had been changed.


# NVIdb 0.1.2 - (2020-08-10)

## New features:

- `read_Prodtilskudd` includs the option "last" for the parameter `Pkode_month` to select the last Produksjonstilskuddsregister.


# NVIdb 0.1.1 - (2020-07-10)

## New features:

- Included translation for metode and seksjon in `add_PJS_code_description`.


## Bug fixes:

- Removed `get_PAT`, `set_PAT` and `remove_PAT` from NAMESPACE.


# NVIdb 0.1.0 - (2020-07-09)

## New features:

- Introduced `read_prodnr_2_current_prodnr` that reads the translation table "Prodnr2GjeldendeProdnr.csv".

- Improved `add_produsent` so that when no current produsentnr is found for the original produsentnr,
  the original produsentnr is imputed for current produsentnr.

- Introduced `read_Pkode_2_text` and `copy_Pkode_2_text` to manage the register with translation table
  for Produksjonstilskuddskoder.

- Introduced `read_kommune_fylke` and `copy_kommune_fylke` to manage the register with translation tables from kommunenr to kommune, fylke and current kommune and fylke.

- Impoved `add_kommune_fylke` to translate from kommunenr to kommune, fylke and current kommune and fylke.

- Included the parameter `position` = c("front", "left", "right", "last") in the functions `add_kommune_fylke` and `add_MT_omrader` for better control of column position for new columns.


## Other changes:

- Standardised `add_kommune_fylke` by introducing the parameters `code_column` and `new_column` as in other
  add-functions.


## BREAKING CHANGES:

- Removed the parameters `code_colname` and `current_code_colname` from `add_kommune_fylke`.

- Changed column names for produsentnummer from Produsentnr to prodnr and kommunenr to komnr.


# NVIdb 0.0.1 - (2020-06-10)

## New features:

- `set_dir_NVI` sets the correct path to the directories for Produsentregisteret, Produksjonstilskudd,
  Leveranseregisteret og GrunndataLand.


## Other changes:

- Changed dependency of R from R >= 3.5.2 to R >= 3.4.0.


# NVIdb 0.0.0.9004 - (2020-04-27)

## New features:

- `add_PJS_code_description` now accept vectors and can translate more than one PJS-variable at a time.

- `add_MT_omrader`, `read_MT_omrader` and `copy_MT_omrader` are function for managing translation of komnr to MT_avdeling and MT_region in dataframes.

- `read_Prodtilskudd` reads Prodtilskudd from 2019.


# NVIdb 0.0.0.9003 - (2020-03-31)

## BREAKING CHANGES:

- Changed function name from `add_current_komnr` to `add_kommune_fylke`. The functionality hasn`t changed yet.


# NVIdb 0.0.0.9002 - (2020-03-25)

## BREAKING CHANGES:

- Changed function names for connecting to the database sea sites at Azyre. Changed from seacites to sea_sites for the functions `login_sea_sites`, `login_by_credentials_sea_sites`, `login_by_input_sea_sites` and `set_credentials_sea_sites`.


# NVIdb 0.0.0.9001 - (2020-03-25)

## New features:

- Included functions to connect to the database seacites at Azyre. These are wrappers for `set_credentials` and `login`.


## Bug fixes:

- Included import of package `getPass`. This is necessary for `login_by_input` to work.


# NVIdb 0.0.0.9000 - (2020-03-19)

## First release:

  - The package comprises functions for managing credentials and login (including wrappers for PJS and EOS).

  - functions for translating PJS-codes to descriptive text.

  - functions to translate kommunenr to current kommunenr.
