# QCkit v1.2.1
## 2025-05-22
* fix minor bug in create_datastore_script

# QCkit v1.2.0
## 2025-05-20
* create_datastore_script modified to allow upload of large (>32mb) repos

# QCkit v1.1.0
## 2025-07-25
* add function `get_user_orcid`, which will look up a users orcid using active directory. 

## 2025-05-16
* create_datastore_script now invisibly returns the DataStore reference URL

# QCkit v1.0.1

## 2025-03-25
* fix bug that caused several functions to fail to detect certain .csv files

2025-03-12
* Updated license to MIT which is JOSS, NPS, and R compatible!

2025-03-10
* Updated license to OSI-approved "Zero-Clause BSD" in support of JOSS submission.

2025-03-06
* Begin development of `unit_codes_to_names` function to translate NPS unit codes into full unit (park) names
* Add basic unit test for `unit_codes_to_names`

2025-02-25
* Updated `CONTRIBUTING.md`.

2025-02-21
* Add `CONTRIBUTING.md` file

# QCkit v1.0.0 
2024-07-16
* Added experimental function `document_missing_values()`, which searches a file for multiple missing value codes, replaces them all with NA, and generates a new column with the missing value codes so that they can be properly documented in EML. This is a work-around for the fact that there is currently not a good way to get multiple missing value codes in a single column via EMLassemblyline. This function is still under development; expect substantial changes an improvements up to and including removing the function entirely.

2024-07-09
* Added function `get_user_email()`, which accesses NPS active directory via a powershell function to return the user's email address. Probably won't work for non-NPS users and probably won't work for non-windows users.
* Updated rest API from legacy v6 to current v7.

2024-06-28
* Updated `get_park_polygon()` to use the new API (had been using a legacy API). Added documentation to specify that the function is getting the convexhull for the park, which may not work particularly well for some parks.
2024-06-27
* bug fixes for `generate_ll_from_utm()`
* add function `remove_empty_tables()` (and associated unit tests)
* update documentation for `replace blanks()` to indicate it can replace blanks with more than just NA

# QCkit v0.1.7
2024-05-08
* Updated the `replace_blanks()` function to accept any missing value code a user inputs (but it still defaults to NA).
2024-04-18
* Added the function `generate_ll_from_utm()` which supersedes `convert_utm_to_ll()` and improves upon it in several ways, included accepting a column of UTMs and also returns a column of CRS along with the decimal degrees latitude and longitude.
2024-04-17
* Major updates to the DRR template including: using snake case instead of camel case for variables; updating Table 3 to only display filenames only when there are multiple files, fixed multiple issues with footnotes, added citations to NPSdataverse packages, added a section that prints the R code needed to download the data package and load it in to R.
* Updated the DRR documentation to account for new variable names.

# QCkit v0.1.6
2024-03-07
* Update error warning in `check_te()` to not reference VPN since NPS no longer uses VPN.
* add private function `.get_unit_bondaries()`: hits ArcGIS API to pull more precise park unit boundaries than `get_park_polgyon()`
* add `validate_coord_list()` function that takes advantage of improved precision of `.get_unit_boundaries()` and is vectorized, enabling users to input multiple coordinate combinations and park units directly from a data frame.

# QCkit v0.1.5
2024-02-09
* This version adds the DRR template, example files, and associated documentation to the QCkit package.
* Bugfix in `get_custom_flag()`: it was counting both A (accepted) and AE (Accepted, estimated) as Accepted. Fixed the regex such that it Accepted will include all cells that start with A followed by nothing or by any character except AE such that flags can have explanation codes added to them (e.g. A_jenkins if "Jenkins" flagged the data as accepted)

# QCkit v0.1.4
2024-01-23
* Maintenance on `get_custom_flag()` to align with updated DRR requirements
* Added function `replace_blanks()` to ingest a directory of .csvs and write them back out to .csv (overwriting the original files) with blanks converted to NA (except if a file has NO data - then it remains blank and needs to be dealt with manually)

# QC kit v0.1.3
2023-12-18
* Bugfix for `create_datastore_script()`
* Added automated build check and pkgdown github pages builds via github actions
* Code cleanup via linter suggests 
* Added warning about turning off VPN for the `covert_utm_to_ll()` documentation
* Moved `convert_datetime_format()` from DPchecker to QCkit
* Added function `fix_utc_offset()`

# QCkit 0.1.2 "Automated Shuttle Pilot"
2023-11-20
* Added the function `create_datastore_scipt()`, which given a username and repo for GitHub will generate a draft Script Reference on DataStore based on the information found in the latest Release on GitHub.

24 April 2023
* fixed bug in `get_custom_flags()`.

# QCkit 0.1.0.4
17 April 2023

* `get_elevation()` new function for getting elevation from GPS coordinates via USGS API.

21 March 2023 

* `order_cols` new function for ordering columns added 

16 March 2023

* Added a new function, `get_taxon_rank()` which takes a column of scientific names and generates a new column with the most specific scientific name rank listed. It does this purely based on recognizing patterns in the scientific naming scheme and not by matching a list of known genera, families, etc.
* Consolidated `get_taxon_rank()` and `te_check()` into a single file, taxonomy.R.
* Deprecated `te_check()` in favor of `check_te()`
* Deprecated `DC_col_check()` in favor of `check_dc_cols()`
* Deprecated `utm_to_ll()` in favor of `convert_utm_to_ll()`
* Deprecated `long2UTM()` in favor of `convert_long_to_utm()`

***

# QCkit 0.1.0.3
28 February 2023 

* `te_check()` bug fix - exact column name filtering allows for multiple columns with similar names in the input data column. Improved documentation for transparency.

23 February 2023

* updated `te_check()`. It now supports searching multiple park units.

22 February 2023

* updated `te_check()`. Now prints the source of the federal match list data and the date it was accessed to the console. Made the output format prettier. Added an "expansion" option to the function. Defaults to expansion = FALSE, which checks for exact matches between the scientific binomial supplied by the user and the full scientific binomial in the matchlist. When expansion = TRUE, the genera in the data supplied will be checked against the matchlist and all species from a given genera will be returned, regardless of whether a given species is actually in the supplied data set. A new column "InData" will tell the user whether a given species is actually in their data or has been expanded to.

***

# QCkit 0.1.0.2
02 February 2023

* Fixed a major bug in `te_check()` that was causing the function return species that were not threatened or endangered. The function now returns a tibble containing all species that are threatened, endangered, or considered for listing, specifies the status code of each species, and then give a brief explanation of the federal endangered species act status code returned.

***

# QCkit 0.1.0.2

* deprecated `get_dp_flags()`, `get_df_flags()`, and `get_dc_flags` in favor of `get_custom_flags()`. The new `get_custom_flags()` function returns 1-3 data frames, depending on user input that contain the output of the 3 previous functions. It also allows the user to specify additional non-flagged columns to be included in the QC summary.
  * Marked `get_custom_flags()` as experimental.
  * Removed "force" option and removed final print statement
  * Reduced number of summary columns reported
  * fixed RRU calculation to be (A+AE)/(A+AE+P+R+NA) instead of (A+E)/(A+AE+P+R)

# QCkit 0.1.0.1

* Added 3 functions to summarize data quality flags:
  * `get_dp_flags()` returns counts of each data flag (A, AE, R, P) across the whole data package (as well as all cells in the data package).
  * `get_df_flags()` returns counts of data flags within each data file of the data package (as well as counts for all cells within the data package).
  * `get_dc_flags()` returns the name of each flagging column within each data package and the count of each flag within each column as well as the total number of cells across all the data flagging columns.
  * Each function has a `force` option that defaults to `force = FALSE` and prints the results to the screen. setting `force = TRUE` will suppress the on-screen output.

# QCkit 0.1.0.0

* Added a `NEWS.md` file to track changes to the package.
