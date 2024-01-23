# QCkit v0.1.4

* Added function `replace_blanks()` to ingest a directory of .csvs and write them back out to .csv as 

# QC kit v0.1.3
2023-12-18
* Bugfix for `create_datastore_script()`
* Added automated build check and pkgdown github pages builds via github actions
* Code cleanup via linter suggests 
* Added warning about turning off VPN for the `covert_utm_to_ll()` documentation
* Moved `convert_datetime_format()` from DPchecker to QCkit
* Added funtion `fix_utc_offset()`

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
