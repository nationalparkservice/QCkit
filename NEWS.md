# QCkit 0.1.0.1

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

# QCkit 0.1.0

* Added a `NEWS.md` file to track changes to the package.
