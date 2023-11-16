#' Fix UTC offset strings
#' 
#' UTC offsets can be formatted in multiple ways (e.g. -07, -07:00, -0700) and R often struggles to parse these offsets. This function takes date/time strings with valid UTC offsets, and formats them so that they are consistent and readable by R.
#'
#' @param datetime_strings Character vector of dates in ISO 8601 format
#'
#' @return datetime_strings with UTC offsets consistently formatted to four digits (e.g. "2023-11-16T03:32:49-0700").
#' @export
#'
#' @examples
#' datetimes <- c("2023-11-16T03:32:49+07:00","2023-11-16T03:32:49-07","2023-11-16T03:32:49","2023-11-16T03:32:49Z")
#' fix_utc_offset(datetimes)  # returns c("2023-11-16T03:32:49+0700", "2023-11-16T03:32:49-0700", "2023-11-16T03:32:49", "2023-11-16T03:32:49+0000") and warns about missing offset (see third element)
#' 
fix_utc_offset <- function(datetime_strings) {
  datetime_strings <- stringr::str_replace_all(datetime_strings, "[−‐‑‒–—―﹘﹣－]", "-") # replace every possible type of dash with a regular minus sign
  
  # get UTC offset and format it as 4 digits with no special characters (e.g. 0700)
  new_offsets <- datetime_strings %>%
    stringr::str_extract("[Zz]|((?<=[+-])[0-9]{1,2}:?[0-9]{0,2})$") %>%
    stringr::str_remove(":") %>%
    stringr::str_replace("[Zz]", "0000")
  new_offsets <- dplyr::case_when(nchar(new_offsets) == 1 ~ paste0("0", new_offsets, "00"),
                                  nchar(new_offsets) == 2 ~ paste0(new_offsets, "00"),
                                  nchar(new_offsets) == 4 ~ new_offsets,
                                  .default = "")
  if (any(new_offsets == "")) {
    warning("Date strings contain missing or invalid UTC offsets")
  }
  
  # remove old UTC offsets from date strings
  datetime_strings <- datetime_strings %>%
    stringr::str_remove("(?<=[+-])[0-9]{1,2}:?[0-9]{0,2}$") %>%
    stringr::str_replace("[Zz](?=$)", "+")
  
  # add new UTC offsets
  datetime_strings <- paste0(datetime_strings, new_offsets) %>%
    stringr::str_remove("[+-](?=$)")  # Remove trailing + or - where invalid offsets were removed
  
  return(datetime_strings)
}

