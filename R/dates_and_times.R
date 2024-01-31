#' Fix UTC offset strings
#'
#' UTC offsets can be formatted in multiple ways (e.g. -07, -07:00, -0700) and R often struggles to parse these offsets. This function takes date/time strings with valid UTC offsets, and formats them so that they are consistent and readable by R. Here, you can supply a vector of dates in ISO 8601 format and they will be returned in a consistent format compatible with R. Date strings with missing or invalid UTC offsets will result in a warning.
#'
#' @param datetime_strings Character vector of dates in ISO 8601 format
#'
#' @return datetime_strings with UTC offsets consistently formatted to four digits (e.g. "2023-11-16T03:32:49-0700").
#' @export
#'
#' @examples
#' datetimes <- c("2023-11-16T03:32:49+07:00", "2023-11-16T03:32:49-07",
#' "2023-11-16T03:32:49","2023-11-16T03:32:49Z")
#' fix_utc_offset(datetimes)
#'
fix_utc_offset <- function(datetime_strings) {
  datetime_strings <- stringr::str_replace_all(datetime_strings, "[\u2212\u2010\u2011\u2012\u2013\u2014\u2015\ufe58\ufe63\uff0d]", "-") # replace every possible type of dash with a regular minus sign

  # get UTC offset and format it as 4 digits with no special characters (e.g. 0700)
  new_offsets <- datetime_strings %>%
    stringr::str_extract("[Zz]|((?<=[+-])[0-9]{1,2}:?[0-9]{0,2})$") %>%
    stringr::str_remove(":") %>%
    stringr::str_replace("[Zz]", "0000")
  new_offsets <- dplyr::case_when(nchar(new_offsets) == 1 ~ paste0("0",
                                                                   new_offsets,
                                                                   "00"),
                                  nchar(new_offsets) == 2 ~ paste0(new_offsets,
                                                                   "00"),
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

#' Convert EML date/time format string to one that R can parse
#'
#' @details `convert_datetime_format()` is not a sophisticated function. If the EML format string is not valid, it will happily and without complaint return an R format string that will break your code. You have been warned.  Note that UTC offset formats using a colon or only two digits will be parsed by this function, but if parsing datetime values from strings, you will also need to use `fix_utc_offset` to change the UTC offsets to the +/-hhhh format that R can read.
#'
#' @param eml_format_string A character vector of EML date/time format strings. This function understands the following codes: YYYY = four digit year, YY = two digit year, MMM = three letter month abbrev., MM = two digit month, DD = two digit day, hh or HH = 24 hour time, mm = minutes, ss or SS = seconds, +/-hhhh or +/-HHHH = UTC offset.
#' @param convert_z Should a "Z" at the end of the format string (indicating UTC) be replaced by a "%z"? Only set to `TRUE` if you plan to use `fix_utc_offset` to change "Z" in datetime strings to "+0000".
#'
#' @return A character vector of date/time format strings that can be parsed by `readr` or `strptime`.
#' @export
#'
#' @examples
#' convert_datetime_format("MM/DD/YYYY")
#' convert_datetime_format(c("MM/DD/YYYY", "YY-MM-DD"))
#'
convert_datetime_format <- function(eml_format_string, convert_z = FALSE) {
  r_format_string <- eml_format_string %>%
    stringr::str_replace_all("YYYY", "%Y") %>%
    stringr::str_replace_all("YY", "%y") %>%
    stringr::str_replace_all("MMM", "%b") %>%
    stringr::str_replace_all("MM", "%m") %>%
    stringr::str_replace_all("DD", "%d") %>%
    stringr::str_replace_all("(?<![+-])((hh)|(HH))", "%H") %>%
    stringr::str_replace_all("mm", "%M") %>%
    stringr::str_replace_all("(ss)|(SS)", "%S") %>%
    stringr::str_replace_all("(?<!%)M", "%m") %>%  # Replace M with %m, but leave %M alone
    stringr::str_replace_all("D", "%d") %>%
    stringr::str_replace_all("[+-][Hh]{1,2}:?[Hh]{0,2}(?=$)", "%z")  # Replace UTC offset format string (e.g. -hh, -hhhh, -hh:hh) with %z. Note that R seems to only parse UTC offsets when in the format +/-hhhh.
  #stringr::str_replace_all("T", " ")

  if (convert_z) {
    r_format_string <- stringr::str_replace(r_format_string, "Z(?=$)", "%z")
  }

  return(r_format_string)
}