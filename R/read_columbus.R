
#' Read Columbus gps logger .csv file
#'
#' `read_columbus()` imports latitude and longitude from a Columbus gps logger file.
#'
#' @param path string; filepath of input .csv
#' @param tzone_gps character; time zone of the Columbus logger. Default = 'UTC'
#' @param tzone_out character; time zone of the output file. Default = 'America/New_York.'
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' read_columbus(path, tzone_gps = "UTC", tzone_out = "America/New_York.")
#' }
read_columbus <- function(path, tzone_gps = "UTC", tzone_out = "America/New_York") {
  d <- suppressWarnings(readr::read_csv(path,
    skip = 1, col_names = FALSE,
    col_types = readr::cols(
      .default = readr::col_number(),
      X2 = readr::col_character(),
      X4 = readr::col_character(),
      X5 = readr::col_character(),
      X6 = readr::col_character(),
      X10 = readr::col_skip()
    )
  ))
  d <- d %>%
    dplyr::mutate(
      lat_hem = stringr::str_extract(X5, "[a-zA-Z]"),
      lat = readr::parse_number(X5),
      lat = ifelse(lat_hem == "N", lat, lat * -1),
      lon_meridian = stringr::str_extract(X6, "[a-zA-Z]"),
      lon = readr::parse_number(X6),
      lon = ifelse(lon_meridian == "W", lon * -1, lon),
      hr = substr(X4, 1, 2),
      min = substr(X4, 3, 4),
      sec = substr(X4, 5, 6)
    )

  d %>%
    dplyr::mutate(
      date_utc = lubridate::ymd(X3),
      time_utc = chron::times(stringr::str_c(hr, min, sec, sep = ":")),
      Date_Time = lubridate::ymd_hms(paste(date_utc, time_utc)),
      Date_Time = lubridate::force_tzs(Date_Time, tzones = tzone_gps, tzone_out = tzone_out)
    ) %>%
    dplyr::select(Date_Time, lat, lon,
      gps_index = X1,
      tag = X2,
      height = X7,
      speed = X8,
      heading = X9
    )
}
