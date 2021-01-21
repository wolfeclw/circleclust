
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
read_columbus <- function(path, gps_format = 'nmea', tzone_gps = "UTC", tzone_out = "America/New_York") {

  if (gps_format == 'nmea') {
    columbus_nmea(path = path, tzone_gps = tzone_gps, tzone_out = tzone_out)
  } else if (gps_format == 'csv') {
    columbus_nmea(path = path, tzone_gps = tzone_gps, tzone_out = tzone_out)
  }

}


