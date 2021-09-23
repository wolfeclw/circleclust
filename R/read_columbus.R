
#' Read Columbus gps logger .csv file
#'
#' `read_columbus()` imports latitude and longitude from a Columbus gps logger file.
#'
#' @param path string; filepath of input .csv
#' @param gps_format string; format of the input file ('nmea' or 'csv')
#' @param filter_fix logical; should coordinates with invalid GPS fix be filtered out?
#' Default = NULL  'gps_format' must be set to 'NMEA.'
#' @param tzone_gps character; time zone of the Columbus logger. Default = 'UTC'
#' @param tzone_out character; time zone of the output file. Default = 'America/New_York.'
#'
#' @return a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' read_columbus(path, tzone_gps = "UTC", tzone_out = "America/New_York.")
#' }
read_columbus <- function(path, gps_format = 'nmea', filter_fix = NULL, tzone_gps = "UTC", tzone_out = "America/New_York") {

  if (gps_format == 'csv' & isTRUE(filter_fix)) {
    stop('Filtering coordinates by GPS fix is available only for files in "NMEA" format.',
         call. = FALSE)
  }

  if (gps_format == 'nmea') {
    columbus_nmea(path = path, filter_fix = filter_fix, tzone_gps = tzone_gps, tzone_out = tzone_out)
  } else if (gps_format == 'csv') {
    columbus_nmea(path = path, tzone_gps = tzone_gps, tzone_out = tzone_out)
  }

}


