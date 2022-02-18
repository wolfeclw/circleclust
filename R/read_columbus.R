
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
#' @param participant_id  user defined string to denote a personal identifier.
#' This is useful if the GPS unit is deployed during personal sampling.  If specified,
#' a new column is created ('ID'). Default is NULL.
#' @param sample_col character; user defined character string specifying the name of the
#' column to denote sample ID. Default is NULL.
#' @param sample_id user defined string to denote sample ID. If assigned, a
#' value must also be supplied to `sample_col`. Default is NULL.
#'
#' @return a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' read_columbus(path, tzone_gps = "UTC", tzone_out = "America/New_York.")
#' }
read_columbus <- function(path, gps_format = 'nmea', filter_fix = NULL, tzone_gps = "UTC", tzone_out = "America/New_York",
                          participant_id = NULL, sample_col = NULL, sample_id = NULL) {

  if (gps_format == 'csv' & isTRUE(filter_fix)) {
    stop('Filtering coordinates by GPS fix is available only for files in "NMEA" format.',
         call. = FALSE)
  }

  if (gps_format == 'nmea') {
    d_columbus <- columbus_nmea(path = path, filter_fix = filter_fix, tzone_gps = tzone_gps, tzone_out = tzone_out)
  } else if (gps_format == 'csv') {
    d_columbus <- columbus_nmea(path = path, tzone_gps = tzone_gps, tzone_out = tzone_out)
  }

  if (!is.null(sample_col) & !is.character(sample_col)) {
    stop("`sample_col` must be a character string.",
         call. = FALSE
    )
  }

  if (sum(is.null(sample_col), is.null(sample_id)) == 1) {
    stop("Both `sample_col` and `sample_id` must be assigned a value, not one or the other.",
         call. = FALSE
    )
  } else if (sum(is.null(sample_col), is.null(sample_id)) == 0) {
    d_columbus <- dplyr::mutate(d_columbus, {{ sample_col }} := sample_id) %>%
      dplyr::relocate({{ sample_col }})
  } else {
    d_columbus <- d_columbus
  }

  if (!is.null(participant_id)) {
    d_columbus <- dplyr::mutate(d_columbus, ID = participant_id) %>%
      dplyr::relocate(ID)
  }

  d_columbus
}


