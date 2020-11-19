
#' Calculate speed and azimuth of series of latitude and longitude coordinates
#'
#' `move()` calculates the speed and azimuth (bearing) of consecutive
#' coordinates.
#'
#' @param df a data frame containing latitude (`lat`), longitude (`lon`), and a datetime.
#' @param dt_field character; name of datetime field.
#'
#' @return a data frame with columns for speed (m/s) and azimuth (degrees).
#' @export
#'
#' @examples
#' \dontrun{
#'
#' move_move(df, dt_field = "Date_Time")
#' }
move <- function(df, dt_field = NULL) {
  if (sum(stringr::str_detect(names(df), "lat")) == 0) {
    stop("`lat` and/or `lon` colunms found. Latitude and longitude columns should be named appropriately.", call. = FALSE)
  }

  if (is.null(dt_field)) {
    stop("`dt_field` has not been assigned a value.", call. = FALSE)
  } else if (!lubridate::is.POSIXct(df[[dt_field]])) {
    c_dt_field <- class(df[[dt_field]])
    stop(paste0("`dt_field` must be a datetime. `", {{ dt_field }}, "` is of class ", c_dt_field, "."),
      call. = FALSE
    )
  }

  if (sum(is.na(df$lat)) == nrow(df)) {
    message("The input data frame does not have valid `lon`/`lat` coordinates. Speed and azimuth were not calculated.")
  }

  d_speed <- df %>%
    dplyr::filter(!duplicated(.data[[dt_field]])) %>%
    dplyr::arrange(.data[[dt_field]]) # arrange by datetime field just to be safe

  d_speed <- d_speed %>%
    dplyr::mutate(
      lag_time = lag(lubridate::ymd_hms(.data[[dt_field]])),
      lag_time.diff = lubridate::ymd_hms(.data[[dt_field]]) - dplyr::lag(lubridate::ymd_hms(.data[[dt_field]])),
      lag_lat = dplyr::lag(lat),
      lag_lon = dplyr::lag(lon),
      distance = geosphere::distHaversine(cbind(lon, lat), cbind(lag_lon, lag_lat)),
      azimuth = geosphere::bearing(cbind(lon, lat), cbind(lag_lon, lag_lat)) + 180,
      speed_ms = round(distance / as.numeric(lag_time.diff), digits = 1)
    ) %>%
    dplyr::select(-c(starts_with("lag"), distance))

  # message(paste("A total of", sum(duplicated(.data[[dt_field]])), "rows had dupliated timestamps and were removed."))

  d_speed
}
