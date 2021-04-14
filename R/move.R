
#' Calculate speed and azimuth of series of latitude and longitude coordinates
#'
#' `move()` calculates the speed and azimuth (bearing) of consecutive
#' coordinates.
#'
#' @param df a data frame containing latitude (`lat`), longitude (`lon`), and a datetime.
#' @param dt_field character; name of datetime field.
#' @param jitter_coords logical; if TRUE, coordinates are jittered by a negligible
#' amount (~1 cm). Jittering is recommended if the input data frame contains
#' sequentially duplicated coordinates. Eliminating repeated coordinates is necessary to
#' optimize the *circleclust()* algorithm.
#' @return a data frame with columns for speed (m/s) and azimuth (degrees).
#' @export
#'
#' @examples
#' \dontrun{
#'
#' move_move(df, dt_field = "Date_Time")
#' }
move <- function(df, dt_field = NULL, jitter_coords = FALSE) {
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
    warning("The input data frame does not have valid `lon`/`lat` coordinates. Speed and azimuth were not calculated.",
            call. = FALSE)
  }


  dup_coords <- df %>%
    dplyr::select(lat, lon) %>%
    dplyr::mutate(dup_lat = ifelse(dplyr::lag(lat) == lat & dplyr::lag(lon) == lon, 1, 0))

  time_unit <- floor(quantile(diff(df[[dt_field]]), 0.75))
  units(time_unit) <- "secs"
  time_unit <- as.numeric(time_unit)

  tdups <- table(dup_coords$dup_lat)

  if (is.na(tdups[2])) {
    pdups <- 0
    dup_mins <- 0
  } else {
    pdups <- round(tdups[2]/(tdups[1] + tdups[2]), digits = 2)*100
    dup_mins <- as.numeric(round(tdups[2]/(60/time_unit), digits = 1))
  }

  if (pdups > 5 & dup_mins > 3 & jitter_coords == FALSE) {

    warning(paste0('Approximately ', tdups[2], ' coordinates (', pdups,
                   '%; ', dup_mins,
                   ' min) are sequentially duplicated. Jittering is \n recommended--set `jitter_coords = TRUE`.'),
            call. = FALSE)
  }

  if (jitter_coords == TRUE) {
    j <- wgs_sf(df)

    j <- j %>%
      dplyr::select(-c(lat, lon)) %>%
      sf::st_jitter(amount = 0.0000001)

    jc <- sf::st_coordinates(j)

    d_coords <- j %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate(lat = jc[ , 2],
                    lon = jc[ , 1])
  } else {
    d_coords <- df
  }

  d_speed <- d_coords %>%
    dplyr::filter(!duplicated(.data[[dt_field]])) %>%
    dplyr::arrange(.data[[dt_field]]) # arrange by datetime field

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
