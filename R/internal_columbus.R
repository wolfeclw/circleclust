

### internal functions to read Columbus GPS data

## read data from csv format:

columbus_csv <- function (path, tzone_gps = NULL, tzone_out = NULL) {
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

## read data from nmea format:

columbus_nmea <- function(path, tzone_gps = NULL, tzone_out = NULL, filter_fix = NULL) {

  t_nmea <- readr::read_lines(path)
  lgl_gpgga <- stringr::str_detect(t_nmea, 'GPGGA')

  t_gpgga <- t_nmea[lgl_gpgga]
  t_gpmrc <- t_nmea[!lgl_gpgga]

  l_gpgga <- stringr::str_split(t_gpgga, ',')
  l_gprmc <- stringr::str_split(t_gpmrc, ',')

  lgl_15 <- map_lgl(l_gpgga, ~length(.) == 15)
  lgl_13 <- map_lgl(l_gprmc, ~length(.) == 13)

  l_gpgga <- l_gpgga[lgl_15]
  l_gprmc <- l_gprmc[lgl_13]

  nmea_enf <- function(l, e_num) {
    l_var <- purrr::map(l, e_num)
    v_var <- unlist(l_var)
    dvar <- tibble::enframe(v_var, name = NULL)
  }

  d_gpgga <- purrr::map(1:14, ~nmea_enf(l_gpgga, .)) %>%
    purrr::reduce(., dplyr::bind_cols) %>% suppressMessages()

  d_gprmc <- purrr::map(1:12, ~nmea_enf(l_gprmc, .)) %>%
    purrr::reduce(., dplyr::bind_cols) %>% suppressMessages()

  d_gpgga <- purrr::set_names(d_gpgga, c('gpgga', 'time', 'latitude', 'gpgga_lat_hem', 'longitude', 'gpgga_lon_hem', 'gpgga_fix',
                                         'gpgga_sats', 'gpgga_accuracy','gpgga_alt', 'gpgga_alt_units', 'gpgga_geoidal_sep',
                                         'gpgga_geoidal_unit', 'gpgga_seconds'))

  d_gprmc <- purrr::set_names(d_gprmc, c('gprmc', 'time', 'gprmc_fix', 'latitude', 'gprmc_lat_hem', 'longitude', 'gprmc_lon_hem',
                                         'gprmc_speed', 'gprmc_bearing', 'gprmc_date', 'gprmc_variation', 'gprmc_ew'))

  d_gps <- dplyr::inner_join(d_gpgga, d_gprmc) %>% suppressMessages()
  d_gps <- map_df(d_gps, ~str_remove_all(., '[*]'))

  d_gps <- d_gps %>%
    dplyr::select(time:gpgga_alt, gprmc_speed:gprmc_date) %>%
    dplyr::relocate(gprmc_date) %>%
    dplyr::rename_with(., ~stringr::str_replace_all(., 'gpgga_|gprmc_', ''))

  d_deg <- d_gps %>%
    dplyr::select(starts_with(c('lat', 'lon'))) %>%
    dplyr::mutate(deg_lat = substr(latitude, start = 1, stop = 2),
           lat_char = max(nchar(latitude)),
           mm_lat = substr(latitude, start = 3, stop = lat_char),
           deg_lon = substr(longitude, start = 1, stop = 3),
           lon_char = max(nchar(longitude)),
           mm_lon = substr(longitude, start = 4, stop = lon_char)) %>%
    dplyr::mutate(dplyr::across(c(deg_lat, mm_lat, deg_lon, mm_lon), as.numeric)) %>%
    suppressWarnings() %>%
    dplyr::mutate(ds_lat = mm_lat / 60,
           ds_lon = mm_lon / 60,
           lat = deg_lat + ds_lat,
           lon = (deg_lon + ds_lon),
           lat = ifelse(lat_hem == 'N', lat, lat*-1),
           lon = ifelse(lon_hem == 'E', lon, lon*-1)) %>%
    dplyr::select(lat, lon)

  gps_bind <- dplyr::bind_cols(d_gps, d_deg)

  if (isTRUE(filter_fix)) {
    v_gps <- gps_bind %>%
      dplyr::filter(fix %in% c('1', '2'))
  } else {
    v_gps <- gps_bind
  }

  v_gps <- v_gps %>%
    dplyr::mutate(date_utc = lubridate::ymd(date),
           hr = substr(time, 1, 2),
           min = substr(time, 3, 4),
           sec = substr(time, 5, 6),
           time_utc = chron::times(stringr::str_c(hr, min, sec, sep = ":")),
           Date_Time = lubridate::ymd_hms(paste(date_utc, time_utc)),
           Date_Time = lubridate::force_tzs(Date_Time, tzones = tzone_gps, tzone_out = tzone_out),
           Date = as.Date(Date_Time)) %>%
    suppressWarnings() %>%
    dplyr::select(Date_Time, Date, lat, lon,
           gps_fix = fix,
           sats_inuse = sats,
           gps_accuracy = accuracy,
           gps_altitude = alt,
           gps_bearing = bearing,
           gps_speed = speed) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(dplyr::starts_with(c('gps', 'sats'))), as.numeric))

  v_gps
}
