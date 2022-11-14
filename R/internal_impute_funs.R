##### INTERNAL IMPUTE FUNCTIONS


impute_coords_dist <- function(df, dt_field = NULL, distance_threshold = 100, jitter_amount = 0.00005,
                               show_lapse_distance = FALSE) {
  d_r <- df %>%
    dplyr::mutate(r = dplyr::row_number())

  rm_open_lapse <- d_r %>%
    dplyr::filter(
      cumsum(!is.na(lat)) != 0,
      rev(cumsum(rev(!is.na(lat))) != 0)
    )

  d_lapse <- rm_open_lapse %>%
    dplyr::mutate(lapse = ifelse(is.na(lat), 1, 0)) %>%
    dplyr::filter(lapse == 1)

  if (nrow(d_lapse) > 0 & sum(!is.na(d_lapse$lat) == nrow(d_lapse))) {
    d_lapse <- d_lapse %>%
      dplyr::mutate(
        lag_rownum = dplyr::lag(r),
        r_diff = r - lag_rownum,
        break_yn = ifelse(r_diff > 1 | is.na(lag_rownum), 1, 0),
        lapse_grp = cumsum(break_yn)
      ) %>%
      dplyr::select(-c(lag_rownum, r_diff, break_yn, lapse))

    d_lapse_join <- suppressMessages(dplyr::full_join(d_r, d_lapse))

    d_row_vector <- d_lapse_join %>%
      dplyr::group_by(lapse_grp) %>%
      dplyr::summarise(
        min_minus = min(r) - 1,
        max_plus = max(r) + 1
      ) %>%
      tidyr::drop_na()

    minus_coords <- d_row_vector$min_minus
    plus_coords <- d_row_vector$max_plus

    dminus <- purrr::map_df(d_lapse_join, `[`, minus_coords) %>% dplyr::select(lon, lat)
    dplus <- purrr::map_df(d_lapse_join, `[`, plus_coords) %>% dplyr::select(lon, lat)

    lapse_coords_bind <- suppressMessages(dplyr::bind_cols(dminus, dplus))
    lapse_coords_bind <- lapse_coords_bind %>%
      dplyr::mutate(
        lapse_grp = dplyr::row_number(),
        lapse_distance = round(geosphere::distHaversine(cbind(lon...1, lat...2), cbind(lon...3, lat...4),
                                                        r = 6378137
        ),
        digits = 2
        )
      ) %>%
      dplyr::select(lapse_grp, lapse_distance)

    d_dist <- suppressMessages(dplyr::full_join(d_lapse_join, lapse_coords_bind))

    d_coords_fill <- d_dist %>%
      dplyr::mutate(
        impute_lat = ifelse(is.na(lat) & lapse_distance < distance_threshold,
                            zoo::na.locf(lat, na.rm = FALSE), lat
        ),
        impute_lon = ifelse(is.na(lon) & lapse_distance < distance_threshold,
                            zoo::na.locf(lon, na.rm = FALSE), lon
        )
      ) %>%
      dplyr::filter(!is.na(lapse_grp) & !is.na(impute_lat)) %>%
      sf::st_as_sf(coords = c("impute_lon", "impute_lat"), crs = 4326)

    d_jitter <- sf::st_jitter(d_coords_fill, amount = jitter_amount)
    d_jitter <- d_jitter %>%
      dplyr::mutate(
        jlat = sf::st_coordinates(.)[, 2],
        jlon = sf::st_coordinates(.)[, 1]
      ) %>%
      sf::st_drop_geometry()

    jitter_join <- suppressMessages(dplyr::full_join(d_dist, d_jitter))

    l_dist_imputed <- dplyr::group_split(jitter_join, lapse_grp) %>%
      purrr::map(., ~dplyr::mutate(.,
                                   imputed_coord = ifelse(!is.na(jlat) & lapse_distance < distance_threshold, 1, 0),
                                   lat = ifelse(is.na(lat) & !is.na(jlat) & lapse_distance < distance_threshold, jlat, lat),
                                   lon = ifelse(is.na(lon) & !is.na(jlon) & lapse_distance < distance_threshold, jlon, lon)))


    distances <- l_dist_imputed %>%
      purrr::map_dbl(., ~unique(.$lapse_distance)) %>%
      na.omit() %>%
      as.numeric()

    d_dist_imputed <- dplyr::bind_rows(l_dist_imputed) %>%
      dplyr::arrange(.[[dt_field]]) %>%
      dplyr::select(-c(r, lapse_grp, jlat, jlon))

    n_imputed <- sum(distances < distance_threshold)
    min_dist <- min(distances)
    max_dist <- max(distances)

    message(crayon::cyan(paste0('\nA total of ', length(distances), ' `lat/lon` lapses were identified.')))
    message(crayon::cyan(paste0(
      "The minimum and maximum distances between lapses are ", min_dist, " and ",
      max_dist, " meters, respectively."
    )))

    wch_gt_threshold <- which(distances > distance_threshold)
    n_gt_threshold <- length(wch_gt_threshold)

    if (n_gt_threshold > 0 & n_gt_threshold == length(distances)) {
      message(crayon::cyan(paste0(
        "The minimum distance between for all missing coordinates (", min_dist,
        ") meters is greater than the distance threshold (", distance_threshold, ") meters. \n `lon/lat` for rows with missing GPS information were not imputed."
      )))
    } else if (n_gt_threshold > 0) {
      message(crayon::red(paste0('  - A lapse greater than the distance threshold (', distances[wch_gt_threshold],
                                 ' meters) was identified. `lon/lat` for these rows were not imputed. \n')))
    }
  } else {
    d_dist_imputed <- df %>%
      dplyr::mutate(
        lapse_distance = NA,
        imputed_coord = ifelse(!is.na(lat), 0, NA)
      )
    message(crayon::green("GPS lapses were not imputed based on distance. There are no lapses enclosed with GPS coordinates."))
  }

  if (show_lapse_distance == TRUE) {
    d_dist_imputed <- d_dist_imputed
  } else {
    d_dist_imputed <- d_dist_imputed %>%
      dplyr::select(-lapse_distance)
  }
  d_dist_imputed
}



###

impute_coords_open <- function(df, jitter_amount = 0.00005, speed_threshold = 5, speed_window = 60, open_lapse_length = 600) {

  d_r <- df %>%
    dplyr::mutate(r = dplyr::row_number())

  open_lapse_head <- d_r %>%
    dplyr::filter(cumsum(!is.na(lat)) == 0)

  open_lapse_tail <- d_r %>%
    dplyr::filter(rev(cumsum(rev(!is.na(lat))) == 0))

  speed_f <- function(df) {
    d_speed <- df %>%
      dplyr::filter(!duplicated(Date_Time)) %>%
      dplyr::mutate(
        lag_time.diff = lubridate::ymd_hms(Date_Time) - dplyr::lag(lubridate::ymd_hms(Date_Time)),
        distance = geosphere::distHaversine(cbind(lon, lat), cbind(dplyr::lag(lon), dplyr::lag(lat)),
                                            r = 6378137
        ),
        speed_ms = round(distance / as.numeric(lag_time.diff), digits = 2)
      ) %>%
      dplyr::select(distance, speed_ms)

    d_speed$speed_ms
  }

  if (nrow(open_lapse_head) > 1 & nrow(open_lapse_head) < open_lapse_length) {
    lower_row_h <- max(open_lapse_head$r) + 1
    upper_row_h <- max(open_lapse_head$r) + speed_window
    head_impute_set <- d_r[lower_row_h:upper_row_h, ]
    h_speed <- speed_f(head_impute_set)
    h_speed <- median(h_speed, na.rm = TRUE)

    if (h_speed < speed_threshold) {
      first_coords_r <- d_r[(max(open_lapse_head$r) + 1), ]
      h_impute <- bind_rows(open_lapse_head, first_coords_r)
      h_impute <- h_impute %>%
        dplyr::mutate(
          h_speed = h_speed,
          impute_lat = zoo::na.locf(lat, fromLast = TRUE),
          impute_lon = zoo::na.locf(lon, fromLast = TRUE)
        ) %>%
        sf::st_as_sf(coords = c("impute_lon", "impute_lat"), crs = 4326)

      h_jitter <- sf::st_jitter(h_impute, amount = jitter_amount)
      h_jitter <- h_jitter %>%
        dplyr::mutate(
          jlat = sf::st_coordinates(.)[, 2],
          jlon = sf::st_coordinates(.)[, 1]
        ) %>%
        sf::st_drop_geometry()
      h_jitter <- h_jitter[-nrow(h_jitter), ]

      dh_fill <- suppressMessages(dplyr::full_join(d_r, h_jitter))

      message(crayon::green(paste0('An open lapse at the head of the data frame was detected. ',
                                  paste0(nrow(open_lapse_head), ' missiing coordinates were imputed.'))))
    } else {
      dh_fill <- d_r
      message(crayon::red(paste0(
        "The speed threshold is less than calcualted speed at the head of the data frame (", h_speed,
        " m/s) -- the open lapse was not imputed.")))
    }
  } else if (nrow(open_lapse_head) == 0) {
    dh_fill <- d_r
    message(crayon::cyan('\nAn open lapse at the head of the data frame was not detected.'))
  } else {
    dh_fill <- d_r
    message(crayon::red("\nThe lapse at the head of the data frame exceeded the length threshold. Coordinates were not imputed."))
  }


  if (nrow(open_lapse_tail) > 1 & nrow(open_lapse_tail) < open_lapse_length) {
    lower_row_t <- min(open_lapse_tail$r) - speed_window
    upper_row_t <- min(open_lapse_tail$r)
    tail_impute_set <- d_r[lower_row_t:upper_row_t, ]
    t_speed <- speed_f(tail_impute_set)
    t_speed <- median(t_speed, na.rm = TRUE)

    if (t_speed < speed_threshold) {
      last_coords_r <- d_r[(min(open_lapse_tail$r) - 1), ]
      t_impute <- dplyr::bind_rows(last_coords_r, open_lapse_tail)
      t_impute <- t_impute %>%
        dplyr::mutate(
          t_speed = t_speed,
          impute_lat = zoo::na.locf(lat),
          impute_lon = zoo::na.locf(lon)
        ) %>%
        sf::st_as_sf(coords = c("impute_lon", "impute_lat"), crs = 4326)

      t_jitter <- sf::st_jitter(t_impute, amount = jitter_amount)
      t_jitter <- t_jitter %>%
        dplyr::mutate(
          jlat = sf::st_coordinates(.)[, 2],
          jlon = sf::st_coordinates(.)[, 1]
        ) %>%
        sf::st_drop_geometry()

      t_jitter <- t_jitter[-1, ]
      dt_fill <- suppressMessages(dplyr::full_join(dh_fill, t_jitter))

      message(crayon::green(paste0('An open lapse at the end of the data frame was detected. ',
                                  paste0(nrow(open_lapse_tail), ' missiing coordinates were imputed.'))))

    } else {

      dt_fill <- dh_fill
      message(crayon::red(paste0(
        "The speed threshold is less than calcualted speed at the end of the data frame (", t_speed,
        " m/s) -- the open lapse was not imputed.")))
    }
  } else if (nrow(open_lapse_tail) == 0) {
    dt_fill <- dh_fill
    message(crayon::cyan('An open lapse at the end of the data frame was not detected.'))
  } else {
    dt_fill <- dh_fill
    message(crayon::red("The lapse at the end of the data frame exceeded the length threshold. Coordinates were not imputed."))
  }

  if ('jlat' %in% names(dt_fill)) {
    d_imputed_open <- dt_fill %>%
      dplyr::mutate(
        imputed_coord = ifelse(!is.na(jlat), 1, imputed_coord),
        lat = ifelse(is.na(jlat), lat, jlat),
        lon = ifelse(is.na(jlon), lon, jlon)) %>%
      dplyr::select(-c(r:length(.)))
  } else {
    d_imputed_open <- dt_fill %>%
      dplyr::select(-r)
  }

  d_imputed_open
}
