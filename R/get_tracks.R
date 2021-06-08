
#' Extract periods of mobile activity
#'
#' `get_tracks()` extracts periods of mobile activity or tracks identified by
#' `circleclust()`
#'
#' @param df a data frame created by `circleclust()` containing cluster group (`cluster_grp`)
#' @param nested logical; if TRUE, metadata for each track is nested in a list
#' column (`t_data`)
#' @param geometry logical; if TRUE, an `sf` tibble with a corresponding `geometry`
#' column is returned. Tracks are converted into a `LINESTRING`, which connect the
#' beginning and ending coordinate of each track.
#' @param summary logical; if TRUE, the returned data frame is appended with summary
#' values for duration, start time, and end time for each track. The
#' dominant bearing of each track is also calculated.
#' @param dt_field character; name of datetime field used to calculate summary
#' information.
#'
#' @return a data frame or `sf` object
#' @export
#'
#' @examples
#'  \dontrun{
#'get_tracks(df, nested = TRUE, geometry = TRUE, summary = TRUE, dt_field = NULL)
#'}
#'
get_tracks <- function(df, nested = TRUE, geometry = TRUE, summary = TRUE, dt_field = NULL) {

  if (!'cluster_grp' %in% names(df)) {
    stop('Column `cluster_grp` is not in the input data frame. Did you use `circleclust()` to identify periods of stationary activity?',
         call. = FALSE)
  }

  if (sum(!is.na(df$cluster_grp)) == nrow(df)) {
    stop('The input data frame does not contain periods of mobile activity.')
  }


  l_tracks <- df %>%
    dplyr::mutate(r = dplyr::row_number()) %>%
    dplyr::filter(is.na(cluster_grp) & !is.na(lat)) %>%
    dplyr::mutate(
      lag_rownum = dplyr::lag(r),
      r_diff = r - lag_rownum,
      break_yn = ifelse(r_diff > 1 | is.na(lag_rownum), 1, 0),
      track_grp = cumsum(break_yn)
    ) %>%
    dplyr::select(-c(lag_rownum:break_yn)) %>%
    dplyr::group_split(track_grp) %>%
    purrr::map(., ~dplyr::mutate(., start_lat = dplyr::first(lat),
                                 start_lon = dplyr::first(lon),
                                 end_lat = dplyr::last(lat),
                                 end_lon = dplyr::last(lon)))

  dt <- l_tracks %>%
    purrr::map_df(., ~dplyr::group_nest(., .key = 't_data',
                                        track_grp, start_lat, start_lon, end_lat, end_lon))

  np <- nrow(dt)
  message(paste0('A total of ', np, ' tracks were detected.'))

  if (nested == FALSE) {
    dt <- dt %>%
      dplyr::select(-t_data)
  }

  if (summary == TRUE) {

    if (is.null(dt_field)) {
      stop('Duration and start end/end times for each track cannot be calculated. Did you assign the \n correct column to `dt_field`?',
           call. = FALSE)
    }

    time_unit <- floor(quantile(diff(df[[dt_field]]), 0.75))
    units(time_unit) <- "secs"
    time_unit <- as.numeric(time_unit)

    if (is.na(time_unit)) {
      stop('Duration and start end/end times for each t cannot be calculated. Did you assign the \n correct column to `dt_field`?',
           call. = FALSE)
    }

    tv <- purrr::map_dbl(l_tracks, nrow)
    durs <- as.numeric(round(tv/(60/time_unit), digits = 1)) %>%
      purrr::map_df(., ~tibble::enframe(., name = NULL, value = 'duration_mins'))
    start_time <- purrr::map_df(l_tracks, ~dplyr::summarise(., start_time = min(.[[dt_field]])))
    end_time <- purrr::map_df(l_tracks, ~dplyr::summarise(., end_time = max(.[[dt_field]])))


    az_dominant <- function(d) {

      d_trig <- d %>%
        dplyr::mutate(
          a_rad = circular::rad(azimuth),
          a_sin = sin(a_rad),
          a_cos = cos(a_rad))

      d_trig %>%
        dplyr::summarise(n = dplyr::n(),
                         sum_sin = sum(a_sin),
                         sum_cos = sum(a_cos)) %>%
        dplyr::mutate(a_y = (sum_sin / n),
                      a_x = (sum_cos / n),
                      a_y2 = a_y^2,
                      a_x2 = a_x^2,
                      res_length = sqrt(a_y2 + a_x2),
                      cos_mean = a_x/res_length,
                      sin_mean = a_y/res_length,
                      theta_r = abs(circular::deg(atan2(sin_mean, cos_mean))),
                      dom_bearing = dplyr::case_when(sin_mean > 0 & cos_mean > 0 ~ theta_r,
                                                     sin_mean > 0 & cos_mean < 0 ~  theta_r,
                                                     sin_mean < 0 & cos_mean < 0 ~ 360 - theta_r,
                                                     sin_mean < 0 & cos_mean > 0 ~ 360 - theta_r)) %>%
        dplyr::select(dom_bearing)
    }

    d_az <- purrr::map_df(l_tracks, az_dominant)

    dt <- dplyr::bind_cols(dt, durs, start_time, end_time, d_az)

    message(paste0('Measurements appear to have been recorded at a ', time_unit,
                   ' second sampling interval.'))
  }

  if (geometry == TRUE) {

    track_sf <- function(l) {

      lrows <- purrr::map_dbl(l, nrow)

      start_end_coords <- l %>%
        purrr::map(., ~dplyr::select(., track_grp, Date_Time, lat, lon)) %>%
        purrr::map2(., lrows, ~dplyr::filter(.x, dplyr::row_number() == 1 | dplyr::row_number() == .y))

      t_line <- function(coords) {
        coords %>%
          wgs_sf() %>%
          dplyr::summarise(do_union = FALSE) %>%
          sf::st_cast("LINESTRING")
      }

      purrr::map_df(start_end_coords, t_line)

    }

    track_lines <- track_sf(l_tracks)

    dt <- dplyr::bind_cols(track_lines, dt)
    dt <- dt %>%
      dplyr::relocate(geometry, .after = dplyr::last_col())
  }

  dt
}
