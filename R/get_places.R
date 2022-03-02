
#' Extract locations of stationary activity
#'
#' `get_places()` analyzes periods of stationary activity within each spatiotemporal
#' cluster identified by `circleclust()` and transforms the coordinates into a
#' single location or 'place'.
#'
#' @param df a data frame created by `circleclust()` containing cluster group (`cluster_grp`)
#' @param nested logical; if TRUE, metadata for each cluster is nested in a list
#' column (`p_data`)
#' @param geometry logical; if TRUE, an `sf` tibble with a corresponding `geometry`
#' column is returned
#' @param summary logical; if TRUE, the returned data frame is appended with summary
#' values for duration, start time, and end time for stationary activity recorded
#' for each cluster
#' @param dt_field character; name of datetime field used to calculate summary
#' information
#'
#' @return a data frame or `sf` object
#' @export
#'
#' @examples
#' \dontrun{
#'
#' get_places(df, nested = TRUE, geometry = TRUE, summary = TRUE, dt_field = NULL)
#' }
#'
get_places <- function(df, nested = TRUE, geometry = TRUE, summary = FALSE, dt_field = NULL) {

  if (!'cluster_grp' %in% names(df)) {
    stop('Column `cluster_grp` is not in the input data frame. Did you use `circleclust()` to identify periods of stationary activity?',
         call. = FALSE)
  }

  if (sum(is.na(df$cluster_grp)) == nrow(df)) {
    stop('The input data frame does not contain periods of stationary activity/clustered coordinates.')
  }


  l_places <- df %>%
    dplyr::filter(!is.na(cluster_grp)) %>%
    dplyr::group_split(cluster_grp) %>%
    purrr::map(., ~dplyr::mutate(., p_lat = median(lat, na.rm = TRUE),
                          p_lon = median(lon, na.rm = TRUE)))

  dc <- l_places %>%
    purrr::map_df(., ~dplyr::group_nest(., .key = 'p_data', cluster_grp, p_lat, p_lon))



  if (nested == FALSE) {
    dc <- dc %>%
      dplyr::select(-p_data)
  }

  if (summary == TRUE) {

    if (is.null(dt_field)) {
      stop('Summary data for each place cannot be calculated. Did you assign the correct column to `dt_field`?',
           call. = FALSE)
    }

    time_unit <- floor(quantile(diff(df[[dt_field]]), 0.75))
    units(time_unit) <- "secs"
    time_unit <- as.numeric(time_unit)

    tv <- purrr::map_dbl(l_places, nrow)
    durs <- as.numeric(round(tv/(60/time_unit), digits = 1)) %>%
      purrr::map_df(., ~tibble::enframe(., name = NULL, value = 'duration_mins'))
    start_time <- purrr::map_df(l_places, ~dplyr::summarise(., start_time = min(.[[dt_field]])))
    end_time <- purrr::map_df(l_places, ~dplyr::summarise(., end_time = max(.[[dt_field]])))

    dc <- dplyr::bind_cols(dc, durs, start_time, end_time)

    message(paste0('Measurements appear to have been recorded at a ', time_unit,
                   ' second sampling interval.'))
  }

  if (geometry == TRUE) {
    dc <- dc %>%
      sf::st_as_sf(., coords = c('p_lon', 'p_lat'), crs = 4326)
  }

  np <- nrow(dc)

  if ('spatiotemp_cluster_grp' %in% names(df)) {
    message(paste0('A total of ', np, ' places were detected.'))
  } else {
    message(paste0('A total of ', np, ' spatiotemporal places were detected.'))
  }

  if ('spatiotemp_cluster_grp' %in% names(df) & summary == TRUE) {
    dc$start_time <- NA
    dc$end_time <- NA

    warning('Looks like the input data frame includes merged clusters. Start and end times cannot be calculted.',
            call. = FALSE)
  }

  dc
}

