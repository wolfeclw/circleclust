

#' Cluster coordinates based on circular variance
#'
#' `circleclust` calculates the circular variance of consecutive latitude and
#' longitude coordinates within a moving window and identifies spatiotemporal clusters
#' based on a set of flexible parameters.  These parameters include the size of the
#' window in which to calculate circular variance, the threshold value (circular variance)
#' to identify clustering, and the minimum number of observations allowed in a
#' cluster.
#'
#' Imputing lon/lat values using `impute_coords()` is recommended if GPS
#' coordinates are missing.
#'
#' @param df a data frame with columns `speed` and `azimuth` created by `move()`.
#' The data frame must also include datetime, longitude, and latitude colums.
#' @param circvar_threshold numeric; Circular variance threshold to determine clustering.
#' Default = 0.7.
#' @param window numeric; Window (number of rows) in which to calculate circular variance.
#' @param cluster_threshold numeric; The minimum allowable number of observations
#' in each cluster.  If the number of observations in an identified cluster is less than this threshold,
#' the observations are retained but not assigned a cluster value.
#' @param show_circvar logical; if `TRUE`, a column will be added to the output data
#' frame listing the circular variance (`circvar`) for each observation.
#' Default = `FALSE`.
#'
#' @return a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' circleclust(df,
#'   circvar_threshold = .7, window = 60, cluster_threshold = NULL,
#'   show_circvar = FALSE
#' )
#' }
circleclust <- function(df, dt_field = NULL, circvar_threshold = .7, window = 60,
                        cluster_threshold = NULL, show_circvar = FALSE) {
  if (sum(stringr::str_detect(names(df), "azimuth")) == 0) {
    stop("Column 'azimuth' not found.  Use `move()` to calculate the azimuth",
      call. = FALSE
    )
  }

  time_unit <- floor(median(diff(df[[dt_field]])))
  units(time_unit) <- 'secs'
  time_unit <- as.numeric(time_unit)

  t_unit_window <- 60/time_unit ## 1 minute rolling window to calculate speed

  if(time_unit > 60) {
    stop('The `Date_Time` interval is greater than 60 seconds. Reduce the time unit when aggregating (5 seconds is recommended).',
         call. = FALSE)
  }

  d_variance <- df %>%
    dplyr::mutate(
      a_rad = circular::rad(azimuth),
      a_sin = sin(a_rad),
      a_cos = cos(a_rad),
      sum_sin = zoo::rollsum(a_sin, window, na.rm = TRUE, fill = NA, align = "center"),
      sum_sin = zoo::na.locf(sum_sin, na.rm = FALSE, maxgap = window / 2),
      sum_cos = zoo::rollsum(a_cos, window, na.rm = TRUE, fill = NA, align = "center"),
      sum_cos = zoo::na.locf(sum_cos, na.rm = FALSE, maxgap = window / 2),
      a_y2 = (sum_sin / window)^2,
      a_x2 = (sum_cos / window)^2,
      res_length = ifelse(is.na(lat), NA, sqrt(a_y2 + a_x2)),
      circvar = round(1 - res_length, digits = 1))

  rspeed_minute <- function(x, rs_window) {

    if (sum(is.na(x)) > 0) {
      zoo::rollmedian(x, rs_window, na.rm = TRUE, fill = NA, align = "center")
    } else {
      zoo::rollmedian(x, rs_window, fill = NA, align = "center")
    }
  }

  d_break <- d_variance %>%
    mutate(roll_speed = rspeed_minute(speed_ms, t_unit_window),
           roll_speed = zoo::na.locf(roll_speed, na.rm = FALSE, maxgap = t_unit_window),
           move_break = ifelse(circvar >= circvar_threshold & roll_speed <= 2, 1, 0),
           rw_num = row_number()) %>%
    select(-c(a_rad:res_length, roll_speed))

  if (sum(d_break$move_break, na.rm = TRUE) > 0) {
    d_places <- places(d_break)
    d_places$clustered_coord <- ifelse(is.na(d_places$place_grp), 0, 1)

    d_places <- d_places %>%
      place_lapse() %>%
      place_lapse_dist()

    if (max(d_places$place_grp, na.rm = TRUE) > 1) {
      d_places <- d_places %>%
        mutate(place_grp = ifelse(is.na(place_grp) & pl_distance < 100, zoo::na.locf(place_grp, na.rm = FALSE),
          place_grp
        ))
    }

    d_clusters <- cluster(d_places, cluster_threshold = cluster_threshold)

    if (is.na(d_clusters$cluster_grp[window / 2 - 1])) {
      c_grp1 <- d_clusters$cluster_grp[window/2]
      d_clusters$cluster_grp[1:(window/2 - 1)] <- c_grp1
    }
  } else if (sum(!is.na(df$lat) > 0) & sum(d_break$move_break, na.rm = TRUE) == 0) {
    d_clusters <- d_break %>%
      select(-c(move_break, rw_num)) %>%
      mutate(cluster_grp = NA)
    message(paste0(
      "NO CLUSTERS IDENTIFIED - the individual may have been in transit",
      "\n for the duration of the sampling session."
    ))
  } else if (sum(is.na(df$lat)) == nrow(df)) {
    d_clusters <- df
    message(paste0(
      "INVALID INPUT DATA - the input data frame does not have valid 'lon/lat' coordinates.",
      "\n Data unable to be clustered."
    ))
  }

  if (show_circvar == TRUE & sum(is.na(df$lat)) != nrow(df)) {
    d_clusters <- d_clusters
  } else if (show_circvar == FALSE & sum(is.na(df$lat)) != nrow(df)) {
    d_clusters <- d_clusters %>% select(-circvar)
  } else if (sum(is.na(df$lat)) == nrow(df)) {
    d_clusters <- d_clusters %>% mutate(
      circvar = NA,
      cluster_grp = NA
    )
  }
  d_clusters
}
