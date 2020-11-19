

#' Cluster coordinates based on circular variance
#'
#' `circleclust` calculates the circular variance of consecutive latitude and
#' longitude coordinates within a moving window and identifies spatiotemporal clusters
#' based on a set of flexible parameters.  These parameters include the size of the
#' window in which to calculate circular variance, the threshold value (circular variance)
#' to identify clustering, and the minimum number of observations allowed in a
#' cluster.
#'
#' Observations belonging to a cluster are assigned to a cluster group,
#' which are ordered temporally. Further, observations are marked as either
#' static or mobile.
#'
#' The `circleclust()` function has been optimized to detect changes in activity
#' pattern within a 5-minute moving window and a threshold circular variance of 0.7.
#' These parameters can be adjusted if desired.
#'
#' Imputing lon/lat values using `impute_coords()` is recommended if GPS
#' coordinates are missing.
#'
#' @param df a data frame with columns `speed` and `azimuth` created by `move()`.
#' The data frame must also include datetime, longitude, and latitude columns.
#' @param dt_field POSIXct; name of datetime field.
#' @param circvar_threshold numeric; circular variance threshold to determine clustering.
#' Default = 0.7.
#' @param window numeric; window (number of rows) in which to calculate circular variance.
#' @param show_circvar logical; if `TRUE`, a column will be added to the output data
#' frame listing the circular variance (`circvar`) for each observation.
#' Default = `FALSE`.
#' @param rspeed_threshold numeric; if assigned a numeric value, the 1-minute rolling
#' median speed (m/s) is calculated. Observations with a circular variance above
#' 'circvar_threshold' and below 'rspeed_threshold' are assigned to a cluster. Assigning
#' a value to this parameter can mitigate incidental clustering of coordinates due
#' to stop-and-go transit.
#' @param pl_dist_threshold numeric; distance threshold (meters) used to aggregate
#' clusters. If the distance between consecutive clusters is below this threshold,
#' each cluster, and the coordinates between them, are combined into a single
#' grouping of coordinates. Setting this parameter may be useful if location data
#' includes transit at a low speed with frequent stops (i.e. walking).
#' @param cluster_threshold numeric; the minimum allowable number of observations
#' in each cluster.  If the number of observations in an identified cluster is less than this threshold,
#' the observations are retained but not assigned a to a cluster.
#'
#'
#' @return a data frame. New columns `cluster_grp` and `activity_status` are appended
#' to the input data frame.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' circleclust(df,
#'   dt_field = NULL, circvar_threshold = .7, window = 60,
#'   cluster_threshold = NULL, show_circvar = FALSE, rspeed_threshold = NULL,
#'   pl_dist_threshold = NULL, cluster_threshold = NULL
#' )
#'
#'
#' zoo_trip %>%
#'   impute_coords("Date_Time") %>%
#'   dt_aggregate("Date_Time") %>%
#'   move("Date_Time") %>%
#'   circleclust("Date_Time", pl_dist_threshold = 25, show_circvar = TRUE)
#' }
circleclust <- function(df, dt_field = NULL, circvar_threshold = .7, window = 60,
                        show_circvar = FALSE, rspeed_threshold = NULL,
                        pl_dist_threshold = NULL, cluster_threshold = NULL) {
  if (sum(stringr::str_detect(names(df), "azimuth")) == 0) {
    stop("Column 'azimuth' not found.  Use `move()` to calculate the azimuth",
      call. = FALSE
    )
  }

  if (is.null(dt_field)) {
    stop("`dt_field` has not been assigned a value.", call. = FALSE)
  } else if (!lubridate::is.POSIXct(df[[dt_field]])) {
    c_dt_field <- class(df[[dt_field]])
    stop(paste0("`dt_field` must be a datetime. `", {{ dt_field }}, "` is of class ", c_dt_field, "."),
      call. = FALSE
    )
  }

  time_unit <- floor(median(diff(df[[dt_field]])))
  units(time_unit) <- "secs"
  time_unit <- as.numeric(time_unit)

  t_unit_window <- 60 / time_unit ## 1 minute rolling window to calculate speed

  if (time_unit > 60) {
    stop("The `Date_Time` interval is greater than 60 seconds. Reduce the time unit when aggregating (5 seconds is recommended).",
      call. = FALSE
    )
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
      circvar = round(1 - res_length, digits = 1)
    )

  if (is.null(rspeed_threshold)) {
    d_break <- d_variance %>%
      dplyr::mutate(
        move_break = ifelse(circvar >= circvar_threshold, 1, 0),
        r = dplyr::row_number()
      ) %>%
      dplyr::select(-c(a_rad:res_length))
  } else if (is.numeric(rspeed_threshold)) {
    d_break <- d_variance %>%
      dplyr::mutate(
        roll_speed = rspeed_minute(speed_ms, t_unit_window),
        roll_speed = zoo::na.locf(roll_speed, na.rm = FALSE, maxgap = t_unit_window),
        move_break = ifelse(circvar >= circvar_threshold & roll_speed <= rspeed_threshold, 1, 0),
        r = dplyr::row_number()
      ) %>%
      dplyr::select(-c(a_rad:res_length, roll_speed))
  } else {
    stop("`rspeed_threshold` must be numeric or set to NULL", call. = FALSE)
  }

  if (sum(d_break$move_break, na.rm = TRUE) > 0) {
    d_places <- places(d_break)
    d_places$clustered_coord <- ifelse(is.na(d_places$place_grp), 0, 1)

    d_places <- d_places %>%
      place_lapse() %>%
      place_lapse_dist()

    if (!is.null(pl_dist_threshold) & max(d_places$place_grp, na.rm = TRUE) > 1) {
      d_places <- d_places %>%
        dplyr::mutate(place_grp = ifelse(is.na(place_grp) & pl_distance < pl_dist_threshold, zoo::na.locf(place_grp, na.rm = FALSE),
          place_grp
        ))
    }

    d_clusters <- cluster(d_places, cluster_threshold = cluster_threshold)

    if (is.na(d_clusters$cluster_grp[window / 2 - 1])) {
      c_grp1 <- d_clusters$cluster_grp[window / 2]
      d_clusters$cluster_grp[1:(window / 2 - 1)] <- c_grp1
    }
  } else if (sum(!is.na(df$lat) > 0) & sum(d_break$move_break, na.rm = TRUE) == 0) {
    d_clusters <- d_break %>%
      dplyr::select(-c(move_break, r)) %>%
      dplyr::mutate(cluster_grp = NA)
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
    d_clusters <- d_clusters %>% dplyr::select(-circvar)
  } else if (sum(is.na(df$lat)) == nrow(df)) {
    d_clusters <- d_clusters %>% dplyr::mutate(
      circvar = NA,
      cluster_grp = NA
    )
  }

  d_clusters <- d_clusters %>%
    dplyr::mutate(activity_status = dplyr::case_when(
      !is.na(cluster_grp) ~ "static",
      is.na(cluster_grp) & !is.na(lat) ~ "mobile"
    ))
  d_clusters
}
