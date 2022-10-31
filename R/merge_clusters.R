
#' Spatially merge clusters
#'
#' `merge_clusters()` aggregates spatiotemporal clusters within a specified distance using the
#' Density-based Spatial Clustering of Applications with Noise (\code{\link[dbscan]{DBSCAN}}) algorithm.
#' After merging, the remaining clusters are not temporally unique.
#'
#' `merge_clusters()` spatially combines clusters based on the Euclidean distance between points. Because the Earth is sphere, the calculated
#' distances are not exact. See [here](http://wiki.gis.com/wiki/index.php/Decimal_degrees).

#'
#' @param df a data frame created by `circleclust()` with a `sp_temporal_cluster` column and datetime column.
#' @param dt_field POSIXct; name of datetime field.
#' @param radius numeric; distance threshold (meters) used to aggregate clusters.
#' @param minPts numeric; minimum number of points points required in each cluster.
#' @param borderPoints logical; should border points be assigned to clusters. Default = TRUE.
#' If FALSE, border points are considered noise.
#' @param keep_noise logical; should noise points be retained? Default = FALSE.
#' @param noise_threshold numeric; threshold value (%) to determine if noise points should be retained.
#' If the percentage of noise points is above this value, noise points are retained and column `noise` is
#' appended to the output data frame. Noise points are deleted otherwise. This argument is ignored if
#' 'keep_noise' is set to FALSE.
#' @return a data frame. The original spatiotemporal cluster values are retained
#' in a column called `sp_temporal_cluster`. New spatially merged cluster values are
#' listed under `spatial_cluster`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' merge_clusters(
#'      df, dt_field = NULL, radius = 100, minPts = 5, borderPoints = TRUE,
#'      keep_noise = FALSE, noise_threshold = 1)
#' }

merge_clusters <- function(df, dt_field = NULL, radius = 100, minPts = 5, borderPoints = TRUE, keep_noise = FALSE, noise_threshold = 1) {

  if (!'sp_temporal_cluster' %in% names(df)) {
    stop('Column `sp_temporal_cluster` is not in the input data frame. Did you use `circleclust()` to identify periods of stationary activity?',
         call. = FALSE)
  }

  if (sum(is.na(df$sp_temporal_cluster)) == nrow(df)) {
    stop('The input data frame does not contain periods of stationary activity/clustered coordinates.',
         call. = FALSE)
  }

  stop_quietly <- function() {
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }

  if (max(df$sp_temporal_cluster, na.rm = TRUE) == 1) {
    warning('The input data frame only has 1 identified cluster. Execution halted--returning the input data frame.',
            call. = FALSE)
    stop_quietly()
  }

  if (is.null(dt_field)) {
    stop("`dt_field` has not been assigned a value.", call. = FALSE)
  } else if (!lubridate::is.POSIXct(df[[dt_field]])) {
    c_dt_field <- class(df[[dt_field]])
    stop(paste0("`dt_field` must be a datetime. `", {{ dt_field }}, "` is of class ", c_dt_field, "."),
         call. = FALSE
    )
  }

  d_xy <- df %>%
    dplyr::filter(!is.na(lat) | !is.na(lon)) %>%
    dplyr::filter(!is.na(sp_temporal_cluster))

  d_xy_lonlat <- d_xy %>%
    dplyr::select(lon, lat)

  mdb <- dbscan::dbscan(d_xy_lonlat, eps = ((0.001/111)*radius), minPts = minPts, borderPoints = borderPoints)

  d_xy$db_cluster <- mdb$cluster
  d_mc <- suppressMessages(dplyr::left_join(df, d_xy))

  d_mc <- d_mc %>%
    dplyr::rename(spatial_cluster = db_cluster) %>%
    dplyr::arrange(.[[dt_field]])

  n_spatio <- length(table(d_mc$sp_temporal_cluster))
  n_db <- length(table(d_mc$spatial_cluster))

  if (n_spatio > n_db) {

    message(crayon::green(paste0('The number of spatiotemporal clusters was reduced from ', n_spatio, ' to ', n_db, ' spatial clusters.')))


    noise_table <- table(d_xy$db_cluster)
    noise_lgl <- '0' %in% names(noise_table)

    if (noise_lgl) {

      n_noise <- noise_table[['0']]
      message(crayon::cyan(paste(n_noise,
                                  'coordinates were classified as "noise."')))
    } else {
      message(crayon::cyan('Noise points were not detected.'))
    }

    if (keep_noise == TRUE & noise_lgl) {

      pct_noise <- round(n_noise/nrow(df)*100, digits = 3)

      if (pct_noise < noise_threshold) {
        message(crayon::red(
          paste0('The percentage of noise coordinates (', pct_noise, '%) was below the threshold value (',
                 noise_threshold, '%), and these observations were deleted.')))

        d_mc <- dplyr::filter(d_mc, spatial_cluster != 0 | is.na(spatial_cluster))

      } else {

        mc_noise <- dplyr::mutate(d_mc,
                                  noise = ifelse(spatial_cluster == 0, 1, 0),
                                  noise_break = dplyr::if_else((dplyr::lag(noise) == 0 | is.na(dplyr::lag(noise))) & noise == 1, 1, 0, missing = 0)) %>%
          dplyr::filter(noise == 1) %>%
          dplyr::mutate(noise_grp = cumsum(noise_break),
                        nc = ifelse(noise == 1, noise_grp + n_db, spatial_cluster)) %>%
          dplyr::select(-c(noise_break, noise_grp))

        mc_noise <- mc_noise %>%
          dplyr::mutate(spatial_cluster = dplyr::if_else(is.na(nc), as.numeric(spatial_cluster), nc)) %>%
          dplyr::select(-nc)

        d_mc <- dplyr::bind_rows(d_mc, mc_noise) %>%
          dplyr::filter(spatial_cluster != 0 | is.na(spatial_cluster))
      }
    }
  } else {
    message(crayon::cyan('Clusters were not merged. Multiple clusters do not exist within the specified radius.'))

    d_mc <- d_mc %>%
      # dplyr::rename(cluster_grp = sp_temporal_cluster) %>%
      dplyr::select(-spatial_cluster)
  }
  d_mc %>%
    dplyr::arrange(.[[dt_field]])
}
