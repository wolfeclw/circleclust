

#' Aggregate spatiotemporal clusters
#'
#' `merge_clusters()` aggregates spatiotemporal clusters within a specific distance.
#' Essentially, clusters are aggregated spatially and the temporal combonent is removed.
#'
#' @param df a data frame created by `circleclust()` with a `cluster_grp` column.
#' The data frame must also include datetime, longitude, and latitude columns.
#' @param dt_field POSIXct; name of datetime field.
#' @param distance_threshold numeric; distance threshold (meters) used to aggregate
#' clusters.
#'
#' @return a data frame. The original spatiotemporal cluster values are retained
#' in a column called `spatiotemp_cluster_grp`. Aggregated cluster values are
#' listed under `cluster_grp`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' merge_clusters(df,
#'   distance_threshold = 100, dt_field = NULL)
#' }
merge_clusters <- function(df, distance_threshold = 100, dt_field = NULL) {

  if (!'cluster_grp' %in% names(df)) {
    stop('Column `cluster_grp` is not in the input data frame. Did you use `circleclust()` to identify periods of stationary activity?',
         call. = FALSE)
  }

  if (sum(is.na(df$cluster_grp)) == nrow(df)) {
    stop('The input data frame does not contain periods of stationary activity/clustered coordinates.')
  }

  if (is.null(dt_field)) {
    stop("`dt_field` has not been assigned a value.", call. = FALSE)
  } else if (!lubridate::is.POSIXct(df[[dt_field]])) {
    c_dt_field <- class(df[[dt_field]])
    stop(paste0("`dt_field` must be a datetime. `", {{ dt_field }}, "` is of class ", c_dt_field, "."),
         call. = FALSE
    )
  }

  c_grps <- grp_clusters(df = df, distance_threshold = distance_threshold)

  c_list <- dplyr::group_split(df, cluster_grp)

  reorder_grp_clusters(df, dt_field = dt_field, c_grps, c_list)

}



