
#' Create a `mapdeck` pointcloud
#'
#' 'cluster_deck' is a convenience function to create a pointcloud map to
#' visualize clustered coordinates.
#'
#' See (\code{\link[mapdeck]{add_pointcloud}})
#'
#' @param df an sf object
#' @param tooltip character; name of column or HTML text to render as a tooltip.
#' @param fill_colour character; name of column or hex color used to fill points.
#' @param elevation character; name of column with elevation values.
#' @param radius numeric; value in pixels of each point.
#' @param palette string or matrix; indicates a color palette.
#' @param legend logical; either a logical indicating if the legend should be
#' displayed, or a named list indicating which color attributes should be included
#' in the legend.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' cluster_deck(df,
#'   tooltip = "cluster_grp", fill_colour = "cluster_grp",
#'   elevation = NULL, radius = 10, palette = "viridis", legend = TRUE
#' )
#' }
cluster_deck <- function(df, tooltip = "sp_temporal_cluster", fill_colour = "sp_temporal_cluster", elevation = NULL,
                         radius = 10, palette = "viridis", legend = TRUE) {
  df_class <- class(df)

  if (sum(stringr::str_detect(df_class, "sf")) == 0) {
    stop("The input data frame is not an `sf` object. Try using `wgs_sf()` to transform the
         input data frame.",
      call. = FALSE
    )
  }

  if (!is.null(tooltip)) {
    tt <- df[tooltip] %>% sf::st_drop_geometry()

    if (purrr::map_lgl(tt, lubridate::is.POSIXct)) {
      df$tltp <- lubridate::ymd_hms(df[[tooltip]], tz = "UTC")
    } else {
      df$tltp <- df[[tooltip]]
    }
  }

  mapdeck::mapdeck() %>%
    mapdeck::add_pointcloud(
      data = df,
      tooltip = "tltp",
      fill_colour = fill_colour,
      elevation = elevation,
      radius = radius,
      palette = palette,
      legend = legend
    )
}
