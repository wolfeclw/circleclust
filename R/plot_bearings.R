
#' Plot Bearings
#'
#' `plot_bearings` generates an interactive plot of personal sampling locations
#' using arrow markers. The direction of the arrows indicate the bearing between
#' sequential coordinates. Markers are characterized by activity status (mobile
#' vs. stationary).
#'
#' See (\code{\link[highcharter]{hchart}})
#'
#' @param df a data frame created by `circleclust()` containing column `activity_status`
#' @param dt_field character; name of datetime field. If specified, the coordinate
#' timestamp is displayed in the plot tooltip. Default = NULL.
#' @param divisor numeric; If specified, the input data frame is reduced by a
#' a factor of 1/divisor. A divisor of 2 will omit every other row of the input
#' data frame reducing the data by half. This can be useful to visualize large
#' data sets. Default = NULL.
#' @param export_options logical; If TRUE, a drop down menu will be shown in plot
#' area with options to export a static image. Default = FALSE.
#'
#' @return a `highcharter` vector plot.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_bearings(df, dt_field = NULL, divisor = NULL, export_options = FALSE )
#' }
#'
plot_bearings <- function(df, dt_field = NULL, divisor = NULL, export_options = FALSE) {

  d_hc <- dplyr::mutate(df,
                        hc_activity = stringr::str_to_title(activity_status),
                        hc_bearing = round(ifelse(azimuth > 180, azimuth - 180, azimuth + 180),
                                           digits = 0),
                        hc_bearing_label = round(azimuth, digits = 0),
                        dt_utc = lubridate::ymd_hms(df[[dt_field]], tz = "UTC")) %>%
    dplyr::rename(`Activity Status` = hc_activity) %>%
    dplyr::filter(!is.na(lon))

  if (is.null(divisor)) {
    d_hc <- d_hc
  } else {
    d_hc <- dplyr::filter(d_hc, dplyr::row_number() %% divisor == 0)
  }

  dc_mobile <- dplyr::filter(d_hc, activity_status == 'mobile')
  dc_static <- dplyr::filter(d_hc, activity_status == 'static')

  x <- c("Status", 'Bearing', 'Cir. Variance', 'Timestamp')

  if (is.null(dt_field)) {
    y <- sprintf("{point.%s}", c('Activity Status', 'hc_bearing_label', 'circvar'))
  } else {
    y <- sprintf("{point.%s}", c('Activity Status', 'hc_bearing_label', 'circvar', 'dt_utc'))
  }

  tltip <- highcharter::tooltip_table(x, y)

  vpal <- viridis::plasma(20)

  b_hc <- highcharter::highchart() %>%
    highcharter::hc_add_series(dc_mobile, highcharter::hcaes(lon, lat, group = `Activity Status`,
                                                             direction = hc_bearing, length = .75),
                               type = "vector", showInLegend = TRUE, color = vpal[1]) %>%
    highcharter::hc_add_series(dc_static, highcharter::hcaes(lon, lat, group = `Activity Status`,
                                                             direction = hc_bearing, length = .85),
                               type = "vector", showInLegend = TRUE, color = vpal[18]) %>%
    highcharter::hc_chart(zoomType = 'xy',
                          backgroundColor = highcharter::hex_to_rgba('#e6e6e6', alpha = .25)) %>%
    highcharter::hc_tooltip(crosshairs = FALSE, headerFormat = "", useHTML = TRUE, table = TRUE,
                            sort = TRUE, pointFormat = tltip) %>%
    highcharter::hc_plotOptions(vector = list(cluster = list(enabled = TRUE))) %>%
    highcharter::hc_yAxis(title = list(text = 'LATITUDE'), gridLineWidth = 0, showLastLabel = FALSE, showFirstLabel = FALSE,
                          style = list(fontSize = '16px',
                                       color = 'black')) %>%
    highcharter::hc_xAxis(title = list(text = 'LONGITUDE',
                                       y = 5),
                          labels = list(y = 25),
                          style = list(fontSize = '16px',
                                       color = 'black'))

  if (export_options == TRUE) {
    b_hc <- b_hc %>%
      highcharter::hc_exporting(enabled = TRUE,
                                buttons = list(
                                  contextButton = list(
                                    menuItems = list('downloadPNG', 'downloadJPEG', 'downloadPDF'))))
  }
  b_hc
}
