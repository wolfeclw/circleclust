

cluster_deck <- function(d, tltp_var = NULL, fill_var = NULL) {

  if (!is.null(tltp_var)) {
    tt <- d[tltp_var] %>% sf::st_drop_geometry()

    if (map_lgl(tt, lubridate::is.POSIXct)) {
      d$tltp <- lubridate::ymd_hms(d[[tltp_var]], tz = 'UTC')
    } else {
      d$tltp <- d[[tltp_var]]
    }
  }

  mapdeck::mapdeck() %>%
    mapdeck::add_pointcloud(
      data = d
      , fill_colour = fill_var
      , legend = TRUE
      , tooltip = 'tltp'
    )

}
