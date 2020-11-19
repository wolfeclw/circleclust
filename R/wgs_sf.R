
#' Create `sf` Object
#'
#' Creates a multipoint `sf` object. Points are projected using WGS84 (crs = 4326).
#' Missing coordinates are allowed, but given an empty geometry.
#'
#' @param df a data frame with columns for latitude (`lat`) and longitude (`lon`).
#'
#' @return a `POINT` object.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ufp_sf(df)
#' }
wgs_sf <- function(df) {
  if (sum(stringr::str_detect(names(df), "lon|lat")) < 2) {
    stop("The input data frame is missing longitude and latitude, or the columns are not properly labeled (i.e. `lon` and `lat`).")
  }

  if (sum(is.na(df$lat)) == nrow(df)) {
    stop("The input data frame does not have any valid 'lon/lat' coordinates. The data frame cannont be converted to an `sf` object.")
  }

  d_coords <- dplyr::filter(df, !is.na(lat))
  d_coords_sf <- suppressWarnings(sf::st_as_sf(d_coords, coords = c("lon", "lat"), crs = 4326))
  d_join <- suppressMessages(dplyr::full_join(df, d_coords_sf))
  df_sf <- sf::st_as_sf(d_join, crs = 4326)
  sf_empty_rows <- sum(sf::st_is_empty(df_sf))
  pct_empty <- round(sf_empty_rows / nrow(df_sf), digits = 2)

  if (pct_empty > .5) {
    warning(paste("Greater than 50% of the measurements in the input data frame have invalid GPS coordinates."),
      call. = FALSE
    )
  }
  return(df_sf)
}
