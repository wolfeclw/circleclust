
#' Set `mapdeck` token
#'
#' Makes mapbox API token available for `mapdeck` calls
#'
#' See (\code{\link[mapdeck]{set_token}}).
#'
#' You can request a mapbox acces token [here](https://docs.mapbox.com/help/how-mapbox-works/access-tokens/).
#'
#' @param token mapbox access token
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' set_token('my_mapbox_token')
#' }
set_token <- function(token) {
  mapdeck::set_token(token)
}
