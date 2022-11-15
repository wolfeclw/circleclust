
#' Impute missing GPS data
#'
#' `impute_coords()` imputes missing lon/lat coordinates that occur during GPS lapses.
#'
#' @param df data frame containing latitude (`lat`) and longitude (`lon`)
#' @param dt_field character; name of datetime field.
#' @param distance_threshold numeric; distance (meters) between the last known coordinates before
#' GPS signal loss and the first known coordinates following signal loss are compared to
#' this value.  If the distance exceeds this threshold, coordinates are not
#' imputed. Default = 100 meters.
#' @param jitter_amount numeric; amount of jitter to apply to imputed coords.
#' Default = 0.00005 decimal degrees. See \code{\link[sf]{st_jitter}}.
#' @param show_lapse_distance logical; if `TRUE`, a column will be added to the output data frame
#' listing the distance between the last and first know coordinates for each lapse in
#' GPS signal. Default = `FALSE`.
#' @param fill_open_lapses logical; if `TRUE`,missing coordinates at the
#' beginning and end of the data frame are imputed (i.e. lapses not enclosed by known
#' coordinates). Default = `FALSE`.
#' @param speed_threshold numeric; criteria to impute open lapses. If the median speed (m/s)
#' of coordinates before or after an open lapse exceeds this threshold,
#' coordinates are not imputed.
#' @param speed_window numeric; number of rows used to calculate `speed_threshold`.
#' @param open_lapse_length numeric; if the number of rows in an open lapse exceed this
#' threshold, coordinates are not imputed.
#'
#' @return a data frame.  An additional column is created to indicate whether
#' coordinates were imputed ('imputed_coord').
#'
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' impute_coords(df,
#'   distance_threshold = 100, jitter_amount = 0.00001, fill_open_lapses = FALSE,
#'   speed_threshold = NULL, speed_window = NULL, open_lapse_length = NULL
#' )
#' }
impute_coords <- function(df, dt_field = NULL, distance_threshold = 100, jitter_amount = 0.00005,
                          show_lapse_distance = FALSE, fill_open_lapses = FALSE, speed_threshold = NULL,
                          speed_window = NULL, open_lapse_length = NULL) {

  if (is.null(dt_field)) {
    stop("`dt_field` has not been assigned a value.", call. = FALSE)
  } else if (!lubridate::is.POSIXct(df[[dt_field]])) {
    c_dt_field <- class(df[[dt_field]])
    stop(paste0("`dt_field` must be a datetime. `", {{ dt_field }}, "` is of class ", c_dt_field, "."),
         call. = FALSE
    )
  } else if (is.unsorted(df[[dt_field]])) {
    stop(paste0("The input data frame should be sorted by ascending ", {{ dt_field }}, "."),
         call. = FALSE
    )
  }

  open_parms_lgl <- sum(purrr::map_lgl(
    c(speed_threshold, speed_window, open_lapse_length),
    is.numeric
  ))

  if (fill_open_lapses == TRUE & open_parms_lgl != 3) {

    stop(paste("To impute open lapses, numeric values must be assigned to `speed_threshold`, `speed_window`,",
               "and `open_lapse_length`.",
               sep = "\n"
    ),
    call. = FALSE
    )
  }

  if (sum(!is.na(df$lat)) == nrow(df)) {

    d_imputed <- df %>%
      dplyr::mutate(imputed_coord = 0)
    message("All location data is complete - coordinates were not imputed.")

  } else if (fill_open_lapses == TRUE) {

    d_dist_imputed <- impute_coords_dist(df, dt_field = dt_field, distance_threshold = distance_threshold,
                                         jitter_amount = jitter_amount, show_lapse_distance = show_lapse_distance)

    d_imputed <- impute_coords_open(d_dist_imputed, jitter_amount = jitter_amount, speed_threshold = speed_threshold,
                                    speed_window = speed_window, open_lapse_length = open_lapse_length)

  } else {

    d_imputed <- impute_coords_dist(df, dt_field = dt_field, distance_threshold = distance_threshold,
                                    jitter_amount = jitter_amount, show_lapse_distance = show_lapse_distance)

  }

  if (sum(!is.na(df$lat)) != nrow(df)) {
    n_imputed <- sum(d_imputed$imputed_coord, na.rm = TRUE)
    n_na_coords <- sum(is.na(d_imputed$lat))

    message(crayon::magenta(paste0(
      "\nA total of ", n_imputed, " (", scales::percent(n_imputed / nrow(d_imputed)), ")",
      " of the coordinates were imputed."
    )))
    message(crayon::magenta(paste0(
      "A total of ", n_na_coords, " (", scales::percent(n_na_coords / nrow(d_imputed)), ")",
      " of the coordinates are missing GPS data after imputation."
    )))
  }

  if (fill_open_lapses == FALSE & open_parms_lgl > 0) {
    warning(paste("Values assigned to `speed_threshold`, `speed_window`, and/or `open_lapse_lenth` were ignored.",
                  "Set `fill_open_lapses` == `TRUE`.",
                  sep = "\n"
    ),
    call. = FALSE
    )
  }

  if (sum(!is.na(df$lat)) == 0) {
    stop("All `lat/lon` values are invalid.", call. = FALSE)
  }

  d_imputed
}

