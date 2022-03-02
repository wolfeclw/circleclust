
#' Impute datetime for gaps in GPS data
#'
#' `impute_time()` imputes datetime for gaps in GPS data.  Gaps in time may result
#' from filtering of invalid GPS data or because the GPS logger goes into 'sleep'
#' mode (i.e. Columbus GPS logger). Datetime rows are added to the input data frame
#' under the specified datetime column (`dt_field`) to fill gaps between time stamps.
#' The sampling interval is automatically calculated.
#'
#' `impute_coords()` can be used in conjunction with `impute_time()` to assign
#' latitude and longitude to the added time stamps/rows.
#'
#' @param df a data frame with a datetime field.
#' @param dt_field character; name of datetime field.
#'
#' @return data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' impute_time(df, dt_field = 'Date_Time') %>%
#'   impute_coords()
#' }
#'
impute_time <- function(df, dt_field = NULL) {

  time_unit <- floor(quantile(diff(df[[dt_field]]), 0.75))
  units(time_unit) <- "secs"
  time_unit <- as.numeric(time_unit)

  if (is.na(time_unit)) {
    stop('A datetime column muste be assigned to `dt_field`.',
         call. = FALSE)
  }

  if (!lubridate::is.POSIXct(df[[dt_field]])) {
    c_dt_field <- class(df[[dt_field]])
    stop(paste0("`dt_field` must be a datetime. `", {{ dt_field }}, "` is of class ", c_dt_field, "."),
         call. = FALSE)
  }


  d_tlapse <- df %>%
    dplyr::mutate(r = dplyr::row_number(),
           lag_dt = dplyr::lag(.[[dt_field]]),
           tlag = as.numeric(.[[dt_field]] - lag_dt),
           tbreak = ifelse(tlag > time_unit & !is.na(tlag), 1, 0)) %>%
    dplyr::filter(tbreak == 1) %>%
    dplyr::mutate(tlapse_grp = cumsum(tbreak))

  l_tlapse <- dplyr::group_split(d_tlapse, tlapse_grp)

  add_time_rows <- function(d) {
    d_add_time <- tibble::tibble(new_dt = d$lag_dt + lubridate::dseconds(time_unit*1:(d$tlag/time_unit))) %>%
      dplyr::rename({{dt_field}} := new_dt)
  }

  d_time_imputed <- purrr::map_df(l_tlapse, add_time_rows)
  n_imputed <- nrow(d_time_imputed)
  dur_imputed <- round(n_imputed/(60/time_unit), digits = 1)

  message(paste0('Measurements appear to have been recorded at a ', time_unit,
                 ' second sampling interval.'))
  message(paste0('A total of ', length(l_tlapse), ' lapses in time were identified.'))
  message(paste0('A total of ', n_imputed, ' datetime rows (', dur_imputed, ' mins) were imputed.'))

  d_imputed <- suppressMessages(dplyr::full_join(df, d_time_imputed))

  d_imputed %>%
    dplyr::arrange(., .[[dt_field]])

}





