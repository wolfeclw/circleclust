
#' Impute datetime for gaps in time series data
#'
#' `impute_time()` fills datetime gaps in time series data. Specifically,
#' this function was designed to impute gaps in GPS data that may result
#' from signal loss or because the GPS logger goes into 'sleep' mode (i.e. Columbus GPS logger).
#' Datetime rows are added to the input data frame under the specified datetime
#' column (`dt_field`) to fill gaps between time stamps. The sampling interval is
#' automatically calculated.
#'
#' Columns that have one unique value and are unaffected by imputation  (i.e.
#' participant or sample ID) can be specified under '`fill_cols`. Values for these
#' columns will be carried forward from the last observation.  If NULL (default),
#' only the datetime column (`dt_field`) will be assigned values for the imputed rows.
#'
#' `impute_coords()` can be used in conjunction with `impute_time()` to assign
#' latitude and longitude to the added timestamps/rows.
#'
#' @param df a data frame with a datetime field.
#' @param dt_field character; name of datetime field.
#' @param fill_cols character; names of columns that should have values carried forward.
#'
#' @export
#'
#' @importFrom stats quantile
#'
#' @examples
#' \dontrun{
#' impute_time(df, dt_field = 'Date_Time', fill_cols = 'ID') %>%
#'   impute_coords(dt_field = 'Date_Time')
#' }
#'
impute_time <- function(df, dt_field = NULL, fill_cols = NULL) {

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

  time_unit <- floor(quantile(diff(df[[dt_field]]), 0.95))
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
    d_add_time
  }

  d_time_imputed <- purrr::map_df(l_tlapse, add_time_rows)
  n_imputed <- nrow(d_time_imputed)
  dur_imputed <- round(n_imputed/(60/time_unit), digits = 1)

  message(crayon::green(paste0('Measurements appear to have been recorded at a ', time_unit,
                               ' second sampling interval.')))
  message(crayon::cyan(paste0('A total of ', length(l_tlapse), ' datetime lapses in time were identified.')))
  message(crayon::cyan(paste0('A total of ', n_imputed, ' datetime rows (', dur_imputed, ' mins) were imputed.')))

  d_imputed <- suppressMessages(dplyr::full_join(df, d_time_imputed))

  if (!is.null(fill_cols)) {

    fill_lgl <- fill_cols %in% names(df)

    if (any(!fill_lgl)) {
      stop(paste0('All values assigned to `fill_cols` must be in the input data frame. Not found: ',
                  paste0('"', fill_cols[!fill_lgl], collapse = '", '), '"'),
           call. = FALSE)
    }
    d_imputed <- d_imputed %>%
      tidyr::fill(., {{fill_cols}})
    message(crayon::cyan(paste0('Column(s) `', paste0(fill_cols, collapse = '`, `'), '` were carried forward from the last observation for the imputed rows.')))
  }

  d_imputed %>%
    dplyr::arrange(., .[[dt_field]])

}





