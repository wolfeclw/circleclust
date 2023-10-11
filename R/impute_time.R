
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
#' @param force logical; force the function to fill time between sampling intervals
#' even if no lapses are detected. For example, if location is recorded at a 5 second
#' sampling interval, extra rows can be inserted to convert the sampling resolution
#' to 1 measurement per second (`force = TRUE, force_interval = 1`). Default = FALSE.
#' @param force_interval numeric; the new sampling frequency in seconds to be
#' forced upon the input data frame. `force_interval` must be less than the
#' sampling interval of the input data frame.
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
impute_time <- function(df, dt_field = NULL, fill_cols = NULL, force = FALSE, force_interval = 1) {

  if (is.null(dt_field)) {
    stop("`dt_field` has not been assigned a value.", call. = FALSE)
  } else if (!lubridate::is.POSIXct(df[[dt_field]])) {
    c_dt_field <- class(df[[dt_field]])
    stop(paste0("`dt_field` must be a datetime. `", {{ dt_field }}, "` is of class ", c_dt_field, "."),
         call. = FALSE
    )
  } else if (is.unsorted(df[[dt_field]], strictly = FALSE)) {
    message(cli::col_red(paste0("The input data frame should be sorted by ascending ", {{ dt_field }}, ".")))
    message(cli::col_magenta('Sorting the data frame now...'))
    Sys.sleep(0.2)
  }

  time_unit <- floor(quantile(diff(df[[dt_field]]), 0.95))
  units(time_unit) <- "secs"
  time_unit <- as.numeric(time_unit)

  message(cli::col_green(paste0('Measurements appear to have been recorded at a ', time_unit,
                                ' second sampling interval.')))

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


  if (nrow(d_tlapse) == 0 & isFALSE(force)) {
    message(cli::col_cyan('No time lapses were detected--returning the input data frame.'))
    message(cli::col_magenta('If you wish to add time between timestamp intervals, you must specify `force = TRUE` and assign a value to `force_interval`.'))
    d_imputed <- df
  } else if (nrow(d_tlapse) == 0 & isTRUE(force)) {

    if (force_interval >= time_unit) {
      stop(paste0('`force_interval` must be less than the sampling interval: ', time_unit, ' second(s).'))
    }

    dt_range <- range(df[[dt_field]])
    force_diff <- as.numeric(difftime(dt_range[2], dt_range[1], units = 'secs'))
    force_len <- seq(to = force_diff, by = force_interval)
    dt_force <- dt_range[1] + lubridate::seconds(force_len)
    d_add_time <- tibble::enframe(dt_force, name = NULL, value = dt_field)

    d_imputed <- suppressMessages(dplyr::full_join(df, d_add_time))

    message(cli::col_cyan(paste0(force_len[length(force_len)] - length(df), ' datetime rows have been added.')))
    message(cli::col_cyan(paste0('The output sampling frequency is: ', force_interval, ' second(s).')))

  } else {

    l_tlapse <- dplyr::group_split(d_tlapse, tlapse_grp)

    add_time_rows <- function(d) {
      d_add_time <- tibble::tibble(new_dt = d$lag_dt + lubridate::dseconds(time_unit*1:(d$tlag/time_unit))) %>%
        dplyr::rename({{dt_field}} := new_dt)
      d_add_time
    }

    d_time_imputed <- purrr::map_df(l_tlapse, add_time_rows)
    n_imputed <- nrow(d_time_imputed)
    dur_imputed <- round(n_imputed/(60/time_unit), digits = 1)

    message(cli::col_cyan(paste0('A total of ', length(l_tlapse), ' datetime lapses in time were identified.')))
    message(cli::col_cyan(paste0('A total of ', n_imputed, ' datetime rows (', dur_imputed, ' mins) were imputed.')))

    d_imputed <- suppressMessages(dplyr::full_join(df, d_time_imputed))
  }

  d_imputed <- d_imputed %>%
    dplyr::arrange(., .[[dt_field]])

  if (!is.null(fill_cols)) {

    fill_lgl <- fill_cols %in% names(df)

    if (any(!fill_lgl)) {
      stop(paste0('All values assigned to `fill_cols` must be in the input data frame. Not found: ',
                  paste0('"', fill_cols[!fill_lgl], collapse = '", '), '"'),
           call. = FALSE)
    }
    d_imputed <- d_imputed %>%
      tidyr::fill(., {{fill_cols}})
    message(cli::col_cyan(paste0('Column(s) `', paste0(fill_cols, collapse = '`, `'), '` were carried forward from the last observation for the imputed rows.')))
  }

  d_imputed
}





