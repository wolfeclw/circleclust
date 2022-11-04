
#' Filter time series data by a specific time interval
#'
#' `filter_time()` can be used to filter time series data according to a specific time interval.
#' Traditionally, filtering by time can be messy due to the circular nature of time and
#' the 24 hour clock. `filter_time()` uses circular statistics to convert time to degrees, which enables
#' time series data to be filtered by time intervals that include midnight and span different
#' dates (i.e. '23:00:00' to '04:00:00') without issue. Because `filter_time()` is agnostic
#' to date, time series data can easily be filtered across multiple days.
#'
#' @param df a data frame with a datetime  or time column.
#' @param t_var character; name of time or datetime column to filter.
#' @param t1 character; start time of filtering interval. Start and and times should be in 'HH:MM:SS' format.
#' @param t2 character; end time of filtering interval.
#' @param span_days logical; does the filtering interval include midnight? Default = FALSE.
#' @param summary_tbl logical; if TRUE (default), a summary table is included in the output showing the
#' number of observations occurring in each hour of the filtering interval.
#'
#' @export
#'
#' @examples
#'
#'   filter_time(zoo_trip, t_var = 'Date_Time', t1 = '13:00:00', t2 = '14:30:00',
#'               summary_tbl = TRUE, span_days = TRUE)
#'
#'
filter_time <- function(df, t_var = NULL, t1 = NULL, t2 = NULL, span_days = FALSE, summary_tbl = TRUE) {


  if (is.null(t_var)) {
    stop('`t_var` must e assigned a value.')
  }

  if (is.null(t1)) {
    stop('`t_1` must e assigned a value.')
  }

  if (is.null(t_2)) {
    stop('`t_2` must e assigned a value.')
  }

  tc <- class(df[[t_var]])

  if (!sum(tc %in% c("POSIXct", "POSIXt", 'hms', 'times') > 0)) {
    stop(paste0('`', t_var, '` is not formatted as a datetime or time column. Try using `lubridate::as_datetime()` or `hms::as_hms()`.'))
  }

  te1 <- try(hms::as_hms(t1), silent = TRUE)
  te2 <- try(hms::as_hms(t2), silent = TRUE)

  if ('try-error' %in% class(te1)) {
    stop('`t1` should be formatted as HH:MM:SS.')
  }

  if ('try-error' %in% class(te2)) {
    stop('`t2` should be formatted as HH:MM:SS.')
  }

  ht1 <- hms::as_hms(t1)
  ht2 <- hms::as_hms(t2)

  message(paste0('Returning observations recorded between ', ht1, ' and ', ht2, '.'))

  d_deg <- df %>%
    dplyr::mutate(deg_time = (360*(lubridate::hour(.data[[t_var]]) + (lubridate::minute(.data[[t_var]])/60))/24))

  deg_t1 <- (360*(lubridate::hour(ht1) + (lubridate::minute(ht1)/60))/24)
  deg_t2 <- (360*(lubridate::hour(ht2) + (lubridate::minute(ht2)/60))/24)

  if (span_days == TRUE) {

    d_deg <- d_deg %>%
      dplyr::mutate(deg_time = ifelse(lubridate::hour(.data[[t_var]]) < 12, deg_time + 180,
                                   deg_time - 180))

    deg_t1 <- (360*(lubridate::hour(ht1) + (lubridate::minute(ht1)/60))/24) - 180
    deg_t2 <- (360*(lubridate::hour(ht2) + (lubridate::minute(ht2)/60))/24) + 180

  }

  d_filtered <- d_deg %>%
    dplyr::filter(deg_time > deg_t1 & deg_time < deg_t2)

  if (nrow(d_filtered) == 0 & sum(ht1 > ht2) > 0 & span_days == FALSE) {
    message(crayon::red('`filter_time` returned zero columns.'))
    message(crayon::cyan('Does your time interval include midnight? Set `span_days = TRUE`. \n'))
  }

  # if (span_days == TRUE & (min(lubridate::hour(d_filtered[[t_var]])) < lubridate::hour(ht1))) {
  if (span_days == TRUE & (lubridate::hour(ht1) < lubridate::hour(ht2))) {
    warning('Check the output data frame.  Times outside the requested time interval were returned.')
    message(crayon::red('\n If the requested time interval does not include midnight,`span_days should be set to `FALSE`. \n'))
  }

  d_filtered <- d_filtered[, !names(d_filtered) %in% 'deg_time']

  if (summary_tbl == TRUE & nrow(d_filtered) > 0) {

    tbl_f <- table(lubridate::hour(d_filtered[[t_var]]))
    vnames <- paste0(purrr::flatten(attributes(tbl_f)$dimnames), ':00:00')
    dimnames(tbl_f) <- list(vnames)

    l_filtered <- list(d_filtered, tbl_f) %>% purrr::set_names(., c('d_filter_time', 'hour_summary_tbl'))
    l_filtered
  } else {
    d_filtered
  }

}



