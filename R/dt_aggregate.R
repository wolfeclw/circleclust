
#' Aggregate data by time unit
#'
#' `dt_aggregate()` aggregates numeric values to a specified time unit. Columns with
#' uniform character values are retained (i.e. patient ID, sensor name).
#'
#' @param df a data frame with a datetime field.
#' @param dt_field character; name of datetime field.
#' @param unit character; string specifying a time unit or a multiple of a unit to be rounded
#' @param summary_fun character; summary function (i.e. 'mean', 'median'). Default = 'median.'
#' @param na_rm logical; should NA values be removed before the chosen
#' `summary_fun` computes?
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' dt_aggregate(df,
#'   dt_field = "Date_Time",
#'   unit = "5 seconds",
#'   summary_fun = "median", na_rm = TRUE
#' )
#' }
#' @importFrom stats median
#'
dt_aggregate <- function(df, dt_field = NULL, unit = "5 seconds",
                         summary_fun = "median", na_rm = TRUE) {
  if (is.null(dt_field)) {
    stop("`dt_field` has not been assigned a value.", call. = FALSE)
  } else if (!lubridate::is.POSIXct(df[[dt_field]])) {
    c_dt_field <- class(df[[dt_field]])
    stop(paste0("`dt_field` must be a datetime. `", {{ dt_field }}, "` is of class ", c_dt_field, "."),
         call. = FALSE
    )
  }

  ###

  d_agg_grp <- df %>%
    dplyr::mutate(agg_dt = lubridate::floor_date(.data[[dt_field]], unit = unit)) %>%
    dplyr::group_by(agg_dt)

  new_agg <- d_agg_grp %>%
    dplyr::select(agg_dt) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::rename('{dt_field}' := agg_dt)

  sfun_list <- list(mean = mean,
                    sd = sd,
                    median = median,
                    min = min,
                    max = max)

  sfun_wch <- names(sfun_list) %in% summary_fun
  slist <- sfun_list[sfun_wch]

  if (isTRUE(na_rm)) {
    d_agg_grp <- tidyr::drop_na(d_agg_grp, dplyr::where(is.numeric))
  }

  d_agg <- d_agg_grp %>%
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric), slist, .names = '{.col}'), .groups = 'drop') %>%
    dplyr::rename('{dt_field}' := agg_dt)

  d_agg <- dplyr::left_join(new_agg, d_agg, by = dt_field)

  if ('imputed_coord' %in% names(d_agg)) {
    d_agg$imputed_coord <- ceiling(d_agg$imputed_coord)
  }

  ####

  no_num_cols <- dplyr::select(df, -dplyr::where((is.numeric))) %>%
    dplyr::select(-dplyr::all_of({dt_field})) %>%
    purrr::map(., unique)

  no_num_cols_len <- no_num_cols %>%
    purrr::map_dbl(., length)

  if (any(length(no_num_cols_len)) == 0) {
    d_agg
  } else {
    unq_lgl <- no_num_cols_len %>%
      purrr::map_lgl(., ~ . == 1)

    char_keep <- no_num_cols[unq_lgl] %>%
      dplyr::bind_cols()
    rm_cols <- no_num_cols[!unq_lgl] %>% names()

    d_agg <- dplyr::bind_cols(char_keep, d_agg)
    d_agg <- dplyr::relocate(d_agg, .data[[dt_field]])

    if (length(rm_cols) > 0) {

      cp <- ifelse(length(rm_cols) > 1, 'Columns ', 'Column ')
      is_are <- ifelse(length(rm_cols) > 1, ' are', ' is')
      cli::col_magenta(message(
        paste0(cp, paste0('`', rm_cols, '`', collapse = ", "), is_are,
               " non-numeric and contain more than one unique value. These columns were \n removed during aggregation.")
      ))
    }
  }

  d_agg

}
