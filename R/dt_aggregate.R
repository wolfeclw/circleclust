
#' Aggregate data by time unit
#'
#' `dt_aggregate()` aggregates numeric values to a specified time unit. Columns with
#' uniform character values are retained (i.e. patient ID, sensor name).
#'
#' @param df a data frame with a datetime field.
#' @param dt_field POSIXct; name of datetime field.
#' @param unit character; string specifying a time unit or a multiple of a unit to be rounded
#' @param floor_or_celiling character; either 'floor'(\code{\link[lubridate]{floor_date}})
#' or 'ceiling'(\code{\link[lubridate]{ceiling_date}}). Default = 'floor.'
#' @param summary_fun character; summary function (i.e. 'mean', 'median'). Default = 'median.'
#' @param na_rm_agg logical; should NA values be removed before the chosen
#' `summary_fun` computes?
#'
#' @return a data frame aggregated by the specified time unit.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' dt_aggregate(df,
#'   dt_field = "Date_Time",
#'   unit = "5 seconds", floor_or_ceiling = "floor",
#'   summary_fun = "median", na_rm_agg = TRUE
#' )
#' }
#' @importFrom stats median
dt_aggregate <- function(df, dt_field = NULL, unit = "5 seconds", floor_or_celiling = "floor",
                         summary_fun = "median", na_rm_agg = TRUE) {
  if (is.null(dt_field)) {
    stop("`dt_field` has not been assigned a value.", call. = FALSE)
  } else if (!lubridate::is.POSIXct(df[[dt_field]])) {
    c_dt_field <- class(df[[dt_field]])
    stop(paste0("`dt_field` must be a datetime. `", {{ dt_field }}, "` is of class ", c_dt_field, "."),
      call. = FALSE
    )
  }

  if (floor_or_celiling == "floor") {
    d_agg <- df %>%
      dplyr::mutate(agg_dt = lubridate::floor_date(.data[[dt_field]], unit = unit)) %>%
      dplyr::group_by(agg_dt) %>%
      dplyr::summarise_if(is.numeric, summary_fun, na.rm = na_rm_agg) %>%
      dplyr::rename("{{dt_field}}" := agg_dt)

    n <- gsub('["]', "", names(d_agg))
    colnames(d_agg) <- n
  } else {
    d_agg <- df %>%
      dplyr::mutate(agg_dt = lubridate::floor_date(.data[[dt_field]], unit = unit)) %>%
      dplyr::group_by(agg_dt) %>%
      dplyr::summarise_if(is.numeric, summary_fun, na.rm = na_rm_agg) %>%
      dplyr::rename("{{dt_field}}" := agg_dt)

    n <- gsub('["]', "", names(d_agg))
    colnames(d_agg) <- n
  }

  no_num_cols <- dplyr::select_if(df, ~ !is.numeric(.)) %>% dplyr::select(., -{{ dt_field }})

  if (ncol(no_num_cols) == 0) {
    d_agg
  } else {
    unq_lgl <- no_num_cols %>%
      purrr::map(., unique) %>%
      purrr::map_chr(., length) %>%
      purrr::map_lgl(., ~ . == 1)

    char_keep <- no_num_cols[unq_lgl][1:nrow(d_agg), ]
    rm_cols <- no_num_cols[!unq_lgl] %>% names()
  }

  if (length(rm_cols > 0)) {
    message(
      "Column(s) `", paste(rm_cols, collapse = ", "),
      "` are of class 'character' and contain more than one unique value. These columns were \n removed during aggregation."
    )

    d_agg <- dplyr::bind_cols(char_keep, d_agg)
    d_agg <- dplyr::relocate(d_agg, .data[[dt_field]])
  }

  d_agg
}
