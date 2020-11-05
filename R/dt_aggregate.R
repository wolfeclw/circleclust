
#' Aggregate PUFP data
#'
#' `dt_aggregate()` aggregates numeric values to a specified time unit. Columns with
#' unique character values are retained (i.e. patient ID, sensor name).
#'
#' @param df a data frame with a datetime field.
#' @param dt_field character; name of datetime field.
#' @param unit character; string specifying a time unit or a multiple of a unit to be rounded
#' @param floor_or_celiling character; either 'floor'(\code{\link[lubridate]{floor_date}})
#' or 'ceiling'(\code{\link[lubridate]{ceiling_date}}). Default = 'floor.'
#' @param summary_fun character; summary function (i.e. 'mean', 'median'). Default = 'median.'
#' @return a tibble
#' @export
#'
#' @examples
#' \dontrun{
#'
#' dt_aggregate(df,
#'   unit = "5 seconds", floor_or_ceiling = "floor",
#'   summary_fun = "median"
#' )
#' }
#' @importFrom stats median
#' @importFrom purrr map map_chr map_lgl
dt_aggregate <- function(df, dt_field = NULL, unit = "5 seconds", floor_or_celiling = "floor",
                          summary_fun = "median") {

  if (is.null(dt_field)) {
    stop('`dt_field` has not been assigned a value.', call. = FALSE)
  } else if (!lubridate::is.POSIXct(df[[dt_field]])) {
    stop('`dt_field` is not a datetiime column.', call. = FALSE)
  }

  if (floor_or_celiling == "floor") {
    d_agg <- df %>%
      mutate(agg_dt = lubridate::floor_date(.data[[dt_field]], unit = unit)) %>%
      group_by(agg_dt) %>%
      summarise_if(is.numeric, summary_fun, na.rm = TRUE) %>%
      relocate(agg_dt)
  } else {
    d_agg <- df %>%
      mutate(agg_dt = lubridate::floor_date(.data[[dt_field]], unit = unit)) %>%
      group_by(agg_dt) %>%
      summarise_if(is.numeric, summary_fun, na.rm = TRUE) %>%
      relocate(agg_dt)
    }

  char_cols <- select_if(df, is.character)
  char_lgl <- char_cols %>%
    map(., unique) %>%
    map_chr(., length) %>%
    map_lgl(., ~. == 1)

  char_keep <- char_cols[char_lgl][1:nrow(d_agg), ]
  rm_cols <- char_cols[!char_lgl] %>% names()

  if (length(rm_cols > 0)) {
    message(
      "Columns `", paste(rm_cols, collapse = ", "),
      "` are of class 'character' and were removed during aggregation."
    )
  }

  d_agg_char <- bind_cols(char_keep, d_agg)
  d_agg_char <- relocate(d_agg_char, .after = last_col())

  d_agg_char
}
