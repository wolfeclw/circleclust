test_that("imputed time", {
  d_imputed <- zoo_trip %>%
    dplyr::mutate(ID = 'test') %>%
    impute_time("Date_Time", fill_cols = 'ID')

  imputed <- nrow(d_imputed)
  n_id <- as.numeric(table(d_imputed$ID))

  expect_equal(imputed, 10408)
  expect_equal(n_id, 10408)
})
