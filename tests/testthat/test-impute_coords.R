test_that("imputed coords", {

  d_imputed <- zoo_trip %>%
    impute_coords('Date_Time')

  d_zero <- zoo_trip %>%
    impute_coords('Date_Time', distance_threshold = 0)

  d_some <- zoo_trip %>%
    impute_coords('Date_Time', distance_threshold = 3)

  imputed <- sum(d_imputed$imputed_coord)
  imputed_zero <- sum(d_zero$imputed_coord)
  imputed_some <- sum(d_some$imputed_coord)

  expect_equal(imputed, 1191)
  expect_equal(imputed_zero, 0)
  expect_equal(imputed_some, 1182)
})
