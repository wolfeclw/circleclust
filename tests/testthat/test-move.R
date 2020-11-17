test_that("move datetime error", {

  expect_error(move())
})

test_that("move speed bearing", {

  mfun <- function(df) {
    df %>%
      impute_coords('Date_Time') %>%
      dt_aggregate('Date_Time') %>%
      move('Date_Time')
  }

  mzoo <- mfun(zoo_trip)

  expect_equal(median(mzoo$speed_ms, na.rm = TRUE), 0.4)

})


