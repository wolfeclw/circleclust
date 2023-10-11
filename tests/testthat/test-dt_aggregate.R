test_that("dt_aggregate", {
  zoo_agg <- dt_aggregate(zoo_trip, "Date_Time")
  wood_agg <- dt_aggregate(woodland, "Date_Time")
  friend_agg <- dt_aggregate(friendship, "Date_Time")

  expect_equal(nrow(zoo_agg), 2074)
  expect_equal(nrow(wood_agg), 1279)
  expect_equal(nrow(friend_agg), 637)

  zoo_min_f <- dt_aggregate(zoo_trip, "Date_Time", unit = "1 min")

  expect_equal(nrow(zoo_min_f), 174)
})
