
test_that("correct clusters", {
  d <- woodland %>%
    impute_coords("Date_Time") %>%
    dt_aggregate("Date_Time") %>%
    move("Date_Time") %>%
    circleclust("Date_Time", show_circvar = TRUE, pl_dist_threshold = 25)

  d$mobile <- ifelse(d$activity_status == "mobile", 1, 0)

  clusters <- max(d$cluster_grp, na.rm = TRUE)

  expect_equal(clusters, 8)
  expect_equal(sum(d$mobile), 814)
})
