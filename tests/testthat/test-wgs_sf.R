test_that("wgs_sf class", {
  dsf <- wgs_sf(zoo_trip)
  expect_true(sf::st_is_longlat(dsf))
})

test_that("wgs_sf lon|lat error", {
  zoo_no_lat <- zoo_trip %>% dplyr::select(-lat)
  zoo_na <- zoo_trip %>% dplyr::mutate(lat = NA, lon = NA)

  expect_error(wgs_sf(zoo_no_lat))
  expect_error(wgs_sf(zoo_na))
})
