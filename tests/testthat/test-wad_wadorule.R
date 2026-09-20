test_that("wad_wadorule returns the required output columns", {
  dates <- seq.Date(as.Date("2020-01-01"), by = "day", length.out = 20)

  inflows <- data.frame(
    Date = dates,
    inflow = seq(20, 39)
  )

  targets <- data.frame(
    Date = dates,
    Storage = rep(500, 20),
    Elevation = rep(100, 20)
  )

  result <- wad_wadorule(
    inflows = inflows,
    targets = targets,
    Smin = 100,
    Smax = 1000,
    Rmin = 5,
    Rmax = 100
  )

  expect_named(
    result,
    c("Date", "Storage_Target", "Elevation_Target", "Release_Target")
  )
  expect_equal(nrow(result), 20)
  expect_true(all(result$Release_Target >= 5))
  expect_true(all(result$Release_Target <= 100))
})

test_that("wad_wadorule requires the expected input columns", {
  dates <- seq.Date(as.Date("2020-01-01"), by = "day", length.out = 3)

  expect_error(
    wad_wadorule(
      inflows = data.frame(Date = dates, Q = 10),
      targets = data.frame(Date = dates, Storage = 500, Elevation = 100),
      Smin = 100, Smax = 1000, Rmin = 5, Rmax = 100
    ),
    "Missing inflow columns"
  )

  expect_error(
    wad_wadorule(
      inflows = data.frame(Date = dates, inflow = 10),
      targets = data.frame(Date = dates, Storage = 500),
      Smin = 100, Smax = 1000, Rmin = 5, Rmax = 100
    ),
    "Missing target columns"
  )
})
