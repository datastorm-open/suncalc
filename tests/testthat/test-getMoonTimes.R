# Nantes (Europe/Paris) : on these dates the J-1 / J+1 correction sets a NA, which used to turn
# rise / set into logical columns, then failed (CRAN check error on 2026-10-20).
# On 2026-11-02 the moon doesn't rise.

test_that("getMoonTimes keeps POSIXct columns for a single date without rise or set", {
  skip_if_tz_unavailable("Europe/Paris")

  for (d in c("2026-10-20", "2026-11-02")) {
    res <- expect_silent(getMoonTimes(date = as.Date(d), lat = 47.21, lon = -1.557, tz = "Europe/Paris"))
    expect_s3_class(res$rise, "POSIXct")
    expect_s3_class(res$set, "POSIXct")
    expect_equal(attr(res$rise, "tzone"), "Europe/Paris")
    expect_equal(attr(res$set, "tzone"), "Europe/Paris")
  }

  res <- getMoonTimes(date = as.Date("2026-11-02"), lat = 47.21, lon = -1.557, tz = "Europe/Paris")
  expect_true(is.na(res$rise))
  expect_false(is.na(res$set))
})

test_that("getMoonTimes returns times within the requested day", {
  skip_if_tz_unavailable("Europe/Paris")

  dates <- seq(as.Date("2026-10-01"), as.Date("2026-11-30"), by = 1)
  res <- getMoonTimes(date = dates, lat = 47.21, lon = -1.557, tz = "Europe/Paris")

  expect_true(all(is.na(res$rise) | as.Date(res$rise, tz = "Europe/Paris") == res$date))
  expect_true(all(is.na(res$set) | as.Date(res$set, tz = "Europe/Paris") == res$date))
})

test_that("getMoonTimes gives the same result for single and multiple dates", {
  skip_if_tz_unavailable("Europe/Paris")

  dates <- seq(as.Date("2026-10-15"), as.Date("2026-11-05"), by = 1)
  res_multi <- getMoonTimes(date = dates, lat = 47.21, lon = -1.557, tz = "Europe/Paris")
  res_single <- do.call(rbind, lapply(as.list(dates), function(d) {
    getMoonTimes(date = d, lat = 47.21, lon = -1.557, tz = "Europe/Paris")
  }))

  expect_equal(res_single, res_multi)
})
