vars <- c("solarNoon", "nadir", "sunrise", "sunset", "sunriseEnd", "sunsetStart",
          "dawn", "dusk", "nauticalDawn", "nauticalDusk", "nightEnd", "night",
          "goldenHourEnd", "goldenHour")

expect_in_day <- function(res, tz) {
  for (v in intersect(vars, colnames(res))) {
    expect_true(all(is.na(res[[v]]) | as.Date(res[[v]], tz = tz) == res$date), info = paste(tz, v))
  }
}

test_that("getSunlightTimes returns times of the requested day (#2)", {
  skip_if_tz_unavailable(c("America/Los_Angeles", "Australia/Brisbane", "America/Santiago", "Asia/Tokyo",
                           "Pacific/Kiritimati", "Etc/GMT+12"))

  res <- getSunlightTimes(date = as.Date("2018-11-01"), lat = 34.052235, lon = -118.243683,
                          tz = "America/Los_Angeles", keep = c("sunrise", "sunset"))
  expect_equal(as.Date(res$sunrise, tz = "America/Los_Angeles"), as.Date("2018-11-01"))
  expect_equal(as.Date(res$sunset, tz = "America/Los_Angeles"), as.Date("2018-11-01"))

  res <- getSunlightTimes(date = as.Date("2015-12-17"), lat = -27.53103, lon = 152.8666,
                          tz = "Australia/Brisbane", keep = "solarNoon")
  expect_equal(as.Date(res$solarNoon, tz = "Australia/Brisbane"), as.Date("2015-12-17"))

  # events around midnight, DST at midnight (Santiago), tz far from solar time (Kiritimati, Etc/GMT+12)
  dates <- seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = 2)
  for (tz in c("America/Los_Angeles", "America/Santiago", "Asia/Tokyo", "Pacific/Kiritimati", "Etc/GMT+12")) {
    for (lon in c(-180, -157, -90, 0, 90, 180)) {
      res <- getSunlightTimes(date = dates, lat = 40, lon = lon, tz = tz)
      expect_in_day(res, tz)
    }
  }
})

test_that("getSunlightTimes finds the event of the day when tz is far from the solar time", {
  skip_if_tz_unavailable("Etc/GMT+12")

  # UTC-12 at lon 180 : solar time is 24h ahead, nadir is computed 2 days later
  res <- getSunlightTimes(date = seq(as.Date("2025-04-20"), as.Date("2025-04-29"), by = 1),
                          lat = 0, lon = 180, tz = "Etc/GMT+12", keep = c("nadir", "solarNoon"))
  expect_false(anyNA(res$solarNoon))
  expect_false(anyNA(res$nadir))
  expect_in_day(res, "Etc/GMT+12")
})

test_that("getSunlightTimes doesn't depend on the system time zone", {
  skip_if_tz_unavailable(c("America/Los_Angeles", "Asia/Seoul", "Asia/Jakarta"))

  old_tz <- Sys.getenv("TZ", unset = NA)
  on.exit(if (is.na(old_tz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = old_tz))

  data <- data.frame(date = as.Date("2023-06-01"), lat = 0, lon = seq(-150, 150, by = 25))
  Sys.setenv(TZ = "America/Los_Angeles")
  res_la <- getSunlightTimes(data = data, tz = "Asia/Jakarta")
  Sys.setenv(TZ = "Asia/Seoul")
  res_seoul <- getSunlightTimes(data = data, tz = "Asia/Jakarta")

  expect_equal(res_la, res_seoul)
  expect_in_day(res_la, "Asia/Jakarta")
})

test_that("getSunlightTimes returns NA when the sun doesn't reach the altitude (#13)", {
  skip_if_tz_unavailable(c("America/Edmonton", "Europe/Berlin"))

  # Edmonton : sun only 15.75 degrees below the horizon at nadir, no astronomical night
  res <- getSunlightTimes(date = as.Date("2018-05-24"), lat = 53.53043, lon = -113.51,
                          keep = c("dusk", "night", "nightEnd", "dawn", "nadir"), tz = "America/Edmonton")
  expect_true(is.na(res$night))
  expect_true(is.na(res$nightEnd))
  expect_false(is.na(res$dusk))
  expect_false(is.na(res$dawn))
  alt <- getSunlightPosition(date = res$nadir, lat = 53.53043, lon = -113.51)$altitude * 180 / pi
  expect_gt(alt, -18)

  # polar day / polar night
  res <- getSunlightTimes(date = as.Date(c("2026-06-21", "2026-12-21")), lat = 78.2, lon = 15.6,
                          tz = "Europe/Berlin", keep = c("sunrise", "sunset", "solarNoon"))
  expect_true(all(is.na(res$sunrise)))
  expect_true(all(is.na(res$sunset)))
  expect_false(anyNA(res$solarNoon))
})

test_that("getSunlightTimes keeps the types of the output", {
  skip_if_tz_unavailable("Europe/Paris")

  res <- getSunlightTimes(date = as.Date("2026-06-21"), lat = 78.2, lon = 15.6, tz = "Europe/Paris")
  expect_s3_class(res$date, "Date")
  for (v in vars) {
    expect_s3_class(res[[v]], "POSIXct")
    expect_equal(attr(res[[v]], "tzone"), "Europe/Paris")
  }
})
