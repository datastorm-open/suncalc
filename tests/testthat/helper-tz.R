# time zones can be missing on some systems (e.g. legacy names like "CET" or "US/Pacific"
# on Debian without tzdata-legacy), or listed by OlsonNames() but not usable by
# lubridate / timechange (seen with "America/Edmonton" on R-devel Windows) :
# skip the test rather than fail
tz_usable <- function(tz) {
  if (!tz %in% OlsonNames()) return(FALSE)
  x <- tryCatch(
    lubridate::force_tz(as.POSIXct("2020-01-01", tz = "UTC"), tz, roll_dst = "boundary"),
    error = function(e) NA
  )
  !is.na(x)
}

skip_if_tz_unavailable <- function(tz) {
  missing_tz <- tz[!vapply(tz, tz_usable, logical(1))]
  testthat::skip_if(length(missing_tz) > 0,
                    paste("time zone not available on this system:", paste(missing_tz, collapse = ", ")))
}
