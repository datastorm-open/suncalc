# time zones can be missing on some systems (e.g. legacy names like "CET" or "US/Pacific"
# on Debian without tzdata-legacy) : skip the test rather than fail
skip_if_tz_unavailable <- function(tz) {
  missing_tz <- setdiff(tz, OlsonNames())
  testthat::skip_if(length(missing_tz) > 0,
                    paste("time zone not available on this system:", paste(missing_tz, collapse = ", ")))
}
