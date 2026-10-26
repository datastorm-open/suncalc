# suncalc 0.5.4 (2026-10-26)

* Fix getMoonTimes error (and logical rise/set columns) for a single date when the moon does not rise or set that day
* getSunlightTimes & getMoonTimes : times are always within the requested day in `tz`, NA if the event doesn't occur this day. Fix remaining cases around midnight, on DST changes at midnight and when `tz` is far from the solar time (#2)
* getSunlightTimes & getMoonTimes : much faster (about x20 on large data)
* Document NA values in getSunlightTimes (sun not reaching the altitude, e.g. no `night` in summer at high latitudes) (#13)
* Add unit tests (testthat)
* lubridate >= 1.9.0 required

# suncalc 0.5.3 (2026-07-15)

* Fix CRAN note

# suncalc 0.5.2 (2023-11-02)

* Fix getMoonTimes not keeping "rise" and "set"
* Dates with times before a certain time return times for previous/newt day (#2)

# suncalc 0.5.1 (2022-09-28)

* Update maintener

# suncalc 0.5 (2019-04-02)

## Major changes 
* Full R implementation of all functions

## Bugfixes
* getSunlightTimes & getMoonTimes always return Date (rather than Date + 12:00:00...)

# suncalc 0.4 (2018-02-22)

## Bugfixes
* Dates with times before a certain time return times for previous day. See https://github.com/mourner/suncalc/issues/11

# suncalc 0.3 (2017-10-04)

## Bugfixes
* Dates with times before a certain time return times for previous day. See https://github.com/mourner/suncalc/issues/11

# suncalc 0.2 (2017-09-08)

## Bugfixes
* Keeping only one variable in all functions
