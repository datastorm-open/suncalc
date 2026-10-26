[![CRAN Status Badge](https://www.r-pkg.org/badges/version/suncalc)](https://cran.r-project.org/package=suncalc)
[![CRAN Downloads Badge](https://cranlogs.r-pkg.org/badges/suncalc)](https://cran.r-project.org/package=suncalc)

# suncalc

R package for calculating sun position, sunlight phases (times for sunrise, sunset, dusk, etc.), moon position, moon rise and set times, and lunar phase for the given location and time.

It is a full R implementation of the JavaScript library [SunCalc](https://github.com/mourner/suncalc), vectorized to work on large data. Most calculations are based on the formulas given in the Astronomy Answers articles about [position of the sun](https://www.aa.quae.nl/en/reken/zonpositie.html) and [the planets](https://www.aa.quae.nl/en/reken/hemelpositie.html).

## Installation

```r
# from CRAN
install.packages("suncalc")

# development version from GitHub
# install.packages("remotes")
remotes::install_github("datastorm-open/suncalc")
```

## Usage

| Function                | Computes                                                    | `date`                          |
| ----------------------- | ----------------------------------------------------------- | ------------------------------- |
| `getSunlightTimes()`    | sunlight phases (sunrise, sunset, dusk, night, ...)         | `Date`                          |
| `getSunlightPosition()` | sun altitude and azimuth                                    | `Date`, `POSIXct` or `character` |
| `getMoonTimes()`        | moon rise and set times                                     | `Date`                          |
| `getMoonPosition()`     | moon altitude, azimuth, distance and parallactic angle      | `Date`, `POSIXct` or `character` |
| `getMoonIllumination()` | moon illuminated fraction, phase and angle                  | `Date`, `POSIXct` or `character` |

All functions return a `data.frame`, and share the same arguments:

* `date`, `lat`, `lon`: one or several dates, for a single location,
* or `data`: a `data.frame` with columns `date`, `lat` and `lon`, for several locations,
* `keep`: the variables to return (all by default),
* `tz` (`getSunlightTimes()` and `getMoonTimes()`): time zone of the results (`"UTC"` by default).

```r
library(suncalc)

getSunlightTimes(date = as.Date("2026-06-21"), lat = 48.86, lon = 2.35, tz = "Europe/Paris",
                 keep = c("sunrise", "solarNoon", "sunset", "night"))
#         date   lat  lon             sunrise           solarNoon              sunset night
# 1 2026-06-21 48.86 2.35 2026-06-21 05:48:09 2026-06-21 13:53:35 2026-06-21 21:59:01  <NA>

# several locations
data <- data.frame(date = as.Date(c("2026-06-21", "2026-12-21")),
                   lat = c(48.86, 69.65),
                   lon = c(2.35, 18.96))
getSunlightTimes(data = data, keep = c("sunrise", "sunset", "night"), tz = "Europe/Paris")
#         date   lat   lon             sunrise              sunset               night
# 1 2026-06-21 48.86  2.35 2026-06-21 05:48:09 2026-06-21 21:59:01                <NA>
# 2 2026-12-21 69.65 18.96                <NA>                <NA> 2026-12-21 16:57:10

getMoonTimes(date = as.Date("2026-06-21"), lat = 48.86, lon = 2.35, tz = "Europe/Paris")
#         date   lat  lon                rise                 set alwaysUp alwaysDown
# 1 2026-06-21 48.86 2.35 2026-06-21 13:20:51 2026-06-21 01:28:57    FALSE      FALSE

getSunlightPosition(date = as.POSIXct("2026-06-21 12:00:00", tz = "Europe/Paris"), lat = 48.86, lon = 2.35)
#                  date   lat  lon  altitude    azimuth
# 1 2026-06-21 12:00:00 48.86 2.35 0.9823635 -0.8941455

getMoonIllumination(date = as.Date(c("2026-06-21", "2026-06-29")))
#         date  fraction     phase     angle
# 1 2026-06-21 0.3982594 0.2173872 -1.168386
# 2 2026-06-29 0.9894033 0.4671750 -1.940759
```

### Times and `NA` values

`getSunlightTimes()` and `getMoonTimes()` return, for each requested day, the times of the events occurring that day in `tz` (from 00:00 to 24:00), whatever the system time zone.

A time is `NA` when the event doesn't occur that day:

* the sun doesn't reach the corresponding altitude: no `night` / `nightEnd` in summer at high latitudes (the sun never goes 18° below the horizon, as in Paris on June 21 above), no `sunrise` / `sunset` during polar day or polar night (Tromsø on December 21 above),
* the moon doesn't rise or set that day (about once a month, as moonrise and moonset are about 50 minutes later each day).

## Reference

You can read about the different twilight phases in the [Twilight article on Wikipedia](https://en.wikipedia.org/wiki/Twilight).

### Sunlight times: `getSunlightTimes()`

Each variable is a `POSIXct` in `tz`:

| Variable        | Description                                                              |
| --------------- | ------------------------------------------------------------------------ |
| `sunrise`       | sunrise (top edge of the sun appears on the horizon)                     |
| `sunriseEnd`    | sunrise ends (bottom edge of the sun touches the horizon)                |
| `goldenHourEnd` | morning golden hour (soft light, best time for photography) ends         |
| `solarNoon`     | solar noon (sun is in the highest position)                              |
| `goldenHour`    | evening golden hour starts                                               |
| `sunsetStart`   | sunset starts (bottom edge of the sun touches the horizon)               |
| `sunset`        | sunset (sun disappears below the horizon, evening civil twilight starts) |
| `dusk`          | dusk (evening nautical twilight starts)                                  |
| `nauticalDusk`  | nautical dusk (evening astronomical twilight starts)                     |
| `night`         | night starts (dark enough for astronomical observations)                 |
| `nadir`         | nadir (darkest moment of the night, sun is in the lowest position)       |
| `nightEnd`      | night ends (morning astronomical twilight starts)                        |
| `nauticalDawn`  | nautical dawn (morning nautical twilight starts)                         |
| `dawn`          | dawn (morning nautical twilight ends, morning civil twilight starts)     |

### Sun position: `getSunlightPosition()`

* `altitude`: sun altitude above the horizon in radians, e.g. `0` at the horizon and `pi/2` at the zenith (straight over your head)
* `azimuth`: sun azimuth in radians (direction along the horizon, measured from south to west), e.g. `0` is south and `pi * 3/4` is northwest

### Moon rise and set times: `getMoonTimes()`

* `rise`: moonrise time (`POSIXct` in `tz`)
* `set`: moonset time (`POSIXct` in `tz`)
* `alwaysUp`: `TRUE` if the moon never rises / sets and is always _above_ the horizon during the day
* `alwaysDown`: `TRUE` if the moon is always _below_ the horizon

### Moon position: `getMoonPosition()`

* `altitude`: moon altitude above the horizon in radians
* `azimuth`: moon azimuth in radians
* `distance`: distance to moon in kilometers
* `parallacticAngle`: parallactic angle of the moon in radians

### Moon illumination: `getMoonIllumination()`

* `fraction`: illuminated fraction of the moon; varies from `0.0` (new moon) to `1.0` (full moon)
* `phase`: moon phase; varies from `0.0` to `1.0`, described below
* `angle`: midpoint angle in radians of the illuminated limb of the moon reckoned eastward from the north point of the disk;
 the moon is waxing if the angle is negative, and waning if positive

Moon phase value should be interpreted like this:

| Phase | Name            |
| -----:| --------------- |
| 0     | New Moon        |
|       | Waxing Crescent |
| 0.25  | First Quarter   |
|       | Waxing Gibbous  |
| 0.5   | Full Moon       |
|       | Waning Gibbous  |
| 0.75  | Last Quarter    |
|       | Waning Crescent |
| 1     | New Moon        |

By subtracting the `parallacticAngle` from the `angle` one can get the zenith angle of the moon's bright limb (anticlockwise).
The zenith angle can be used to draw the moon shape from the observer's perspective (e.g. moon lying on its back).

## News

See [NEWS.md](NEWS.md) for the changes of each version.
