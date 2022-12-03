[![CRAN Status Badge](http://www.r-pkg.org/badges/version/suncalc)](https://cran.r-project.org/package=suncalc) 
[![CRAN Downloads Badge](https://cranlogs.r-pkg.org/badges/suncalc)](https://cran.r-project.org/package=suncalc)

# suncalc

### R implementation for calculating sun position, sunlight phases (times for sunrise, sunset, dusk, etc.), moon position and lunar phase for the given location and time. Most calculations are based on the formulas given in Astronomy Answers articles about position of the sun and the planets : <https://www.aa.quae.nl/en/reken/zonpositie.html>

# News

## ``0.5.0`` dev version

  * fully recode in R rather than call suncalc.js, so up to 500x faster...! 

# Installation

```` 
# from cran
install.packages("suncalc")
devtools::install_github("tuberculo/suncalc") for version from this repo.
````

# Use

````
require(suncalc)
?getSunlightTimes
getSunlightTimes(date = Sys.Date(), lat = 50.1, lon = 1.83, tz = "CET")
````

# About suncalc

Most calculations are based on the formulas given in the excellent Astronomy Answers articles
about [position of the sun](https://www.aa.quae.nl/en/reken/zonpositie.html)
and [the planets](https://www.aa.quae.nl/en/reken/hemelpositie.html).
You can read about different twilight phases calculated by SunCalc
in the [Twilight article on Wikipedia](https://en.wikipedia.org/wiki/Twilight).


## Reference

### Sunlight times


Returns an object with the following properties (each is a `Date` object):

| Property        | Description                                                              |
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


### Sun position

Returns an object with the following properties:

 * `altitude`: sun altitude above the horizon in radians,
 e.g. `0` at the horizon and `PI/2` at the zenith (straight over your head)
 * `azimuth`: sun azimuth in radians (direction along the horizon, measured from south to west),
 e.g. `0` is south and `Math.PI * 3/4` is northwest


### Moon position



Returns an object with the following properties:

 * `altitude`: moon altitude above the horizon in radians
 * `azimuth`: moon azimuth in radians
 * `distance`: distance to moon in kilometers
 * `parallacticAngle`: parallactic angle of the moon in radians


### Moon illumination

Returns an object with the following properties:

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

By subtracting the `parallacticAngle` from the `angle` one can get the zenith angle of the moons bright limb (anticlockwise).
The zenith angle can be used do draw the moon shape from the observers perspective (e.g. moon lying on its back).

### Moon rise and set times

Returns an object with the following properties:

 * `rise`: moonrise time as `Date`
 * `set`: moonset time as `Date`
 * `alwaysUp`: `true` if the moon never rises/sets and is always _above_ the horizon during the day
 * `alwaysDown`: `true` if the moon is always _below_ the horizon

By default, it will search for moon rise and set during local user's day (frou 0 to 24 hours).
If `inUTC` is set to true, it will instead search the specified date from 0 to 24 UTC hours.
