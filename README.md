# suncalc

### R interface to suncalc.js library, part of the SunCalc.net project <http://suncalc.net>, for calculating sun position, sunlight phases (times for sunrise, sunset, dusk, etc.), moon position and lunar phase for the given location and time.

# Installation

```` 
# need pacakge "V8"
install.packages("V8")
devtools::install_github("datastorm-open/suncalc") for developpement version
````

# Use

````
require(suncalc)
?getSunlightTimes
getSunlightTimes(date = Sys.Date(), lat = 50.1, lon = 1.83, tz = "CET")
````
