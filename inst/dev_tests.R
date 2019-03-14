## Install packages for testing 

# Optimized version (V0.5)
remotes::install_github(repo = "AchrafElmar/suncalc", ref = "fullr_optimized", upgrade = F)
# V0.4
remotes::install_github(repo = "datastorm-open/suncalc", ref = "suncalc_js", upgrade = F)

###############################
## getSunlightTimes function ##
###############################

# one date
new <- suncalc::getSunlightTimes(date = Sys.Date(), lat = 50.1, lon = 1.83, tz = "CET")
old <- suncalcjs::getSunlightTimes(date = Sys.Date(), lat = 50.1, lon = 1.83, tz = "CET")
stopifnot(all.equal(old[, -1], new[, -1], check.attributes = F))

new <- suncalc::getSunlightTimes(date = Sys.Date(), lat = 50.1, lon = 1.83, tz = "UTC")
old <- suncalcjs::getSunlightTimes(date = Sys.Date(), lat = 50.1, lon = 1.83, tz = "UTC")
stopifnot(all.equal(old[, -1], new[, -1], check.attributes = F))


# multiple date + subset
new <- suncalc::getSunlightTimes(date = seq.Date(Sys.Date() - 9, Sys.Date(), by = 1), 
                                 keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"), 
                                 lat = 50.1, lon = 1.83, tz = "CET")
old <- suncalcjs::getSunlightTimes(date = seq.Date(Sys.Date() - 9, Sys.Date(), by = 1), 
                                 keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"), 
                                 lat = 50.1, lon = 1.83, tz = "CET")
stopifnot(all.equal(old[, -1], new[, -1], check.attributes = F))

# multiple coordinates
data <- data.frame(date = seq.Date(Sys.Date() - 9, Sys.Date(), by = 1), 
                   lat = c(rep(50.1, 10), rep(49, 10)), 
                   lon = c(rep(1.83, 10), rep(2, 10)))

new <- suncalc::getSunlightTimes(data = data, keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"), 
                                 tz = "CET")
old <- suncalcjs::getSunlightTimes(data = data, keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"), 
                                   tz = "CET")
stopifnot(all.equal(old[, -1], new[, -1], check.attributes = F))

new <- suncalc::getSunlightTimes(data = data, keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"), 
                                 tz = "UTC")
old <- suncalcjs::getSunlightTimes(data = data, keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"), 
                                   tz = "UTC")
stopifnot(all.equal(old[, -1], new[, -1], check.attributes = F))


# Perf
data <- data.frame(date = rep(seq.Date(Sys.Date() - 4999, Sys.Date(), by = 1), 2), 
                   lat = c(rep(50.1, 5000), rep(49, 5000)), 
                   lon = c(rep(1.83, 5000), rep(2, 5000)))

system.time(new <- suncalc::getSunlightTimes(data = data, keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"), 
                                             tz = "CET"))
system.time(old <- suncalcjs::getSunlightTimes(data = data, keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"), 
                                               tz = "CET"))
stopifnot(all.equal(old[, -1], new[, -1], check.attributes = F))

# grid lat lon tz
grid <- expand.grid(
  date = seq.Date(Sys.Date() - 365, Sys.Date(), by = 50),
  lat = seq(-90, 90, by = 10), 
  lon = seq(-180, 180, by = 20)
)

v_tz <-  c("UTC", "CET", "Australia/Perth", "Africa/Johannesburg", "America/Bahia", "Asia/Bahrain", "Europe/Moscow")

# a creuser ici....
tz <- "UTC"
ctrl <- lapply(v_tz, function(tz){
  print(tz)
  system.time(new <- suncalc::getSunlightTimes(data = grid, tz = tz))
  system.time(old <- suncalcjs::getSunlightTimes(data = grid, tz = tz))
  old$date <- as.Date(old$date)
  stopifnot(all.equal(old, new))
  

  # pour verifier la difference numerique
  lapply(colnames(old)[-c(1:3)], function(x){
    print(x)
    stopifnot( all(as.character(old[[x]]) == as.character(new[[x]])))
  })
  
  x <- "solarNoon"
  
  ind_diff <- which(as.character(old[[x]]) != as.character(new[[x]]))
  
  View(new[ind_diff, ])
  View(old[ind_diff, ])
  
  ind_diff
  
  old[old$lat == -70 & old$lon == -160,]
  new[new$lat == -70 & new$lon == -160,]
  
  View(new[new$date != as.Date(new$solarNoon), ])
  View(old[old$date != as.Date(old$solarNoon), ])
  head(old[as.character(old[[x]]) != as.character(new[[x]]),])
  head(new[as.character(old[[x]]) != as.character(new[[x]]),])
})


##################################
## getSunlightPosition function ##
##################################

# one date
new <- suncalc::getSunlightPosition(date = Sys.Date(), lat = 50.1, lon = 1.83)
old <- suncalcjs::getSunlightPosition(date = Sys.Date(), lat = 50.1, lon = 1.83)
stopifnot(all.equal(old, new))

# in character
new <- suncalc::getSunlightPosition(date = c("2017-05-12", "2017-05-12 00:00:00"),
                                    lat = 50.1, lon = 1.83)
old <- suncalcjs::getSunlightPosition(date = c("2017-05-12", "2017-05-12 00:00:00"),
                                      lat = 50.1, lon = 1.83)
stopifnot(all.equal(old, new))


# in POSIXct
new <- suncalc::getSunlightPosition(date = as.POSIXct("2017-05-12 00:00:00", tz = "UTC"),
                                    lat = 50.1, lon = 1.83)
old <- suncalcjs::getSunlightPosition(date = as.POSIXct("2017-05-12 00:00:00", tz = "UTC"),
                                      lat = 50.1, lon = 1.83)
stopifnot(all.equal(old, new))

new <- suncalc::getSunlightPosition(date = as.POSIXct("2017-05-12 02:00:00", tz = "CET"),
                                    lat = 50.1, lon = 1.83)
old <- suncalcjs::getSunlightPosition(date = as.POSIXct("2017-05-12 02:00:00", tz = "CET"),
                                      lat = 50.1, lon = 1.83)
stopifnot(all.equal(old, new))

# multiple date + subset
new <- suncalc::getSunlightPosition(date = seq.Date(Sys.Date() - 9, Sys.Date(), by = 1), 
                                    keep = c("altitude"), 
                                    lat = 50.1, lon = 1.83)
old <- suncalcjs::getSunlightPosition(date = seq.Date(Sys.Date() - 9, Sys.Date(), by = 1), 
                                      keep = c("altitude"), 
                                      lat = 50.1, lon = 1.83)
stopifnot(all.equal(old, new))

# multiple coordinates
data <- data.frame(date = seq.Date(Sys.Date() - 9, Sys.Date(), by = 1), 
                   lat = c(rep(50.1, 10), rep(49, 10)), 
                   lon = c(rep(1.83, 10), rep(2, 10)))
new <- suncalc::getSunlightPosition(data = data, 
                                    keep = c("altitude", "azimuth"))
old <- suncalcjs::getSunlightPosition(data = data, 
                                      keep = c("altitude", "azimuth"))
stopifnot(all.equal(old, new))

# Perf
data <- data.frame(date = rep(seq.Date(Sys.Date() - 4999, Sys.Date(), by = 1), 2), 
                   lat = c(rep(50.1, 5000), rep(49, 5000)), 
                   lon = c(rep(1.83, 5000), rep(2, 5000)))
system.time(new <- suncalc::getSunlightPosition(data = data, 
                                                keep = c("altitude", "azimuth")))
system.time(old <- suncalcjs::getSunlightPosition(data = data, 
                                                  keep = c("altitude", "azimuth")))
stopifnot(all.equal(old, new))

# grid lat lon
grid <- expand.grid(
  date = seq.Date(Sys.Date() - 365, Sys.Date(), by = 10),
  lat = seq(-90, 90, by = 10), 
  lon = seq(-180, 180, by = 20)
)

system.time(new <- suncalc::getSunlightPosition(data = grid))
system.time(old <- suncalcjs::getSunlightPosition(data = grid))
stopifnot(all.equal(old, new))

###########################
## getMoonTimes function ##
###########################

# one date
new <- suncalc::getMoonTimes(date = Sys.Date() - 1, lat = 47.21, lon = -1.557, tz = "CET")
old <- suncalcjs::getMoonTimes(date = Sys.Date() - 1, lat = 47.21, lon = -1.557, tz = "CET")
stopifnot(all.equal(old[, -1], new[, -1]))

# one date
new <- suncalc::getMoonTimes(date = Sys.Date() - 1, lat = 47.21, lon = -1.557, tz = "UTC")
old <- suncalcjs::getMoonTimes(date = Sys.Date() - 1, lat = 47.21, lon = -1.557, tz = "UTC")
stopifnot(all.equal(old[, -1], new[, -1]))

# suncalcjs : set = NA for 2019-03-11 12:00:00
# new <- suncalc::getMoonTimes(date = Sys.Date() , lat = 47.21, lon = -1.557, tz = "CET")
# old <- suncalcjs::getMoonTimes(date = Sys.Date() , lat = 47.21, lon = -1.557, tz = "CET")
# stopifnot(all.equal(old[, -1], new[, -1]))

# multiple date + subset
new <- suncalc::getMoonTimes(date = seq.Date(Sys.Date() - 9, Sys.Date(), by = 1), 
                             keep = c("rise", "set", "alwaysUp"), 
                             lat = 47.21, lon = -1.557, tz = "CET")
old <- suncalcjs::getMoonTimes(date = seq.Date(Sys.Date() - 9, Sys.Date(), by = 1), 
                               keep = c("rise", "set", "alwaysUp"), 
                               lat = 47.21, lon = -1.557, tz = "CET")
stopifnot(all.equal(old[, -1], new[, -1]))

new <- suncalc::getMoonTimes(date = seq.Date(Sys.Date() - 9, Sys.Date() - 1, by = 1), 
                             keep = c("rise", "set", "alwaysUp"), 
                             lat = 47.21, lon = -1.557, tz = "UTC")
old <- suncalcjs::getMoonTimes(date = seq.Date(Sys.Date() - 9, Sys.Date() - 1, by = 1), 
                               keep = c("rise", "set", "alwaysUp"), 
                               lat = 47.21, lon = -1.557, tz = "UTC")
stopifnot(all.equal(old[, -1], new[, -1]))


# multiple coordinates
data <- data.frame(date = seq.Date(Sys.Date() - 10, Sys.Date() - 1, by = 1), 
                   lat = c(rep(50.1, 10), rep(49, 10)), 
                   lon = c(rep(1.83, 10), rep(2, 10)))

new <- suncalc::getMoonTimes(data = data, tz = "CET")
old <- suncalcjs::getMoonTimes(data = data, tz = "CET")
stopifnot(all.equal(old[, -1], new[, -1]))

# Q set as NA ?

# Perf
data <- data.frame(date = rep(Sys.Date() , 20000), 
                   lat = c(rep(50.1, 10000), rep(49, 10000)), 
                   lon = c(rep(1.83, 10000), rep(2, 10000)))

system.time(new <- suncalc::getMoonTimes(data = data, tz = "UTC"))
system.time(old <- suncalcjs::getMoonTimes(data = data, tz = "UTC"))
stopifnot(all.equal(old[, -1], new[, -1]))


# grid lat lon tz
grid <- expand.grid(
  date = seq.Date(Sys.Date() - 365, Sys.Date(), by = 50),
  lat = seq(-90, 90, by = 10), 
  lon = seq(-180, 180, by = 20)
)

v_tz <-  c("UTC", "CET", "Australia/Perth", "Africa/Johannesburg", "America/Bahia", "Asia/Bahrain", "Europe/Moscow")

tz <- "UTC"

ctrl <- lapply(v_tz, function(tz){
  print(tz)
  system.time(new <- suncalc::getMoonTimes(data = grid, tz = tz))
  system.time(old <- suncalcjs::getMoonTimes(data = grid, tz = tz))
  old$date <- as.Date(old$date)
  stopifnot(all.equal(old, new))
  
  # pour verifier la difference numerique
  lapply(colnames(old)[-c(1:3)], function(x){
    stopifnot( all(as.character(old[[x]]) == as.character(new[[x]])))
  })
  
  x <- "rise"
  x <- "set"
  ind_diff <- which(as.character(old[[x]]) != as.character(new[[x]]))
  
  View(new[ind_diff, ])
  View(old[ind_diff, ])
  
  View(new[new$date != as.Date(new$rise), ])
  View(old[old$date != as.Date(old$rise), ])
  head(old[as.character(old[[x]]) != as.character(new[[x]]),])
  head(new[as.character(old[[x]]) != as.character(new[[x]]),])
})
##############################
## getMoonPosition function ##
##############################


# one date
new <- suncalc::getMoonPosition(date = Sys.Date(), lat = 50.1, lon = 1.83)
old <- suncalcjs::getMoonPosition(date = Sys.Date(), lat = 50.1, lon = 1.83)
stopifnot(all.equal(old, new))

# in character
new <- suncalc::getMoonPosition(date = c("2017-05-12", "2017-05-12 00:00:00"), 
                                lat = 50.1, lon = 1.83)
old <- suncalcjs::getMoonPosition(date = c("2017-05-12", "2017-05-12 00:00:00"), 
                                  lat = 50.1, lon = 1.83)
stopifnot(all.equal(old, new))


# in POSIXct
new <- suncalc::getMoonPosition(date = as.POSIXct("2017-05-12 00:00:00", tz = "UTC"),
                                lat = 50.1, lon = 1.83)
old <- suncalcjs::getMoonPosition(date = as.POSIXct("2017-05-12 00:00:00", tz = "UTC"),
                                  lat = 50.1, lon = 1.83)
stopifnot(all.equal(old, new))

new <- suncalc::getMoonPosition(date = as.POSIXct("2017-05-12 02:00:00", tz = "CET"),
                                lat = 50.1, lon = 1.83)
old <- suncalcjs::getMoonPosition(date = as.POSIXct("2017-05-12 02:00:00", tz = "CET"),
                                  lat = 50.1, lon = 1.83)
stopifnot(all.equal(old, new))

# multiple date + subset
new <- suncalc::getMoonPosition(date = seq.Date(Sys.Date() - 9, Sys.Date(), by = 1), 
                                keep = c("altitude", "azimuth"), 
                                lat = 50.1, lon = 1.83)
old <- suncalcjs::getMoonPosition(date = seq.Date(Sys.Date() - 9, Sys.Date(), by = 1), 
                                  keep = c("altitude", "azimuth"), 
                                  lat = 50.1, lon = 1.83)
stopifnot(all.equal(old, new))

# multiple coordinates
data <- data.frame(date = seq.Date(Sys.Date() - 9, Sys.Date(), by = 1), 
                   lat = c(rep(50.1, 10), rep(49, 10)), 
                   lon = c(rep(1.83, 10), rep(2, 10)))


new <- suncalc::getMoonPosition(data = data, keep = c("altitude", "azimuth"))
old <- suncalcjs::getMoonPosition(data = data, keep = c("altitude", "azimuth"))
stopifnot(all.equal(old, new))

# Perf
data <- data.frame(date = rep(Sys.Date(), 10000), 
                   lat = c(rep(50.1, 5000), rep(49, 5000)), 
                   lon = c(rep(1.83, 5000), rep(2, 5000)))

system.time(new <- suncalc::getMoonPosition(data = data, keep = c("altitude", "azimuth")))
system.time(old <- suncalcjs::getMoonPosition(data = data, keep = c("altitude", "azimuth")))
stopifnot(all.equal(old, new))


grid <- expand.grid(
  date = seq.Date(Sys.Date() - 365, Sys.Date(), by = 20),
  lat = seq(-90, 90, by = 10), 
  lon = seq(-180, 180, by = 20)
)

system.time(new <-  suncalc::getMoonPosition(data = grid))
system.time(old <- suncalcjs::getMoonPosition(data = grid))
stopifnot(all.equal(old, new))

##################################
## getMoonIllumination function ##
##################################


# one date
new <- suncalc::getMoonIllumination(date = Sys.Date())
old <- suncalcjs::getMoonIllumination(date = Sys.Date())
stopifnot(all.equal(old, new))

# in character
new <- suncalc::getMoonIllumination(date = c("2017-05-12", "2017-05-12 00:00:00"), 
                                    keep = c("fraction", "phase"))
old <- suncalcjs::getMoonIllumination(date = c("2017-05-12", "2017-05-12 00:00:00"), 
                                      keep = c("fraction", "phase"))
stopifnot(all.equal(old, new))

# in POSIXct
new <- suncalc::getMoonIllumination(date = as.POSIXct("2017-05-12 00:00:00", tz = "UTC"))
old <- suncalcjs::getMoonIllumination(date = as.POSIXct("2017-05-12 00:00:00", tz = "UTC"))
stopifnot(all.equal(old, new))

new <- suncalc::getMoonIllumination(date = as.POSIXct("2017-05-12 02:00:00", tz = "CET"))
old <- suncalcjs::getMoonIllumination(date = as.POSIXct("2017-05-12 02:00:00", tz = "CET"))
stopifnot(all.equal(old, new))

# Perf
date <- seq(ISOdate(2009,1,1), ISOdate(2010,1,1), "hours")
date_cet <- date
attr(date_cet, "tzone") <- "CET"
system.time(new <- suncalc::getMoonIllumination(date = date_cet))
system.time(old <- suncalcjs::getMoonIllumination(date = date_cet))
stopifnot(all.equal(old, new))

date_utc <- date
attr(date_utc, "tzone") <- "UTC"

system.time(new <- suncalc::getMoonIllumination(date = date_utc))
system.time(old <- suncalcjs::getMoonIllumination(date = date_utc))
stopifnot(all.equal(old, new))
