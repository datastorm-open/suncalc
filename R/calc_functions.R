

.toJulian <- function(date) {
  dayS <- 60 * 60 * 24
  J1970 <- 2440588
  nb_ms_from_J1970 <- as.numeric(as.POSIXct(date, tz = 'UTC'))
  return((nb_ms_from_J1970 / dayS) - 0.5 + J1970)
}

.fromJulian <- function(j) {
  dayS <- 60 * 60 * 24
  J1970 <- 2440588
  date <- as.POSIXct((j + 0.5 - J1970) * dayS , as.POSIXct('1970-01-01', tz = 'UTC'), tz ='UTC') 
  return(date)
}

.toDays <- function(date) {
  J2000 <- 2451545
  return(.toJulian(date) - J2000)
}


.rightAscension <- function(l, b) {
  e <- (pi / 180) * 23.4397
  return(atan2(sin(l) * cos(e) - tan(b) * sin(e), cos(l)))
}


.declination <- function(l, b) {
  e <- (pi / 180) * 23.4397
  return(asin(sin(b) * cos(e) + cos(b) * sin(e) * sin(l)))
}


.azimuth <- function(hm, phi, dec) {
  return(atan2(sin(hm), cos(hm) * sin(phi) - tan(dec) * cos(phi)))
}


.altitude <- function(hm, phi, dec) {
  return(asin(sin(phi) * sin(dec) + cos(phi) * cos(dec) * cos(hm)))
}


.siderealTime <- function(d, lw) {
  return((pi / 180) * (280.16 + 360.9856235 * d) - lw)
}


.astroRefraction <- function(h) {
  # the following formula works for positive .altitudes only.
  # if h = -0.08901179 a div/0 would occur.
  if (h < 0)  h <- 0
  # formula 16.4 of "Astronomical Algorithms" 2nd edition by Jean meeus (Willmann-Bell, Richmond) 1998.
  # 1.02 / tan(h + 10.26 / (h + 5.10)) h in degrees, result in arc minutes -> converted to (pi / 180):
  return (0.0002967 / tan(h + 0.00312536 / (h + 0.08901179)))
}


.solarMeanAnomaly <- function(d) { 
  return ((pi / 180) * (357.5291 + 0.98560028 * d))
}


.eclipticLongitude <- function(m) {
  
  C <- (pi / 180) * (1.9148 * sin(m) + 0.02 * sin(2 * m) + 0.0003 * sin(3 * m)) # equation of center
  P <- (pi / 180) * 102.9372 # perihelion of the Earth
  
  return (m + C + P + pi)
}

.sunCoords <- function(d) {
  
  m <- .solarMeanAnomaly(d)
  l <- .eclipticLongitude(m)
  
  return(list(dec = .declination(l, 0),
              ra = .rightAscension(l, 0)))
  
}


# calculates sun position for a given date and latitude/longitude
.getPosition <- function(date, lat, lng) {
  
  lw  <- (pi / 180) * -lng
  phi <- (pi / 180) * lat
  d <- .toDays(date)
  
  c  <- .sunCoords(d)
  hm  <- .siderealTime(d, lw) - c$ra
  
  return(list(.azimuth = .azimuth(hm, phi, c$dec),
              .altitude = .altitude(hm, phi, c$dec)))
}


# calculations for sun times
.julianCycle <- function(d, lw) {
  return(round(d - 0.0009 - lw / (2 * pi)))
}

.approxTransit <- function(ht, lw, n) {
  return(0.0009 + (ht + lw) / (2 * pi) + n)
}

.solarTransitJ <- function(ds, m, l) {
  J2000 <- 2451545
  return(J2000 + ds + 0.0053 * sin(m) - 0.0069 * sin(2 * l)) 
}

.hourAngle <- function(h, phi, d) {
  return(acos((sin(h) - sin(phi) * sin(d)) / (cos(phi) * cos(d))))
}

# returns set time for the given sun .altitude
.getSetJ <- function(h, lw, phi, dec, n, m, l) {
  w <- .hourAngle(h, phi, dec)
  a <- .approxTransit(w, lw, n)
  return(.solarTransitJ(a, m, l))
}

# calculates sun times for a given date and latitude/longitude
.getTimes <- function (date, lat, lng) {
  
  rad <- (pi / 180)
  lw <- rad * -lng
  phi <- rad * lat
  
  d <- .toDays(date)
  n <- .julianCycle(d, lw)
  ds <- .approxTransit(0, lw, n)
  
  M <- .solarMeanAnomaly(ds)
  L <- .eclipticLongitude(M)
  dec <- .declination(L, 0)
  
  Jnoon <- .solarTransitJ(ds, M, L)
  
  available_var <- c("solarNoon", "nadir", "sunrise", "sunset", "sunriseEnd", "sunsetStart",  
                     "dawn", "dusk", "nauticalDawn", "nauticalDusk", "nightEnd", "night",
                     "goldenHourEnd", "goldenHour")
  
  result <- list(solarNoon = .fromJulian(Jnoon),
                 nadir = .fromJulian(Jnoon - 0.5),
                 sunrise = .fromJulian(Jnoon - (.getSetJ(-0.833 * rad, lw, phi, dec, n, M, L) - Jnoon)),
                 sunset = .fromJulian(.getSetJ(-0.833 * rad, lw, phi, dec, n, M, L)),
                 sunriseEnd = .fromJulian(Jnoon - (.getSetJ(-0.3 * rad, lw, phi, dec, n, M, L) - Jnoon)),
                 sunsetStart = .fromJulian(.getSetJ(-0.3 * rad, lw, phi, dec, n, M, L)),
                 dawn = .fromJulian(Jnoon - (.getSetJ(-6 * rad, lw, phi, dec, n, M, L) - Jnoon)),
                 dusk = .fromJulian(.getSetJ(-6 * rad, lw, phi, dec, n, M, L)),
                 nauticalDawn = .fromJulian(Jnoon - (.getSetJ(-12 * rad, lw, phi, dec, n, M, L) - Jnoon)),
                 nauticalDusk = .fromJulian(.getSetJ(-12 * rad, lw, phi, dec, n, M, L)),
                 nightEnd = .fromJulian(Jnoon - (.getSetJ(-18 * rad, lw, phi, dec, n, M, L) - Jnoon)),
                 night = .fromJulian(.getSetJ(-18 * rad, lw, phi, dec, n, M, L)),
                 goldenHourEnd = .fromJulian(Jnoon - (.getSetJ(6 * rad, lw, phi, dec, n, M, L) - Jnoon)),
                 goldenHour = .fromJulian(.getSetJ(6 * rad, lw, phi, dec, n, M, L))
                 )
  
  return(result)
}




# moon calculations, based on http:#aa.quae.nl/en/reken/hemelpositie.html formulas
.moonCoords <- function(d) { # geocentric ecliptic coordinates of the moon
  
  l <- (pi / 180) * (218.316 + 13.176396 * d) # ecliptic longitude
  m <- (pi / 180) * (134.963 + 13.064993 * d) # mean anomaly
  f <- (pi / 180) * (93.272 + 13.229350 * d)  # mean distance
  
  l  <- l + (pi / 180) * 6.289 * sin(m) # longitude
  b  <- (pi / 180) * 5.128 * sin(f)     # latitude
  dt <- 385001 - 20905 * cos(m)  # distance to the moon in km
  
  return(list(ra = .rightAscension(l, b),
              dec = .declination(l, b),
              dist = dt))
}


.getmoonPosition <- function (date, lat, lng) {
  
  lw  <- (pi / 180) * -lng
  phi <- (pi / 180) * lat
  d   <- .toDays(date)
  
  c <- .moonCoords(d)
  hm <- .siderealTime(d, lw) - c$ra
  h <- .altitude(hm, phi, c$dec)
  # formula 14.1 of "Astronomical Algorithms" 2nd edition by Jean meeus (Willmann-Bell, Richmond) 1998.
  pa <- atan2(sin(hm), tan(phi) * cos(c$dec) - sin(c$dec) * cos(hm))
  
  h <- h + .astroRefraction(h) # .altitude correction for refraction
  
  return(list(.azimuth = .azimuth(hm, phi, c$dec),
              .altitude = h,
              distance = c$dist,
              parallacticAngle = pa))
}


# calculations for illumination parameters of the moon,
# based on http:#idlastro.gsfc.nasa.gov/ftp/pro/astro/mphase.pro formulas and
# Chapter 48 of "Astronomical Algorithms" 2nd edition by Jean meeus (Willmann-Bell, Richmond) 1998.
.getmoonIllumination <- function(date) {
  
  d <- .toDays(date)
  s <- .sunCoords(d)
  m <- .moonCoords(d)
  
  sdist <- 149598000 # distance from Earth to Sun in km
  
  phi <- acos(sin(s$dec) * sin(m$dec) + cos(s$dec) * cos(m$dec) * cos(s$ra - m$ra))
  inc <- atan2(sdist * sin(phi), m$dist - sdist * cos(phi))
  angle <- atan2(cos(s$dec) * sin(s$ra - m$ra), sin(s$dec) * cos(m$dec) -
                 cos(s$dec) * sin(m$dec) * cos(s$ra - m$ra))

  
  return(list(fraction = ((1 + cos(inc)) / 2),
              phase = (0.5 + 0.5 * inc * ifelse(angle < 0, -1, 1) / pi),
              angle = angle))
}


.hourslater <- function(date, h) {
  #return(date + lubridate::hours(h))
  return(date + lubridate::seconds(h*3600))
  
}

# calculations for moon rise/set times are based on http:#www.stargazing.net/kepler/moonrise.html article
.getmoonTimes <- function (date, lat, lng, inUTC) {
  
  t <- date
  lubridate::hour(t) <- 0
  
  hc <- 0.133 * (pi / 180)
  h0 <- .getmoonPosition(t, lat, lng)$.altitude - hc
  
  rise <- NULL
  set <- NULL
  # go in 2-hour chunks, each time seeing if a 3-point quadratic curve crosses zero (which means rise or set)
  for (i in seq(1,24,2)) {
    h1 <- .getmoonPosition(.hourslater(t, i), lat, lng)$.altitude - hc
    h2 <- .getmoonPosition(.hourslater(t, i + 1), lat, lng)$.altitude - hc
    
    a <- (h0 + h2) / 2 - h1
    b <- (h2 - h0) / 2
    xe <- -b / (2 * a)
    ye <- (a * xe + b) * xe + h1
    d <- b * b - 4 * a * h1
    roots <- 0
    
    if (d >= 0) {
      dx <- sqrt(d) / (abs(a) * 2)
      x1 <- xe - dx
      x2 <- xe + dx
      if (abs(x1) <= 1) roots <- roots + 1
      if (abs(x2) <= 1) roots <- roots + 1
      if (x1 < -1) x1 <- x2
    }
    
    if (roots == 1) {
      if (h0 < 0) rise <- i + x1
      else set <- i + x1
      
    } else if (roots == 2) {
      rise <- i + ifelse(ye < 0, x2, x1)
      set <- i + ifelse(ye < 0, x1, x2)
    }
    
    if (!is.null(rise) && !is.null(set)) break
    
    h0 <- h2
  }
  
  result <- list(rise = NULL, 
                 set = NULL, 
                 alwaysUp = FALSE,
                 alwaysDown = FALSE)
  
  if (!is.null(rise)) result[["rise"]] <- .hourslater(t, rise)
  if (!is.null(set)) result[["set"]] <- .hourslater(t, set)
  if (is.null(rise) && is.null(set)) result[[ifelse(ye > 0, 'alwaysUp', 'alwaysDown')]] <- TRUE
  
  return(result)
}

