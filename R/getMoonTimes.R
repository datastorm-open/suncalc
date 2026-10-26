#' Get Moon times
#' 
#' @param date \code{Date}. Single or multiple Date. YYYY-MM-DD
#' @param lat \code{numeric}. Single latitude
#' @param lon \code{numeric}. Single longitude
#' @param data \code{data.frame}. Alternative to use \code{date}, \code{lat}, \code{lon} for passing multiple coordinates
#' @param keep \code{character}. Vector of variables to keep. See \code{Details}
#' @param tz \code{character}. Timezone of results
#' @param ... Not used. (Maintenance only)
#' 
#' @return \code{data.frame}
#' 
#' @details 
#' 
#' Available variables are :
#' 
#' \describe{
#'   \item{"rise"}{\code{Date}. moonrise time}
#'   \item{"set"}{\code{Date}. moonset time}
#'   \item{"alwaysUp"}{\code{Logical}. TRUE if the moon never rises or sets and is always above the horizon during the day}
#'   \item{"alwaysDown"}{\code{Logical}. TRUE if the moon is always below the horizon}
#' }
#'
#' Times are given for the requested day in \code{tz} (from 00:00 to 24:00).
#' \code{rise} / \code{set} are \code{NA} when the moon doesn't rise / set this day
#' (about once a month, as moonrise and moonset are about 50 minutes later each day).
#'
#' @examples 
#' 
#' # one date
#' getMoonTimes(date = Sys.Date(), lat = 47.21, lon = -1.557, tz = "Europe/Paris")
#' 
#' # multiple date + subset
#' getMoonTimes(date = seq.Date(Sys.Date()-9, Sys.Date(), by = 1), 
#'   keep = c("rise", "set", "alwaysUp"), 
#'   lat = 47.21, lon = -1.557, tz = "Europe/Paris")
#'   
#' # multiple coordinates
#' data <- data.frame(date = seq.Date(Sys.Date()-9, Sys.Date(), by = 1), 
#'     lat = c(rep(50.1, 10), rep(49, 10)), 
#'     lon = c(rep(1.83, 10), rep(2, 10)))
#'     
#' getMoonTimes(data = data, tz = "Europe/Paris")
#'       
#' @rawNamespace import(data.table, except = hour)
#' @import magrittr 
#' @importFrom lubridate as_datetime
#' @importFrom lubridate hours
#' @importFrom lubridate seconds
#' @importFrom lubridate hour 
#' 
#' @export
#' 
#' @family suncalc functions
#' 
getMoonTimes <- function(date = NULL, lat = NULL, lon = NULL, data = NULL,
                             keep = c("rise", "set", "alwaysUp", "alwaysDown"), 
                             tz = "UTC", ...){
  
  
  if("inUTC" %in% names(list(...))){
    warning("inUTC is deprecated since suncalc >= 0.5.2")
  }
  
  # data control
  data <- .buildData(date = date, lat = lat, lon = lon, data = data)
  
  if (!"Date" %in% class(data$date)) {
    stop("date must to be a Date object (class Date)")
  }
  
  # variable control
  available_var <- c("rise", "set", "alwaysUp", "alwaysDown")
  stopifnot(all(keep %in% available_var))
  
  times <- .timesInDay(fun = .getMoonTimes, date = data$date, lat = data$lat, lon = data$lon, 
                       tz = tz, vars = intersect(c("rise", "set"), keep))
  
  data <- data.table(date = data$date, lat = data$lat, lon = data$lon) %>% 
    .[, (keep) := times[keep]]
  
  return(as.data.frame(data))
  
}
