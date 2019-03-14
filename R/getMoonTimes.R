#' Get Moon times
#' 
#' @param date : \code{Date}. Single or multiple Date. YYYY-MM-DD
#' @param lat : \code{numeric}. Single latitude
#' @param lon : \code{numeric}. Single longitude
#' @param data : \code{data.frame}. Alternative to use \code{date}, \code{lat}, \code{lon} for passing multiple coordinates
#' @param keep : \code{character}. Vector of variables to keep. See \code{Details}
#' @param tz : \code{character}. timezone of results
#' @param inUTC : \code{logical}. By default, it will search for moon rise and set during local user's day (from 0 to 24 hours). If TRUE, it will instead search the specified date from 0 to 24 UTC hours.
#' 
#' @return \code{data.frame}
#' 
#' @details 
#' 
#' Availabled variables are :
#' 
#' \itemize{
#'   \item{"rise"}{ : \code{Date}. moonrise time}
#'   \item{"set"}{ : \code{Date}. moonset time}
#'   \item{"alwaysUp"}{ : \code{Logical}. TRUE if the moon never rises or sets and is always above the horizon during the day}
#'   \item{"alwaysDown"}{ : \code{Logical}. TRUE if the moon is always below the horizon}
#' }
#' 
#' @examples 
#' 
#' # one date
#' getMoonTimes(date = Sys.Date(), lat = 47.21, lon = -1.557, tz = "CET")
#' 
#' # multiple date + subset
#' getMoonTimes(date = seq.Date(Sys.Date()-9, Sys.Date(), by = 1), 
#'   keep = c("rise", "set", "alwaysUp"), 
#'   lat = 47.21, lon = -1.557, tz = "CET")
#'   
#' # multiple coordinates
#' data <- data.frame(date = seq.Date(Sys.Date()-9, Sys.Date(), by = 1), 
#'     lat = c(rep(50.1, 10), rep(49, 10)), 
#'     lon = c(rep(1.83, 10), rep(2, 10)))
#'     
#' getMoonTimes(data = data, tz = "CET")
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
#' @seealso \link{getSunlightTimes}, \link{getMoonTimes}, \link{getMoonIllumination},
#' \link{getMoonPosition},\link{getSunlightPosition}
#' 
getMoonTimes <- function(date = NULL, lat = NULL, lon = NULL, data = NULL,
                             keep = c("rise", "set", "alwaysUp", "alwaysDown"), 
                             tz = "UTC", inUTC = FALSE){
  
  
  # data control
  data <- .buildData(date = date, lat = lat, lon = lon, data = data)
  
  if (!"Date" %in% class(data$date)) {
    stop("date must to be a Date object (class Date)")
  }
  
  # variable control
  available_var <- c("rise", "set", "alwaysUp", "alwaysDown")
  stopifnot(all(keep %in% available_var))
  
  data <- data %>%
    .[, date := lubridate::as_datetime(date, tz = "UTC") + lubridate::hours(12)] %>% 
    .[, (available_var) := .getMoonTimes(date = as.Date(date), lat = lat, lng = lon, inUTC = inUTC)] %>%
    .[, c("date", "lat", "lon", keep), with = FALSE] %>% 
    .[, date := as.Date(date)] %>% 
    as.data.frame()
  
  if (!is.null(tz)) {
    invisible(lapply(c("rise", "set"),
                     function(x) attr(data[[x]], "tzone") <<- tz)
    )
  }
  
  return(data)
  
}
