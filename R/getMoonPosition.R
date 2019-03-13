#' Get Moon position
#' 
#' @param date : Single or multiple DateTime. Can be a \code{Date} (YYYY-MM-DD), 
#' a \code{character} in UTC (YYYY-MM-DD HH:mm:ss) or a \code{POSIXct}
#' @param lat : \code{numeric}. Single latitude
#' @param lon : \code{numeric}. Single longitude
#' @param data : \code{data.frame}. Alternative to use \code{date}, \code{lat}, \code{lon} for passing multiple coordinates
#' @param keep : \code{character}. Vector of variables to keep. See \code{Details}
#' 
#' @return \code{data.frame}
#' 
#' @details 
#' 
#' Returns an object with the following properties:
#' 
#' \itemize{
#'   \item{"altitude"}{ : moon altitude above the horizon in radians}
#'   \item{"azimuth"}{ : moon azimuth in radians}
#'   \item{"distance"}{ : distance to moon in kilometers}
#'   \item{"parallacticAngle"}{ : parallactic angle of the moon in radians}
#' }
#' 
#' @examples 
#' 
#' # one date
#' getMoonPosition(date = Sys.Date(), lat = 50.1, lon = 1.83)
#' 
#' # in character
#' getMoonPosition(date = c("2017-05-12", "2017-05-12 00:00:00"), 
#'     lat = 50.1, lon = 1.83)
#' 
#' # in POSIXct
#' getMoonPosition(date = as.POSIXct("2017-05-12 00:00:00", tz = "UTC"),
#'     lat = 50.1, lon = 1.83)
#' getMoonPosition(date = as.POSIXct("2017-05-12 02:00:00", tz = "CET"),
#'     lat = 50.1, lon = 1.83)
#' 
#' # multiple date + subset
#' getMoonPosition(date = seq.Date(Sys.Date()-9, Sys.Date(), by = 1), 
#'   keep = c("altitude", "azimuth"), 
#'   lat = 50.1, lon = 1.83)
#'   
#' # multiple coordinates
#' data <- data.frame(date = seq.Date(Sys.Date()-9, Sys.Date(), by = 1), 
#'     lat = c(rep(50.1, 10), rep(49, 10)), 
#'     lon = c(rep(1.83, 10), rep(2, 10)))
#'     
#' getMoonPosition(data = data, 
#'   keep = c("altitude", "azimuth"))
#' 
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
getMoonPosition <- function(date = NULL, lat = NULL, lon = NULL, data = NULL,
                            keep = c("altitude", "azimuth", "distance", "parallacticAngle")){
  
  # data control
  data <- .buildData(date = date, lat = lat, lon = lon, data = data)
  
  # variable control
  available_var <- c("altitude", "azimuth", "distance", "parallacticAngle")
  stopifnot(all(keep %in% available_var))
  
  # tz and date control
  requestDate <- .buildRequestDate(data$date)
  data$date <- requestDate
  
  data <- data %>% 
    .[, (available_var) := .getMoonPosition(date = date, lat = lat, lng = lon)] %>% 
    .[, c("date", "lat", "lon", keep), with = FALSE] %>% 
    as.data.frame()
  
  return(data)
}
