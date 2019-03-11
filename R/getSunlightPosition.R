#' Get Sunlight position
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
#'   \item{"altitude"}{ : sun altitude above the horizon in radians, e.g. 0 at the horizon and PI/2 at the zenith (straight over your head)}
#'   \item{"azimuth"}{ : sun azimuth in radians (direction along the horizon, measured from south to west), e.g. 0 is south and Math.PI * 3/4 is northwest}
#' }
#' 
#' @examples 
#' 
#' # one date
#' getSunlightPosition(date = Sys.Date(), lat = 50.1, lon = 1.83)
#' 
#' # in character
#' getSunlightPosition(date = c("2017-05-12", "2017-05-12 00:00:00"), 
#'     lat = 50.1, lon = 1.83)
#' 
#' # in POSIXct
#' getSunlightPosition(date = as.POSIXct("2017-05-12 00:00:00", tz = "UTC"),
#'     lat = 50.1, lon = 1.83)
#' getSunlightPosition(date = as.POSIXct("2017-05-12 02:00:00", tz = "CET"),
#'     lat = 50.1, lon = 1.83)
#' 
#' # multiple date + subset
#' getSunlightPosition(date = seq.Date(Sys.Date()-9, Sys.Date(), by = 1), 
#'   keep = c("altitude"), 
#'   lat = 50.1, lon = 1.83)
#'   
#' # multiple coordinates
#' data <- data.frame(date = seq.Date(Sys.Date()-9, Sys.Date(), by = 1), 
#'     lat = c(rep(50.1, 10), rep(49, 10)), 
#'     lon = c(rep(1.83, 10), rep(2, 10)))
#'     
#' getSunlightPosition(data = data, 
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
getSunlightPosition <- function(date = NULL, lat = NULL, lon = NULL, data = NULL,
                            keep = c("altitude", "azimuth")) {
  
  # data control
  data <- .buildData(date = date, lat = lat, lon = lon, data = data)

  
  # variable control
  available_var <- c("altitude", "azimuth")
  stopifnot(all(keep %in% available_var))
  
  # tz and date control
  requestDate <- .buildRequestDate(data$date)
  data$date <- requestDate
  
  data <- data %>% 
    .[, (available_var) := .getPosition(date = date, lat = lat, lng = lon)] %>% 
    .[, c("date", "lat", "lon", keep), with = FALSE] %>% 
    as.data.frame()
  
  return(data)
}
