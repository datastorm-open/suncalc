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
#' @import V8
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
  
  # tz and date control
  request_date <- .buildRequestDate(data$date)
  
  # variable control
  available_var <- c("altitude", "azimuth", "distance", "parallacticAngle")
  stopifnot(all(keep %in% available_var))
  
  # call suncalc.js
  ct <- v8()
  
  load_suncalc <- ct$source(system.file("suncalc/suncalc.js", package = "suncalc"))
  
  mat_res <- data.frame(matrix(nrow = nrow(data), ncol = length(available_var), NA), 
                                         stringsAsFactors = FALSE)
  colnames(mat_res) <- available_var
  add_res <- lapply(1:nrow(mat_res), function(x){
    ct$eval(paste0("var tmp_res = SunCalc.getMoonPosition(new Date('", 
                   request_date[x], "Z'),", data[x, "lat"], ", ", data[x, "lon"], ");"))
    
    tmp_res <- unlist(ct$get("tmp_res"))
    mat_res[x, names(tmp_res)] <<- tmp_res
    invisible()
  })
  
  # format
  mat_res <- mat_res[, keep, drop = FALSE]
  res <- cbind(data, mat_res)
  res
}