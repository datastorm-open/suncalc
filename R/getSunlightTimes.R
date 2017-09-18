#' Get Sunlight times
#' 
#' @param date : \code{Date}. Single or multiple Date. YYYY-MM-DD
#' @param lat : \code{numeric}. Single latitude
#' @param lon : \code{numeric}. Single longitude
#' @param data : \code{data.frame}. Alternative to use \code{date}, \code{lat}, \code{lon} for passing multiple coordinates
#' @param keep : \code{character}. Vector of variables to keep. See \code{Details}
#' @param tz : \code{character}. timezone of results
#' 
#' @return \code{data.frame}
#' 
#' @details 
#' 
#' Availabled variables are :
#' 
#' \itemize{
#'   \item{"sunrise"}{ : sunrise (top edge of the sun appears on the horizon)}
#'   \item{"sunriseEnd"}{ : sunrise ends (bottom edge of the sun touches the horizon)}
#'   \item{"goldenHourEnd"}{ : morning golden hour (soft light, best time for photography) ends}
#'   \item{"solarNoon"}{ : solar noon (sun is in the highest position)}
#'   \item{"goldenHour"}{ : evening golden hour starts}
#'   \item{"sunsetStart"}{ : sunset starts (bottom edge of the sun touches the horizon)}
#'   \item{"sunset"}{ : sunset (sun disappears below the horizon, evening civil twilight starts)}
#'   \item{"dusk"}{ : dusk (evening nautical twilight starts)}
#'   \item{"nauticalDusk"}{ : nautical dusk (evening astronomical twilight starts)}
#'   \item{"night"}{ : night starts (dark enough for astronomical observations)}
#'   \item{"nadir"}{ : nadir (darkest moment of the night, sun is in the lowest position)}
#'   \item{"nightEnd"}{ : night ends (morning astronomical twilight starts)}
#'   \item{"nauticalDawn"}{ : nautical dawn (morning nautical twilight starts)}
#'   \item{"dawn"}{ : dawn (morning nautical twilight ends, morning civil twilight starts)}
#' }
#' 
#' @examples 
#' 
#' # one date
#' getSunlightTimes(date = Sys.Date(), lat = 50.1, lon = 1.83, tz = "CET")
#' 
#' # multiple date + subset
#' getSunlightTimes(date = seq.Date(Sys.Date()-9, Sys.Date(), by = 1), 
#'   keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"), 
#'   lat = 50.1, lon = 1.83, tz = "CET")
#'   
#' # multiple coordinates
#' data <- data.frame(date = seq.Date(Sys.Date()-9, Sys.Date(), by = 1), 
#'     lat = c(rep(50.1, 10), rep(49, 10)), 
#'     lon = c(rep(1.83, 10), rep(2, 10)))
#'     
#' getSunlightTimes(data = data, 
#'   keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"), tz = "CET")
#'       
#' @import V8
#' 
#' @export
#' 
#' @seealso \link{getSunlightTimes}, \link{getMoonTimes}, \link{getMoonIllumination},
#' \link{getMoonPosition},\link{getSunlightPosition}
#' 
getSunlightTimes <- function(date = NULL, lat = NULL, lon = NULL, data = NULL,
           keep = c("solarNoon", "nadir", "sunrise", "sunset", "sunriseEnd", "sunsetStart",  
                    "dawn", "dusk", "nauticalDawn", "nauticalDusk", "nightEnd", "night", "goldenHourEnd", "goldenHour"), 
           tz = "UTC"){
  
  
  # data control
  data <- .buildData(date = date, lat = lat, lon = lon, data = data)
  
  if(!"Date" %in% class(data$date)){
    stop("date must to be a Date object (class Date)")
  }

  data$date <- paste0(data$date, " 12:00:00")
  
  # variable control
  available_var <- c("solarNoon", "nadir", "sunrise", "sunset", "sunriseEnd", "sunsetStart",  
                     "dawn", "dusk", "nauticalDawn", "nauticalDusk", "nightEnd", "night", "goldenHourEnd", "goldenHour")
  stopifnot(all(keep %in% available_var))

  
  # call suncalc.js
  ct <- v8()

  load_suncalc <- ct$source(system.file("suncalc/suncalc.js", package = "suncalc"))
  
  mat_res <- data.frame(matrix(nrow = nrow(data), ncol = length(available_var), NA), stringsAsFactors = FALSE)
  colnames(mat_res) <- available_var
  add_res <- lapply(1:nrow(mat_res), function(x){
    ct$eval(paste0("var tmp_res = SunCalc.getTimes(new Date('", 
                   data[x, "date"], "'),", data[x, "lat"], ", ", data[x, "lon"], ");"))
    
    tmp_res <- unlist(ct$get("tmp_res"))
    mat_res[x, names(tmp_res)] <<- tmp_res
    invisible()
  })
  
  # format
  mat_res <- mat_res[, keep, drop = FALSE]
  
  # tz
  ctrl_tz <- lapply(1:ncol(mat_res), function(x){
    mat_res[, x] <<- as.POSIXct(mat_res[, x], format = "%Y-%m-%dT%H:%M:%S.", tz = "UTC")
    if(!is.null(tz)){
      if(tz != "UTC"){
        attr(mat_res[, x], "tzone") <<- tz
      }
    }
    invisible()
  })
  
  res <- cbind(data, mat_res)
  res
}
