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
getSunlightTimes <- function(date = NULL, lat = NULL, lon = NULL, data = NULL,
           keep = c("solarNoon", "nadir", "sunrise", "sunset", "sunriseEnd", "sunsetStart",  
                    "dawn", "dusk", "nauticalDawn", "nauticalDusk", "nightEnd", "night", "goldenHourEnd", "goldenHour"), 
           tz = "UTC"){
  
  # data control
  if(!is.null(data)){
    if(!is.null(date) | !is.null(lat) | !is.null(lon)){
      stop("Must use only 'data' argument, or 'date', 'lat', and 'lon' together. See examples")
    }
    
    if(!isTRUE(all.equal("data.frame", class(data)))){
      data <- data.frame(data)
    }
    
  } else {
    if(is.null(date) | is.null(lat) | is.null(lon)){
      stop("Must use only 'data' argument, or 'date', 'lat', and 'lon' together. See examples")
    }
    
    if(length(lat) > 1){
      stop("'lat' must be a unique element. Use 'data' for multiple 'lat'")
    }
    if(length(lon) > 1){
      stop("'lon' must be a unique element. Use 'data' for multiple 'lon'")
    }
    data <- data.frame(date = date, lat = lat, lon = lon)
  }
  
  stopifnot(all(c("date", "lat", "lon") %in% colnames(data)))
  
  if(!"Date" %in% class(data$date)){
    stop("date must to be a Date object (class Date)")
  }

  # variable control
  available_var <- c("solarNoon", "nadir", "sunrise", "sunset", "sunriseEnd", "sunsetStart",  
                     "dawn", "dusk", "nauticalDawn", "nauticalDusk", "nightEnd", "night", "goldenHourEnd", "goldenHour")
  stopifnot(all(keep %in% available_var))

  
  # call suncalc.js
  ct <- v8()

  load_suncalc <- ct$source(system.file("suncalc/suncalc.js", package = "suncalc"))
  
  mat_res <- data.frame(matrix(nrow = nrow(data), ncol = length(available_var), ""), stringsAsFactors = FALSE)
  add_res <- lapply(1:nrow(data), function(x){
    ct$eval(paste0("var tmp_res = SunCalc.getTimes(new Date('", 
                   data[x, "date"], "'),", data[x, "lat"], ", ", data[x, "lon"], ");"))
    
    mat_res[x, ] <<- unlist(ct$get("tmp_res"))
    invisible()
  })
  
  # format
  colnames(mat_res) <- available_var
  mat_res <- mat_res[, keep]
  
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
