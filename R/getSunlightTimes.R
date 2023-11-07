#' Get Sunlight times
#' 
#' @param date : \code{Date}. Single or multiple Date. YYYY-MM-DD
#' @param lat : \code{numeric}. Single latitude
#' @param lon : \code{numeric}. Single longitude
#' @param data : \code{data.frame}. Alternative to use \code{date}, \code{lat}, \code{lon} for passing multiple coordinates
#' @param keep : \code{character}. Vector of variables to keep. See \code{Details}
#' @param tz : \code{character}. Timezone of results
#' 
#' @return \code{data.frame}
#' 
#' @details 
#' 
#' Available variables are :
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
getSunlightTimes <- function(date = NULL, lat = NULL, lon = NULL, data = NULL,
                             keep = c("solarNoon", "nadir", "sunrise", "sunset", "sunriseEnd", "sunsetStart",  
                                      "dawn", "dusk", "nauticalDawn", "nauticalDusk", "nightEnd", "night",
                                      "goldenHourEnd", "goldenHour"), 
                             tz = "UTC"){
  
  
  # data control
  data <- .buildData(date = date, lat = lat, lon = lon, data = data)
  
  if (!"Date" %in% class(data$date)) {
    stop("date must to be a Date object (class Date)")
  }
  
  # variable control
  available_var <- c("solarNoon", "nadir", "sunrise", "sunset", "sunriseEnd", "sunsetStart",  
                     "dawn", "dusk", "nauticalDawn", "nauticalDusk", "nightEnd", "night",
                     "goldenHourEnd", "goldenHour")
  stopifnot(all(keep %in% available_var))
  
  # date := lubridate::force_tz(lubridate::as_datetime(Sys.Date()) + lubridate::hours(12), Sys.timezone())
  data <- data %>% 
    .[, date := lubridate::force_tz(lubridate::as_datetime(date) + lubridate::hours(12), "UTC")] %>% 
    .[, (available_var) := .getTimes(date = date, lat = lat, lng = lon, tz = "UTC")] %>% 
    .[, c("date", "lat", "lon", keep), with = FALSE] %>% 
    .[, date := as.Date(date)]
  
  invisible({
    lapply(
      setdiff(colnames(data), c("date", "lat", "lon")),
      function(x) attr(data[[x]], "tzone") <<- tz
    )
  })
  
  # check value with datetime before date in TZ
  mat <- as.matrix(data[, setdiff(colnames(data), c("date", "lat", "lon")), with = FALSE])
  is_inf <- which(is.na(mat) | mat < as.character(lubridate::force_tz(lubridate::as_datetime(data$date) + lubridate::hours(0), tz)), arr.ind =  T)
  if(nrow(is_inf) > 0){
    
    is_inf <- as.data.table(is_inf)
    
    unique_inf_row <- data.frame(row = sort(unique(is_inf$row)))
    unique_inf_row$id_row <- 1:nrow(unique_inf_row)
    
    is_inf[, id_row := unique_inf_row$id_row[match(row, unique_inf_row$row)]]
 
    data_inf <- data[unique_inf_row$row, c("date", "lat", "lon"), with = FALSE]
    
    data_inf <- data_inf %>% 
      .[, date := lubridate::force_tz(lubridate::as_datetime(date + 1) + lubridate::hours(12), "UTC")] %>% 
      .[, (available_var) := .getTimes(date = date, lat = lat, lng = lon, tz = "UTC")] %>% 
      .[, c("date", "lat", "lon", keep), with = FALSE] %>% 
      .[, date := as.Date(date)]
    
    invisible({
      lapply(
        setdiff(colnames(data_inf), c("date", "lat", "lon")),
        function(x) attr(data_inf[[x]], "tzone") <<- tz
      )
    })
    
    invisible({
      lapply(
        unique(is_inf$col),
        function(x){
          col_names <- setdiff(colnames(data), c("date", "lat", "lon"))[x]
          data[is_inf[col == x, row], c(col_names) := data_inf[is_inf[col == x, id_row], get(col_names)]]
        }
      )
    })
  }
  
  # check value with datetime after date in TZ
  mat <- as.matrix(data[, setdiff(colnames(data), c("date", "lat", "lon")), with = FALSE])
  is_sup <- which(is.na(mat) | mat >= as.character(lubridate::force_tz(lubridate::as_datetime(data$date + 1) + lubridate::hours(0), tz)), arr.ind =  T)
  if(nrow(is_sup) > 0){
    
    is_sup <- as.data.table(is_sup)
    
    unique_sup_row <- data.frame(row = sort(unique(is_sup$row)))
    unique_sup_row$id_row <- 1:nrow(unique_sup_row)
    
    is_sup[, id_row := unique_sup_row$id_row[match(row, unique_sup_row$row)]]
    
    data_sup <- data[unique_sup_row$row, c("date", "lat", "lon"), with = FALSE]
    
    data_sup <- data_sup %>% 
      .[, date := lubridate::force_tz(lubridate::as_datetime(date - 1) + lubridate::hours(12), "UTC")] %>% 
      .[, (available_var) := .getTimes(date = date, lat = lat, lng = lon, tz = "UTC")] %>% 
      .[, c("date", "lat", "lon", keep), with = FALSE] %>% 
      .[, date := as.Date(date)]
    
    invisible({
      lapply(
        setdiff(colnames(data_sup), c("date", "lat", "lon")),
        function(x) attr(data_sup[[x]], "tzone") <<- tz
      )
    })
    
    invisible({
      lapply(
        unique(is_sup$col),
        function(x){
          col_names <- setdiff(colnames(data), c("date", "lat", "lon"))[x]
          data[is_sup[col == x, row], c(col_names) := data_sup[is_sup[col == x, id_row], get(col_names)]]
        }
      )
    })
  }
  
  return(as.data.frame(data))
}
