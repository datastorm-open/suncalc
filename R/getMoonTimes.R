#' Get Moon times
#' 
#' @param date : \code{Date}. Single or multiple Date. YYYY-MM-DD
#' @param lat : \code{numeric}. Single latitude
#' @param lon : \code{numeric}. Single longitude
#' @param data : \code{data.frame}. Alternative to use \code{date}, \code{lat}, \code{lon} for passing multiple coordinates
#' @param keep : \code{character}. Vector of variables to keep. See \code{Details}
#' @param tz : \code{character}. Timezone of results
#' @param ... :Not used. (Maintenance only)
#' 
#' @return \code{data.frame}
#' 
#' @details 
#' 
#' Available variables are :
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
  
  data <- data %>%
    # .[, date := lubridate::as_datetime(date, tz = "UTC") + lubridate::hours(12)] %>% 
    .[, date := lubridate::force_tz(lubridate::as_datetime(date) + lubridate::hours(12), "UTC")] %>%
    .[, (available_var) := .getMoonTimes(date = date, lat = lat, lng = lon)] %>%
    .[, c("date", "lat", "lon", keep), with = FALSE] %>% 
    .[, date := as.Date(date)]
  
  if (!is.null(tz)) {
    invisible(lapply(c("rise", "set"),
                     function(x) if(x %in% names(data)) attr(data[[x]], "tzone") <<- tz)
    )
  }
  
  if(any(c("rise", "set") %in% colnames(data))){
  # browser()
    # check value with datetime before date in TZ
    mat <- as.matrix(data[, intersect(colnames(data), c("rise", "set")), with = FALSE])
    is_inf <- which(is.na(mat) | mat < as.character(lubridate::force_tz(lubridate::as_datetime(data$date) + lubridate::hours(0), tz)), arr.ind =  T)
    
    if(nrow(is_inf) > 0){
      
      is_inf <- as.data.table(is_inf)
      
      unique_inf_row <- data.frame(row = sort(unique(is_inf$row)))
      unique_inf_row$id_row <- 1:nrow(unique_inf_row)
      
      is_inf[, id_row := unique_inf_row$id_row[match(row, unique_inf_row$row)]]
      
      data_inf <- data[unique_inf_row$row, c("date", "lat", "lon"), with = FALSE]
      
      data_inf <- data_inf %>% 
        .[, date := lubridate::force_tz(lubridate::as_datetime(date + 1) + lubridate::hours(12), "UTC")] %>% 
        .[, (available_var) := .getMoonTimes(date = date, lat = lat, lng = lon)] %>%
        .[, c("date", "lat", "lon", keep), with = FALSE] %>% 
        .[, date := as.Date(date)]
      
      if (!is.null(tz)) {
        invisible(lapply(c("rise", "set"),
                         function(x) if(x %in% names(data_inf)) attr(data_inf[[x]], "tzone") <<- tz)
        )
      }
      
      invisible({
        lapply(
          unique(is_inf$col),
          function(x){
            col_names <- intersect(colnames(data), c("rise", "set"))[x]
            data[is_inf[col == x, row], c(col_names) := data_inf[is_inf[col == x, id_row], get(col_names)]]
            data[is_inf[col == x, row] & as.Date(get(col_names), tz = tz) != date, c(col_names) := NA]
          }
        )
      })
    }
    
    # check value with datetime after date in TZ
    mat <- as.matrix(data[, intersect(colnames(data), c("rise", "set")), with = FALSE])
    is_sup <- which(is.na(mat) | mat >= as.character(lubridate::force_tz(lubridate::as_datetime(data$date + 1) + lubridate::hours(0), tz)), arr.ind =  T)
    if(nrow(is_sup) > 0){
      
      is_sup <- as.data.table(is_sup)
      
      unique_sup_row <- data.frame(row = sort(unique(is_sup$row)))
      unique_sup_row$id_row <- 1:nrow(unique_sup_row)
      
      is_sup[, id_row := unique_sup_row$id_row[match(row, unique_sup_row$row)]]
      
      data_sup <- data[unique_sup_row$row, c("date", "lat", "lon"), with = FALSE]
      
      data_sup <- data_sup %>% 
        .[, date := lubridate::force_tz(lubridate::as_datetime(date - 1) + lubridate::hours(12), "UTC")] %>% 
        .[, (available_var) := .getMoonTimes(date = date, lat = lat, lng = lon)] %>%
        .[, c("date", "lat", "lon", keep), with = FALSE] %>% 
        .[, date := as.Date(date)]
      
      if (!is.null(tz)) {
        invisible(lapply(c("rise", "set"),
                         function(x) if(x %in% names(data_sup)) attr(data_sup[[x]], "tzone") <<- tz)
        )
      }
      
      invisible({
        lapply(
          unique(is_sup$col),
          function(x){
            col_names <- intersect(colnames(data), c("rise", "set"))[x]
            data[is_sup[col == x, row], c(col_names) := data_sup[is_sup[col == x, id_row], get(col_names)]]
            data[is_sup[col == x, row] & as.Date(get(col_names), tz = tz) != date, c(col_names) := NA]
          }
        )
      })
    }
  }
  
  return(as.data.frame(data))
  
}
