#' Get Moon times
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
#' @import V8
#' 
#' @export
#' 
#' @seealso \link{getSunlightTimes}, \link{getMoonTimes}, \link{getMoonIllumination},
#' \link{getMoonPosition},\link{getSunlightPosition}
#' 
getMoonTimes <- function(date = NULL, lat = NULL, lon = NULL, data = NULL,
                             keep = c("rise", "set", "alwaysUp", "alwaysDown"), 
                             tz = "UTC"){
  
  
  # data control
  data <- .buildData(date = date, lat = lat, lon = lon, data = data)
  
  if(!"Date" %in% class(data$date)){
    stop("date must to be a Date object (class Date)")
  }
  
  # variable control
  available_var <- c("rise", "set", "alwaysUp", "alwaysDown")
  stopifnot(all(keep %in% available_var))
  
  
  # call suncalc.js
  ct <- v8()
  
  load_suncalc <- ct$source(system.file("suncalc/suncalc.js", package = "suncalc"))
  
  mat_res <- data.frame(matrix(nrow = nrow(data), ncol = length(available_var), NA), stringsAsFactors = FALSE)
  colnames(mat_res) <- available_var
  mat_res$alwaysUp <- FALSE
  mat_res$alwaysDown <- FALSE
  add_res <- lapply(1:nrow(mat_res), function(x){
    ct$eval(paste0("var tmp_res = SunCalc.getMoonTimes(new Date('", 
                   data[x, "date"], "'),", data[x, "lat"], ", ", data[x, "lon"], ", true);"))
    
    tmp_res <- unlist(ct$get("tmp_res"))
    mat_res[x, names(tmp_res)] <<- tmp_res
    invisible()
  })
  
  # format
  mat_res <- mat_res[, keep]
  
  # tz
  col_tz <- which(colnames(mat_res) %in% c("rise", "set"))
  if(length(col_tz) > 0){
    ctrl_tz <- lapply(col_tz, function(x){
      mat_res[, x] <<- as.POSIXct(mat_res[, x], format = "%Y-%m-%dT%H:%M:%S.", tz = "UTC")
      if(!is.null(tz)){
        if(tz != "UTC"){
          attr(mat_res[, x], "tzone") <<- tz
        }
      }
      invisible()
    })
  }

  res <- cbind(data, mat_res)
  res
}