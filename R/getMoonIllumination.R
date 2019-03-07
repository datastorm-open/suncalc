#' Get Moon illumination
#' 
#' @param date : Single or multiple DateTime. Can be a \code{Date} (YYYY-MM-DD), 
#' a \code{character} in UTC (YYYY-MM-DD HH:mm:ss) or a \code{POSIXct}
#' @param keep : \code{character}. Vector of variables to keep. See \code{Details}
#' 
#' @return \code{data.frame}
#' 
#' @details 
#' 
#' Returns an object with the following properties:
#' 
#' \itemize{
#'   \item{"fraction"}{ :  illuminated fraction of the moon; varies from 0.0 (new moon) to 1.0 (full moon)}
#'   \item{"phase"}{ :  moon phase; varies from 0.0 to 1.0, described below}
#'   \item{"angle"}{ :  midpoint angle in radians of the illuminated limb of the moon reckoned eastward from 
#' the north point of the disk; the moon is waxing if the angle is negative, and waning if positive}
#' }
#' 
#' Moon phase value should be interpreted like this:
#' \itemize{
#'   \item{0}{ : New Moon}
#'   \item{}{Waxing Crescent}
#'   \item{0.25}{ : First Quarter}
#'   \item{}{ : Waxing Gibbous}
#'   \item{0.5}{Full Moon}
#'   \item{}{ : Waning Gibbous}
#'   \item{0.75}{Last Quarter}
#'   \item{}{ : Waning Crescent}
#'} 
#'
#' By subtracting the parallacticAngle from the angle one can get the zenith angle of the moons bright limb (anticlockwise). The zenith angle can be used do draw the moon shape from the observers perspective (e.g. moon lying on its back).
#' 
#' @examples 
#' 
#' # one date
#' getMoonIllumination(date = Sys.Date())
#' 
#' # in character
#' getMoonIllumination(date = c("2017-05-12", "2017-05-12 00:00:00"), 
#'   keep = c("fraction", "phase"))
#' 
#' # in POSIXct
#' getMoonIllumination(date = as.POSIXct("2017-05-12 00:00:00", tz = "UTC"))
#' getMoonIllumination(date = as.POSIXct("2017-05-12 02:00:00", tz = "CET"))
#' 
#'       
#' @import V8
#' 
#' @export
#' 
#' @seealso \link{getSunlightTimes}, \link{getMoonTimes}, \link{getMoonIllumination},
#' \link{getMoonPosition},\link{getSunlightPosition}
#' 
getMoonIllumination <- function(date = Sys.Date(),
                         keep = c("fraction", "phase", "angle")){
  # tz and date control
  request_date <- .buildRequestDate(date)
  
  # variable control
  available_var <- c("fraction", "phase", "angle")
  stopifnot(all(keep %in% available_var))
  
  # call suncalc.js
  ct <- v8()
  
  load_suncalc <- ct$source(system.file("suncalc/suncalc.js", package = "suncalc"))
  
  mat_res <- cbind.data.frame(date = date, 
                              data.frame(matrix(nrow = length(date), ncol = length(available_var), NA), 
                                         stringsAsFactors = FALSE))
  colnames(mat_res)[-1] <- available_var
  add_res <- lapply(1:nrow(mat_res), function(x){
    ct$eval(paste0("var tmp_res = SunCalc.getMoonIllumination(new Date('", 
                   request_date[x], "Z'));"))
    
    tmp_res <- unlist(ct$get("tmp_res"))
    mat_res[x, names(tmp_res)] <<- tmp_res
    invisible()
  })
  print(ct$get("d_test"))
  # format
  mat_res <- mat_res[, c("date", keep)]
  
  mat_res
}