.buildData <- function(date = NULL, lat = NULL, lon = NULL, data = NULL){
  # data control
  if(!is.null(data)) {
    if(!is.null(date) | !is.null(lat) | !is.null(lon)){
      stop("Must use only 'data' argument, or 'date', 'lat', and 'lon' together. See examples")
    }
    
    if(!inherits(data, "data.table")) {
      data <- data.table(data)
    }
    
  } else {
    if(is.null(date) | is.null(lat) | is.null(lon)){
      stop("Must use only 'data' argument, or 'date', 'lat', and 'lon' together. See examples")
    }
    
    if(length(lat) > 1) {
      stop("'lat' must be a unique element. Use 'data' for multiple 'lat'")
    }
    if(length(lon) > 1) {
      stop("'lon' must be a unique element. Use 'data' for multiple 'lon'")
    }
    data <- data.table(date = date, lat = lat, lon = lon, stringsAsFactors = FALSE)
  }
  
  stopifnot(all(c("date", "lat", "lon") %in% colnames(data)))
  
  return(data)
}

.buildRequestDate <- function(date){
  if(!any(c("Date", "POSIXct", "character") %in% class(date))){
    stop("date must to be a Date object (class Date) or POSIXct or character")
  }
  request_date <- date
  if("POSIXct" %in% class(date)){
    if(is.null(attr(date, "tzone"))){
      attr(request_date, "tzone") <- "UTC"
      warning("'date' is convert to 'UTC' for request using 'attr(date, 'tzone') <- 'UTC'")
    } else if(attr(date, "tzone") != "UTC"){
      attr(request_date, "tzone") <- "UTC"
      warning("'date' is convert to 'UTC' for request using 'attr(date, 'tzone') <- 'UTC'")
    }
  }
  request_date
}