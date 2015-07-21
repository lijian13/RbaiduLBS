
##' Get geocoding of an address or get the address of geocoding.
##' 
##' @title Geocoding API.
##' @name Geocoding API
##' @aliases getGeocoding revGeocoding
##' @param address String of address.
##' @param city City name.
##' @param ak Access key of an application.
##' @param location A numeric vecter with 2 elements indicating latitude and longitude.
##' @param coordtype Coordinate type, one of the "bd09ll", "gcj02ll" and "wgs84ll".
##' @param around Whether to output the POIs around.
##' @return a data.frame. 
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @references \url{http://developer.baidu.com/map/index.php?title=webapi/guide/webservice-geocoding}
##' @examples \dontrun{
##' getGeocoding("KFC")
##' revGeocoding(c(22.3087, 114.2019))
##' }

getGeocoding <- function(address, city, ak = NULL) {
	#address <- URLencode(address)
	#city <- URLencode(city)
	if (missing(city)) city <- intToUtf8(c(20840, 22269), multiple = FALSE)
	if (is.null(ak)) ak <- getOption("baidu.ak")
	strurl <- paste0("http://api.map.baidu.com/geocoder/v2/?address=",
			address, "&city=", city, "&output=json&ak=", ak)
	res.json <- getURL(strurl, ssl.verifypeer = FALSE, 
			curl = getCurlHandle(), .encoding = "UTF-8")
	res.list <- fromJSON(json_str = res.json)
	
	if (res.list$status == 0 && length(res.list$result) > 0) {
		res <- data.frame(address = address, city = city,
				lat = res.list$result$location$lat,
				lng = res.list$result$location$lng,
				precise = res.list$result$precise,
				confidence = res.list$result$confidence,
				level = res.list$result$level,
				stringsAsFactors = FALSE)
	} else {
		res <- data.frame(address = character(), city = character(),
				lat = numeric(),
				lng = numeric(),
				precise = numeric(),
				confidence = numeric(),
				level = character(),
				stringsAsFactors = FALSE)
	}
	
	return(res)
}


##' @name Geocoding API
##' @aliases getGeocoding revGeocoding
revGeocoding <- function(location, coordtype = c("bd09ll", "gcj02ll", "wgs84ll"), 
		around = FALSE, ak = NULL) 
{
	location <- unlist(location)
	location <- paste(location, collapse = ",")
	coordtype <- match.arg(coordtype)
	pois <- ifelse(identical(around, TRUE), 1, 0)
	if (is.null(ak)) ak <- getOption("baidu.ak")
		
	strurl <- paste0("http://api.map.baidu.com/geocoder/v2/?location=",
			location, "&coordtype=", coordtype, "&pois=", pois, 
			"&output=json&ak=", ak)
	res.json <- getURL(strurl, ssl.verifypeer = FALSE, 
			curl = getCurlHandle(), .encoding = "UTF-8")
	res.list <- fromJSON(json_str = res.json)
	
	return(res.list)
}



