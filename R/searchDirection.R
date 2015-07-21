
##' Search navigation scheme.
##' 
##' @title Search navigation scheme.
##' @param origin Origin place.
##' @param destination Destination place.
##' @param region City of the places. If \code{mode} is "walking" or "transit", it's required.
##' @param mode Method of transport, one of "driving", "walking" and "transit".
##' @param origin_region City of the origin place. If \code{mode} is "driving", it's required.
##' @param destination_region City of the destination place. If \code{mode} is "driving", it's required.
##' @param coordtype Coordinate type, one of the "bd09ll", "gcj02ll" and "wgs84ll".
##' @param tactics Navigation tactic, "nohighway" means avoiding highway, 
##'        "mintime" means shortest time, "mindist" means shortest path.
##' @param ak Access key of an application.
##' @return 
##'  A list of the details.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @references \url{http://developer.baidu.com/map/index.php?title=webapi/direction-api}
##' @examples \dontrun{
##' searchDirection(origin = "RUC", destination = "PKU", mode = "walking", region = "Beijing")	
##' }

searchDirection <- function(origin, destination, region,
		mode = c("driving", "walking", "transit"),
		origin_region, destination_region,
		coordtype = c("bd09ll", "gcj02ll", "wgs84ll"),
		tactics = c("nohighway", "mintime", "mindist"),
		ak = NULL) 
{
	if (is.null(ak)) ak <- getOption("baidu.ak")
	mode <- match.arg(mode)
	coordtype <- match.arg(coordtype)
	tactics <- match.arg(tactics)
	tactics <- switch(tactics,
			nohighway = 10,
			mintime = 11,
			mindist = 12)
	
	if (mode == "driving") {
		strurl <- paste0("http://api.map.baidu.com/direction/v1?mode=driving&origin=",
				origin, "&destination=", destination, "&origin_region=", origin_region, 
				"&destination_region=", destination_region, "&coord_type=", coordtype,
				"&tactics=", tactics, "&output=json&ak=", ak)
	} else {
		strurl <- paste0("http://api.map.baidu.com/direction/v1?mode=",
				mode, "&origin=", origin, "&destination=", destination, 
				"&region=", region, "&coord_type=", coordtype,
				"&tactics=", tactics, "&output=json&ak=", ak)
	}
	
	res.json <- getURL(strurl, ssl.verifypeer = FALSE, 
			curl = getCurlHandle(), .encoding = "UTF-8")
	res.list <- fromJSON(json_str = res.json)
	
	return(res.list)
}




