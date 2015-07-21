
##' Convert other coordinates to Baidu coordinate.
##' 
##' @title Convert other coordinates to Baidu coordinate.
##' @param geodf data frame to be converted.
##' @param from From what coordinate.
##' @param to To what coordinate.
##' @param lonCol Column name of the longitude.
##' @param latCol Column name of the latitude.
##' @return a data.frame. 
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @references \url{http://developer.baidu.com/map/index.php?title=webapi/guide/changeposition}

geoconv <- function(geodf, 
		from = c("GPS", "GPSmc", "google", "googlemc", "bd09ll", "bd09mc", "mapbar", "51map"),
		to = c("bd09ll", "bd09mc"),
		lonCol = "Lon", latCol = "Lat", ak = NULL) 
{
	
	if (is.null(ak)) ak <- getOption("baidu.ak")
	from <- match.arg(from)
	to <- match.arg(to)
	from <- which(c("GPS", "GPSmc", "google", "googlemc", "bd09ll", "bd09mc", "mapbar", "51map") == from)
	to <- which(c("bd09ll", "bd09mc") == to) + 4
	OUT <- geodf[, c(lonCol, latCol)]
	
	n <- ceiling(nrow(geodf) / 100)
	for (i in 1:n) {
		tmp.lon <- geodf[[lonCol]][(100*(i-1)+1):(min(100*i, nrow(geodf)))]
		tmp.lat <- geodf[[latCol]][(100*(i-1)+1):(min(100*i, nrow(geodf)))]
		
		strgeo <- paste(tmp.lon, tmp.lat, sep = ",", collapse = ";")
		strurl <- paste0("http://api.map.baidu.com/geoconv/v1/?coords=",
				strgeo, "&from=", from, "&to=", to, "&output=json&ak=", ak)
		
		res.json <- getURL(strurl, ssl.verifypeer = FALSE, 
				curl = getCurlHandle(), .encoding = "UTF-8")
		res.list <- fromJSON(json_str = res.json)
		
		OUT[[lonCol]][(100*(i-1)+1):(min(100*i, nrow(geodf)))] <- sapply(res.list$result, "[[", 1)
		OUT[[latCol]][(100*(i-1)+1):(min(100*i, nrow(geodf)))] <- sapply(res.list$result, "[[", 2)
		
	}
	
	return(OUT)
}





