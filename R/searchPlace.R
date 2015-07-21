
##' Search detail of a place.
##' 
##' One of the arguments 'region', 'bounds' and 'location/radius' should be specified.
##' @title Search detail of a place.
##' @param q Query string.
##' @param region City name.
##' @param bounds A numeric vecter with 4 elements indicating latitude of bottom left, 
##'               longitude of bottom left, latitude of upper right, longitude of upper right.
##' @param location A numeric vecter with 2 elements indicating latitude and longitude of the centre of a circle.
##' @param radius The radius in meters.
##' @param page_num Which page of the results should be outputed.
##' @param page_size How many items in each page. Default is 20.
##' @param ak Access key of an application.
##' @return 
##'  A data.frame of the details: 
##'  \item{uid}{ID of POI.}
##'  \item{name}{Name of POI.}
##'  \item{lat}{Latitude.}
##'  \item{lng}{Longitude}
##'  \item{address}{Address.}
##'  \item{telephone}{Telephone number.}
##'  \item{distance}{Distance to the centre.}
##'  \item{type}{Type of POI, such as 'hotel' and 'cater'.}
##'  \item{tag}{Tag.}
##'  \item{price}{Price of the business.}
##'  \item{shop_hours}{Business hours.}
##'  \item{overall_rating}{Overall rating.}
##'  \item{taste_rating}{Taste rating.}
##'  \item{service_rating}{Service rating.}
##'  \item{environment_rating}{Environment rating.}
##'  \item{facility_rating}{Facility rating.}
##'  \item{hygiene_rating}{Hygiene rating.}
##'  \item{technology_rating}{Technology rating.}
##'  \item{image_num}{Image number.}
##'  \item{groupon_num}{Groupon number.}
##'  \item{discount_num}{Discount number.}
##'  \item{comment_num}{Comments number.}
##'  \item{favorite_num}{Favorite number.}
##'  \item{checkin_num}{Checkin number.}
##'  \item{detail_url}{URL of the page.}
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @references \url{http://developer.baidu.com/map/index.php?title=webapi/guide/webservice-placeapi}
##' @examples \dontrun{
##' searchPlace("KFC")
##' searchPlace("KFC", bounds = c(39.915, 116.404, 39.975, 116.414))
##' searchPlace("KFC", location = c(39.915, 116.404), radius = 2000)
##' }

searchPlace <- function(q, region, bounds, location, radius,
		page_num = 1, page_size = 20, detail = TRUE, ak = NULL
) {
	q <- URLencode(q)
	if (is.null(ak)) ak <- getOption("baidu.ak")
	page_size <- min(20, page_size)
	page_num <- page_num - 1
	if (!identical(detail, TRUE)) page_num <- 1
	
	if(!missing(bounds)) {
		bounds <- unlist(bounds)
		bounds <- paste(bounds, collapse = ",")
		strurl <- paste0("http://api.map.baidu.com/place/v2/search?&query=",
				q, "&bounds=", bounds, "&output=json&ak=",
				ak, "&page_size=", page_size, "&page_num=",
				page_num,"&scope=2")
	} else if (!missing(location) && !missing(radius)) {
		location <- unlist(location)
		location <- paste(location, collapse = ",")
		strurl <- paste0("http://api.map.baidu.com/place/v2/search?&query=",
				q, "&location=", location, "&radius=", radius, "&output=json&ak=",
				ak, "&page_size=", page_size, "&page_num=",
				page_num,"&scope=2")
	}  else {
		if (missing(region)) region <- intToUtf8(c(20840, 22269), multiple = FALSE)
		region <- URLencode(region)
		strurl <- paste0("http://api.map.baidu.com/place/v2/search?&query=",
				q, "&region=", region, "&output=json&ak=",
				ak, "&page_size=", page_size, "&page_num=",
				page_num,"&scope=2")
	}
	
	res.json <- getURL(strurl, ssl.verifypeer = FALSE, 
			curl = getCurlHandle(), .encoding = "UTF-8")
	res.list <- fromJSON(json_str = res.json)
	
	if (!identical(detail, TRUE)) {
		res <- res.list$total
	} else {
		res <- data.frame(uid = as.character(sapply(res.list$results, ".getElement", "uid")),
				name = as.character(sapply(res.list$results, ".getElement", "name")),
				lat = as.numeric(sapply(res.list$results, ".getElement", "location", "lat")),
				lng = as.numeric(sapply(res.list$results, ".getElement", "location", "lng")),
				address = as.character(sapply(res.list$results, ".getElement", "address")),
				telephone = as.character(sapply(res.list$results, ".getElement", "telephone")),
				distance = as.numeric(sapply(res.list$results, ".getElement", "detail_info", "distance")),
				type = as.character(sapply(res.list$results, ".getElement", "detail_info", "type")),
				tag = as.character(sapply(res.list$results, ".getElement", "detail_info", "tag")),
				price = as.character(sapply(res.list$results, ".getElement", "detail_info", "price")),
				shop_hours = as.character(sapply(res.list$results, ".getElement", "detail_info", "shop_hours")),
				overall_rating = as.character(sapply(res.list$results, ".getElement", "detail_info", "overall_rating")),
				taste_rating = as.character(sapply(res.list$results, ".getElement", "detail_info", "taste_rating")),
				service_rating = as.character(sapply(res.list$results, ".getElement", "detail_info", "service_rating")),
				environment_rating = as.character(sapply(res.list$results, ".getElement", "detail_info", "environment_rating")),
				facility_rating = as.character(sapply(res.list$results, ".getElement", "detail_info", "facility_rating")),
				hygiene_rating = as.character(sapply(res.list$results, ".getElement", "detail_info", "hygiene_rating")),
				technology_rating = as.character(sapply(res.list$results, ".getElement", "detail_info", "technology_rating")),
				image_num = as.character(sapply(res.list$results, ".getElement", "detail_info", "image_num")),
				groupon_num = as.numeric(sapply(res.list$results, ".getElement", "detail_info", "groupon_num")),
				discount_num = as.numeric(sapply(res.list$results, ".getElement", "detail_info", "discount_num")),
				comment_num = as.character(sapply(res.list$results, ".getElement", "detail_info", "comment_num")),
				favorite_num = as.character(sapply(res.list$results, ".getElement", "detail_info", "favorite_num")),
				checkin_num = as.character(sapply(res.list$results, ".getElement", "detail_info", "checkin_num")),
				detail_url = as.character(sapply(res.list$results, ".getElement", "detail_info", "detail_url"))
		)	
	}
	return(res)
}




