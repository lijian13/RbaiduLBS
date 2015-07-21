
##' Search detail of a POI.
##' 
##' @title Search detail of a POI.
##' @param uid ID of POI.
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
##'  \item{atmosphere}{Atmosphere.}
##'  \item{featured_service}{Featured service.}
##'  \item{alias}{Alias of the POI.}
##'  \item{description}{Description of the POI.}
##'  \item{detail_url}{URL of the page.}
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @references \url{http://developer.baidu.com/map/index.php?title=webapi/guide/webservice-placeapi}
##' @examples \dontrun{
##' searchPOI("72aa251304ef0b5e067ce896")
##' }

searchPOI <- function(uid, ak = NULL) {
	if (is.null(ak)) ak <- getOption("baidu.ak")
	strurl <- paste0("http://api.map.baidu.com/place/v2/detail?uid=",
			uid, "&output=json&scope=2&ak=", ak)
	res.json <- getURL(strurl, ssl.verifypeer = FALSE, 
			curl = getCurlHandle(), .encoding = "UTF-8")
	res.list <- fromJSON(json_str = res.json)
	
	res <- data.frame(uid = uid,
			name = .getElement(res.list$result, "name"),
			lat = .getElement(res.list$result, "location", "lat"),
			lng = .getElement(res.list$result, "location", "lng"),
			address = .getElement(res.list$result, "address"),
			telephone = .getElement(res.list$result, "telephone"),
			type = .getElement(res.list$result, "detail_info", "type"),
			tag = .getElement(res.list$result, "detail_info", "tag"),
			price = .getElement(res.list$result, "detail_info", "price"),
			shop_hours = .getElement(res.list$result, "detail_info", "shop_hours"),
			overall_rating = .getElement(res.list$result, "detail_info", "overall_rating"),
			taste_rating = .getElement(res.list$result, "detail_info", "taste_rating"),
			service_rating = .getElement(res.list$result, "detail_info", "service_rating"),
			environment_rating = .getElement(res.list$result, "detail_info", "environment_rating"),
			facility_rating = .getElement(res.list$result, "detail_info", "facility_rating"),
			hygiene_rating = .getElement(res.list$result, "detail_info", "hygiene_rating"),
			technology_rating = .getElement(res.list$result, "detail_info", "technology_rating"),
			image_num = .getElement(res.list$result, "detail_info", "image_num"),
			groupon_num = .getElement(res.list$result, "detail_info", "groupon_num"),
			discount_num = .getElement(res.list$result, "detail_info", "discount_num"),
			comment_num = .getElement(res.list$result, "detail_info", "comment_num"),
			favorite_num = .getElement(res.list$result, "detail_info", "favorite_num"),
			checkin_num = .getElement(res.list$result, "detail_info", "checkin_num"),
			atmosphere = .getElement(res.list$result, "detail_info", "atmosphere"),
			featured_service = .getElement(res.list$result, "detail_info", "featured_service"),
			alias = .getElement(res.list$result, "detail_info", "alias"),
			description = .getElement(res.list$result, "detail_info", "description"),
			detail_url = .getElement(res.list$result, "detail_info", "detail_url")
	)	
	
	return(res)
}




