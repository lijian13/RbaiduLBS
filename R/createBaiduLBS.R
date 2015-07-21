

##' Create an authorized object
##' 
##' @title Create an Baidu LBS object
##' @param app_id ID of the application.
##' @return An reference object of \code{\link{BaiduLBS}}.
##' @note There is only one BaiduLBS object needed.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @seealso \code{\link{registerKey}}
##' @references \url{http://developer.baidu.com/map}
##' @keywords authorization
##' @examples \dontrun{
##' 
##' baidu <- createBaiduLBS("1234567")
##' }
createBaiduLBS <- function(app_id) {
	oauthobj <- new("BaiduLBS", appID = app_id)
	
	if (oauthobj$testaccess()) {
		oauthobj$apiMsg <- "Access Baidu LBS API successfully!"
	}
	
	return(oauthobj)
}



