
##' Get static image and save it as png file.
##' 
##' @title Static image API.
##' @param location A numeric vecter with 2 elements indicating latitude and longitude.
##' @param outfile Path of the output png file.
##' @param width Width of the image.
##' @param height Height of the image.
##' @param zoom Zoom size.
##' @return The path of output file. 
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @references \url{http://developer.baidu.com/map/index.php?title=static}
##' @examples \dontrun{
##' getStaticImage(c(22.3087, 114.2019))
##' }

getStaticImage <- function(location, outfile = "staticimage.png", width = 400, height = 300, zoom = 19, 
		labels = FALSE, labelcontent = "\u6210\u529F\u7387: ", labelfontsize = 14, labelfontcol = "0xffffff",
		labelbgcol = "0x000fff"
) {
	
	location <- unlist(location)
	location <- paste(sort(location, decreasing = TRUE), collapse = ",")
	
	if (identical(labels, TRUE)) {
		strurl <- paste0("http://api.map.baidu.com/staticimage?center=",
				location, "&width=", width, "&height=", height, "&zoom=", zoom,
				"&labels=", location, "&labelStyles=", labelcontent, ",1,", 
				labelfontsize, ",", labelfontcol, ",", labelbgcol, ",1")
	} else {
		strurl <- paste0("http://api.map.baidu.com/staticimage?center=",
				location, "&width=", width, "&height=", height, "&zoom=", zoom,
				"&markers=", location, "&markerStyles=1,A")
	}
	
	res.bin <- getURLContent(strurl, binary = TRUE)
	res.png <- readPNG(res.bin)
	
	outfile <- normalizePath(outfile, mustWork = FALSE)
	writePNG(res.png, target = outfile)
	return(outfile)
	
}


