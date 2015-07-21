

.fromJSON <- function(json, api = c("rjson", "RJSONIO"), ...) {
	api <- match.arg(api)
	iscontent <- inherits(json, "AsIs") || (!file.exists(json) && length(grep("^[[:space:]]*[[{]", json)))
	
	if (api == "rjson") {
		if (iscontent) {
			OUT <- rjson:::fromJSON(json_str = json)
		} else {
			OUT <- rjson:::fromJSON(file = json)
		}
	}
	
	if (api == "RJSONIO") {
		OUT <- RJSONIO:::fromJSON(content = json, ...)
	}
	
	return(OUT)
}

.getElement <- function(object, name1, name2) {
	if (missing(name2)) {
		res <- object[[name1, exact = TRUE]]
	} else {
		res <- object[[name1, exact = TRUE]][[name2, exact = TRUE]]
	}
	
	if (is.null(res)) res <- NA
	return(res)
}



