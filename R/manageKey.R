


##' @description Reigster, modify and delete application.
##' 
##' @title Reigster, modify and delete application.
##' @name Manage keys
##' @aliases registerKey modifyKey deleteKey listKeys
##' @param app_id ID of an application
##' @param app_name name of an application
##' @param app_ak access key of an application
##' @return a logical value 
##' @note You should register an application on baidu firstly.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @references \url{http://developer.baidu.com/map}
##' @keywords authorization
##' @examples \dontrun{
##' registerKey(app_id = "1234567", "test", "MCD8BKwGdgPHvAuvgvz4EQpD")
##' listKeys("1234567")
##' modifyKey("1234567", "t1", "t2")
##' deleteKey("1234567")
##' }
registerKey <- function(app_id, app_name, app_ak) {
	app_id <- as.character(app_id)
	app_name <- as.character(app_name)
	app_ak <- as.character(app_ak)
	apppath <- getOption("baidu.ak.path")
	if (!file.exists(apppath)) dir.create(apppath)
	if (file.exists(file.path(apppath, app_id))) {
		warning(paste0("The key '", app_id, "' has been registered, please use 'modifyKey' to make change."))
		invisible(FALSE)
	} else {
		applist <- list(app_id = app_id, app_name = app_name, app_ak = app_ak)
		appfile <- file(file.path(apppath, app_id) , open = "w" )
		writeLines(toJSON(applist), appfile)
		close(appfile)
		invisible(TRUE)
	}
	if (is.null(getOption("baidu.ak"))) options(baidu.ak = app_ak)
}

##' @name Manage keys
##' @aliases registerKey modifyKey deleteKey listKeys
modifyKey <- function(app_id, app_name, app_ak) {
	apppath <- getOption("baidu.ak.path")
	if (!file.exists(apppath)) dir.create(apppath)
	if (app_id %in% list.files(apppath)) {
		applist <- .fromJSON(file.path(apppath, app_id))
		applist$app_name <- app_name
		applist$app_ak <- app_ak
		appfile <- file(file.path(apppath, app_id) , open = "w" )
		writeLines(toJSON(applist), appfile)
		close(appfile)
	} else {
		stop(paste(app_name, "doesn't exist, please use 'registerKey' to create"))
	}
	return(TRUE)
}

##' @name Manage keys
##' @aliases registerKey modifyKey deleteKey listKeys
deleteKey <- function(app_id) {
	apppath <- getOption("baidu.ak.path")
	if (!file.exists(apppath)) dir.create(apppath)
	if (file.exists(file.path(apppath, app_id))) {
		unlink(file.path(apppath, app_id))
	} else {
		stop(paste(app_id, "doesn't exist"))
	}
	return(TRUE)
}

##' @name Manage keys
##' @aliases registerKey modifyKey deleteKey listKeys
listKeys <- function(app_id) {
	apppath <- getOption("baidu.ak.path")
	if (!file.exists(apppath)) dir.create(apppath)
	appfiles <- list.files(apppath, full.names = FALSE)
	
	if (missing(app_id)) {
		app_id <- appfiles
	}
	
	applist <- list()
	for (i in 1:length(app_id)) {
		if (app_id[i] %in% appfiles) {
			applist[[app_id[i]]] <- .fromJSON(file.path(apppath, app_id[i]))
		}
	}
	
	return(applist)

}

