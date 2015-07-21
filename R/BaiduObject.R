
setRefClass("BaiduLBS", 
		fields = list(
				appPath = "character", 
				appID = "character", 
				appName = "character", 
				appAK = "character", 
				apiMsg = "character",
				webCurl = "ANY"
		),
		
		methods = list(
			initialize = function(appID) {
				.self$appPath <- getOption("baidu.ak.path")
				.self$appID <- appID
				applist <- listKeys(appID)[[appID]]
				.self$appName <- applist$app_name
				.self$appAK <- applist$app_ak
				.self$apiMsg <- "Can not access baidu API!"
				.self$webCurl <- getCurlHandle()
				
			},	
			testaccess = function() {
				
				OUT <- FALSE
				strurl <- paste0("http://api.map.baidu.com/geocoder/v2/?address=%B0%D9%B6%C8%B4%F3%CF%C3&output=json&ak=",
						.self$appAK)
				resjson <- try(getURL(strurl, ssl.verifypeer = FALSE, curl = .self$webCurl, .encoding = "UTF-8"), silent = TRUE)
				reslist <- try(.fromJSON(json = resjson), silent = TRUE)
				if (is.list(reslist) && identical(reslist$status, 0)) OUT <- TRUE
				return(OUT)
			},
			list = function() {
				OUT <- base::list(
						"appPath" = .self$appPath, 
						"appID" = .self$appID, 
						"appName" = .self$appName, 
						"appAK" = .self$appAK
				)
				return(OUT)
			}
		)
)


setMethod("show", signature="BaiduLBS", 
		function(object) {
			print(paste("Application: ", object$appID, " (", object$appName, ")", sep = ""))
			print(object$apiMsg)
		}
)






