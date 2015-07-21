# TODO: Add comment
# 
# Author: jli
###############################################################################

.onAttach <- function(libname, pkgname ){
	options(baidu.ak.path = file.path(system.file(package = "RbaiduLBS"), "keys"))
	akfile <- list.files(getOption("baidu.ak.path"), full.names = FALSE)
	if (length(akfile) > 0) {
		akfile <- akfile[1]
		akcode <- try(listKeys(akfile)[[akfile]][["app_ak"]], silent = TRUE)
		if (inherits(akcode, "character")) options(baidu.ak = akcode)
	}
	packageStartupMessage( paste("# RbaiduLBS Version:", packageDescription("RbaiduLBS", fields = "Version")) )
}

