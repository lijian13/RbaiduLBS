\name{BaiduLBS-class}
\docType{class}
\alias{BaiduLBS-class}
\alias{BaiduLBS}

\title{Class "BaiduLBS" }
\description{To use the APIs of Baidu LBS, you have to register a application first. 
The open platform will provide a unique App ID and App AK for every application. You can use the
AK to invoke each API. All the authorization information will be 
stored in this object of BaiduLBS.}
\section{Objects from the Class}{
Objects can be created by calls of the function \code{\link{createBaiduLBS}}. 
}
\section{Fields}{
	\describe{
	\item{\code{appPath}:}{Object of class \code{"character"}. Path of the application folder.}
	\item{\code{appID}:}{Object of class \code{"character"}. ID of the application.}
    \item{\code{appName}:}{Object of class \code{"character"}. Name of the application.}
    \item{\code{appAK}:}{Object of class \code{"character"}. Access key of the application. }
    \item{\code{appTimes}:}{Object of class \code{"character"}. Access times.}
    \item{\code{apiMsg}:}{Object of class \code{"character"}. Message.}
    \item{\code{webCurl}:}{Object of class \code{"character"}. Curl handle.}
    }
}

\section{Methods}{
  \describe{
    \item{\code{testaccess()}:}{Test connection.}
    \item{\code{list()}:}{List the fields of the object.}
	\item{\code{show()}:}{Show all the fields of the object.}
  }
}


\author{Jian li <rweibo@sina.com>}

\examples{
	showClass("BaiduLBS")
}
\keyword{classes}
