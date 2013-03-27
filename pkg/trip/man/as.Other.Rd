%% $Id$
\name{as.SpatialLinesDataFrame.trip}
\alias{as.SpatialLinesDataFrame.trip}

\alias{coerce,trip,SpatialLinesDataFrame-method}
\alias{as.ltraj.trip}
\alias{coerce,ltraj,trip-method}
\alias{as.ppp.trip}
\alias{as.psp.trip}
\alias{coerce,trip,psp-method}
\alias{coerce,trip,ppp-method}


\title{Coercion from \code{trip} objects to other classes}


\description{Coercing \code{trip} objects to other classes.}


\section{Methods}{
  
  \describe{
    
    \item{coerce}{\code{signature(from="trip",
	to="SpatialLinesDataFrame")}}

    \item{coerce}{\code{signature(from="trip", to="ltraj")}}

    \item{coerce}{\code{signature(from="trip", to="ppp")}}
    
    \item{coerce}{\code{signature(from="trip", to="psp")}}
    
  }
}

 
\usage{
as.SpatialLinesDataFrame.trip(from)
%% as(from, "SpatialLinesDataFrame")

as.ltraj.trip(xy, typeII=TRUE, slsp="remove")

\S3method{as.ppp}{trip}(X, \ldots, fatal)
\S3method{as.psp}{trip}(x, \ldots, from, to)

}


\arguments{
  
  \item{from, X, x}{\code{trip} object. For \code{from} in
    \code{as.psp}, see \code{\link[spatstat]{as.psp}}.}

  \item{xy}{trip object.}
  
  \item{typeII}{see \code{\link[adehabitatLT]{as.ltraj}}.}
  
  \item{slsp}{details for the \code{\link[adehabitatLT]{ltraj}} turning
    angles.}

  \item{\ldots}{Ignored.}

  \item{fatal}{Logical value, see Details of
    \code{\link[spatstat]{as.ppp}}.}

  \item{to}{See \code{\link[spatstat]{as.psp}}.}
  
}


\author{Michael D. Sumner}


\examples{

## Continuing the example from '?trip-methods:
utils::example("trip-methods", package="trip",
               ask=FALSE, echo=FALSE)

as.SpatialLinesDataFrame.trip(tr)
as(tr, "SpatialLinesDataFrame")

if (require(adehabitatLT)) {
    as.ltraj.trip(tr)
}

if (require(spatstat)) {
    as.ppp(tr)
    as.psp(tr)
}

}


\keyword{spatial}
\keyword{manip}
