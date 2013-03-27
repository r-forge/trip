%% $Id$
\name{as.trip.SpatialLinesDataFrame}
\alias{as.trip.SpatialLinesDataFrame}

\alias{coerce,trip,SpatialLinesDataFrame-method}


\title{Coercion between trip objects and sp line objects}


\description{coercion between classes}


\section{Methods}{
  
  \describe{
    
    \item{coerce}{\code{signature(from="trip",
	to="SpatialLinesDataFrame")}}
     
  }
}

 
\usage{
as.trip.SpatialLinesDataFrame(from)
%% as(from, "SpatialLinesDataFrame")
}


\arguments{
  
  \item{from}{trip object}

}


\author{Michael D. Sumner}


\examples{

## Continuing the example from '?trip-methods:
utils::example("trip-methods", package="trip",
               ask=FALSE, echo=FALSE)

as.trip.SpatialLinesDataFrame(tr)
as(tr, "SpatialLinesDataFrame")

}


\keyword{spatial}
\keyword{manip}
