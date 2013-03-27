%% $Id$
\name{as.SpatialLinesDataFrame.trip}
\alias{as.SpatialLinesDataFrame.trip}

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
as.SpatialLinesDataFrame.trip(from)
%% as(from, "SpatialLinesDataFrame")
}


\arguments{
  
  \item{from}{\code{trip} object}

}


\author{Michael D. Sumner}


\examples{

## Continuing the example from '?trip-methods:
utils::example("trip-methods", package="trip",
               ask=FALSE, echo=FALSE)

as.SpatialLinesDataFrame.trip(tr)
as(tr, "SpatialLinesDataFrame")

}


\keyword{spatial}
\keyword{manip}
