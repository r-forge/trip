%% $Id$
\name{as.ltraj.trip}
\alias{as.ltraj.trip}

\alias{ltraj2trip}
\alias{coerce,trip,ltraj-method}
\alias{coerce,ltraj,trip-method}

\title{Coercion between trip objects and ltraj objects}

\description{coercion between classes}

\section{Methods}{
  
  \describe{
    
    \item{coerce}{\code{signature(from="trip", to="ltraj")}}
    
    \item{coerce}{\code{signature(from="ltraj", to="trip")}}
    
  }
}

 
\usage{

as.ltraj.trip(xy, typeII=TRUE, slsp="remove")
ltraj2trip(ltr)

}


\arguments{
  
  \item{xy}{trip object}
  
  \item{typeII}{see \code{\link[adehabitatLT]{as.ltraj}}}
  
  \item{slsp}{details for the \code{\link[adehabitatLT]{ltraj}} turning
    angles}
  
  \item{ltr}{ltraj object}
  
}


\author{Michael D. Sumner}


\examples{

## Continuing the example from '?trip-methods:
utils::example("trip-methods", package="trip",
               ask=FALSE, echo=FALSE)

if (require(adehabitatLT)) {
    l <- as.ltraj.trip(tr)
    ltraj2trip(l)
}

}


\keyword{spatial}
\keyword{manip}
