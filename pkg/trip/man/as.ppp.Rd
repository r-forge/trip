%% $Id$
\name{as.ppp.trip}
\alias{as.ppp.trip}

\alias{as.psp.trip}
\alias{as.ppp}
\alias{as.psp}
\alias{coerce,trip,psp-method}
\alias{coerce,trip,ppp-method}


\title{Coercion between trip objects and spatstat objects}


\description{coercion between classes}


\section{Methods}{

  \describe{
    
    \item{coerce}{\code{signature(from="trip", to="ppp")}}
    
    \item{coerce}{\code{signature(from="trip", to="psp")}}
     
  }
}

 
\usage{

\method{as.ppp}{trip}(X, \ldots, fatal)
\method{as.psp}{trip}(x, \ldots, from, to)

}


\arguments{
  \item{X, x}{trip object}
  \item{\ldots}{Ignored}
  \item{fatal}{Logical value, see Details of as.ppp}
  \item{from, to}{See as.psp}
}


\author{Michael D. Sumner}


\examples{

## Continuing the example from '?trip-methods:
utils::example("trip-methods", package="trip",
               ask=FALSE, echo=FALSE)

if (require(spatstat)) {
    as.ppp(tr)
    as.psp(tr)
}

}


\keyword{spatial}
\keyword{manip}
