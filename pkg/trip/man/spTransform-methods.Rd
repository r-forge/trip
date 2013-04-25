%% $Id$
\name{spTransform-methods}
\docType{methods}
\alias{spTransform-methods}
\alias{spTransform}
%- Also NEED an '\alias' for EACH other topic documented here.

\alias{spTransform,trip,CRS-method}
\alias{spTransform,trip,character-method}


\title{Reproject trip objects}


\description{
  Projection transformation based on CRS strings in rgdal
}


\section{Methods}{
  \describe{

    \item{\code{signature(x="trip", CRSobj="CRS")}}{ Given a trip object
      with projection metadata, returns a a trip object with its
      coordinates transformed using the projection arguments in
      \code{CRSobj}, of class \code{CRS}. }

    \item{\code{signature(x="trip", CRSobj="character")}}{ Same as
      signature above, but allowing \code{CRSobj} of class
      \code{character}. }

    \item{\dots}{Further arguments to \code{\link[rgdal]{spTransform}}.}

}


\value{

  \code{trip} object, with spatial coordinates reprojected.

}


\author{ Michael D. Sumner }


\seealso{
\code{\link[rgdal]{spTransform}}
}


\examples{

## Continuing the example from '?trip-methods:
utils::example("trip-methods", package="trip",
               ask=FALSE, echo=FALSE)

proj4string(tr) <- CRS("+proj=laea +lon_0=146")
spTransform(tr, "+proj=longlat")

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
\keyword{manip}
