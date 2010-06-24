\name{trackDistance}
\alias{trackDistance}
%\alias{gcdist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Determine distance along a track}
\description{

  Calculate the distance between subsequent 2-D coordinates using
     Euclidean or Great Circle distance (WGS84 ellipsoid) methods.

}
\usage{
trackDistance(track, longlat = FALSE)

%gcdist(lon1, lat1, lon2, lat2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{matrix of 2-columns, with x/y coordinates }
  \item{longlat}{if FALSE, Euclidean distance, if TRUE Great Circle distance}

 % \item{lon1, lon2}{vectors of paired longitude coordinates}
 % \item{lat1, lat2}{vectors of paired latitude coordinates}

  %\item{push}{an offset for measuring distance between this and the next (push)
 %location}
}
\details{

  trackDistance calls source code identical to sp's spDistN when longlat is TRUE.

  This originally used \code{spDistsN1} but now implements the sp gcdist
  source directly in R.

}
\references{
  Original source taken from sp package.


  }
\value{
Vector of distances between coordinates.
}

\keyword{manip}% at least one, from doc/KEYWORDS

