%% $Id$
\name{speedfilter}
\alias{speedfilter}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Filter track data for speed}


\description{
  
 Create a filter of a track for "bad" points implying a speed of motion
 that is unrealistic.
 
}


\usage{
speedfilter(x, max.speed=NULL, test=FALSE)
}
%- maybe also 'usage' for other objects documented here.


\arguments{
  
  \item{x}{trip object}

  \item{max.speed}{speed in kilometres per hour}

  \item{test}{cut the algorithm short and just return first pass}
  
}


\details{

  Using an algorithm (McConnnell et al, 1992), points are tested for
  speed between previous / next and 2nd previous / next points.
  Contiguous sections with an root mean square speed above a given
  maximum have their highest rms point removed, then rms is
  recalculated, until all points are below the maximum.  By default an
  (internal) root mean square function is used, this can be specified by
  the user.
     
  If the coordinates of the \code{trip} data are not projected, or NA
  the distance calculation assumeds longlat and kilometres (great
  circle). For projected coordinates the speed must match the units of
  the coordinate system.  (The PROJ.4 argument "units=km" is suggested).

}


\value{
  
 Logical vector matching positions in the coordinate records that pass
 the filter.
 
}


\references{

  The algorithm comes from McConnell, B. J. and Chambers, C. and Fedak,
  M. A. (1992) Foraging ecology of southern elephant seals in relation
  to the bathymetry and productivity of the southern ocean.  Antarctic
  Science \emph{4} 393-398

}


\author{Michael D. Sumner}


\note{

  This algorithm was originally taken from IDL code by David Watts at
  the Australian Antarctic Division, and used in various other
  environments before the development of this version.

}
   

\section{Warning}{

  This algorithm is not considered to be particularly relevant to the
  problems involved with location uncertainty in animal tracking.  It is
  provided merely as an illustrative benchmark for further work.

  It is possible for the filter to become stuck in an infinite loop,
  depending on the function passed to the filter.  Several minutes is
  probably too long for hundreds of points, test on smaller sections if
  unsure.

}


\seealso{\code{\link{trip}}}


\keyword{manip}% at least one, from doc/KEYWORDS
