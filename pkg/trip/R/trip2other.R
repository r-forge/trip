# $Id$

ltraj2trip <- function (ltr)
{
    require(adehabitatLT)
    if (!inherits(ltr, "ltraj"))
        stop("ltr should be of class \"ltraj\"")
    ltr <-  lapply(ltr, function(x) {
        x$id=attr(x,  "id")
        x$burst=attr(x,  "burst")
        x})
    tr <- do.call("rbind", ltr)
    class(tr) <- "data.frame"
    xy <- tr[!is.na(tr$x), c("x", "y")]
    tr <- tr[!is.na(tr$x), ]
    tr$y <- tr$x <- NULL
    res <- SpatialPointsDataFrame(xy, tr)
    trip(res, c("date", "id"))
}

## ltraj from adehabitat

as.ltraj.trip <- function(xy, typeII=TRUE, slsp="remove") {
    require(adehabitatLT)
    tor <- getTORnames(xy)
    crds <- coordinates(xy)
    as.ltraj(as.data.frame(crds), date=xy[[tor[1]]],
             id=xy[[tor[2]]], typeII=typeII, slsp=slsp)
}

setAs("trip", "ltraj", function(from) as.ltraj.trip(from))
setAs("ltraj", "trip", function(from) ltraj2trip(from))

## cases

##  lines, dTime, pixellate - tripGrid
##  lines, sigma, density - tripGrid
##  ??lines, weights, pixellate - as.psp.trip (default is dTime)
##  points, weights, pixellate - as.ppp.trip
##  points, sigma, density - as.ppp.trip

## do we want IDs or times? (let the user do it?)
as.ppp.trip <- function(X, ..., fatal) {
    ## still needed? with importFrom(maptools,as.ppp.SpatialPointsDataFrame)?
    ##    require(spatstat)
    ## not needed, and not possible since classes imported using
    ## NAMESPACE MDS 2012-10-09
    ## suppressMessages(require(maptools
    ## require(maptools)
    as.ppp.SpatialPointsDataFrame(X)
}

setAs("trip", "ppp", function(from) as.ppp.trip(from))

## spatstat 1.22
##as.psp.trip <- function(X) {
as.psp.trip <- function(x, ..., from, to) {
    require(spatstat)
    split.X <- split(x, x[[getTORnames(x)[2]]])
    ow <- owin(bbox(x)[1,], bbox(x)[2,])
    as.psp.trip1 <- function(this, ow=NULL) {
        if (is.null(ow)) ow <- owin(bbox(this)[1,], bbox(this)[2,])
        tor <- getTORnames(this)
        cc <- coordinates(this)
        xs <- coordinates(this)[, 1]
        ys <- coordinates(this)[, 2]
        dt <- diff(unclass(this[[tor[1]]]))
        psp(xs[-length(xs)], ys[-length(ys)],
            xs[-1], ys[-1], window=ow, marks=dt)
    }
    ## there is no split.psp
    ## spatstat 1.22    do.call("superimposePSP", lapply(split.X, as.psp.trip1, ow=ow))
    do.call("superimpose", lapply(split.X, as.psp.trip1, ow=ow))
}

setAs("trip", "psp", function(from) as.psp.trip(from))

## GIS integration
## as.trip.SpatialLinesDataFrame (use summary info - distance, time duration, ID)

as.trip.SpatialLinesDataFrame <- function(from) {
    split.from <- split(from, from[[getTORnames(from)[2]]])
    sdf <- summary(from)
    df <- data.frame(tripID=sdf$tripID, tripStart=sdf$tmins,
                     tripEnd=sdf$tmaxs,
                     tripDur=as.vector(sdf$tripDurationSeconds),
                     row.names=sdf$tripID)
    lns <- vector("list", nrow(df))
    for (i in 1:length(lns)) {
        lns[[i]] <- Lines(list(Line(coordinates(split.from[[i]]))),
                          ID=sdf$tripID[i])
    }
    SpatialLinesDataFrame(SpatialLines(lns,
                                       proj4string=CRS(proj4string(from))),
                          df)
}

setAs("trip", "SpatialLinesDataFrame", as.trip.SpatialLinesDataFrame)
