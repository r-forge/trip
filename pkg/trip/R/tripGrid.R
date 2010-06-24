
## TODO:
##a version of tripGrid that takes Lines, so as.SpatialLinesDataFrame.trip, and then to grid

## allow sigmas argument for density version, for each line segment


## replaces tripGrid, old version is now called tripGrid.interp

tripGrid <-
function (x, grid = NULL, method = "pixellate", ...)
{
    if (method %in% c("kde", "count"))
        warning("kde and count methods no longer supported from trip_1.1-6 and will be ignored, see ?tripGrid.interp for legacy function")
    if (!is.null(list(...)$dur))
        stop("dur(ation) not necessary for this function from trip_1.1-6 and will be ignored - time sum is now exact\n see ?tripGrid.interp 
for legacy function")
    require(spatstat)
  g2ow <- function(x) {
        mn <- x@cellcentre.offset - x@cellsize/2
        mx <- mn + x@cells.dim * x@cellsize
        owin(c(mn[1], mx[1]), c(mn[2], mx[2]), mask = matrix(TRUE, x@cells.dim[2], x@cells.dim[1]),
	xy =list(x = seq(mn[1], mx[1], length = x@cells.dim[1]), y = seq(mn[2], mx[2], length = x@cells.dim[2])) )

#owin(c(mn[2], mx[2]), c(mn[1], mx[1]), mask = matrix(TRUE, x@cells.dim[2], x@cells.dim[1]),
#	xy =list(x = seq(mn[1], mx[1], length = x@cells.dim[1]), y = seq(mn[2], mx[2], length = x@cells.dim[2])) )
    }
    if (is.null(grid))
        grid <- makeGridTopology(x)
    res <- as.image.SpatialGridDataFrame(SpatialGridDataFrame(grid,
        data.frame(z = rep(0, prod(grid@cells.dim)))))
    tor <- x@TOR.columns
    trip.list <- split.data.frame(x[, tor], x[[tor[2]]])
    ow <- g2ow(grid)
    sm <- 0
    zero.lengths <- FALSE
    sz <- 0
    for (this in trip.list) {
        xs <- coordinates(this)[, 1]
        ys <- coordinates(this)[, 2]
        dt <- diff(unclass(this[[tor[1]]]))
        sm <- sm + sum(dt)
        x.psp <- psp(xs[-length(xs)], ys[-length(ys)], xs[-1],
            ys[-1], window = ow)
        lngths <- lengths.psp(x.psp)


        if (any(!lngths > 0)) {
            ## trim psp objects (0-lines give NaNs)

            zero.lengths <- TRUE
            zeros <- which(!lngths > 0)
            cc <- coordinates(this)[zeros, , drop = FALSE]
            x.ppp <- ppp(cc[, 1], cc[, 2], window = ow)
            if (method == "pixellate") {
                v <- pixellate(x.ppp, W = ow, weights = dt[zeros])$v
            }
            if (method == "density") {
                v <- density(x.ppp, ...)$v
            }

            res$z <- res$z + t(v)
            sz <- sz + sum(dt[zeros])
        }
        x.psp <- x.psp[lngths > 0]
        weights <- dt/ifelse(lngths > 0, lngths, .Machine$double.eps)
        weights <- weights[lngths > 0]
        if (method == "pixellate") {
            v <- pixellate(x.psp, W = ow, weights = weights)$v
        }
        if (method == "density") {
            v <- density(x.psp, ...)$v
        }
        res$z <- res$z + t(v)
    }
    if (zero.lengths) {
        warning("zero length lines present, time durations binned into cells assuming point-presence of degenerate line segment")
        cat("\n")
        if (method == "pixellate") {
        cat(paste("Total time of trips:", sm, "\n"))
        cat(paste("Total time without zero length lines:", sm -
            sz, "\n"))
    }
    }
    image2Grid(res, p4 = proj4string(x))
}


## cases

##  lines, dTime, pixellate - tripGrid
##  lines, sigma, density - tripGrid
##  ??lines, weights, pixellate - as.psp.trip (default is dTime)
##  points, weights, pixellate - as.ppp.trip
##  points, sigma, density - as.ppp.trip

## do we want IDs or times? (let the user do it?)
as.ppp.trip <- function(X) {
    as.ppp.SpatialPointsDataFrame(X)
}
setAs("trip", "ppp", function(from) as.ppp.trip(from))

as.psp.trip <- function(X) {
    split.X <- split(X, X[[getTORnames(X)[2]]])
    ow <- owin(bbox(X)[1,], bbox(X)[2,])

    as.psp.trip1 <- function(this, ow = NULL) {
        if (is.null(ow)) ow <- owin(bbox(this)[1,], bbox(this)[2,])
        tor <- getTORnames(this)
        cc <- coordinates(this)
        xs <- coordinates(this)[, 1]
        ys <- coordinates(this)[, 2]
        dt <- diff(unclass(this[[tor[1]]]))

       psp(xs[-length(xs)], ys[-length(ys)], xs[-1], ys[-1], window = ow, marks = dt)
    }
    ## there is no split.psp
    do.call("superimposePSP", lapply(split.X, as.psp.trip1, ow = ow))
}

setAs("trip", "psp", function(from) as.psp.trip(from))

## GIS integration
## as.trip.SpatialLinesDataFrame (use summary info - distance, time duration, ID)

as.trip.SpatialLinesDataFrame <- function(from) {
    split.from <- split(from, from[[getTORnames(from)[2]]])
    sdf <- summary(from)
    df <- data.frame(tripID = sdf$tripID, tripStart = sdf$tmins, tripEnd = sdf$tmaxs, tripDur = as.vector(sdf$tripDurationSeconds), row.names = sdf$tripID)
    lns <- vector("list", nrow(df))
    for (i in 1:length(lns)) {
        lns[[i]] <- Lines(list(Line(coordinates(split.from[[i]]))), ID = sdf$tripID[i])
    }
    SpatialLinesDataFrame(SpatialLines(lns, proj4string = CRS(proj4string(from))), df)
}

setAs("trip", "SpatialLinesDataFrame", as.trip.SpatialLinesDataFrame)


## tripGrid <- function(x, grid = NULL, method = "pixellate",...) {


##     ## deal with legacy
##     if (method %in% c("kde", "count")) warning("kde and count methods no longer supported from trip_1.1-6 and will be ignored, see ?tripGrid.interp for legacy function")

##     if (!is.null(list(...)$dur)) stop("dur(ation) not necessary for this function from trip_1.1-6 and will be ignored - time sum is now exact\n see ?tripGrid.interp for legacy function")
##     require(spatstat)
##     ## SpatialGridDataFrame to owin
##     g2ow <- function(x) {
##         mn <- x@cellcentre.offset - x@cellsize/2
##         mx <- mn + x@cells.dim * x@cellsize
##         owin(c(mn[1], mx[1]), c(mn[2], mx[2]), mask = matrix(TRUE, x@cells.dim[1], x@cells.dim[2]))
##     }
##     if (is.null(grid)) grid <- makeGridTopology(x)
##     res <- as.image.SpatialGridDataFrame(SpatialGridDataFrame(grid, data.frame(z = rep(0, prod(grid@cells.dim)))))
##     tor <- x@TOR.columns

##     trip.list <- split.data.frame(x[, tor], x[[tor[2]]])
##     ow <- g2ow(grid)
##     sm <- 0
##     zero.lengths <-  FALSE
##     sz <- 0
##     for (this in trip.list) {
##         xs <- coordinates(this)[,1]
##         ys <- coordinates(this)[,2]
##         dt <- diff(unclass(this[[tor[1]]]))
##         sm <- sm + sum(dt)

##         x.psp <- psp(xs[-length(xs)], ys[-length(ys)], xs[-1], ys [-1], window = ow)
##         lngths <- lengths.psp(x.psp)

##            if (any(!lngths > 0)) {
##                zero.lengths <- TRUE
##                zeros <- which(!lngths > 0)
##                cc <- coordinates(this)[zeros,,drop = FALSE]
##                #idx <- getGridIndex(cc, grid)
##                x.ppp <- ppp(cc[,1], cc[,2], window = ow)
##                if (method == "pixellate") {
##                    v <- pixellate(x.ppp, W = ow, weights = dt[zeros])$v
##                }
##                if (method == "density") {
##                    v <- density(x.ppp, ...)$v
##                }
##                res$z <- res$z + t(v)
##                sz <- sz + sum(dt[zeros])
##            }


##         ## line lengths may be zero
##         weights <- dt/ifelse(lngths > 0, lngths, .Machine$double.eps)
##         if (method == "pixellate") {
##             v <- pixellate(x.psp, W = ow, weights = weights)$v
##         }
##         if (method == "density") {
##             v <- density(x.psp,  ...)$v
##         }
##         res$z <- res$z + t(v)

##         #    x.psp <- psp(xs[i-1], ys[i-1], xs[i], ys[i], window = ow)
##         #    v <- dt[i-1] * pixellate(x.psp)$v / lengths.psp(x.psp)
##         #    res$z <- res$z + v
##         #}
##     }

##    # if (zero.lengths) {
##     #       if (zero.lengths) {
##      #          cat("\n")
## #
##  #           warning(paste("discrepancy in time sum is due to zero length line segments in trip", if (length(trip.list) > 1) "s", sep = ""))
##   #         }

##     if (zero.lengths) {
##         warning("zero length lines present, time durations summed into cells assuming point-presence of degenerate line segment")
##         cat("\n")
##         cat(paste("Total time of trips:", sm, "\n"))
##         cat(paste("Total time without zero length lines:", sm - sz, "\n"))

##     }
##     image2Grid(res, p4 = proj4string(x))
## }
