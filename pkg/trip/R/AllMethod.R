# $Id$

###_ + trip

setMethod("trip", signature(obj="ANY", TORnames="TimeOrderedRecords"),
          function(obj, TORnames) {
              new("trip", obj, TORnames)
          })

setMethod("trip", signature(obj="trip", TORnames="TimeOrderedRecords"),
          function(obj, TORnames) {
              new("trip",
                  as(obj, "SpatialPointsDataFrame"),
                  TORnames)
          })

setMethod("trip", signature(obj="trip", TORnames="ANY"),
          function(obj, TORnames) {
              ##trip.default(as(obj, "SpatialPointsDataFrame"), TORnames)
              trip(as(obj, "SpatialPointsDataFrame"), TORnames)
          })

setReplaceMethod("[[", c("trip", "ANY", "missing", "ANY"),
                 function(x, i, j, value) {
                     tor <- getTORnames(x)
                     x <- as(x, "SpatialPointsDataFrame")
                     x[[i]] <- value
                     trip(x, tor)
                 })

###_ + sp methods

setMethod("points", "trip",
          function(x, ...) points(as(x, "SpatialPointsDataFrame"), ...))
setMethod("text", "trip",
          function(x, ...) text(as(x, "SpatialPointsDataFrame"), ...))

#setMethod("split", "SpatialPointsDataFrame", split.data.frame)

## MDS 2010-07-06
setMethod("lines", signature(x="trip"),
          function(x,
                   col=hsv(seq(0, 0.9, length=length(summary(x)$tripID)),
                     0.8, 0.95),
                   ...) {
              plot(as(x, "SpatialLinesDataFrame"),  col=col, add=TRUE, ...)
          })

setMethod("plot", signature(x="trip", y="missing"),
          function(x, y, ...) {
              plot(as(x, "SpatialPoints"), ...)
          })


###_ + Subsetting trip

subset.trip <- function(x,  ...) {
    spdf <- subset(as(x, "SpatialPointsDataFrame"), ...)
    tor <- getTORnames(x)
    if ( is.factor(spdf[[tor[2]]]))
        spdf[[tor[2]]] <- factor(spdf[[tor[2]]])
    if (any(is.na(match(tor, names(spdf))))) {
        msg <- paste("trip-defining Date or ID columns dropped,",
                     "reverting to SpatialPointsDataFrame\n\n")
        cat(msg)
        return(spdf)
    } else if (any(tapply(spdf[[tor[1]]], spdf[[tor[2]]], length) < 3)) {
        msg <- paste("subset loses too many locations,",
                     "reverting to SpatialPointsDataFrame\n\n")
        cat(msg)
        return(spdf)
    } else {
        return(trip(spdf, tor))
    }
}

setMethod("subset", "trip", subset.trip)

setMethod("[", "trip",
          function(x, i, j, ... , drop=TRUE) {
              missing.i <- missing(i)
              missing.j <- missing(j)
              nargs <- nargs() # e.g., a[3,] gives 2 for nargs, a[3] gives 1.
              if (missing.i && missing.j) {
                  i <- j <- TRUE
              } else if (missing.j && !missing.i) {
                  if (nargs == 2) {
                      j <- i; i <- TRUE
                  } else j <- TRUE
              } else if (missing.i && !missing.j) i <- TRUE
              if (is.matrix(i)) {
                  msg <- paste("matrix argument not supported in",
                               "SpatialPointsDataFrame selection")
                  stop(msg)
              }
              if (any(is.na(i)))
                  stop("NAs not permitted in row index")
              spdf <- as(x, "SpatialPointsDataFrame")[i, j, ..., drop=drop]
              tor <- getTORnames(x)
              if (is.factor(spdf[[tor[2]]]))
                  spdf[[tor[2]]] <- factor(spdf[[tor[2]]])
              if (any(is.na(match(tor, names(spdf))))) {
                  msg <- paste("trip-defining Date or ID columns dropped,",
                               "reverting to SpatialPointsDataFrame\n\n")
                  cat(msg)
                  return(spdf)
              } else {
                  tst <- any(tapply(spdf[[tor[1]]],
                                    spdf[[tor[2]]], length) < 3)
                  if (tst) {
                      msg <- paste("subset loses too many locations,",
                                   "reverting to SpatialPointsDataFrame\n\n")
                      cat(msg)
                      return(spdf)
                  } else {
                      return(trip(spdf, tor))
                  }
              }
          })


###_ + Summary, print, and show

summary.tordata <- function(object, ...) {
    obj <- list(spdf=summary(as(object, "SpatialPointsDataFrame")))
    ## method or not here?
    time <- object[[object@TOR.columns[1]]]
    ids <- object[[object@TOR.columns[2]]]
    tmins <- tapply(time, ids, min) +
        ISOdatetime(1970, 1, 1, 0, 0,0, tz="GMT")
    tmaxs <- tapply(time, ids, max) +
        ISOdatetime(1970, 1, 1, 0, 0,0, tz="GMT")
    nlocs <- tapply(time, ids, length)
    obj[["class"]] <- class(object)
    obj[["tmins"]] <- tmins
    obj[["tmaxs"]] <- tmaxs
    obj[["tripID"]] <- levels(factor(ids))
    obj[["nRecords"]] <- nlocs
    obj[["TORnames"]] <- getTORnames(object)
    obj[["tripDuration"]] <- tapply(time, ids, function(x) {
        x <- format(diff(range(x)))
    })
    obj[["tripDurationSeconds"]] <- tapply(time, ids, function(x) {
        x <- diff(range(unclass(x)))
    })
    class(obj) <- "summary.tordata"
    ## invisible(obj)
    obj
}

setMethod("summary", "trip", summary.tordata)

print.summary.tordata <- function(x, ...) {
    dsumm <- data.frame(tripID=x$tripID,
                        No.Records=x$nRecords,
                        startTime=x$tmins,
                        endTime=x$tmaxs,
                        tripDuration=x$tripDuration)
    names(dsumm)[1] <- paste(names(dsumm)[1],
                             " (\"", x[["TORnames"]][2], "\")", sep="")
    names(dsumm)[3] <- paste(names(dsumm)[3],
                             " (\"", x[["TORnames"]][1], "\")", sep="")
    names(dsumm)[4] <- paste(names(dsumm)[4],
                             " (\"", x[["TORnames"]][1], "\")", sep="")
    rownames(dsumm) <- 1:nrow(dsumm)
    ## dsumm <- as.data.frame(lapply(dsumm, as.character))
    cat(paste("\nObject of class ", x[["class"]], "\n", sep=""))
    print(format(dsumm, ...))
    tripDurationSeconds <- sum(x$tripDurationSeconds)
    tripDurationHours <- sum(x$tripDurationSeconds) / 3600
    cat(paste("\nTotal trip duration: ",
              tripDurationSeconds, " seconds (",
              as.integer(tripDurationHours), " hours, ",
              round((tripDurationHours -
                     as.integer(tripDurationHours)) * 3600),
              " seconds)\n", sep=""))
    cat(paste("\nDerived from Spatial data:\n\n", sep=""))
    print(x$spdf)
    cat("\n")
}

print.trip <- function(x, ...) {
    xs <- summary(x)
    dsumm <- data.frame(tripID=xs$tripID,
                        No.Records=xs$nRecords,
                        startTime=xs$tmins,
                        endTime=xs$tmaxs,
                        tripDuration=xs$tripDuration)
    names(dsumm)[1] <- paste(names(dsumm)[1], " (\"",
                             xs[["TORnames"]][2], "\")", sep="")
    names(dsumm)[3] <- paste(names(dsumm)[3], " (\"",
                             xs[["TORnames"]][1], "\")", sep="")
    names(dsumm)[4] <- paste(names(dsumm)[4], " (\"",
                             xs[["TORnames"]][1], "\")", sep="")
    rownames(dsumm) <- 1:nrow(dsumm)
    ## dsumm <- as.data.frame(lapply(dsumm, as.character))
    cat(paste("\nObject of class ", xs[["class"]], "\n", sep=""))
    print(format(dsumm, ...))
    cat("\n")
    nms <- names(x)
    clss <- unlist(lapply(as.data.frame(x@data),  function(x) class(x)[1]))
    sdf <- data.frame(data.columns=nms, data.class=clss)
    sdf[[" "]] <- rep("", nrow(sdf))
    sdf[[" "]][which(names(x) == xs[["TORnames"]][1])] <- "**trip DateTime**"
    sdf[[" "]][which(names(x) == xs[["TORnames"]][2])] <- "**trip ID**      "
    row.names(sdf) <- 1:nrow(sdf)
    print(sdf)
    cat("\n")
}

setMethod("show", "trip", function(object) print.trip(object))

## setMethod("print", "trip",
##           function(x, ...) print(as(x, "SpatialPointsDataFrame")))

recenter.trip <- function(obj) {
    proj <- is.projected(obj)
    if (is.na(proj)) {
        warning("unknown coordinate reference system: assuming longlat")
        ## projargs <- CRS("+proj=longlat")
    }
    if (!is.na(proj) & proj)
        stop("cannot recenter projected coordinate reference system")
    projargs <- CRS(proj4string(obj))
    crds <- coordinates(obj)
    inout <- (crds[, 1] < 0)
    if (all(inout)) {
        crds[, 1] <- crds[, 1] + 360
        if (!is.na(proj)) projargs <- CRS(paste(proj4string(obj), "+over"))
    } else {
        if (any(inout)) {
            crds[, 1] <- ifelse(inout, crds[, 1] + 360, crds[, 1])
            if (!is.na(proj))
                projargs <- CRS(paste(proj4string(obj), "+over"))
        }
    }
    trip(new("SpatialPointsDataFrame",
             SpatialPoints(crds, projargs),
             data=obj@data, coords.nrs=obj@coords.nrs),
         obj@TOR.columns)
}

setMethod("recenter", "trip", recenter.trip)



###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:
