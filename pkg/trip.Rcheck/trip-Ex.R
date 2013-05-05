pkgname <- "trip"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('trip')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("TimeOrderedRecords-class")
### * TimeOrderedRecords-class

flush(stderr()); flush(stdout())

### Name: TimeOrderedRecords-class
### Title: Class "TimeOrderedRecords"
### Aliases: TimeOrderedRecords-class trip,ANY,TimeOrderedRecords-method
###   trip,trip,TimeOrderedRecords-method
### Keywords: classes

### ** Examples

tor <- new("TimeOrderedRecords", TOR.columns = c("datetime", "ID"))
tor <- TimeOrderedRecords(c("datetime", "ID"))



cleanEx()
nameEx("TimeOrderedRecords")
### * TimeOrderedRecords

flush(stderr()); flush(stdout())

### Name: TimeOrderedRecords
### Title: Functions to specify and obtain DateTime and ID data from within
###   (Spatial) data frames.
### Aliases: TimeOrderedRecords getTORnames getTimeID
### Keywords: manip

### ** Examples


tor <- TimeOrderedRecords(c("time", "id"))
getTORnames(tor)




cleanEx()
nameEx("adjust.duplicateTimes")
### * adjust.duplicateTimes

flush(stderr()); flush(stdout())

### Name: adjust.duplicateTimes
### Title: Adjust duplicate DateTime values
### Aliases: adjust.duplicateTimes
### Keywords: manip

### ** Examples

    ## DateTimes with a duplicate within ID
    tms <- Sys.time() + c(1:6, 6, 7:10) *10 
    id <- rep("a", length(tms))
    range(diff(tms))
    
    ## duplicate record is now moved one second forward
    tms.adj <- adjust.duplicateTimes(tms, id)
    range(diff(tms.adj))



cleanEx()
nameEx("argos.sigma")
### * argos.sigma

flush(stderr()); flush(stdout())

### Name: argos.sigma
### Title: Assign numeric values for Argos "class"
### Aliases: argos.sigma
### Keywords: manip

### ** Examples

cls <- ordered(sample(c("Z", "B", "A", "0", "1", "2", "3"), 30, replace = TRUE),
            levels = c("Z", "B", "A", "0", "1", "2", "3"))
argos.sigma(cls)



cleanEx()
nameEx("as.SpatialLinesDataFrame")
### * as.SpatialLinesDataFrame

flush(stderr()); flush(stdout())

### Name: as.trip.SpatialLinesDataFrame
### Title: Coercion between trip objects and sp line objects
### Aliases: as.trip.SpatialLinesDataFrame
###   coerce,trip,SpatialLinesDataFrame-method
### Keywords: spatial manip

### ** Examples

d <- data.frame(x = 1:10, y = rnorm(10), tms = Sys.time() + 1:10, id = gl(2, 5))

coordinates(d) <- ~x+y

tr <- trip(d, c("tms", "id"))

as.trip.SpatialLinesDataFrame(tr)

as(tr, "SpatialLinesDataFrame")




cleanEx()
nameEx("as.ltraj")
### * as.ltraj

flush(stderr()); flush(stdout())

### Name: as.ltraj.trip
### Title: Coercion between trip objects and ltraj objects
### Aliases: as.ltraj.trip ltraj2trip coerce,trip,ltraj-method
###   coerce,ltraj,trip-method
### Keywords: spatial manip

### ** Examples

d <- data.frame(x = 1:10, y = rnorm(10), tms = Sys.time() + 1:10, id = gl(2, 5))

coordinates(d) <- ~x+y

tr <- trip(d, c("tms", "id"))
require(adehabitatLT)
l <- as.ltraj.trip(tr)

ltraj2trip(l)





cleanEx()
nameEx("as.ppp")
### * as.ppp

flush(stderr()); flush(stdout())

### Name: as.ppp.trip
### Title: Coercion between trip objects and spatstat objects
### Aliases: as.ppp.trip as.psp.trip as.ppp as.psp coerce,trip,psp-method
###   coerce,trip,ppp-method
### Keywords: spatial manip

### ** Examples

d <- data.frame(x = 1:10, y = rnorm(10), tms = Sys.time() + 1:10, id = gl(2, 5))

coordinates(d) <- ~x+y

tr <- trip(d, c("tms", "id"))

require(spatstat)

as.ppp(tr)

as.psp(tr)




cleanEx()
nameEx("filter.penSS")
### * filter.penSS

flush(stderr()); flush(stdout())

### Name: filter.penSS
### Title: Non-destructive smoothing filter.
### Aliases: filter.penSS
### Keywords: ~kwd1 ~kwd2

### ** Examples

## Not run: 
##D ## Example takes a few minutes
##D 
##D ## Fake some data
##D 
##D ## Brownian motion tethered at each end
##D brownian.bridge <- function(n, r) {
##D   x <- cumsum(rnorm(n, 0, 1))
##D   x <- x - (x[1] + seq(0, 1, length = n) * (x[n] - x[1]))
##D   r * x
##D }
##D 
##D ## Number of days and number of obs
##D days <- 50
##D n <- 200
##D 
##D ## Make separation between obs gamma distributed
##D x <- rgamma(n, 3)
##D x <- cumsum(x)
##D x <- x/x[n]
##D 
##D ## Track is lissajous + brownian bridge
##D b.scale <- 0.6
##D r.scale <- sample(c(0.1, 2, 10.2), n, replace = TRUE, prob = c(0.8, 0.18, 0.02))
##D set.seed(44)
##D 
##D tms <- ISOdate(2001, 1, 1) + trunc(days * 24 * 60 * 60 *x)
##D lon <- 120 + 20 * sin(2 * pi * x) + brownian.bridge(n, b.scale) + rnorm(n, 0, r.scale)
##D lat <- -40 + 10 *(sin(3 * 2 * pi * x) + cos(2 * pi * x) - 1) + brownian.bridge(n, b.scale) + rnorm(n, 0, r.scale)
##D 
##D tr <- new("trip", SpatialPointsDataFrame(cbind(lon, lat), data.frame(gmt = tms, id = "lbb")), TimeOrderedRecords(c("gmt", "id")))
##D 
##D plot(tr)
##D 
##D ## the filtered version
##D trf <- filter.penSS(tr, lambda = 1, iterlim = 400, print.level = 1)
##D 
##D lines(trf)
##D 
##D 
## End(Not run)




cleanEx()
nameEx("oc.theme")
### * oc.theme

flush(stderr()); flush(stdout())

### Name: oc.theme
### Title: SeaWiFS ocean colour colours
### Aliases: oc.theme oc.colors
### Keywords: color

### ** Examples

oc.colors(10)
library(lattice)
trellis.par.set(oc.theme)





cleanEx()
nameEx("sepIdGaps")
### * sepIdGaps

flush(stderr()); flush(stdout())

### Name: sepIdGaps
### Title: Separate a set of IDs based on gaps
### Aliases: sepIdGaps
### Keywords: manip

### ** Examples

  id <- gl(2, 8)
  gd <- Sys.time() + 1:16
  gd[c(4:6, 12:16)] <- gd[c(4:6, 12:16)] + 10000
  
  sepIdGaps(id, gd, 1000)
  



cleanEx()
nameEx("trip")
### * trip

flush(stderr()); flush(stdout())

### Name: trip
### Title: Function to handle animal track data, organized as "trip"s
### Aliases: trip
### Keywords: manip

### ** Examples

d <- data.frame(x = 1:10, y = rnorm(10), tms = Sys.time() + 1:10, id = gl(2, 5))
coordinates(d) <- ~x+y
tr <- trip(d, c("tms", "id"))

## Not run: 
##D 
##D ## a simple example with the common fixes required for basic track data
##D 
##D dat <- read.csv("trackfile.csv")
##D names(dat)  ## e.g. [1] "long" "lat"  "seal"  "date"    "local"     "lq"
##D library(sp)
##D coordinates(dat) <- c("long", "lat")
##D 
##D ## date/times may be in a particular time zone, please check
##D dat$gmt <- as.POSIXct(strptime(paste(dat$date, dat$local),
##D                       "##D 
##D 
##D 
##D ## if there are problems in the data, this will error
##D tr <- trip(dat, c("gmt", "seal"))
##D 
##D 
##D ## the following code tries to fix common problems
##D 
##D ## remove completely-duplicated rows
##D dat <- dat[!duplicated(dat), ]
##D 
##D ## order the rows by seal, then by time
##D dat <- dat[order(dat$seal, dat$gmt), ]
##D 
##D ## fudge duplicated times
##D dat$gmt <- adjust.duplicateTimes(dat$gmt, dat$seal)
##D 
##D 
##D ## finally, convert to Spatial and create trip object
##D coordinates(dat) <- c("long", "lat")
##D tr <- trip(dat, c("gmt", "seal"))
##D 
##D 
##D 
## End(Not run)




cleanEx()
nameEx("trip.split.exact")
### * trip.split.exact

flush(stderr()); flush(stdout())

### Name: trip.split.exact
### Title: Split trip events into exact time-based boundaries.
### Aliases: trip.split.exact
### Keywords: manip chron

### ** Examples

## Not run: 
##D set.seed(66)
##D d <- data.frame(x = 1:100, y = rnorm(100, 1, 10), tms = Sys.time() + c(seq(10,
##D 1000, length = 50), seq(100, 1500, length = 50)), id = gl(2, 50))
##D coordinates(d) <- ~x+y
##D tr <- trip(d, c("tms", "id"))
##D 
##D bound.dates <- seq(min(tr$tms)-1, max(tr$tms)+1, length = 5)
##D trip.list <- trip.split.exact(tr, bound.dates)
##D bb <- bbox(tr)
##D cn <- c(20, 8)
##D g <- GridTopology(bb[,1], apply(bb, 1, diff) / (cn - 1), cn)
##D 
##D tg <- tripGrid(tr, grid = g)
##D tg <- as.image.SpatialGridDataFrame(tg)
##D tg$x <- tg$x - diff(tg$x[1:2])/2
##D tg$y <- tg$y - diff(tg$y[1:2])/2
##D 
##D 
##D op <- par(mfcol = c(4, 1))
##D for (i in 1:length(trip.list)) {
##D   plot(coordinates(tr), pch = 16, cex = 0.7)
##D   title(names(trip.list)[i], cex.main = 0.9)
##D   lines(trip.list[[i]])
##D   abline(h = tg$y, v = tg$x, col = "grey")
##D 
##D   image(tripGrid(trip.list[[i]], grid = g), interpolate = FALSE, col =
##D   c("white", grey(seq(0.2, 0.7,  length = 256))), add =TRUE)
##D   abline(h = tg$y, v = tg$x,  col = "grey")
##D 
##D   lines(trip.list[[i]])
##D   points(trip.list[[i]], pch = 16, cex = 0.7)
##D }
##D 
##D par(op)
##D print("you may need to resize the window to see the grid data")
##D 
##D cn <- c(200, 80)
##D g <- GridTopology(bb[,1], apply(bb, 1, diff) / (cn - 1), cn)
##D 
##D tg <- tripGrid(tr, grid = g)
##D tg <- as.image.SpatialGridDataFrame(tg)
##D tg$x <- tg$x - diff(tg$x[1:2])/2
##D tg$y <- tg$y - diff(tg$y[1:2])/2
##D 
##D 
##D op <- par(mfcol = c(4, 1))
##D for (i in 1:length(trip.list)) {
##D   plot(coordinates(tr), pch = 16, cex = 0.7)
##D   title(names(trip.list)[i], cex.main = 0.9)
##D  
##D  
##D 
##D   image(tripGrid(trip.list[[i]], grid = g, method = "density", sigma = 1),
##D     interpolate = FALSE, col = c("white", grey(seq(0.2, 0.7,  length = 256))), add =TRUE)
##D   lines(trip.list[[i]])
##D   points(trip.list[[i]], pch = 16, cex = 0.7)
##D }
##D 
##D par(op)
##D print("you may need to resize the window to see the grid data")
##D 
## End(Not run)



cleanEx()
nameEx("tripTransform")
### * tripTransform

flush(stderr()); flush(stdout())

### Name: tripTransform
### Title: Reproject trip objects.
### Aliases: tripTransform
### Keywords: manip

### ** Examples

d <- data.frame(x = 1:10, y = rnorm(10), tms = Sys.time() + 1:10, id = gl(2, 5))
coordinates(d) <- ~x+y

tr <- trip(d, c("tms", "id"))
proj4string(tr) <- CRS("+proj=laea +lon_0=146")

tripTransform(tr, "+proj=longlat")




### * <FOOTER>
###
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
