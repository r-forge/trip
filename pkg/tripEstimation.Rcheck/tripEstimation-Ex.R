pkgname <- "tripEstimation"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('tripEstimation')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("astro")
### * astro

flush(stderr()); flush(stdout())

### Name: astro
### Title: Calculations for position of the sun and moon
### Aliases: astro EQUHOR FRAC LMST lunar mini.sun MJD POLAR
### Keywords: manip

### ** Examples


## the moon
tm <- Sys.time() + seq(by = 3600, length = 100)
moon <- lunar(tm)
rtp <- astro(147, -42, moon)
op <- par(mfrow = c(2,1))
plot(tm, rtp$theta, main = "lunar elevation, Hobart")
plot(tm, rtp$phi, main = "lunar azimuth, Hobart")
par(op)

## the sun
tm <- Sys.time() + seq(by = 3600, length = 100)
sun <- mini.sun(tm)
rtp <- astro(147, -42, sun)
op <- par(mfrow = c(2,1))
plot(tm, rtp$theta, main = "solar elevation, Hobart")
plot(tm, rtp$phi, main = "solar azimuth, Hobart")
par(op)
## Not run: 
##D   elev.gmt <- mkElevationSeg(1, tm)
##D   plot(tm, rtp$theta, main = "solar elevation mini.sun versus NOAA") 
##D   lines(tm, elev.gmt(1, 147, -42))
## End(Not run)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("bits")
### * bits

flush(stderr()); flush(stdout())

### Name: bits
### Title: Set and get bits from binary masks.
### Aliases: bits bits<-
### Keywords: manip

### ** Examples

a <- 1
bits(a, 0)  ## 1
bits(a, 2) <- 1
a   # 5




cleanEx()
nameEx("get.mask")
### * get.mask

flush(stderr()); flush(stdout())

### Name: get.mask
### Title: Create, access and manipulate spatial masks
### Aliases: get.mask mkSmall set.mask<- mkMaskObject
### Keywords: manip

### ** Examples

      
      data(volcano)
      d <- list(x = seq(-10, 10, length = nrow(volcano)), 
                y = seq(-5, 5, length = ncol(volcano)),
                z = array(0, c(nrow(volcano), ncol(volcano), 2)) )
      mv <- min(volcano)
      
      for (i in 0:61) {
        blk <- (i %/% 31) + 1
        bit <- (i - 1) %% 31
        bits(d$z[,,blk], bit) <- volcano > (mv + i*1.6 )
      }
      for (i in 0:61) image(get.mask(d, i))
      
      ## an object with 62 masks is only twice the size of the source data
      object.size(d) / object.size(volcano)
      
      ## Not run: 
##D       ## plot a smaller version
##D       image(get.mask(d, 20), 5)
##D       
##D        ## pretend we have only one masks
##D       lookup <- mkLookup(get.mask(d, 30), by.segment = FALSE)
##D       
##D       ## interactive to show use of lookup function
##D       image(get.mask(d, 30), main = "Click on the red (FALSE) and cream (TRUE) areas")
##D       for (i in 1:10) {x <- matrix(unlist(locator(1)), ncol = 2);text(x[1], x[2], lookup(x) > 0)}
##D       
## End(Not run)
      



cleanEx()
nameEx("get.sst")
### * get.sst

flush(stderr()); flush(stdout())

### Name: get.sst
### Title: Read SST data from online service.
### Aliases: get.sst
### Keywords: manip spatial

### ** Examples

## Not run: 
##D d0 <- read.url.sst(tlim = range(ISOdatetime(2009, 1:3, 1, 0, 0, 0)))
##D image(d0$x, d0$y, d0$z[,,1], axes = TRUE)
##D 
##D 
## End(Not run)



cleanEx()
nameEx("pick")
### * pick

flush(stderr()); flush(stdout())

### Name: pick
### Title: Choose twilight segments interactively from light data.
### Aliases: pick picksegs
### Keywords: manip dplot

### ** Examples

## Not run: 
##D  
##D  d <- sin(seq(0, 10, by = 0.01))
##D  id <- 1:length(d)
##D  ## choose a series of start-begin pairs
##D  pk <- pick(id, d, 1000)
##D  ## your start/ends should be marked as blue versus red
##D  plot(id, d, col = c("red", "blue")[is.na(picksegs(pk, 1000))+1])
##D  
##D  
## End(Not run)



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
