## obtain a file
fsrc <- "http://www.bom.gov.au/fwo/IDTE9212/IDTE9212.0904.7D.gif"
f <- basename(fsrc)
if (!file.exists(f)) download.file(fsrc, f, mode = "wb")

## raster and also need rgdal to read GIF and transform coordinates 
library(rgdal)
library(raster)

d <- raster(f)
click2 <- function(x,  ...) {
  plot(x, ...)
  title("click on two points that you know")
  do.call(cbind, locator(2))
}

## WARNING, uncomment "click2" step to be interactive
##  plotting the raw image to harvest two known
##  pixel positions from user
## 
## click two points diagonally orientated
##  - first on  known point near bottom left
##  - second on  known point near top right

##rawxy <- click2(d)

## HARDCODED step for IDTE9212.0904.7D.gif
rawxy <- matrix(c(178, 639, 182, 399), 2, 2)
## enter the coordinates
llpts <- cbind(c(105, 115), c(-66, -64))

## guess the projection
prj <- "+proj=stere +lon_0=110 +lat_0=-90 +lat_ts=-71 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
pts <- project(llpts, prj)

## do the math 
## scale = size of pixels in X/Y
## offset = bottom left corner of bottom left pixel)
scalex <- diff(pts[, 1])/ diff(rawxy[, 1])
scaley <- diff(pts[, 2])/ diff(rawxy[, 2])
offsetx <- pts[1,1] - rawxy[1,1] * scalex
offsety <- pts[1,2] - rawxy[1,2] * scaley

## x0, (x0 + ncol * pixelX), y0, (y0 + nrow  * pixelY)
pex <- extent(offsetx, offsetx + scalex * (ncol(d) + 1), offsety, offsety + scaley * (nrow(d) + 1))

## override raw index-transform applied to input image
pd <- setExtent(d, pex)
projection(pd) <- prj

## prepare an object to build graticule lines
temp <- as(extent(pd), "SpatialPolygons")
projection(temp) <- prj

plot(pd)
llgridlines(temp, ndiscr = 50)
