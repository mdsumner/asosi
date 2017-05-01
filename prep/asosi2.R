apt <- function() {
  ## guess the projection
  prj <- sprintf("+proj=stere +lon_0=%s +lat_0=-90 +lat_ts=-71 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0", 
                 centre)
  
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
  return(pd)
}

burl <- "http://www.bom.gov.au/fwo"

fs <- c(casey = "IDTE9212/IDTE9212.0922.9D.gif", 
        davis = "IDTE9222/IDTE9222.0922.9D.gif", 
        durville = "IDTE9232/IDTE9232.0921.9D.gif", 
        mawson = "IDTE9242/IDTE9242.0921.9D.gif", 
        shackleton = "IDTE9252/IDTE9252.0922.9D.gif", 
        terranova = "IDTE9262/IDTE9262.0921.9D.gif", 
        westice = "IDTE9272/IDTE9272.0922.9D.gif", 
        pragnhild = "IDTE9282/IDTE9282.0922.1D.gif", 
        enderby = "IDTE9412/IDTE9412.0921.9D.gif", 
        capeadare = "IDTE9422/IDTE9422.0921.9D.gif", 
        sabrina = "IDTE9462/IDTE9462.0922.9D.gif")
        ## no wilkes?
       
wd <- "C:/temp/asosi"

setwd(wd)

fullname <- file.path(burl, fs)
for (i in seq_along(fullname)) {
  if (!file.exists(names(fs)[i])) dir.create(names(fs)[i])
  localfile <- file.path(names(fs)[i], basename(fullname[i]))
  if (!file.exists(localfile)) {
    download.file(fullname[i], localfile, mode = "wb")
}
}


folders <- names(fs)
setwd("C:/temp/asosi")

j <- 6
setwd(folders[j])
library(raster)
graphics.off()
d <- readAll(raster(basename(fullname)[j]))
click2 <- function(x,  ...) {
  windows(height = 13, width = 20)
  plot(x, ...)
  title("click on two points that you know")
  y <- do.call(cbind, locator(2))
  points(y, pch = 16, col = "green")
  y
}
rawxy <- click2(d)

##c("casey", "davis", "durville", "mawson", "shackleton", "terranova", 
##  "westice", "pragnhild", "enderby", "capeadare", "sabrina")

## enter the coordinates

## Casey
##llpts <- cbind(c(105, 110), c(-66, -64))
##centre <- "110"

## Davis
##llpts <- cbind(c(70, 80), c(-66, -68))
##centre <- "76"

## Durville
##llpts <- cbind(c(140, 150), c(-68, -62))
##centre <- "148"

## Mawson
##llpts <- cbind(c(55, 65), c(-68, -64))
##centre <- "64"

## Shackleton
##llpts <- cbind(c(90, 105), c(-68, -64))
##centre <- "97"

## Terra Nova

llpts <- cbind(c(160, 175), c(-78, -74))
centre <- "170"

## West Ice 
##llpts <- cbind(c(80, 90), c(-68, -64))
##centre <- 88 


## Pragnhild

# llpts <- cbind(c(10, 30), c(-72, -66))
# centre <- 23


## Enderby
##llpts <- cbind(c(35, 50), c(-70, -64))
##centre <- 49


## Cape Adare
##llpts <- cbind(c(160, 170 ), c(-74,-70 ))
##centre <- 168


# ## Sabrina
# llpts <- cbind(c(115, 130 ), c(-68,-64 ))
# centre <- 122


##abline(v = ncol(d)/ 2)

rd <- apt()

## all good? 
writeRaster(rd, paste0(folders[j], ".tif"))
system(sprintf("C:/OSGeo4W64/bin/gdal_translate -of VRT %s %s", paste0(folders[j], ".tif"), paste0(folders[j], ".vrt")))
setwd("..")



        