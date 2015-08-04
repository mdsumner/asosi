

#' Title
#'
#' @param date 
#' @param region 
#' @param band 
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dates <- Sys.Date() - c(1, 2, 3, 4, 5)
#' for (i in seq_along(dates)) {
#' r <- getbom(dates[i])
#' writeRaster(r, sprintf("infrared%s.tif", format(dates[i])))
#' r2 <- getbom(dates[i], band = "visible")
#' writeRaster(r2, sprintf("visible%s.tif", format(dates[i])))
#' }

#' ## prepare an object to build graticule lines
#' temp <- as(extent(r), "SpatialPolygons")
#' #' projection(temp) <- projection(r)
#' 
#' plot(r);llgridlines(temp)
#' }
getbom <- function(date, region = c("DavisPrydz", "Durville"), band = c("infrared", "visible")) {
  ##http://www.bom.gov.au/fwo/IDTE9221/IDTE9221.0223.4D.gif
  ##http://www.bom.gov.au/fwo/IDTE9222/IDTE9222.0224.1D.gif
  ##http://www.bom.gov.au/fwo/IDTE9222/IDTE9222.0223.3D.gif
  ##http://www.bom.gov.au/fwo/IDTE9221/IDTE9221.0223.4D.gif
  band <- match.arg(band)
  pp <- c(infrared = "9D", visible = "4D")[band]
  region <- match.arg(region)
  regions <- list(
    DavisPrydz = list(xy = structure(c(135, 562, 394, 187), .Dim = c(2L, 2L), .Dimnames = list( NULL, c("x", "y"))), 
                      ll = structure(c(70, 80, -66, -68), .Dim = c(2L, 2L)),
                      token = "IDTE9222", 
                      prj = "+proj=stere +lon_0=76 +lat_0=-90 +lat_ts=-70 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"), 
    Durville = list(xy = structure(c(186, 680, 750 , 123), .Dim = c(2L, 2L), .Dimnames = list( NULL, c("x", "y"))),  
                  ll = structure(c(140, 150, -62, -68), .Dim = c(2L, 2L)), 
                  token = "IDTE9232", 
                    prj = "+proj=stere +lon_0=148 +lat_0=-90 +lat_ts=-70 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"))
  
  
  
  
  
  ## Durville
  ##llpts <- cbind(c(140, 150), c(-68, -62))
  ##centre <- "148"
  
  fname <- sprintf("http://www.bom.gov.au/fwo/%s/%s.%s.%s.gif", regions[[region]]$token, regions[[region]]$token, format(date, "%m%d"), pp)
  print(fname)
  tfile <- sprintf("%s.gif", tempfile())
  d <- try(download.file(fname, tfile, mode = "wb"))
  
  
  r <- try(raster(tfile))
  if (inherits(r, "try-error")) {
    ## try again
    fname <- sprintf("http://www.bom.gov.au/fwo/%s/%s.%s.%s.gif", regions[[region]]$token, regions[[region]]$token, format(date, "%m%d"), "7D")
    print(fname)
    tfile <- sprintf("%s.gif", tempfile())
    try(download.file(fname, tfile, mode = "wb"))
    r <- try(raster(tfile))
  }
  if (inherits(r, "RasterLayer")) {
    params <- regions[[region]]
    prj <- params$prj
    rawxy <- params$xy
    llpts <- params$ll
    pts <- project(llpts, prj)
    
    ## do the math
    ## scale = size of pixels in X/Y
    ## offset = bottom left corner of bottom left pixel)
    scalex <- diff(pts[, 1])/ diff(rawxy[, 1])
    scaley <- diff(pts[, 2])/ diff(rawxy[, 2])
    offsetx <- pts[1,1] - rawxy[1,1] * scalex
    offsety <- pts[1,2] - rawxy[1,2] * scaley
    
    ## x0, (x0 + ncol * pixelX), y0, (y0 + nrow  * pixelY)
    pex <- extent(offsetx, offsetx + scalex * (ncol(r) + 1), offsety, offsety + scaley * (nrow(r) + 1))
    
    ## override raw index-transform applied to input image
    pd <- setExtent(r, pex)
    projection(pd) <- prj
    
    ## prepare an object to build graticule lines
    temp <- as(extent(pd), "SpatialPolygons")
    projection(temp) <- prj
    return(pd)
  }
  stop("cannot find file at", fname, "or", gsub("3D", pp, fname))
}


