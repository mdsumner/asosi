#   fs <- c(casey = "IDTE9212/IDTE9212.0922.9D.gif",
#           davis = "IDTE9222/IDTE9222.0922.9D.gif",
#           durville = "IDTE9232/IDTE9232.0921.9D.gif",
#           mawson = "IDTE9242/IDTE9242.0921.9D.gif",
#           shackleton = "IDTE9252/IDTE9252.0922.9D.gif",
#           terranova = "IDTE9262/IDTE9262.0921.9D.gif",
#           westice = "IDTE9272/IDTE9272.0922.9D.gif",
#           pragnhild = "IDTE9282/IDTE9282.0922.1D.gif",
#           enderby = "IDTE9412/IDTE9412.0921.9D.gif",
#           capeadare = "IDTE9422/IDTE9422.0921.9D.gif",
#           sabrina = "IDTE9462/IDTE9462.0922.9D.gif")


.baseprj <- function(clon) {
  sprintf(
    "+proj=stere +lon_0=%f +lat_0=-90 +lat_ts=-70 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs",
    clon
  )
}
.mkregion <-
  function(xmin, xmax, ymin, ymax, lonmin, lonmax, latmin, latmax, proj) {
    list(
      xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
      lonmin = lonmin, lonmax = lonmax, latmin = latmin, latmax = latmax,
      proj = proj
    )
  }
#.regionnames <- c("casey", "davis", "durville", "mawson", "shackleton", "terranova",
#                  "westice", "pragnhild", "enderby", "capeadare", "sabrina")
.regionindex <- function(name) {
  c(
    "casey" = "21", "davis" = "22", "durville" = "23", "mawson" = "24", "shackleton" = "25", "terranova" = "26",
    "westice" = "27", "pragnhild" = "28", "enderby" = "41", "capeadare" = "42", "sabrina" = "46"
  )[name]
}
.token <- function(idx) {
  sprintf("IDTE9%s", .regionindex(idx))
}
.regions <- function(name) {
  x <- switch(
    name,
    davis = .mkregion(
     # xmin = 135, xmax = 562, ymin = 187, ymax = 394,
      xmin = 160, xmax = 575, ymin = 179, ymax = 402,
      lonmin = 70, lonmax = 80, latmin = -68, latmax = -66,
      proj = .baseprj(76)
    ),
    durville  =  .mkregion(279, 704, 101, 775,
                           140, 150,-68,-62,
                           proj = .baseprj(148))
  )
  x$token <- .token(name)
  x
}


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
getbom <-
  function(date, region = c(
    "casey", "davis", "durville", "mawson", "shackleton", "terranova",
    "westice", "pragnhild", "enderby", "capeadare", "sabrina"
  ),
  
  band = c("infrared", "visible")) {
    ##http://www.bom.gov.au/fwo/IDTE9221/IDTE9221.0223.4D.gif
    ##http://www.bom.gov.au/fwo/IDTE9222/IDTE9222.0224.1D.gif
    ##http://www.bom.gov.au/fwo/IDTE9222/IDTE9222.0223.3D.gif
    ##http://www.bom.gov.au/fwo/IDTE9221/IDTE9221.0223.4D.gif
    ## accept 1 (IR) or 2 (VIS)
    if (missing(date)) date <- Sys.Date() - 1
    band <- band[1L]
    if (is.numeric(band))
      band <- c("infrared", "visible")[band]
    pp <- seq(9, 1, by = -2) - c(infrared = 0, visible = 1)[band]
    app <- c(infrared = 2, visible = 1)[band]
    region <- match.arg(region)
    
    regionObj <- .regions(region)
   ## Durville
    ##llpts <- cbind(c(140, 150), c(-68, -62))
    ##centre <- "148"
    
    token <- sprintf("%s%s", regionObj$token, as.character(app))
    
    for (ipop in seq_along(pp)) {
      fname <-
        sprintf(
          "http://www.bom.gov.au/fwo/%s/%s.%s.%sD.gif", token, token, format(date, "%m%d"), as.character(pp[ipop])
        )
     
      tfile <- file.path(tempdir(), basename(fname))
      if (!file.exists(tfile)) {
        d <- try(download.file(fname, tfile, mode = "wb"))
      }
      
      r <- try(raster(tfile))
      if (!inherits(r, "try-error")) {
        break;
      }
    }
    prj <- regionObj$proj
    rawxy <-
      matrix(unlist(regionObj[c("xmin", "xmax", "ymin", "ymax")]), ncol = 2)
    llpts <-
      matrix(unlist(regionObj[c("lonmin", "lonmax", "latmin", "latmax")]), ncol = 2)
    pts <- project(llpts, prj)
    
    ## do the math
    ## scale = size of pixels in X/Y
    ## offset = bottom left corner of bottom left pixel)
    scalex <- diff(pts[, 1]) / diff(rawxy[, 1])
    scaley <- diff(pts[, 2]) / diff(rawxy[, 2])
    offsetx <- pts[1,1] - rawxy[1,1] * scalex
    offsety <- pts[1,2] - rawxy[1,2] * scaley
    
    ## x0, (x0 + ncol * pixelX), y0, (y0 + nrow  * pixelY)
    pex <-
      extent(offsetx, offsetx + scalex * (ncol(r) + 1), offsety, offsety + scaley * (nrow(r) + 1))
    ## override raw index-transform applied to input image
    pd <- setExtent(r, pex)
    projection(pd) <- prj
    
    ## prepare an object to build graticule lines
    temp <- as(extent(pd), "SpatialPolygons")
    projection(temp) <- prj
    return(pd)
    
    stop("cannot find file at", fname, "or", gsub("3D", pp, fname))
  }
