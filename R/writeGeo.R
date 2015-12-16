extent_plot <- function(usr) {
  extent(usr)
}

writeGeo <- function(filename, device = dev.cur(), proj = NA_character_, ...) {
 tfile <- sprintf("%s.tif", tempfile())
 on.exit(unlink(tfile))
  savePlot(tfile, type = "tif", device = device)
  r <- raster(tfile)
  extent(r) <-  extent_plot(par("usr"))
  projection(r) <- proj
  writeRaster(r, filename, ...)
}
