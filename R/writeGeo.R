extent_plot <- function(usr) {
  extent(usr)
}

## something like this . . .
writeGeo <- function(filename, device = dev.cur(), proj = NA_character_, ...) {
 tfile <- sprintf("%s.tif", tempfile())
 on.exit(unlink(tfile))
  savePlot(tfile, type = "tif", device = device)
  r <- brick(tfile)
  r <- brick(r[[3]], r[[2]], r[[1]])
  extent(r) <-  extent_plot(par("usr"))
  projection(r) <- proj
  writeRaster(r, filename, ...)
}
