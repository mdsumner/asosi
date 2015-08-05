regions <- c(
  "casey", "davis", "durville", "mawson", "shackleton", "terranova",
  "westice", "pragnhild", "enderby", "capeadare", "sabrina"
)


library(gris)
library(asosi)
for (ip in seq_along(regions)) {
im <- asosi(region = regions[ip])
x <- bgl(im)
ptcoords <- t(x$vb[1:2, ])

x$vb[1:3, ] <- t(llh2xyz(cbind(project(ptcoords, projection(im), inv = TRUE), 0)))
library(rgl)

pngfile <- gsub("gif$", "png", filename(im))

ct2rgb <- function(x) {
  rrggbb <- col2rgb(x@legend@colortable[values(x)])
  setValues(brick(x, x, x), t(rrggbb))
}
r <- ct2rgb(im)

writeGDAL(as(r, "SpatialGridDataFrame"), pngfile, type = "Byte", drivername = "PNG")

tcoords <- xyFromCell(setExtent(im, extent(0, 1, 0, 1)), cellFromXY(im, ptcoords))
shade3d(x, texcoords = tcoords[x$ib, ], texture = pngfile, col = "white", lit = FALSE)
}

#library(rworldmap)
library(maptools)
data(wrld_simpl)
library(gris)
library(rgl)
library(RTriangle)
#data(countriesLow)
w <- subset(wrld_simpl, NAME == "Antarctica")
map <- gris(w)
pp <- "+proj=laea +ellps=WGS84 +lat_0=-90"
g <- local({
  #xyz <- llh2xyz(map$v %>% select(x, y) %>% as.matrix %>% cbind(0))
  xy <- project(map$v %>% dplyr::select(x, y) %>% as.matrix, pp)
  map1 <- map
  map1$v$x <- xy[,1]
  map1$v$y <- xy[,2]
  
  map1
})
p <- mkpslg(g)
rgl.pop()
tr <- RTriangle::triangulate(p, a = 2e10)
tri <- rgl::tetrahedron3d()
tri$vb <- t(cbind(llh2xyz(cbind(project(tr$P, pp, inv = TRUE), 0)),1))

tri$it <- t(tr$T)
#shade3d(tri, col = "grey", alpha = 0.1)
wire3d(tri, col = "red")


