<!-- README.md is generated from README.Rmd. Please edit that file -->
asosi
=====

Obtain Antarctic and Southern Ocean Sea Ice imagery from the BOM

This is a rough prototype and only some of the parameters are built in and generalized properly. Work in progress.

Only D'Urville and Davis work for now.

``` r
library(asosi)
library(rworldxtra)
library(rgdal)
ant <- subset(countriesHigh, SOVEREIGNT == "Antarctica")
library(graticule)

durv <- getbom(as.Date("2015-07-30"), region = "durville")
#> Warning in download.file(fname, tfile, mode = "wb"): cannot open: HTTP
#> status was '404 Not Found'
durvmap <- spTransform(ant, projection(durv))
durvgrat <- graticule(seq(135, 160, by = 5), seq(-70, -60, by = 2), proj = projection(durv))
plot(durv, addfun = function() {plot(durvmap, add = TRUE); plot(durvgrat, add = TRUE)}, maxpixels = ncell(durv))
```

![](readmefigs/README-unnamed-chunk-3-1.png) ![](readmefigs/README-unnamed-chunk-3-2.png)

``` r
#writeRaster(durv, gsub(".gif", ".tif", basename(durv)))


davis <- getbom(region = "davis")
davismap <- spTransform(ant, projection(davis))
davisgrat <- graticule(seq(-65, 85, by = 5), seq(-70, -64, by = 2), proj = projection(davis))
plot(davis, addfun = function() {plot(davismap, add = TRUE); plot(davisgrat, add = TRUE)}, maxpixels = ncell(davis))
```

![](readmefigs/README-unnamed-chunk-3-3.png) ![](readmefigs/README-unnamed-chunk-3-4.png)

``` r
#writeRaster(davis, gsub(".gif", ".tif", basename(davis)))
```
