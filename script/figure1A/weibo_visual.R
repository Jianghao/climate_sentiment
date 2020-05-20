# title         : fig1a_weibo_visual.R
# purpose       : Visualize geeotagged weibo data
# author        : Jianghao Wang wangjh@lreis.ac.cn
# updates       : May 1, 2020
# ----------------------------------------------------------
setwd("~/Documents/GitHub/climate_sentiment")

library(raster)
library(rasterVis)
library(openair)
library(rgdal)

mapTheme <- rasterTheme(region = openair::openColours(scheme = "jet", n = 100))
bound <- readOGR("data/shp/china-with-illustrator-modify.geo.json")
rs <- raster("script/figure1A/CN-201401_201412-res0.01_sum.tif")
pt <- levelplot(rs, zscaleLog=TRUE, cuts = 99,
                par.settings = mapTheme,
                panel = function(...) {
                  panel.fill(col = "white")
                  panel.levelplot(...)
                },
                xlab = NULL, 
                ylab = NULL,
                maxpixels = dim(rs)[1] * dim(rs)[2],
                scales=list(y=list(rot=90), tck=c(0.5,0.5)), 
                margin = FALSE,
                colorkey=list(space='right'))

# detach(package:tidyverse, unload=TRUE)
pt <- pt + layer(sp.polygons(bound, fill='transparent', col = "grey25", alpha = 0.6, lwd = 0.8))
png("script/figure1A/CN-201401_201412-res0.01-white_sum.tif", width = 5000, height = 3500, res = 600)
print(pt)
dev.off()

## end of script