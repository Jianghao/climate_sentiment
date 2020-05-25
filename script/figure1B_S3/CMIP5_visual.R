# purpose       : Get the plot of time series of NEX_GDDP data
# author        : Jianghao Wang wangjh@lreis.ac.cn
# updates       : May 1, 2020
# ----------------------------------------------------------

# initial #
setwd("~/Documents/GitHub/climate_sentiment/")
library(tidyverse)
library(raster)
library(ncdf4)
library(rgdal)
library(rasterVis)


# ---- load geographic data ----
levelPlotRaster <- function(rs, scheme = "jet", rev = T, n = 50, pal.n = 11, names.attr = names(rs), main = NULL) {
  if(!rev) {
    par.settings = rasterTheme(region = rev(openair::openColours(scheme = scheme, n = n)))
  } else {
    par.settings = rasterTheme(region = openair::openColours(scheme = scheme, n = n))
  }
  if(scheme!="jet") {
    if(rev) {
      par.settings = rasterTheme(region = brewer.pal(pal.n, scheme))
    } else {
      par.settings = rasterTheme(region = rev(brewer.pal(pal.n, scheme)))
    }
  }
  
  pt <- levelplot(rs, cuts = n,
                  par.settings = par.settings,
                  maxpixels = dim(rs)[1] * dim(rs)[2],
                  scales=list(y=list(rot=90), tck=c(0.5,0.5)), 
                  margin = FALSE,
                  xlab = NULL, 
                  ylab = NULL,
                  main = main, 
                  names.attr = names.attr, 
                  par.strip.text = list(cex=1.25, lines=1, col="grey15", face="bold"), 
                  colorkey=list(space='right')) +
    layer(sp.polygons(bound, fill='transparent', col = "grey25", alpha = 0.6, lwd = 0.8))
  return(pt)
}


# ---------------------------------------------
# ---- time serie plot of 2014, 2050, 2099 ----
# ---------------------------------------------
bound <- readOGR("data/shp/china-with-illustrator-modify.geo.json")
rcp <- "rcp85" # rcp85
var <- "tasmax"
inputdir <- "~/Documents/GitHub/climate_sentiment/data/NEX-GDDP/"
outputdir <- "script/figure1B_S3/"

# ---------------------------------------------
# Figure 1B
## NEX_GDDP data: yearly
# ---------------------------------------------
load(paste0(inputdir, "cmip5-year-", rcp, ".Rdata"))

pdf(file = paste0(outputdir, var, "_map_", rcp, ".pdf"), width = 12, height = 3.5)
levelPlotRaster(rs = rs, names.attr = c("2014", "2050", "2099"), scheme = "RdYlBu", rev = F)
dev.off()

# ---------------------------------------------
## Figure S3
## NEX_GDDP data: season
# ---------------------------------------------
load(paste0(inputdir, "cmip5-season-", rcp, ".Rdata"))

season.name <- c("Spring", "Summer", "Fall", "Winter")
pdf(file = paste0(outputdir, var, "_map_season_", rcp, ".pdf"), width = 12, height = 7)
levelPlotRaster(rs = rs, scheme = "RdYlBu", rev = F)
levelPlotRaster(rs = rs)
dev.off()
