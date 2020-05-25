# purpose       : Future projection
# author        : Jianghao Wang wangjh@lreis.ac.cn
# updates       : May 1, 2020
# ----------------------------------------------------------

# initial #
setwd("~/Documents/GitHub/climate_sentiment/")
library(tidyverse)
library(raster)
library(ncdf4)
library(sf)
library(geojsonio)
library(rasterVis)
library(lubridate)
library(splines)


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

rasterPredict <- function(rs, season = "Spring.2050", model) {
  rs.pred <- rs[[season]]
  names(rs.pred) <- "temp"
  rs.pred <- raster::predict(rs.pred, model, progress = "text")
  names(rs.pred) <- season
  return(rs.pred)
}


# ---------------------------------------------
# initial
# ---------------------------------------------
bound <- readOGR("data/shp/china-with-illustrator-modify.geo.json")

rcp <- "rcp85" # rcp45
var <- "tasmax"
inputdir <- "~/Documents/GitHub/climate_sentiment/data/NEX-GDDP/"
outputdir <- "script/figure5_S7/"

## NEX_GDDP data


# --------------------
# ---- prediction yearly ----
# --------------------
## load yearly data
load(paste0(inputdir, "cmip5-year-", rcp, ".Rdata"))
df <- read.csv("data/regression/tmax.csv")
model <- lm(coef ~ ns(temp, df = 1), data = df)
summary(model)

Year.2014 <- rasterPredict(rs, season = "Year.2014", model)
Year.2050 <- rasterPredict(rs, season = "Year.2050", model)
Year.2099 <- rasterPredict(rs, season = "Year.2099", model)
rs.pred.year <- stack(Year.2050 - Year.2014, Year.2099 - Year.2014)
names(rs.pred.year) <- paste0("Year ", c("2050", "2099"))
pdf(file = paste0(outputdir, var, "_map_year_pred_", rcp, ".pdf"), width = 6, height = 4.5)
levelPlotRaster(rs = rs.pred.year, scheme = "OrRd", n = 20, pal.n = 6, rev = F, 
                names.attr = c("2050", "2099"))
levelPlotRaster(rs = rs.pred.year[[1]], scheme = "RdYlBu", n = 20, pal.n = 9, rev = T, 
                names.attr = "2050", main = "Year 2050")
levelPlotRaster(rs = rs.pred.year[[2]], scheme = "RdYlBu", n = 20, pal.n = 9, rev = T, 
                names.attr = "2099", main = "Year 2099")
dev.off()



# ------------------
# ---- prediction season----
# ------------------
## load seasonal data
load(paste0(inputdir, "cmip5-season-", rcp, ".Rdata"))
df <- read.csv("data/regression/season_tmax.csv")

# --- Spring ----
spr.model <- lm(coef ~ ns(temp, df = 2), data = subset(df, group == "Spring"))
summary(spr.model)
Spring.2014 <- rasterPredict(rs, season = "Spring.2014", spr.model)
Spring.2050 <- rasterPredict(rs, season = "Spring.2050", spr.model)
Spring.2099 <- rasterPredict(rs, season = "Spring.2099", spr.model)
Spring.pred <- stack(Spring.2050 - Spring.2014, Spring.2099 - Spring.2014)
names(Spring.pred) <- c("Spring.2050", "Spring.2099")

# --- Summer ----
sum.model <- lm(coef ~  poly(temp, 2), data = subset(df, group == "Summer"))
summary(sum.model)
Summer.2014 <- rasterPredict(rs, season = "Summer.2014", sum.model)
Summer.2050 <- rasterPredict(rs, season = "Summer.2050", sum.model)
Summer.2099 <- rasterPredict(rs, season = "Summer.2099", sum.model)
Summer.pred <- stack(Summer.2050 - Summer.2014, Summer.2099 - Summer.2014)
names(Summer.pred) <- c("Summer.2050", "Summer.2099")

# --- Fall ----
fal.model <- lm(coef ~  ns(temp, df = 3), data = subset(df, group == "Fall"))
summary(fal.model)
Fall.2014 <- rasterPredict(rs, season = "Fall.2014", fal.model)
Fall.2050 <- rasterPredict(rs, season = "Fall.2050", fal.model)
Fall.2099 <- rasterPredict(rs, season = "Fall.2099", fal.model)
Fall.pred <- stack(Fall.2050 - Fall.2014, Fall.2099 - Fall.2014)
names(Fall.pred) <- c("Fall.2050", "Fall.2099")

# --- Winter ----
win.model <- lm(coef ~  ns(temp, df = 2), data = subset(df, group == "Winter"))
summary(win.model)
Winter.2014 <- rasterPredict(rs, season = "Winter.2014", win.model)
Winter.2050 <- rasterPredict(rs, season = "Winter.2050", win.model)
Winter.2099 <- rasterPredict(rs, season = "Winter.2099", win.model)
Winter.pred <- stack(Winter.2050 - Winter.2014, Winter.2099 - Winter.2014)
names(Winter.pred) <- c("Winter.2050", "Winter.2099")

rs.pred.season.2050 <-
  stack(Spring.pred[["Spring.2050"]], Summer.pred[["Summer.2050"]],
        Fall.pred[["Fall.2050"]], Winter.pred[["Winter.2050"]])
rs.pred.season.2099 <-
  stack(Spring.pred[["Spring.2099"]], Summer.pred[["Summer.2099"]],
        Fall.pred[["Fall.2099"]], Winter.pred[["Winter.2099"]])

pdf(file = paste0(outputdir, var, "_map_season_pred_", rcp, "_sep.pdf"), width = 8, height = 6)
names.attr <- paste(c("Spring", "Summer", "Fall", "Winter"), "2050")
levelPlotRaster(rs = rs.pred.season.2050, scheme = "RdYlBu", names.attr = names.attr, n = 20)
names.attr <- paste(c("Spring", "Summer", "Fall", "Winter"), "2099")
levelPlotRaster(rs = rs.pred.season.2099, scheme = "RdYlBu", names.attr = names.attr, n = 20)
dev.off()

pdf(file = paste0(outputdir, var, "_map_season_pred_", rcp, ".pdf"), width = 12, height = 5)
rs.pred.season <- stack(rs.pred.season.2050, rs.pred.season.2099)
names.attr <- c(paste(season.name, "2050"), paste(season.name, "2099"))
levelPlotRaster(rs = rs.pred.season, scheme = "RdYlBu", n = 20, names.attr = names.attr)
dev.off()

pdf(file = paste0(outputdir, var, "_map_season_pred_", rcp, "_arrange.pdf"), width = 12, height = 5)
rs.pred.season <- stack(Spring.pred[["Spring.2050"]], Summer.pred[["Summer.2050"]],
                        Spring.pred[["Spring.2099"]], Summer.pred[["Summer.2099"]],
                        Fall.pred[["Fall.2050"]], Winter.pred[["Winter.2050"]],
                        Fall.pred[["Fall.2099"]], Winter.pred[["Winter.2099"]])
names.attr <- c("Spring 2050", "Summer 2050", "Spring 2099", "Summer 2099", 
                "Fall 2050", "Winter 2050", "Fall 2099", "Winter 2099")
levelPlotRaster(rs = rs.pred.season, scheme = "RdYlBu", n = 20, names.attr = names.attr)
dev.off()
## End of script
