# purpose : Get the plot of time series of NEX_GDDP data
# Jianghao Wang
# 2018-04-02
# ----------------------------------------------

# initial #
setwd("J:/CMIP5/NEX-GDDP/data/China/")
library(tidyverse)
library(raster)
library(ncdf4)
library(sf)
library(geojsonio)
library(rasterVis)
library(lubridate)
library(splines)

# ---- load geographic data ----
bound <- st_read("../shp/cn_boundary.shp") %>% as("Spatial")

# take tasmax, 2014, rcp85 as an example
# getFileList <- function(dir=getwd(), var="tasmax", year=2014, path="rcp85"){
#   pattern <- paste0(var, "_day_BCSD_", path, "_*", year, ".nc")
#   ll <- dir(path = dir, pattern = glob2rx(pattern))
# }
# getValueName <- function(var="tasmax", year=2014, path="rcp85"){
#   name <- paste0(var, "_", path, "_", year)
# }
# ll <- getFileList(var="tasmax", year=2014, path="rcp85")

# ll <- dir(path = "tasmax_rcp85", pattern = glob2rx("tasmax_day_BCSD*.nc"))
# extract.list <- lapply(ll, function(l){
#   cat(as.character(Sys.time()), l, "\n")
#   r <- brick(l)
#   ex <- extract(r, bound, fun = mean, na.rm = TRUE, df = TRUE) %>%
#     dplyr::select(-ID) %>%
#     gather(key = "date", value = "value") %>%
#     mutate(var = gsub(".nc", "", l))
#   ex$date <- as.Date(x = ex$date, format = "X%Y.%m.%d")
#   return(ex)
# })
# 
# sum <- bind_rows(extract.list)
# write.csv(sum, file = "D:/Cooperation/ZhengSiQi/WeiboEmotion_Nick/data/NEX-GDDP_China-mean-y14-50-99_tasmax.csv", row.names = F)

# ---------------------------------------------
# ---- time serie plot of 2014, 2050, 2099 ----
# ---------------------------------------------
bound <- geojson_read(x = "D:/Cooperation/Wenjie/WenjieMobility/01-rasterise/data/bound/china-with-illustrator-modify.geo.json", what = "sp") %>% as("SpatialPolygons")
bound1 <- geojson_read(x = "D:/Cooperation/Wenjie/WenjieMobility/01-rasterise/data/bound/china.json", what = "sp") %>% as("SpatialPolygons")

rcp <- "rcp45" # rcp45
var <- "tasmax"
outputdir <- "D:/Cooperation/ZhengSiQi/WeiboEmotion_Nick/figs_v2/"


getRasterMean <- function(var = "tasmax", rcp = "rcp85", year = "2014", 
                          yearly = TRUE, seasonly = FALSE) {
  pattern <- paste0(var, "_day_BCSD_", rcp, "_*", year, ".nc")
  ll <- dir(path = paste0(var, "_", rcp, "/"), pattern = glob2rx(pattern), full.names = T)
  rs <- lapply(ll, function(l){
    cat(as.character(Sys.time()), l, "\n")
    r <- brick(l)
    if(yearly) r <- mean(r, na.rm = TRUE)
    if(seasonly) {
      season <- names(r) %>% as.Date(format = "X%Y.%m.%d") %>%
        quarter(with_year = FALSE, fiscal_start = 3)
      r.spr <- mean(r[[which(season == 1)]], na.rm = TRUE)
      r.sum <- mean(r[[which(season == 2)]], na.rm = TRUE)
      r.aut <- mean(r[[which(season == 3)]], na.rm = TRUE)
      r.win <- mean(r[[which(season == 4)]], na.rm = TRUE)
      r <- stack(r.spr, r.sum, r.aut, r.win)
      names(r) <- c("Spring", "Summer", "Fall", "Winter")
    }
    return(r)
  }) 
  if(yearly) rs <- stack(rs) %>% mean(na.rm = TRUE)
  if(seasonly) {
    rss <- stack(rs)
    rs.spr <- mean(rss[[grep("Spring.*", names(rss))]])
    rs.sum <- mean(rss[[grep("Summer.*", names(rss))]])
    rs.aut <- mean(rss[[grep("Fall.*", names(rss))]])
    rs.win <- mean(rss[[grep("Winter.*", names(rss))]])
    rs <- stack(rs.spr, rs.sum, rs.aut, rs.win)
    names(rs) <- paste(c("Spring", "Summer", "Fall", "Winter"), year)
  }
  return(rs)
}

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


# ----------------
# ---- yearly ----
# ----------------
rs.14 <- getRasterMean(var = var, rcp = rcp, year = "2014", yearly = TRUE, seasonly = FALSE)
rs.50 <- getRasterMean(var = var, rcp = rcp, year = "2050", yearly = TRUE, seasonly = FALSE)
rs.99 <- getRasterMean(var = var, rcp = rcp, year = "2099", yearly = TRUE, seasonly = FALSE)
rs <- stack(rs.14, rs.50, rs.99) %>% mask(bound1)
rs <- rs - 273.15
layer.name <- paste0("Year ", c("2014", "2050", "2099"))
names(rs) <- layer.name

pdf(file = paste0(outputdir, var, "_map_", rcp, ".pdf"), width = 12, height = 3.5)
levelPlotRaster(rs = rs, names.attr = c("2014", "2050", "2099"))
dev.off()

pdf(file = paste0(outputdir, var, "_map_anomaly_", rcp, ".pdf"), width = 8, height = 6)
rs.anomaly.50 <- rs.50 - rs.14 %>% mask(bound1)
rs.anomaly.99 <- rs.99 - rs.14 %>% mask(bound1)
rs.anomaly <- stack(rs.anomaly.50, rs.anomaly.99)
names(rs.anomaly) <- paste0("Year ", c("2050", "2099"))
levelPlotRaster(rs = rs.anomaly, names.attr = c("2050", "2099"))
levelPlotRaster(rs = rs.anomaly.50)
levelPlotRaster(rs = rs.anomaly.99)
dev.off()

save(rs, rs.14, rs.50, rs.99, file = paste0(outputdir, "/cmip5-year-", rcp, ".Rdata"))

# --------------------
# ---- prediction yearly ----
# --------------------
## load yearly data
load(paste0(outputdir, "/cmip5-year-", rcp, ".Rdata"))
df <- read.csv("D:/Cooperation/ZhengSiQi/WeiboEmotion_Nick/figs_v2/tmax.csv")

rasterPredict <- function(rs, season = "Spring.2050", model) {
  rs.pred <- rs[[season]]
  names(rs.pred) <- "temp"
  rs.pred <- raster::predict(rs.pred, model, progress = "text")
  names(rs.pred) <- season
  return(rs.pred)
}

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
# ---- seasonly ----
# ------------------
season.name <- c("Spring", "Summer", "Fall", "Winter")
rs.14 <- getRasterMean(var = var, rcp = rcp, year = "2014", yearly = FALSE, seasonly = TRUE)
rs.50 <- getRasterMean(var = var, rcp = rcp, year = "2050", yearly = FALSE, seasonly = TRUE)
rs.99 <- getRasterMean(var = var, rcp = rcp, year = "2099", yearly = FALSE, seasonly = TRUE)
rs <- stack(rs.14, rs.50, rs.99) %>% mask(bound1)
rs <- rs - 273.15

pdf(file = paste0(outputdir, var, "_map_season_", rcp, ".pdf"), width = 12, height = 7)
names.attr <- c(paste(season.name, "2014"), paste(season.name, "2050"), paste(season.name, "2099"))
levelPlotRaster(rs = rs, names.attr = names.attr)
dev.off()

pdf(file = paste0(outputdir, var, "_map_season_anomaly_", rcp, ".pdf"), width = 8, height = 6)
rs.anomaly.50 <- rs.50 - rs.14 %>% mask(bound1)
rs.anomaly.99 <- rs.99 - rs.14 %>% mask(bound1)
rs.anomaly <- stack(rs.anomaly.50, rs.anomaly.99)
levelPlotRaster(rs = rs.anomaly.50)
levelPlotRaster(rs = rs.anomaly.99)
dev.off()

save(rs, rs.14, rs.50, rs.99, file = paste0(outputdir, "/cmip5-season-", rcp, ".Rdata"))

# ------------------
# ---- prediction season----
# ------------------
## load seasonal data
load(paste0(outputdir, "/cmip5-season-", rcp, ".Rdata"))
df <- read.csv("D:/Cooperation/ZhengSiQi/WeiboEmotion_Nick/figs_v2/season_tmax.csv")

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
