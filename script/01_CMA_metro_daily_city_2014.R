library(tidyverse)
library(sf)
library(rgdal)
library(rgeos)

## load city boundary
city <- readOGR("D:/Cooperation/Wenjie/WenjieMobility/02a_mobility_star/data/00_shape_city/City_342.shp", 
                layer = "City_342", use_iconv = TRUE, encoding = "UTF-8")
proj4string(city) <- CRS("+proj=longlat +datum=WGS84")
city <- city[!is.na(city$NAME2004), ]

## load CMA data
load("J:/data.cma.cn/1950-2017-daily-metro/data/02-merge/2012-2017.Rdata") # df
df <- df %>% filter(year == 2014) %>%
  mutate(lat = lat/100) %>%
  mutate(lon = lon/100) %>%
  mutate(alt = alt/10) %>%
  mutate(pressure = pressure / 10) %>% 
  mutate(tmean = tmean / 10) %>%
  mutate(tmax = tmax / 10) %>%
  mutate(tmin = tmin / 10) %>%
  mutate(precipitation = precipitation / 10) %>%
  mutate(windspeed = windspeed / 10) %>%
  mutate(sunshinehours = sunshinehours / 10) %>%
  filter(tmean < 100, tmax < 100, tmin < 100, pressure < 2000, 
         precipitation < 3000, windspeed < 100, sunshinehours < 100)
  
df.sp <- SpatialPointsDataFrame(coords = df[c("lon", "lat")], 
                                data = df, 
                                proj4string = CRS("+proj=longlat +datum=WGS84"))
df.sp.unique <- df.sp[df.sp$month==1 & df.sp$day==1, c("stationid")]

## over : to determine weather a station in a polygon or not
## but some of them are NA
## to use a nearest station instead.
df.over <- cbind(df, over(x = df.sp, y = city)) %>%
  filter(!is.na(AD2004))
## summary of over
df.sum.over <- df.over %>% 
  group_by(year, month, day, AD2004, NAME2004) %>%
  summarise(tmean = mean(tmean),
            tmax = max(tmax),
            tmin = min(tmin),
            humidity = mean(humidity),
            precipitation = mean(precipitation),
            windspeed = mean(windspeed), 
            sunshinehours = mean(sunshinehours))

## The cities which have no station inside
city.left <- city[!city$AD2004 %in% unique(df.sum.over$AD2004),]

## find the nearest station for left cities
city.left.nn <- cbind(city.left, df.sp.unique[apply(gDistance(df.sp.unique, city.left, byid=TRUE), 1, which.min), "stationid"])

## summary of left
df.sum.left <- merge(df, city.left.nn@data, by = "stationid") %>% 
  group_by(year, month, day, AD2004, NAME2004) %>%
  summarise(tmean = mean(tmean, na.rm = T),
            tmax = max(tmax, na.rm = T),
            tmin = min(tmin, na.rm = T),
            humidity = mean(humidity, na.rm = T),
            precipitation = mean(precipitation, na.rm = T),
            windspeed = mean(windspeed, na.rm = T), 
            sunshinehours = mean(sunshinehours, na.rm = T))

df.sum <- rbind(df.sum.over, df.sum.left) %>%
  arrange(year, month, day, AD2004, NAME2004)

write.csv(df.sum, "D:/Cooperation/ZhengSiQi/WeiboEmotion_Nick/data/CMA_metro_daily_city_2014.csv", row.names = F)
