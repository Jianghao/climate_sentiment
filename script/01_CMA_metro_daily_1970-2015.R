library(tidyverse)

## load CMA data 2012 - 2017
load("J:/data.cma.cn/1950-2017-daily-metro/data/02-merge/2012-2017.Rdata") # df
df <- df %>% mutate(alt = alt / 10) %>% select(-alt)

## load CMA data 1950-2011
load("J:/data.cma.cn/1950-2017-daily-metro/data/02-merge/1950-2011.Rdata") # fddf
coords <-
  read.csv("J:/data.cma.cn/1950-2017-daily-metro/data/03-coords/coords.csv")
coords <- coords %>% select(-lat_F,-lon_F)
fddf <- left_join(x = fddf, y = coords, by = "stationid")

## rbind 1950-2017
data <- rbind(fddf, df) %>%
  filter(!is.na(lon)) %>%
  filter(year >= 1970 & year <= 2015) %>%
  mutate(lat = lat / 100) %>%
  mutate(lon = lon / 100) %>%
  mutate(pressure = pressure / 10) %>%
  mutate(pressure = replace(pressure, which(pressure > 2000), NA)) %>%
  mutate(tmean = tmean / 10) %>%
  mutate(tmean = replace(tmean, which(tmean > 100), NA)) %>%
  mutate(tmax = tmax / 10) %>%
  mutate(tmax = replace(tmax, which(tmax > 100), NA)) %>%
  mutate(tmin = tmin / 10) %>%
  mutate(tmin = replace(tmin, which(tmin > 100), NA)) %>%
  mutate(precipitation = precipitation / 10) %>%
  mutate(precipitation = replace(precipitation, which(precipitation > 3000), NA)) %>%
  mutate(windspeed = windspeed / 10) %>%
  mutate(windspeed = replace(windspeed, which(windspeed > 100), NA)) %>%
  mutate(sunshinehours = sunshinehours / 10) %>%
  mutate(sunshinehours = replace(sunshinehours, which(sunshinehours > 100), NA))

## summary
df.sum <- data %>%
  group_by(year) %>%
  summarise(
    tmean = mean(tmean, na.rm = T),
    tmax = max(tmax, na.rm = T),
    tmin = min(tmin, na.rm = T),
    humidity = mean(humidity, na.rm = T),
    precipitation = mean(precipitation, na.rm = T),
    windspeed = mean(windspeed, na.rm = T),
    sunshinehours = mean(sunshinehours, na.rm = T)
  ) %>%
  mutate(tmean_anomaly = tmean - mean(tmean)) %>%
  mutate(tmax_anomaly = tmax - mean(tmax)) %>%
  mutate(tmin_anomaly = tmin - mean(tmin)) %>%
  mutate(precipitation_anomaly = precipitation - mean(precipitation))

write.csv(
  df.sum,
  "D:/Cooperation/ZhengSiQi/WeiboEmotion_Nick/data/CMA_metro_1970-2015_year_mean.csv",
  row.names = F
)
