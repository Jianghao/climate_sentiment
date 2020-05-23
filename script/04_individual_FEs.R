#------------------------------------------------------------------------------------------
# user level fixed effect model for Temperature and mood research.
# -----------------------------------------------------------------------------------------
# see onenote `Individual FEs`
# install.packages(c("lfe", "dplyr", "readr", "lubridate"))
setwd("D:/Cooperation/ZhengSiQi/WeiboEmotion_Nick/data/")

library(lfe)
library(readr)
library(dplyr)
library(lubridate)

# ---- load city level data ----
temp <- read_csv("data_csv.csv") %>%
  mutate(date = as.character(lubridate::dmy(date)))%>%
  select(tmax_f_1:tmax_f_8, delta_t, pm2_5, windspeed, cloud_cover1, 
         humidity, precipitation, date, cityid2, month, provinceid) %>%
  dplyr::rename(cityid = cityid2) %>% # new = old
  mutate(date_cityid = paste(date, cityid, sep = "_"))

# load individual level data ----
load("../../WeiboEmotion/data/individual_temp.Rdata") # fddf.temp, fddf.count.temp
fddf.count.temp <- filter(fddf.count.temp, n >= 60) # the default n is 50
cat(as.character(Sys.time()), "\n")
fddf <- fddf.temp[fddf.temp$user.id %in% fddf.count.temp$user.id, ] %>%
  ungroup(date) %>%
  mutate(date_cityid = paste(date, city.id, sep = "_")) %>%
  mutate(positive_median = positive_median * 100) %>%
  select(user.id, positive_median, date_cityid)
cat(as.character(Sys.time()), "\n")

# ---- merge city and individual level data ----
df <- left_join(fddf, temp, by = "date_cityid")
cat(as.character(Sys.time()), "\n")

# ---- felm regression -----
# The only comment is that 'cityid' and 'userid' are going to be collinear for most users except those who move around. I'd use only 'userid' in the regression. Shouldn't change much if anything, but just a note.
attempt_reg <- felm(positive_median ~ tmax_f_1 + tmax_f_2 + tmax_f_3 + tmax_f_4 + tmax_f_6 + tmax_f_7 + tmax_f_8 + delta_t + pm2_5 + windspeed + cloud_cover1 + humidity + precipitation | user.id + date + factor(month):factor(provinceid) | 0 | provinceid, data=df, exactDOF=TRUE, psdef=FALSE)
# save(attempt_reg, file = "attempt_reg.Rdata")
summary(attempt_reg)
cat(as.character(Sys.time()), "\n")


#------------------------------------------------------------------------------------------
# user level fixed effect model for PM2.5 and mood research.
# -----------------------------------------------------------------------------------------
setwd("D:/Cooperation/ZhengSiQi/WeiboEmotion/data/")
library(lfe)
library(haven)

# ---- load stata data ----
df <- read_stata("individual_data_Suncong.dta")

# ---- felm regression -----
reg <- felm(emotion_median ~ pm2_5+ temp_f_1 + temp_f_2 + temp_f_3 + temp_f_4 + temp_f_5 + temp_f_7 + temp_f_8 + temp_f_9 + wind_speed1 + cloud_cover1 + rainfall1 | cityid + date + userid  | 0 | cityid, data = df, exactDOF=TRUE, psdef=FALSE)

reg <- felm(emotion_median ~ pm2_5+ temp_f_1 + temp_f_2 + temp_f_3 + temp_f_4 + temp_f_5 + temp_f_7 + temp_f_8 + temp_f_9 + wind_speed1 + cloud_cover1 + rainfall1 | date + userid  | 0 | cityid, data = df, exactDOF=TRUE, psdef=FALSE)

summary(reg)

## End of Script