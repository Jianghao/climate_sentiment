setwd("~/Documents/GitHub/climate_sentiment/script/figure1CD/")
devtools::source_gist("https://gist.github.com/Jianghao/d806dcc220a49df21024e0516f102b2f")
library(tidyverse)
library(lubridate)


# ------------------------
# tmax 2014, 2050, 2099
# ------------------------
df <- read.csv("../../data/NEX-GDDP/NEX-GDDP_China-mean-y14-50-99_tasmax.csv")
df <- df %>% separate(col = var, sep = "_day_BCSD_", into = c("var", "left")) %>%
  separate(col = left, sep = "_r1i1p1_", into = c("path", "left")) %>%
  separate(col = left, sep = "_", into = c("model", "year")) 

df.sum <- df %>%
  group_by(date, var, path) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  as.data.frame() %>%
  mutate(value = value - 273.15) %>%
  mutate(date = ymd(date)) %>%
  mutate(yday = yday(date)) %>%
  mutate(year = factor(year(date))) %>%
  filter(path == "rcp85")

ggplot(df.sum, aes(x = yday, y = value, colour = year)) +
  geom_smooth(se = FALSE, lwd = 1.0, span = 0.25, alpha = 0.75) +
  xlab("Julian Day") +
  ylab(expression(paste("Temperature (", degree, C, ")"))) +
  scale_colour_manual(values = c("#d73027", "#4DBBD5FF", "#4575b4")) +
  theme_Publication() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.size= unit(0.8, "cm"),
    legend.direction = "vertical",
    legend.position = c(0.10, 0.90)
  )
ggplot2::ggsave("figure1c_tasmax_rcp85.pdf", width = 5, height = 4)

# ------------------------
# tmean anomaly time series
# ------------------------
df <- read.csv("../../data/metro/CMA_metro_1970-2015_year_mean.csv")
ggplot(df, aes(x = year, y = tmean_anomaly)) +
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_line(size = 0.5, colour = "grey50") +
  geom_point(colour = "grey50", size = 1.5) +
  geom_smooth(method = "auto", fill = "#4DBBD5FF", colour = "#4DBBD5FF", alpha = 0.3) +
  xlab("Year") +
  ylab(expression(paste("Temperature anomaly (", degree, C, ")"))) +
  ylim(-1.5, 1.5) +
  theme_Publication()
ggplot2::ggsave("figure1d_tmean_anomaly.pdf", width = 6, height = 4)
