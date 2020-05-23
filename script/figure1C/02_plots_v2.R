setwd("~/Documents/GitHub/climate_sentiment/script/figure1C/")
library(tidyverse)
devtools::source_gist("https://gist.github.com/Jianghao/d806dcc220a49df21024e0516f102b2f")
# reg <- read_dta(file = "../../data/stataReg/stata_reg_data.dta")


# ------------------------
# tmax 2014, 2050, 2099
# ------------------------
df <- read.csv("NEX-GDDP_China-mean-y14-50-99_tasmax.csv")
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
  scale_colour_manual(values = c("#d73027", "#80cdc1", "#4575b4")) +
  theme_Publication() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.size= unit(0.8, "cm"),
    legend.direction = "vertical",
    legend.position = c(0.10, 0.90)
  )
ggplot2::ggsave("tasmax_rcp85.pdf", width = 5, height = 4)
