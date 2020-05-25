setwd("~/Documents/GitHub/climate_sentiment/script/figureS5/")
devtools::source_gist("https://gist.github.com/Jianghao/d806dcc220a49df21024e0516f102b2f")
library(tidyverse)
library(haven)
library(cowplot)
library(ggjoy)
library(lubridate)

# reg <- read_dta(file = "../../data/stataReg/stata_reg_data.Rds")
reg <- readRDS(file = "../../data/stataReg/stata_reg_data.Rds")

# ------------------------
# tmax - individual
# ------------------------
df <- read.csv("../../data/regression/tmax_individual.csv")
df$max95 <- df$coef + df$se
df$min95 <- df$coef - df$se

pmain <- ggplot(df, aes(x = temp, y = coef)) + 
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_line(size = 1.0, colour = "#4DBBD5FF") +
  geom_ribbon(aes(ymin = min95, ymax = max95), alpha = 0.30, fill = "#4DBBD5FF") +
  geom_point(colour = "#4DBBD5FF", size = 3) +
  annotate("text", x = mean(range(df$temp)), y=Inf, vjust=1.5, colour="grey50", size=5,
           label="Density of max. temperature (°C)") +
  ylab("Sentiment change (%)") +
  xlab(expression(paste("Max. temperature (", degree, C, ")"))) +
  xlim(c(0, 40)) +
  ylim(c(-2, 0.5)) +
  theme_Publication()
xdsty <- axis_canvas(pmain, axis = "x", data=reg, aes(x=tmax)) +
  geom_density(alpha=0.5, size=0.5, colour = "grey50", fill = "grey75")
p.tmax.individual <- insert_xaxis_grob(pmain, xdsty, grid::unit(.10, "null"), position = "top")
ggsave(filename = "tmax_individual.pdf", plot = p.tmax.individual, width = 6, height = 5)

# ------------------------
# the histogram of individuals posts counts greater then 60 
# ------------------------
user.stat <- readRDS("../../data/stataReg/individual_FE.Rds")
# load("../../WeiboEmotion/data/individual_temp.Rdata") # fddf.temp, fddf.count.temp
p.user.count <- ggplot(user.stat, aes(x = n)) + 
  geom_histogram(alpha=0.5, fill="grey75", size=0.2, colour="grey50", binwidth = 5) +
  xlim(c(60, 400)) +
  theme_Publication() +
  ylab("Count") +
  xlab("Weibo posts number")

p.main <- plot_grid(p.tmax.individual, p.user.count, ncol = 2, nrow=1, 
                    align="hv", labels = c("A", "B"))
save_plot("tmax_individual_counts.pdf", p.main, base_width = 10, base_height = 4)
