# https://gist.github.com/Jianghao/d806dcc220a49df21024e0516f102b2f
setwd("D:/Cooperation/ZhengSiQi/WeiboEmotion_Nick/figs_v2/")
library(tidyverse)
library(haven)
library(cowplot)
library(ggjoy)
library(lubridate)
reg <- read_dta(file = "../data/stata_reg_data_shuyang.dta")


# ------------------------
# tmax 2014, 2050, 2099
# ------------------------
df <- read.csv("../data/NEX-GDDP_China-mean-y14-50-99_tasmax.csv")
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
  # geom_line(alpha = 0.5, lwd = 0.6) +
  geom_smooth(se = FALSE, lwd = 1.0, span = 0.25, alpha = 0.75) +
  xlab("Julian Day") +
  ylab(expression(paste("Temperature (", degree, C, ")"))) +
  scale_fill_npg() +
  theme_Publication() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.size= unit(0.8, "cm"),
    legend.direction = "vertical",
    legend.position = c(0.10, 0.90)
  )
ggplot2::ggsave("tasmax_rcp85.pdf", width = 5, height = 4)

# ------------------------
# tmean anomaly time series
# ------------------------
df <- read.csv("../data/CMA_metro_1970-2015_year_mean.csv")
ggplot(df, aes(x = year, y = tmean_anomaly)) +
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_line(size = 0.5, colour = "grey50") +
  geom_point(colour = "grey50", size = 1.5) +
  geom_smooth(method = "auto", fill = "#4DBBD5FF", colour = "#4DBBD5FF", alpha = 0.3) +
  xlab("Year") +
  ylab(expression(paste("Temperature anomaly (", degree, C, ")"))) +
  ylim(-1.5, 1.5) +
  theme_Publication()
ggplot2::ggsave("tmean_anomaly.pdf", width = 6, height = 4)


# ------------------------
# tmax
# ------------------------
df <- read.csv("tmax.csv")
df$max95 <- df$coef + df$se
df$min95 <- df$coef - df$se

pmain <- ggplot(df, aes(x = temp, y = coef)) + 
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_line(size = 1.0, colour = "#4DBBD5FF") +
  geom_ribbon(aes(ymin = min95, ymax = max95), alpha = 0.30, fill = "#4DBBD5FF") +
  geom_point(colour = "#4DBBD5FF", size = 3) +
  annotate("text", x = mean(range(df$temp)), y=Inf, vjust=1.5, colour="grey50", size=5,
           label="Density of max. temperature (°C)") +
  annotate("text", x = -Inf, y = -3, vjust = 0, hjust = -0.2, 
           colour = "grey25", size = 6, label = "Whole China") +
  ylab("Sentiment change (%)") +
  xlab(expression(paste("Max. temperature (", degree, C, ")"))) +
  xlim(c(0, 40)) +
  ylim(c(-3.2, 0.5)) +
  theme_Publication()
xdsty <- axis_canvas(pmain, axis = "x", data=reg, aes(x=tmax)) +
  geom_density(alpha=0.5, size=0.5, colour = "grey50", fill = "grey75")
p.tmax <- insert_xaxis_grob(pmain, xdsty, grid::unit(.10, "null"), position = "top")
ggsave(filename = "tmax.pdf", plot = p.tmax, width = 6, height = 5)

# ------------------------
# tmax - 3bin
# ------------------------
df <- read.csv("tmax_3bin.csv")
df$max95 <- df$coef + df$se
df$min95 <- df$coef - df$se

pmain <- ggplot(df, aes(x = temp, y = coef)) + 
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_line(size = 1.0, colour = "#4DBBD5FF") +
  geom_ribbon(aes(ymin = min95, ymax = max95), alpha = 0.30, fill = "#4DBBD5FF") +
  geom_point(colour = "#4DBBD5FF", size = 3) +
  annotate("text", x = mean(range(df$temp)), y=Inf, vjust=1.5, colour="grey50", size=5,
           label="Density of max. temperature (°C)") +
  annotate("text", x = -Inf, y = -3, vjust = 0, hjust = -0.2, 
           colour = "grey25", size = 6, label = "Whole China") +
  ylab("Sentiment change (%)") +
  xlab(expression(paste("Max. temperature (", degree, C, ")"))) +
  xlim(c(0, 40)) +
  ylim(c(-3.2, 0.5)) +
  theme_Publication()
xdsty <- axis_canvas(pmain, axis = "x", data=reg, aes(x=tmax)) +
  geom_density(alpha=0.5, size=0.5, colour = "grey50", fill = "grey75")
p.tmax.3bin <- insert_xaxis_grob(pmain, xdsty, grid::unit(.10, "null"), position = "top")
ggsave(filename = "tmax_3bin.pdf", plot = p.tmax.3bin, width = 6, height = 5)

# ------------------------
# tmax - individual
# ------------------------
df <- read.csv("tmax_individual.csv")
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
load("../../WeiboEmotion/data/individual_temp.Rdata") # fddf.temp, fddf.count.temp
p.user.count <- ggplot(fddf.count.temp, aes(x = n)) + 
  geom_histogram(alpha=0.5, fill="grey75", size=0.2, colour="grey50", binwidth = 5) +
  xlim(c(60, 400)) +
  theme_Publication() +
  ylab("Count") +
  xlab("Weibo posts number")

p.main <- plot_grid(p.tmax.individual, p.user.count, ncol = 2, nrow=1, 
                    align="hv", labels = c("a", "b"))
save_plot("tmax_individual_counts.pdf", p.main, base_width = 10, base_height = 4)

# ------------------------
# tmax: Prediction
# ------------------------
pmain <- ggplot(df, aes(x = temp, y = coef)) + 
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_smooth(method = "auto", formula =  y ~ splines::ns(x, 1), fill = "grey50", colour = "grey50", alpha = 0.30, fullrange=FALSE) +
  geom_point(colour = "grey50", size = 3) +
  # annotate("text", x = mean(range(df$temp)), y=Inf, vjust=1.5, colour="grey50", size=5,
  #          label="Density of max. temperature (°C)") +
  annotate("text", x = -Inf, y=Inf, vjust=1.65, hjust = -0.35, colour="grey50", size=6, label="Full year") +
  ylab("Sentiment change (%)") +
  xlab(expression(paste("Max. temperature (", degree, C, ")"))) +
  xlim(c(0, 40)) +
  ylim(c(-3.2, 1.0)) +
  theme_Publication()
xdsty <- axis_canvas(pmain, axis = "x", data=reg, aes(x=tmax)) +
  geom_density(alpha=0.5, size=0.5, colour = "grey50", fill = "grey75")
p.tmax.pred <- insert_xaxis_grob(pmain, xdsty, grid::unit(.10, "null"), position = "top")
ggsave(filename = "tmax_prediction.pdf", plot = p.tmax.pred, width = 7, height = 5)


# ------------------------
# tmax North
# ------------------------
df <- read.csv("tmax_north.csv")
df$max95 <- df$coef + df$se
df$min95 <- df$coef - df$se

pmain <- ggplot(df, aes(x = temp, y = coef)) + 
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_line(size = 1.0, colour = "#4DBBD5FF") +
  geom_ribbon(aes(ymin = min95, ymax = max95), alpha = 0.30, fill = "#4DBBD5FF") +
  geom_point(colour = "#4DBBD5FF", size = 3) +
  annotate("text", x = mean(range(df$temp)), y=Inf, vjust=1.5, colour="grey50", size=5,
           label="Density of max. temperature (°C)") +
  annotate("text", x = -Inf, y = -3, vjust = 0, hjust = -0.2, 
           colour = "grey25", size = 6, label = "North China") +
  ylab("Sentiment change (%)") +
  xlab(expression(paste("Max. temperature (", degree, C, ")"))) +
  xlim(c(0, 40)) +
  ylim(c(-3.2, 0.5)) +
  theme_Publication()
xdsty <- axis_canvas(pmain, axis = "x", data=subset(reg, northsouth == "North"), aes(x=tmax)) +
  geom_density(alpha=0.5, size=0.5, colour = "grey50", fill = "grey75")
p.north <- insert_xaxis_grob(pmain, xdsty, grid::unit(.10, "null"), position = "top")
ggsave(filename = "tmax_north.pdf", plot = p.north, width = 6, height = 5)


# ------------------------
# tmax North - 3bin
# ------------------------
df <- read.csv("tmax_north_3bin.csv")
df$max95 <- df$coef + df$se
df$min95 <- df$coef - df$se

pmain <- ggplot(df, aes(x = temp, y = coef)) + 
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_line(size = 1.0, colour = "#4DBBD5FF") +
  geom_ribbon(aes(ymin = min95, ymax = max95), alpha = 0.30, fill = "#4DBBD5FF") +
  geom_point(colour = "#4DBBD5FF", size = 3) +
  annotate("text", x = mean(range(df$temp)), y=Inf, vjust=1.5, colour="grey50", size=5,
           label="Density of max. temperature (°C)") +
  annotate("text", x = -Inf, y = -3, vjust = 0, hjust = -0.2, 
           colour = "grey25", size = 6, label = "North China") +
  ylab("Sentiment change (%)") +
  xlab(expression(paste("Max. temperature (", degree, C, ")"))) +
  xlim(c(0, 40)) +
  ylim(c(-3.2, 0.5)) +
  theme_Publication()
xdsty <- axis_canvas(pmain, axis = "x", data=subset(reg, northsouth == "North"), aes(x=tmax)) +
  geom_density(alpha=0.5, size=0.5, colour = "grey50", fill = "grey75")
p.north.3bin <- insert_xaxis_grob(pmain, xdsty, grid::unit(.10, "null"), position = "top")
ggsave(filename = "tmax_north_3bin.pdf", plot = p.north.3bin, width = 6, height = 5)

# ------------------------
# tmax South
# ------------------------
df <- read.csv("tmax_south.csv")
df$max95 <- df$coef + df$se
df$min95 <- df$coef - df$se

pmain <- ggplot(df, aes(x = temp, y = coef)) + 
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_line(size = 1.0, colour = "#4DBBD5FF") +
  geom_ribbon(aes(ymin = min95, ymax = max95), alpha = 0.30, fill = "#4DBBD5FF") +
  geom_point(colour = "#4DBBD5FF", size = 3) +
  annotate("text", x = 20, y=Inf, vjust=1.5, colour="grey50", size=5,
           label="Density of max. temperature (°C)") +
  annotate("text", x = -Inf, y = -3, vjust = 0, hjust = -0.2, 
           colour = "grey25", size = 6, label = "South China") +
  ylab("Sentiment change (%)") +
  xlab(expression(paste("Max. temperature (", degree, C, ")"))) +
  xlim(c(0, 40)) +
  ylim(c(-3.2, 0.5)) +
  theme_Publication()
xdsty <- axis_canvas(pmain, axis = "x", data=subset(reg, northsouth == "South"), aes(x=tmax)) +
  geom_density(alpha=0.5, size=0.5, colour = "grey50", fill = "grey75") 
p.south <- insert_xaxis_grob(pmain, xdsty, grid::unit(.10, "null"), position = "top")
ggsave(filename = "tmax_south.pdf", plot = p.south, width = 6, height = 5)


# ------------------------
# tmax South - 3bin
# ------------------------
df <- read.csv("tmax_south_3bin.csv")
df$max95 <- df$coef + df$se
df$min95 <- df$coef - df$se

pmain <- ggplot(df, aes(x = temp, y = coef)) + 
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_line(size = 1.0, colour = "#4DBBD5FF") +
  geom_ribbon(aes(ymin = min95, ymax = max95), alpha = 0.30, fill = "#4DBBD5FF") +
  geom_point(colour = "#4DBBD5FF", size = 3) +
  annotate("text", x = 20, y=Inf, vjust=1.5, colour="grey50", size=5,
           label="Density of max. temperature (°C)") +
  annotate("text", x = -Inf, y = -3, vjust = 0, hjust = -0.2, 
           colour = "grey25", size = 6, label = "South China") +
  ylab("Sentiment change (%)") +
  xlab(expression(paste("Max. temperature (", degree, C, ")"))) +
  xlim(c(0, 40)) +
  ylim(c(-3.2, 0.5)) +
  theme_Publication()
xdsty <- axis_canvas(pmain, axis = "x", data=subset(reg, northsouth == "South"), aes(x=tmax)) +
  geom_density(alpha=0.5, size=0.5, colour = "grey50", fill = "grey75") 
p.south.3bin <- insert_xaxis_grob(pmain, xdsty, grid::unit(.10, "null"), position = "top")
ggsave(filename = "tmax_south_3bin.pdf", plot = p.south.3bin, width = 6, height = 5)



# ------------------------
# cowplot p.tmax, p.north, p.south
# ------------------------
p.main <- plot_grid(p.tmax, p.north, p.south, ncol = 2, nrow=2, 
                    align="hv", labels = c("a", "b", "c"))
p.main <- ggdraw() +
  draw_plot(p.tmax,  x = .25, y = .5, width = .5, height = .5) +
  draw_plot(p.north, x = 0, y = 0, width = .5, height = .5) +
  draw_plot(p.south, x = .5, y = 0, width = .5, height = .5) +
  draw_plot_label(c("a", "b", "c"), c(0.25, 0, 0.5), c(1, 0.5, 0.5))
save_plot("tmax_north_south.pdf", p.main, base_width = 10, base_height = 8)


# ------------------------
# cowplot p.tmax.3bin, p.north.3bin, p.south.3bin
# ------------------------
p.main <- plot_grid(p.tmax.3bin, p.north.3bin, p.south.3bin, ncol = 2, nrow=2, 
                    align="hv", labels = c("a", "b", "c"))
p.main <- ggdraw() +
  draw_plot(p.tmax.3bin,  x = .25, y = .5, width = .5, height = .5) +
  draw_plot(p.north.3bin, x = 0, y = 0, width = .5, height = .5) +
  draw_plot(p.south.3bin, x = .5, y = 0, width = .5, height = .5) +
  draw_plot_label(c("a", "b", "c"), c(0.25, 0, 0.5), c(1, 0.5, 0.5))
save_plot("tmax_north_south_3bin.pdf", p.main, base_width = 10, base_height = 8)

# ------------------------
# hete of gender: Female Male
# ------------------------
df <- read.csv("gender_tmax.csv")
df$max95 <- df$coef + df$se
df$min95 <- df$coef - df$se
df$group <- factor(df$group, levels = c("Female", "Male"))

cols <- pal_npg("nrc")(4)[1:2]
pmain <- ggplot(df, aes(x = temp, y = coef, colour = group)) + 
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_line(size = 1.0) +
  geom_ribbon(aes(ymin = min95, ymax = max95, fill = group), alpha = 0.30, colour=NA) +
  geom_point(size = 2) +
  # annotate("text", x = mean(range(df$temp)), y=Inf, vjust=1.5, colour="grey50", size=5,
           # label="Density of max. temperature (°C)") +
  ylab("Sentiment change (%)") +
  xlab(expression(paste("Max. temperature (", degree, C, ")"))) +
  xlim(0, 40) +
  ylim(-3.5, 1) +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  theme_Publication() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.size= unit(0.5, "cm"),
    legend.position = c(0.25, 0.90)
  )
xdsty <- axis_canvas(pmain, axis = "x", data=reg, aes(x=tmax)) +
  geom_density(alpha=0.5, size=0.5, colour = "grey50", fill = "grey75") 
p.gender <- insert_xaxis_grob(pmain, xdsty, grid::unit(.10, "null"), position = "top")
ggsave(filename = "gender_tmax.pdf", plot = p.gender, width = 7, height = 5)


# ------------------------
# hete of income: rich and poor
# ------------------------
df <- read.csv("income_tmax.csv")
df$max95 <- df$coef + df$se
df$min95 <- df$coef - df$se
df$group <- factor(df$group, levels = c("Rich city", "Poor city"))

cols <- pal_npg("nrc")(4)[3:4]
pmain <- ggplot(df, aes(x = temp, y = coef, colour = group)) + 
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_line(size = 1.0) +
  geom_ribbon(aes(ymin = min95, ymax = max95, fill = group), alpha = 0.30, colour=NA) +
  geom_point(size = 2) +
  # annotate("text", x = mean(range(df$temp)), y=Inf, vjust=1.5, colour="grey50", size=5,
           # label="Density of max. temperature (°C)") +
  ylab("Sentiment change (%)") +
  xlab(expression(paste("Max. temperature (", degree, C, ")"))) +
  xlim(0, 40) +
  ylim(-3.5, 1) +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  theme_Publication() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.size= unit(0.5, "cm"),
    legend.position = c(0.30, 0.90)
  )
xdsty <- axis_canvas(pmain, axis = "x", data=reg, aes(x=tmax)) +
  geom_density(alpha=0.5, size=0.5, colour = "grey50", fill = "grey75")
p.income <- insert_xaxis_grob(pmain, xdsty, grid::unit(.10, "null"), position = "top")
ggsave(filename = "income_tmax.pdf", plot = p.income, width = 7, height = 5)



# ------------------------
# hete: Season
# ------------------------
df <- read.csv("season_tmax.csv")
df$max95 <- df$coef + df$se
df$min95 <- df$coef - df$se
df$group <- factor(df$group, levels = c("Spring", "Summer", "Fall", "Winter"))

cols <- pal_npg("nrc")(4)
# --- spring ----
col <- cols[1]
pmain <- ggplot(subset(df, group == "Spring"), aes(x = temp, y = coef)) + 
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_line(size = 1.0, colour=col) +
  geom_ribbon(aes(ymin = min95, ymax = max95), alpha = 0.30, colour=NA, fill=col) +
  geom_point(size = 2, colour=col) +
  annotate("text", x = -Inf, y=Inf, vjust=1.65, hjust = -0.35, colour=col, size=6, label="Spring") +
  xlab(expression(paste("Max. temperature (", degree, C, ")"))) +
  ylab("Sentiment change (%)") +
  xlim(0, 40) + 
  ylim(range(c(df$min95, df$max95))) +
  theme_Publication() +
  theme(legend.position = "none")
xdsty <- axis_canvas(pmain, axis = "x") +
  geom_density(data=reg, aes(x=tmax_spring), alpha=0.5, size=0.5, colour=col, fill = col) 
p.spring <- insert_xaxis_grob(pmain, xdsty, grid::unit(.1, "null"), position = "top")
# ggdraw(p.spring)

# --- summer ----
col <- cols[2]
pmain <- ggplot(subset(df, group == "Summer"), aes(x = temp, y = coef)) + 
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_line(size = 1.0, colour=col) +
  geom_ribbon(aes(ymin = min95, ymax = max95), alpha = 0.30, colour=NA, fill=col) +
  geom_point(size = 2, colour=col) +
  annotate("text", x = -Inf, y=Inf, vjust=1.65, hjust = -0.35, colour=col, size=6, label="Summer") +
  xlab(expression(paste("Max. temperature (", degree, C, ")"))) +
  ylab("Sentiment change (%)") +
  xlim(0, 40) + 
  ylim(range(c(df$min95, df$max95))) +
  theme_Publication() +
  theme(legend.position = "none")
xdsty <- axis_canvas(pmain, axis = "x") +
  geom_density(data=reg, aes(x=tmax_summer), alpha=0.5, size=0.5, colour=col, fill = col)
p.summer <- insert_xaxis_grob(pmain, xdsty, grid::unit(.1, "null"), position = "top")

# --- Fall ----
col <- cols[3]
pmain <- ggplot(subset(df, group == "Fall"), aes(x = temp, y = coef)) + 
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_line(size = 1.0, colour=col) +
  geom_ribbon(aes(ymin = min95, ymax = max95), alpha = 0.30, colour=NA, fill=col) +
  geom_point(size = 2, colour=col) +
  annotate("text", x = -Inf, y=Inf, vjust=1.65, hjust = -0.35, colour=col, size=6, label="Fall") +
  xlab(expression(paste("Max. temperature (", degree, C, ")"))) +
  ylab("Sentiment change (%)") +
  xlim(0, 40) + 
  ylim(range(c(df$min95, df$max95))) +
  theme_Publication() +
  theme(legend.position = "none")
xdsty <- axis_canvas(pmain, axis = "x") +
  geom_density(data=reg, aes(x=tmax_autumn), alpha=0.5, size=0.5, colour=col, fill = col) +
  xlim(range(df$temp))
p.fall <- insert_xaxis_grob(pmain, xdsty, grid::unit(.1, "null"), position = "top")

# --- winter ----
col <- cols[4]
pmain <- ggplot(subset(df, group == "Winter"), aes(x = temp, y = coef)) + 
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_line(size = 1.0, colour=col) +
  geom_ribbon(aes(ymin = min95, ymax = max95), alpha = 0.30, colour=NA, fill=col) +
  geom_point(size = 2, colour=col) +
  annotate("text", x = -Inf, y=Inf, vjust=1.65, hjust = -0.35, colour=col, size=6, label="Winter") +
  xlab(expression(paste("Max. temperature (", degree, C, ")"))) +
  ylab("Sentiment change (%)") +
  xlim(0, 40) + 
  ylim(range(c(df$min95, df$max95))) +
  theme_Publication() +
  theme(legend.position = "none")
xdsty <- axis_canvas(pmain, axis = "x") +
  geom_density(data=reg, aes(x=tmax_winter), alpha=0.5, size=0.5, colour=col, fill = col) 
p.winter <- insert_xaxis_grob(pmain, xdsty, grid::unit(.1, "null"), position = "top")

p.season <- plot_grid(p.spring, p.summer, p.fall, p.winter, ncol = 2, nrow=2,
                      align="hv", labels = c("a", "b", "c", "d"))
save_plot("season_tmax_cowplot.pdf", p.season, base_width = 10, base_height = 8)

p.hete <- plot_grid(p.spring, p.summer, p.fall, p.winter, p.gender, p.income, ncol = 2, nrow=3, 
                      align="hv", labels = c("a", "b", "c", "d", "e", "f"))
save_plot("season_gender_income_cowplot.pdf", p.hete, base_width = 10, base_height = 12)

# -------------------------------
# season plot with cowplot: prediction
# -------------------------------
cols <- pal_npg("nrc")(4)
# --- spring ----
col <- cols[1]
pmain <- ggplot(subset(df, group == "Spring"), aes(x = temp, y = coef)) + 
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_smooth(formula =  y ~ splines::ns(x, 1), fill = col, colour = "grey50", alpha = 0.30) +
  # geom_line(size = 1.0, colour=col) +
  geom_point(size = 2, colour=col) +
  annotate("text", x = -Inf, y=Inf, vjust=1.65, hjust = -0.35, colour=col, size=6, label="Spring") +
  ylab("Sentiment change (%)") +
  xlab(expression(paste("Max. temperature (", degree, C, ")"))) +
  xlim(0, 40) + 
  ylim(range(c(df$min95, df$max95))) +
  theme_Publication() +
  theme(legend.position = "none")
xdsty <- axis_canvas(pmain, axis = "x") +
  geom_density(data=reg, aes(x=tmax_spring), alpha=0.5, size=0.5, colour=col, fill = col) 
p1 <- insert_xaxis_grob(pmain, xdsty, grid::unit(.1, "null"), position = "top")
ggdraw(p1)

# --- summer ----
col <- cols[2]
pmain <- ggplot(subset(df, group == "Summer"), aes(x = temp, y = coef)) + 
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), fill = col, colour = "grey50", alpha = 0.30) +
  # geom_line(size = 1.0, colour=col) +
  geom_point(size = 2, colour=col) +
  annotate("text", x = -Inf, y=Inf, vjust=1.65, hjust = -0.35, colour=col, size=6, label="Summer") +
  ylab("Sentiment change (%)") +
  xlab(expression(paste("Max. temperature (", degree, C, ")"))) +
  xlim(0, 40) + 
  ylim(range(c(df$min95, df$max95))) +
  theme_Publication() +
  theme(legend.position = "none")
xdsty <- axis_canvas(pmain, axis = "x") +
  geom_density(data=reg, aes(x=tmax_summer), alpha=0.5, size=0.5, colour=col, fill = col)
p2 <- insert_xaxis_grob(pmain, xdsty, grid::unit(.1, "null"), position = "top")
ggdraw(p2)

# --- Fall ----
col <- cols[3]
pmain <- ggplot(subset(df, group == "Fall"), aes(x = temp, y = coef)) + 
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_smooth(method = "lm", formula = y ~ splines::ns(x, 3), fill = col, colour = "grey50", alpha = 0.30) +
  # geom_line(size = 1.0, colour=col) +
  geom_point(size = 2, colour=col) +
  annotate("text", x = -Inf, y=Inf, vjust=1.65, hjust = -0.35, colour=col, size=6, label="Fall") +
  ylab("Sentiment change (%)") +
  xlab(expression(paste("Max. temperature (", degree, C, ")"))) +
  xlim(0, 40) + 
  ylim(range(c(df$min95, df$max95))) +
  theme_Publication() +
  theme(legend.position = "none")
xdsty <- axis_canvas(pmain, axis = "x") +
  geom_density(data=reg, aes(x=tmax_autumn), alpha=0.5, size=0.5, colour=col, fill = col) +
  xlim(range(df$temp))
p3 <- insert_xaxis_grob(pmain, xdsty, grid::unit(.1, "null"), position = "top")
ggdraw(p3)

# --- winter ----
col <- cols[4]
pmain <- ggplot(subset(df, group == "Winter"), aes(x = temp, y = coef)) + 
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_smooth(method = "lm", formula = y ~ splines::ns(x, 2), fill = col, colour = "grey50", alpha = 0.30) +
  # geom_line(size = 1.0, colour=col) +
  geom_point(size = 2, colour=col) +
  annotate("text", x = -Inf, y=Inf, vjust=1.65, hjust = -0.35, colour=col, size=6, label="Winter") +
  ylab("Sentiment change (%)") +
  xlab(expression(paste("Max. temperature (", degree, C, ")"))) +
  xlim(0, 40) + 
  ylim(range(c(df$min95, df$max95))) +
  theme_Publication() +
  theme(legend.position = "none")
xdsty <- axis_canvas(pmain, axis = "x") +
  geom_density(data=reg, aes(x=tmax_winter), alpha=0.5, size=0.5, colour=col, fill = col) 
p4 <- insert_xaxis_grob(pmain, xdsty, grid::unit(.1, "null"), position = "top")
ggdraw(p4)

p <- plot_grid(p1, p2, p3, p4, ncol = 2, nrow=2, align="hv", labels = c("a", "b", "c", "d"))
save_plot("season_tmax_prediction_cowplot.pdf", p, base_width = 10, base_height = 8)

p <- ggdraw() +
  draw_plot(p.tmax.pred,  x = .25, y = .666, width = .5, height = .333) +
  draw_plot(p1, x = 0, y = .333, width = .5, height = .333) +
  draw_plot(p2, x = .5, y = .333, width = .5, height = .333) +
  draw_plot(p3, x = 0, y = 0, width = .5, height = .333) +
  draw_plot(p4, x = .5, y = 0, width = .5, height = .333) +
  draw_plot_label(c("a", "b", "c", "d", "e"), 
                  c(0.25, 0.000, 0.500, 0.000, 0.500), 
                  c(1.00, 0.666, 0.666, 0.333, 0.333))
save_plot("season_yearly_tmax_prediction_cowplot.pdf", p, base_width = 10, base_height = 12)


# -------------------------------
# Adoption
# -------------------------------
df <- read.csv("adaption.csv")

df$id <- factor(df$id, levels = df$id)
df$group <- factor(df$group, levels = c("Anomaly", "Heating", "Air conditioner"))
df$semax <- df$coef + df$se
df$semin <- df$coef - df$se

p.anomaly <- ggplot(subset(x = df, group == "Anomaly"), aes(x = id, y = coef)) +
  geom_hline(yintercept = 0, color="grey50", linetype = 1) +
  geom_bar(stat = "identity",
           fill = "#E64B35B2",
           width = 0.6,
           alpha = 0.8) +
  geom_text(
    aes(label = star),
    vjust = 1.2,
    hjust = -0.2,
    colour = "black",
    size = 6
  ) +
  ylab("Marginal effect of °C on \nsentiment change") +
  xlab("Anomaly") +
  geom_errorbar(aes(ymin = coef - se, ymax = coef + se),
                colour = "grey25",
                width = 0.1) +
  theme_Publication() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 0, vjust = 0.6)
    # panel.border = element_rect(colour = "black")
  )

p.heating <- ggplot(subset(x = df, group == "Heating"), aes(x = id, y = coef)) +
  geom_hline(yintercept = 0, color="grey50", linetype = 1) +
  geom_bar(stat = "identity",
           fill = "#4DBBD5B2",
           width = 0.6,
           alpha = 0.8) +
  geom_text(
    aes(label = star),
    vjust = -0.8,
    hjust = -0.2,
    colour = "black",
    size = 6
  ) +
  ylab("Marginal effect of °C on \nsentiment change") +
  xlab("Heating") +
  geom_errorbar(aes(ymin = coef - se, ymax = coef + se),
                colour = "grey25",
                width = 0.1) +
  scale_fill_npg() +
  theme_Publication() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 0, vjust = 0.6)
    # panel.border = element_rect(colour = "black")
  )

# p.air <- ggplot(subset(x = df, group == "Air conditioner"), aes(x = stringr::str_wrap(id, 20), y = coef)) +
#   geom_hline(yintercept = 0, color="grey50", linetype = 1) +
#   geom_bar(stat = "identity",
#            fill = "#00A087B2",
#            width = 0.6,
#            alpha = 0.8) +
#   geom_text(
#     aes(label = star),
#     vjust = 1.2,
#     hjust = -0.2,
#     colour = "black",
#     size = 6
#   ) +
#   ylab("Sentiment change (%)") +
#   xlab("Air conditioner") +
#   geom_errorbar(aes(ymin = coef - se, ymax = coef + se),
#                 colour = "grey25",
#                 width = 0.1) +
#   scale_fill_npg() +
#   theme_Publication() +
#   theme(
#     legend.position = "none",
#     axis.text.x = element_text(angle = 0, vjust = 0.6),
#     panel.border = element_rect(colour = "black")
#   )

df <- read.csv("air_conditioner.csv")
df$group <- factor(df$group, levels = c("Q1", "Q2", "Q3", "Q4"))
df$max95 <- df$coef + df$se
df$min95 <- df$coef - df$se

## ---- v1 -----
p.air <- ggplot(df, aes(x = temp, y = coef, colour = group)) +
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_line(size = 1.0) +
  geom_point(size = 2) +
  annotate("text", x = 0.30, y = -2.2, vjust = 0, hjust = 0, size = 4.5, 
           label = "Quartile of AC number per house") +
  ylab("Sentiment change (%)") +
  xlab(expression(paste("Max. temperature (", degree, C, ")"))) +
  xlim(0, 40) +
  ylim(NA, 0.5) +
  scale_fill_npg() +
  theme_Publication() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.size= unit(0.5, "cm"),
    legend.position = c(0.30, 0.15)
  )


## ---- v2 -----
df <- filter(df, temp > 20)
p.air <- ggplot(df, aes(x = temp, y = coef, colour = group)) +
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_line(size = 1.0) +
  geom_point(size = 2) +
  annotate("text", x = 21, y = -2.1, vjust = 0, hjust = 0, size = 4.5, 
           label = "Quartile of AC\nnumber per house") +
  ylab("Sentiment change (%)") +
  xlab(expression(paste("Max. temperature (", degree, C, ")"))) +
  xlim(20, 40) +
  ylim(NA, 0.5) +
  scale_fill_npg() +
  theme_Publication() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.size= unit(0.5, "cm"),
    legend.position = c(0.30, 0.15)
  )


p <- plot_grid(p.anomaly, p.heating, p.air, ncol = 3, nrow=1, align="hv", labels = c("a", "b", "c"), rel_widths = c(1.2,1.2,1.6))
save_plot("adaption-v2.pdf", p, base_width = 12, base_height = 4.5)
