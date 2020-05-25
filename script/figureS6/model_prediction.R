setwd("~/Documents/GitHub/climate_sentiment/script/figureS6/")
devtools::source_gist("https://gist.github.com/Jianghao/d806dcc220a49df21024e0516f102b2f")
library(tidyverse)
library(haven)
library(cowplot)
library(ggjoy)
library(lubridate)

reg <- readRDS(file = "../../data/stataReg/stata_reg_data.Rds")

# ------------------------
# tmax: Prediction
# ------------------------
df <- read.csv("../../data/regression/tmax.csv")
df$max95 <- df$coef + df$se
df$min95 <- df$coef - df$se

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

# -------------------------------
# season plot with cowplot: prediction
# -------------------------------
df <- read.csv("../../data/regression/season_tmax.csv")
df$max95 <- df$coef + df$se
df$min95 <- df$coef - df$se
df$group <- factor(df$group, levels = c("Spring", "Summer", "Fall", "Winter"))


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

p <- ggdraw() +
  draw_plot(p.tmax.pred,  x = .25, y = .666, width = .5, height = .333) +
  draw_plot(p1, x = 0, y = .333, width = .5, height = .333) +
  draw_plot(p2, x = .5, y = .333, width = .5, height = .333) +
  draw_plot(p3, x = 0, y = 0, width = .5, height = .333) +
  draw_plot(p4, x = .5, y = 0, width = .5, height = .333) +
  draw_plot_label(c("A", "B", "C", "D", "E"), 
                  c(0.25, 0.000, 0.500, 0.000, 0.500), 
                  c(1.00, 0.666, 0.666, 0.333, 0.333))
save_plot("season_yearly_tmax_prediction_cowplot.pdf", p, base_width = 10, base_height = 12)
