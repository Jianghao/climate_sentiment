setwd("~/Documents/GitHub/climate_sentiment/script/figure3/")
devtools::source_gist("https://gist.github.com/Jianghao/d806dcc220a49df21024e0516f102b2f")
library(tidyverse)
library(haven)
library(cowplot)
library(ggjoy)
library(lubridate)

reg <- readRDS(file = "../../data/stataReg/stata_reg_data.Rds")

# ------------------------
# hete of gender: Female Male
# ------------------------
df <- read.csv("../../data/regression/gender_tmax.csv")
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
df <- read.csv("../../data/regression/income_tmax.csv")
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
df <- read.csv("../../data/regression/season_tmax.csv")
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
                      align="hv", labels = c("A", "B", "C", "D"))
save_plot("season_tmax_cowplot.pdf", p.season, base_width = 10, base_height = 8)

p.hete <- plot_grid(p.spring, p.summer, p.fall, p.winter, p.gender, p.income, ncol = 2, nrow=3, 
                    align="hv", labels = c("A", "B", "C", "D", "E", "F"))
save_plot("season_gender_income_cowplot.pdf", p.hete, base_width = 10, base_height = 12)

