setwd("~/Documents/GitHub/climate_sentiment/script/figureS4/")
devtools::source_gist("https://gist.github.com/Jianghao/d806dcc220a49df21024e0516f102b2f")
library(tidyverse)
library(haven)
library(cowplot)
library(ggjoy)
library(lubridate)

# reg <- read_dta(file = "../../data/stataReg/stata_reg_data.Rds")
reg <- readRDS(file = "../../data/stataReg/stata_reg_data.Rds")

# ------------------------
# tmax - 3bin
# ------------------------
df <- read.csv("../../data/regression/tmax_3bin.csv")
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
# tmax North - 3bin
# ------------------------
df <- read.csv("../../data/regression/tmax_north_3bin.csv")
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
# tmax South - 3bin
# ------------------------
df <- read.csv("../../data/regression/tmax_south_3bin.csv")
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
# cowplot p.tmax.3bin, p.north.3bin, p.south.3bin
# ------------------------
p.main <- plot_grid(p.tmax.3bin, p.north.3bin, p.south.3bin, ncol = 2, nrow=2, 
                    align="hv", labels = c("a", "b", "c"))
p.main <- ggdraw() +
  draw_plot(p.tmax.3bin,  x = .25, y = .5, width = .5, height = .5) +
  draw_plot(p.north.3bin, x = 0, y = 0, width = .5, height = .5) +
  draw_plot(p.south.3bin, x = .5, y = 0, width = .5, height = .5) +
  draw_plot_label(c("A", "B", "C"), c(0.25, 0, 0.5), c(1, 0.5, 0.5))
save_plot("tmax_north_south_3bin.pdf", p.main, base_width = 10, base_height = 8)

