setwd("~/Documents/GitHub/climate_sentiment/script/figure2/")
devtools::source_gist("https://gist.github.com/Jianghao/d806dcc220a49df21024e0516f102b2f")
library(tidyverse)
library(haven)
library(cowplot)
library(ggjoy)
library(lubridate)

# reg <- read_dta(file = "../../data/stataReg/stata_reg_data.Rds")
reg <- readRDS(file = "../../data/stataReg/stata_reg_data.Rds")

# ------------------------
# tmax
# ------------------------
df <- read.csv("../../data/regression/tmax.csv")
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
# tmax North
# ------------------------
df <- read.csv("../../data/regression/tmax_north.csv")
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
# tmax South
# ------------------------
df <- read.csv("../../data/regression/tmax_south.csv")
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