setwd("~/Documents/GitHub/climate_sentiment/script/figure4/")
devtools::source_gist("https://gist.github.com/Jianghao/d806dcc220a49df21024e0516f102b2f")
library(tidyverse)
library(haven)
library(cowplot)
library(ggjoy)
library(lubridate)

# -------------------------------
# Adoption
# -------------------------------
df <- read.csv("../../data/regression/adaption.csv")

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

df <- read.csv("../../data/regression/air_conditioner.csv")
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


p <- plot_grid(p.anomaly, p.heating, p.air, ncol = 3, nrow=1, align="hv", labels = c("A", "B", "C"), rel_widths = c(1.2,1.2,1.6))
save_plot("adaption-v2.pdf", p, base_width = 14, base_height = 4.5)
