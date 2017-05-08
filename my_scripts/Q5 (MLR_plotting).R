######################################
# 04.30.2017
# Plotting MLR results for Mad, Max, and Myc
# BISC 577
######################################

## Install and initialize packages
install.packages("ggplot2")
install.packages("grid")
library(ggplot2)
library(grid)

## Theme
my.theme <- theme(
  plot.margin = unit(c(0.1, 0.5, 0.1, 0.1), "cm"),
  axis.text = element_text(colour="black", size=12),
  axis.title.x = element_text(colour="black", size=12),
  axis.title.y = element_text(colour="black", size=12),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "black"),
  axis.ticks = element_line(colour = "black")
)

## Data preparation
sequence <- c(0.7748677, 0.7855400, 0.7778829)
sequence_shape <- c(0.8633198, 0.8642059, 0.8545234)

## Ploting
ggplot() +
  geom_point(aes(x = sequence, y = sequence_shape), color = "red", size=1) +
  geom_abline(slope=1) + geom_vline(xintercept=0) + geom_hline(yintercept=0) +
  coord_fixed(ratio = 1, xlim = c(0,1), ylim = c(0,1)) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  labs(x = "1-mer model " ~R^2~ "", y = "1-mer + shape model " ~R^2~ "") +
  my.theme  
