knitr::opts_chunk$set(size = "small")

load("archivist_20200730.rda")
options(width = 70)

library(ggplot2)
theme_ema <- theme(text = element_text(color = "black", size = 12),
                   plot.title = element_text(color = "black", size = 14, hjust = 0), 
                   plot.subtitle = element_text(color = "black", hjust = 0), 
                   axis.text = element_text(color = "black", size = 12), 
                   axis.text.x = element_text(color = "black", size = 12), 
                   axis.text.y = element_text(color = "black", size = 12), 
                   axis.title = element_text(color = "black", size = 12), 
                   legend.text = element_text(color = "black", size = 12), 
                   strip.text = element_text(color = "black", size = 12, hjust = 0))
