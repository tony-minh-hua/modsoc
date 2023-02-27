rm(list = ls())

library(ggplot2)
library(dplyr)

data <- read.csv("C:/Users/tonym/OneDrive/Documents/GitHub/modsoc/ch06_cooperation/HW6/1-PD_simple experiment-table.csv", skip = 6 )

colnames(data)[7] <- "cooperation"

ggplot(data = data, aes(x = synchronous, y = cooperation, shape = synchronous)) + 
  geom_point() + xlab("Synchonous vs Stoichastic") + ylab("Cooperation")