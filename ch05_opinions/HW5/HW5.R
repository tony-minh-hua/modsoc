rm(list = ls())

library(ggplot2)
library(dplyr)

data <- read.csv("C:/Users/tonym/OneDrive/Documents/GitHub/modsoc/ch05_opinions/HW5/1-opiniondynamics_posinfluence experiment-table.csv", skip = 6 )

ggplot(data = data, aes(x = learning.rate, y = ticks)) + 
  geom_point() + xlab("Learning Rate") + ylab("Ticks") + geom_line()


data <- read.csv("C:/Users/tonym/OneDrive/Documents/GitHub/modsoc/ch05_opinions/HW5/3-opiniondynamics_multi experiment-table.csv", skip = 6 )

ggplot(data = data, aes(x = polarization, y = extremism, shape = spatial.interactions., color = factor(distribution.range))) + 
  geom_point(alpha = 0.5) +
  ggtitle("Polarization and Extremism")

data <- read.csv("C:/Users/tonym/OneDrive/Documents/GitHub/modsoc/ch05_opinions/HW5/4-opiniondynamics_multi experiment-table.csv", skip = 6 )

ggplot(data = data, aes(x = polarization, y = extremism, color = prob.random)) + 
  geom_point(alpha = 0.5) +
  ggtitle("Polarization and Extremism")