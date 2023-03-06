rm(list = ls())

library(ggplot2)
library(dplyr)
library(data.table)

data <- read.csv("C:/Users/tonym/OneDrive/Documents/GitHub/modsoc/ch07_coordination/HW7/2-coordination_simple experiment-table.csv", skip = 6 )

ggplot(data = data, aes(x = coordination.benefit, y = X.step.)) + 
  geom_point() + xlab("Coordination Benefit") + ylab("Ticks") +
  geom_smooth(method="lm", aes(color="Exp Decay"), formula= (y ~ exp(-x)), se=FALSE, linetype = 1)

data <- read.csv("C:/Users/tonym/OneDrive/Documents/GitHub/modsoc/ch07_coordination/HW7/3-coordination_asymmetric experiment-table.csv", skip = 6 )

colnames(data)[8] <- "norm"

ggplot(data = data, aes(x = init.norm1, y = (2 - norm))) + 
  geom_point(alpha = 0.1) + xlab("Initial p") + ylab("Norms") +
  geom_vline(xintercept = 0.33, color = "blue")

data <- read.csv("C:/Users/tonym/OneDrive/Documents/GitHub/modsoc/ch07_coordination/HW7/3-coordination_asymmetric experiment 2-table.csv", skip = 6 )

colnames(data)[8] <- "norm"

ggplot(data = data, aes(x = init.norm1, y = (2 - norm))) + 
  geom_point(alpha = 0.1) + xlab("Initial p") + ylab("Norms") +
  geom_vline(xintercept = 0.33, color = "blue")

data <- read.csv("C:/Users/tonym/OneDrive/Documents/GitHub/modsoc/ch07_coordination/HW7/coordination_divisionoflabor experiment a-table.csv", skip = 6 )

ggplot(data = data, aes(x = prob.outgroup.observation, y = freq.norm1.groupA)) + 
  geom_point(alpha = 0.1, aes(color = "Group A")) +
  geom_point(alpha = 0.1, aes(y = freq.norm1.groupB, color = "Group B")) + xlab("Probability of outgroup observation, m") + ylab("Norm1 Frequency")

