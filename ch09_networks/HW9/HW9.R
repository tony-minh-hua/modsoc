rm(list = ls())

require(aod)
library(ggplot2)
library(dplyr)
library(data.table)

data <- data.frame(c(1, 3, 3, 3, 2))

names(data)[1] = "Degree"

ggplot(data, aes(x=Degree)) + geom_histogram() +
  xlim(0, 5)

data <- read.csv("C:/Users/tonym/OneDrive/Documents/GitHub/modsoc/ch09_networks/HW9/3-SmallWorldDiffusion experiment 1-table.csv", skip = 6 )

data$logstep <- log(data$X.step.)
data$logrewiring <- log(data$rewiring.probability)

ggplot(data = data, aes(x = logrewiring, y = logstep, color = as.factor(prob.spread.one))) + 
  geom_point(alpha = 0.5) + xlab("Log Rewiring Prob") + ylab("Log Time Steps")
