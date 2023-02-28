rm(list = ls())

library(ggplot2)
library(dplyr)
library(data.table)

data <- read.csv("C:/Users/tonym/OneDrive/Documents/GitHub/modsoc/ch06_cooperation/HW6/1-PD_simple experiment-table.csv", skip = 6 )

colnames(data)[7] <- "cooperation"

ggplot(data = data, aes(x = synchronous, y = cooperation, shape = synchronous)) + 
  geom_point() + xlab("Synchonous vs Stoichastic") + ylab("Cooperation")

data <- read.csv("C:/Users/tonym/OneDrive/Documents/GitHub/modsoc/ch06_cooperation/HW6/23-PD_reciprocity experiment-table.csv", skip = 6 )

colnames(data)[19] <- "cooperation"

ggplot(data = data, aes(x = payoff.cost, y = cooperation, shape = factor(payoff.cost))) + 
  geom_point() + xlab("Low vs high cost") + ylab("Cooperation")

replicator <- function (p) {
  
  va <- p*(b - c) / (1 - w) - (1 - p)*c
  vb <- p*b
  return(
    p*(1-p) * (va - vb) / (p*va + (1 - p)*vb)
  )
}

b <- 1
c <- 0.5
p <- seq(0,1,by=0.02)

w <- 0.8

phat <- (1 - w)*c / (w*(b - c))

deltap <- replicator(p)

df <- data.frame(p, deltap)

df$w <- 0.8
df$phat <- phat

replicator_df <- df

w <- 0.5

phat <- (1 - w)*c / (w*(b - c))

deltap <- replicator(p)

df <- data.frame(p, deltap)

df$w <- 0.5
df$phat <- phat

replicator_df <- rbind(replicator_df, df)

w <- 0.2

phat <- (1 - w)*c / (w*(b - c))

deltap <- replicator(p)

df <- data.frame(p, deltap)

df$w <- 0.2
df$phat <- phat

replicator_df <- rbind(replicator_df, df)

ggplot(data = replicator_df, aes(x = p, y = deltap, color = factor(w))) + 
  geom_point() + xlab("p") + ylab("delta p") + geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0.25, color = "blue") +
  geom_vline(xintercept = 1, color = "green")