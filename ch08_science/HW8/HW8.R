rm(list = ls())

require(aod)
library(ggplot2)
library(dplyr)
library(data.table)

data <- read.csv("C:/Users/tonym/OneDrive/Documents/GitHub/modsoc/ch08_science/HW8/science_pub-bias experiment 1-table.csv", skip = 6 )

ggplot(data = data[data$true.hypothesis. == "false",], aes(x = initial.prior, y = prob.true, color = as.factor(pub.bias), shape = true.hypothesis.)) + 
  geom_point(alpha = 0.5) + xlab("Initial Prior") + ylab("Canonized as True or False")

ggplot(data = data[data$true.hypothesis. == "true",], aes(x = initial.prior, y = prob.true, color = as.factor(pub.bias), shape = true.hypothesis.)) + 
  geom_point(alpha = 0.5) + xlab("Initial Prior") + ylab("Canonized as True or False")

count(subset(data , true.hypothesis. == "true" & pub.bias == 0.025 & prob.true < 0.01))

count(subset(data , true.hypothesis. == "true" & pub.bias == 0.05 & prob.true < 0.01))

count(subset(data , true.hypothesis. == "true" & pub.bias == 0.2 & prob.true < 0.01))

count(subset(data , true.hypothesis. == "true" & pub.bias == 0.4 & prob.true < 0.01))

lm(false.positive.rate ~ initial.prior + initial.prior*true.hypothesis. + pub.bias + true.hypothesis. + pub.bias*true.hypothesis., data = data)


lm(false.positive.rate ~ initial.prior + pub.bias + initial.prior*pub.bias, data = data[data$true.hypothesis. == "false",])

lm(false.positive.rate ~ initial.prior + pub.bias + initial.prior*pub.bias, data = data[data$true.hypothesis. == "true",])


glm(false.positive.rate ~ initial.prior + pub.bias, family = binomial(link = "probit"),
    data = data)

W <- seq(0,1,by=0.01)
df <- as.data.frame(W)
e <- 1
df$effort <- 1
df$alpha <- W / (1 + (1 - W)*e)

df_temp <- as.data.frame(seq(0,1,by=0.01))
colnames(df_temp)[1] = "W"
e <- 10
df_temp$effort <- 10
df_temp$alpha <- W / (1 + (1 - W)*e)

df <- rbind(df, df_temp)

df_temp <- as.data.frame(seq(0,1,by=0.01))
colnames(df_temp)[1] = "W"
e <- 75
df_temp$effort <- 75
df_temp$alpha <- W / (1 + (1 - W)*e)

df <- rbind(df, df_temp)

ggplot(data = df, aes(x = W, y = alpha, color = as.factor(effort))) + 
  geom_point() + xlab("Power") + ylab("False Positive Rate")


rm(list = ls())

n <- 0.1
effort <- seq(1, 100, by=1)
df <- as.data.frame(effort)
df$influence <- 0.1
df$prob.new.study <- (1 - n*log10(effort))

n <- 0.2
df_temp <- as.data.frame(seq(1, 100, by=1))
df_temp$influence <- 0.2
colnames(df_temp)[1] = "effort"
df_temp$prob.new.study <- (1 - n*log10(effort))

df <- rbind(df, df_temp)

n <- 0.3
df_temp <- as.data.frame(seq(1, 100, by=1))
df_temp$influence <- 0.3
colnames(df_temp)[1] = "effort"
df_temp$prob.new.study <- (1 - n*log10(effort))

df <- rbind(df, df_temp)

n <- 0.5
df_temp <- as.data.frame(seq(1, 100, by=1))
df_temp$influence <- 0.5
colnames(df_temp)[1] = "effort"
df_temp$prob.new.study <- (1 - n*log10(effort))

df <- rbind(df, df_temp)

ggplot(data = df, aes(x = effort, y = prob.new.study, color = as.factor(influence))) + 
  geom_point() + xlab("Effort") + ylab("Probability of a New Study")