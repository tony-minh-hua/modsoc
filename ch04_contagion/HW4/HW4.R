rm(list = ls())

library(ggplot2)
library(dplyr)

pick <- function(condition){
  function(d) d %>% filter_(condition)
}

data <- read.csv("C:/Users/tonym/OneDrive/Documents/GitHub/modsoc/ch04_contagion/HW4/1-contagion SIR contagion batch 1-table.csv", skip = 6 )

ggplot(data = data, aes(x = prop.safe, y = maximum.infection.rate, color = variable)) + 
  geom_point(data = pick(~num.turtles == 50), aes(y = maximum.infection.rate, col = "50 turtles")) +
  geom_point(data = pick(~num.turtles == 200), aes(y = maximum.infection.rate, col = "200 turtles")) +
  geom_point(data = pick(~num.turtles == 500), aes(y = maximum.infection.rate, col = "500 turtles)")) +
  ggtitle("Prop-safe versus max-infect-rate by 50, 200, 500 turtles")

vaccinate <- function(efficacy) {
  return(1/efficacy * (1 - 0.05/0.1))
}

input <- seq(0.05,1,by=0.05)

vaccine_df <- data.frame(input)

output <- vaccinate(input)

vaccine_df$output <- output

ggplot(data = vaccine_df, aes(x = input, y = output)) + 
  geom_point() + xlab("Efficacy") + ylab("Vaccination Threshold")

data_zombies <- read.csv("C:/Users/tonym/OneDrive/Documents/GitHub/modsoc/ch04_contagion/HW4/3-zombies! zombie batch-table.csv", skip = 6 )

data_zombies %>% 
  ggplot(aes(x=zombie.death.rate,y=dead.zombies,group=zombie.speed,color=zombie.speed))+
  geom_point()+
  geom_smooth(method = 'lm',se=F) +
  xlab("How fast zombies decomposed") + ylab("Casualties of the zombie war") +
  ggtitle("Zombie death rate versus total zombification by relative speed")