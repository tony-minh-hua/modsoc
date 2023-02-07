rm(list = ls())

library(ggplot2)
library(dplyr)

segregration_data <- read.csv("C:/Users/tonym/OneDrive/Desktop/COGS 222/practice-segregation experiment 1-table.csv", skip = 6 )

ggplot(data = segregration_data[segregration_data$density == 0.45,], aes(x = similarity.threshold, y = average.similarity)) + 
  geom_point(color='blue') +
  ggtitle("Low Density - average sim")

ggplot(data = segregration_data[segregration_data$density == 0.9,], aes(x = similarity.threshold, y = average.similarity)) + 
  geom_point(color='blue') +
  ggtitle("High Density - average sim")

ggplot(data = segregration_data[segregration_data$density == 0.45,], aes(x = similarity.threshold, y = prop.uniform)) + 
  geom_point(color='blue') +
  ggtitle("Low Density - prop uniform")

ggplot(data = segregration_data[segregration_data$density == 0.9,], aes(x = similarity.threshold, y = prop.uniform)) + 
  geom_point(color='blue') +
  ggtitle("High Density - prop uniform")


segregration_data_averages <- segregration_data %>%
  group_by(similarity.threshold, density) %>%
  summarize(density_avg = mean(average.similarity),
            density_prop = mean(prop.uniform)
            )

segregration_data_diff <- data.frame(segregration_data_averages[segregration_data_averages$density == 0.45,]$similarity.threshold)

colnames(segregration_data_diff) = "threshold"

segregration_data_diff$avg <- segregration_data_averages[segregration_data_averages$density == 0.9,]$density_avg - segregration_data_averages[segregration_data_averages$density == 0.45,]$density_avg

segregration_data_diff$prop <- segregration_data_averages[segregration_data_averages$density == 0.9,]$density_prop - segregration_data_averages[segregration_data_averages$density == 0.45,]$density_prop

ggplot(data = segregration_data_diff, aes(x = threshold, y = value, color = variable)) + 
  geom_point(aes(y = avg, col = "diff avg (high - low)")) +
  geom_point(aes(y = prop, col = "diff prop (high - low)")) +
  ggtitle("High density - low density segregration measures")



random_walk_data <- read.csv("C:/Users/tonym/OneDrive/Desktop/COGS 222/4-random-walk experiment 1-table.csv", skip = 6 )

ggplot(data = random_walk_data, aes(x = turning.angle, y = distance.from.center)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

random_walk_data_average <- random_walk_data %>%
  group_by(turning.angle) %>%
  dplyr::summarize(mean.distance.from.center = mean(distance.from.center, na.rm=TRUE))

ggplot(data = random_walk_data_average, aes(x = turning.angle, y = mean.distance.from.center)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)