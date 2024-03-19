install.packages("ISLR")
library(ISLR)

head(iris)
str(iris)

any(is.na(iris))

normalized.iris <- scale(iris[1:4])

normalized.iris <- cbind(normalized.iris,iris[5])

head(normalized.iris)

install.packages("caTools")
library(caTools)

# Set the seed for reproducibility
set.seed(101)

# Create a train/test split with a 70/30 ratio
split <- sample.split(normalized.iris$Species, SplitRatio = 0.7)

# Subset the data set into train and test sets based on the split
train_data <- subset(normalized.iris, split == TRUE)
test_data <- subset(normalized.iris, split == FALSE)

library(class)

predicted.species <- knn(train_data[1:4], test_data[1:4], train_data$Species, k=1)

mean(test_data$Species != predicted.species)

## Choosing a k value ##
## Use a for loop ##
predicted.species <- NULL
error.rate <- NULL

for (i in 1:10) {
  set.seed(101)
  predicted.species <- knn(train_data[1:4], test_data[1:4], train_data$Species, k=i)
  error.rate[i] <- mean(test_data$Species != predicted.species)
}

error.rate

## Visualize k elbow method ##
library(ggplot2)

k.values <- 1:10
error.df <- data.frame(error.rate, k.values)

head(error.df)

ggplot(error.df, aes(k.values, error.rate)) + geom_point() + geom_line(lty='dotted', color='red')

## k = 7 or 8 is the ideal value