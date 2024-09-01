# Load necessary libraries
library(tidyverse)
library(caret)
library(randomForest)

# Load the Iris dataset
data("iris")

# Display the first few rows of the dataset
head(iris)

# Check for missing values
sum(is.na(iris))

# Pairplot to visualize relationships between features
pairs(iris, col = iris$Species)

# Boxplot to visualize the distribution of each feature
iris_long <- pivot_longer(iris, cols = -Species, names_to = "Feature", values_to = "Value")
ggplot(iris_long, aes(x = Feature, y = Value, fill = Species)) +
  geom_boxplot() +
  theme_minimal()

# Split the dataset into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
iris_train <- iris[trainIndex, ]
iris_test <- iris[-trainIndex, ]

# Standardize the features
preProc <- preProcess(iris_train[, -5], method = c("center", "scale"))
iris_train_scaled <- predict(preProc, iris_train[, -5])
iris_test_scaled <- predict(preProc, iris_test[, -5])

# Initialize the Random Forest classifier
rf_model <- randomForest(Species ~ ., data = iris_train, ntree = 100, random_state = 42)

# Make predictions
rf_predictions <- predict(rf_model, iris_test)

# Evaluate the model
confusion_matrix <- confusionMatrix(rf_predictions, iris_test$Species)
print(confusion_matrix)
