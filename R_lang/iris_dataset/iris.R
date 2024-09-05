# Load necessary libraries
library(tidyverse)
library(caret)
library(randomForest)
library(GGally)


# Load the Iris dataset
data("iris")

# Display the first few rows of the dataset
head(iris)

# Check for missing values
sum(is.na(iris))

# Pairplot to visualize relationships between features
pairs(iris, col = iris$Species)

ggpairs(iris,   
        aes(color = Species), 
        title = "Pairs Plot of Iris Dataset",
        upper = list(continuous = wrap("cor", size = 5)),
        lower = list(continuous = wrap("smooth", size = 0.5)),
        diag = list(continuous = wrap("barDiag")))

# Boxplot to visualize the distribution of each feature
iris_long <- pivot_longer(iris, cols = -Species, names_to = "Feature", values_to = "Value")
view(iris_long)
ggplot(iris_long, aes(x = Feature, y = Value, fill = Species)) +
  geom_boxplot() +
  theme_minimal()

ggplot(iris_long,aes(x = Feature, y=Value)) +
  geom_boxplot(aes(fill=Species), outliers = FALSE)+
  scale_color_brewer(palette="Dark2")+
  theme_minimal()


##### RandomForest Model Train
# Split the dataset into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
trainIndex
iris_train <- iris[trainIndex, ]
iris_train
iris_test <- iris[-trainIndex, ]
iris_test


# Standardize the features
preProc <- preProcess(iris_train[, -5], method = c("center", "scale"))
iris_train_scaled <- predict(preProc, iris_train)
iris_test_scaled <- predict(preProc, iris_test[, -5])

# Initialize the Random Forest classifier
rf_model <- randomForest(Species ~ ., data = iris_train_scaled, ntree = 1000, random_state = 42)

# Make predictions
rf_predictions <- predict(rf_model, iris_test_scaled)

# Evaluate the model
confusion_matrix <- confusionMatrix(rf_predictions, iris_test$Species)
print(confusion_matrix)

# Custom Input model Test
input_data <- data.frame(
  Sepal.Length=c(5.1,6.2,5.7),
  Sepal.Width=c(3.5,3.2,2.8),
  Petal.Length=c(1.4,4.5,4.5),
  Petal.Width=c(0.2,1.5,1.3)
)

input_data_scaled <- predict(preProc, input_data) 
predict(rf_model, input_data_scaled)

########
## RandomForest and decision tree based models do not necessarily need standardization

# Split the dataset into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
iris_train <- iris[trainIndex, ]
iris_test <- iris[-trainIndex, ]

# Initialize the Random Forest classifier
rf_model <- randomForest(Species ~ ., data = iris_train, ntree = 1000, random_state = 42)

# Make predictions
rf_predictions <- predict(rf_model, iris_test)

# Evaluate the model
confusion_matrix <- confusionMatrix(rf_predictions, iris_test$Species)
print(confusion_matrix)

# Custom Input model Test
input_data <- data.frame(
  Sepal.Length=c(5.1,6.2,5.7),
  Sepal.Width=c(3.5,3.2,2.8),
  Petal.Length=c(1.4,4.5,4.5),
  Petal.Width=c(0.2,1.5,1.3)
)

predict(rf_model, input_data)
