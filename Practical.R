library(ggplot2)
library(GGally)
library(corrplot)
library(caTools)
library(class)
library(caret)
dataset <- read.csv("diabetes.csv")
head(dataset)

dim(dataset)

#summary of the dataset
summary(dataset)
#structure of the dataset
str(dataset)
# Count missing values in each column
missing_count <- colSums(is.na(dataset))
print(missing_count)

#Data visualization of Outcome 
# Data visualization of Outcome 
# Plot the distribution of Outcome
outcome_plot <- ggplot(dataset, aes(x = factor(Outcome))) +
  geom_bar(fill = "#ff7f0e", color = "black") +
  labs(title = "Data Distribution of Outcome column", x = "Outcome", y = "Count")

# Display the Outcome plot
print(outcome_plot)



# Create the scatterplot matrix
pair_plot <- ggpairs(dataset, aes(color = factor(Outcome)))

# Display the scatterplot matrix
print(pair_plot)

# Calculate the correlation matrix
correlation_matrix <- cor(dataset, use="complete.obs")

# Create the correlation heatmap
corrplot(correlation_matrix, method = "color", addCoef.col = "black")

# Create a copy of the dataset
dataset <- dataset

# Replace zero values with NA in selected columns
cols_to_replace <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")
dataset[cols_to_replace][dataset[cols_to_replace] == 0] <- NA

missing_count <- colSums(is.na(dataset))
print(missing_count)

# Replace NA values with mean values in selected columns
dataset$Glucose[is.na(dataset$Glucose)] <- mean(dataset$Glucose, na.rm = TRUE)
dataset$BloodPressure[is.na(dataset$BloodPressure)] <- mean(dataset$BloodPressure, na.rm = TRUE)
dataset$SkinThickness[is.na(dataset$SkinThickness)] <- mean(dataset$SkinThickness, na.rm = TRUE)
dataset$Insulin[is.na(dataset$Insulin)] <- mean(dataset$Insulin, na.rm = TRUE)
dataset$BMI[is.na(dataset$BMI)] <- mean(dataset$BMI, na.rm = TRUE)

missing_count <- colSums(is.na(dataset))
print(missing_count)


# Split the dataset into training and test sets
set.seed(123)
split <- sample.split(dataset$Outcome, SplitRatio = 0.75)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

head(training_set)
head(test_set)

# Standardize the numerical features in the training set
training_set[, 1:8] <- scale(training_set[, 1:8])

# Print the head of the training set to verify
head(training_set)

# Standardize the numerical features in the test set
test_set[, 1:8] <- scale(test_set[, 1:8])

# Print the head of the test set to verify
head(test_set)

# Fit KNN model with probabilities
y_pred <- knn(train = training_set[, -9],  
              test = test_set[, -9],  
              cl = training_set[, 9], 
              k = 5,
              prob = TRUE)

# Print the head of y_pred
head(y_pred)

unique(y_pred)
unique(test_set$Outcome)

sum(is.na(y_pred))
sum(is.na(test_set$Outcome))

test_set$Outcome <- factor(test_set$Outcome, levels = levels(y_pred))
pm <- confusionMatrix(y_pred, test_set$Outcome)
accuracyvr <- pm$overall["Accuracy"]
precision <- pm$byClass["Pos Pred Value"]
recall <- pm$byClass["Sensitivity"]
fscore <- 2 * (precision * recall) / (precision + recall)

# Store KNN performance metrics
model_metrics <- c(Accuracy = accuracyvr, Precision = precision, Recall = recall, F1_Score = fscore)
model_metrics

# Plot the model performance metrics
ggplot(data = data.frame(metrics = model_metrics, metric_names = names(model_metrics)), aes(x = metric_names, y = metrics)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Knn Model Performance Metrics", x = "Metric", y = "Value")


# Define the input data for prediction
new_data <- data.frame(
  Pregnancies = 1,
  Glucose = 148,
  BloodPressure = 72,
  SkinThickness = 35,
  Insulin = 79.799,
  BMI = 33.6,
  DiabetesPedigreeFunction = 0.627,
  Age = 50
)

# Make sure there are no missing values in the new data
if (anyNA(new_data)) {
  stop("New data contains missing values. Please ensure all values are provided.")
}

# Make predictions using the KNN model
y_pred_new <- knn(train = training_set[, -9], test = new_data, 
                  cl = training_set[, 9], k = 5)

# Print the predicted class
print(y_pred_new)

# Print the prediction interpretation
if (y_pred_new == 1) {
  print("Diabetic")
} else {
  print("Non-Diabetic")
}

