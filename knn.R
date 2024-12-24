#upload the dataset given the file_path
movies <- read.csv("file_path", header = TRUE)


summary(movies[1:10])
#check types of the first columns
print(sapply(movies[, 1:11, drop = FALSE], class))
#there are character type...
#convert ReleaseDate in DATE type
#install.packages("lubridate")
library(lubridate)
movies$ReleaseDate <- dmy(movies$ReleaseDate)
sum(is.na(movies$ReleaseDate))
#convert Rating_MyMovies in NUMERIC type
movies$Rating_MyMovies <- gsub(",", ".", movies$Rating_MyMovies)
movies$Rating_MyMovies <- as.numeric(movies$Rating_MyMovies)
movies$Rating_MyMovies[is.na(movies$Rating_MyMovies)] <- 0
sum(is.na(movies$Rating_MyMovies))
#convert Rating_Critic in NUMERIC type
movies$Rating_Critic <- gsub(",", ".", movies$Rating_Critic)
movies$Rating_Critic <- as.numeric(movies$Rating_Critic)
movies$Rating_Critic[is.na(movies$Rating_Critic)] <- 0
sum(is.na(movies$Rating_Critic))
#convert Rating_Public in NUMERIC type
movies$Rating_Public <- gsub(",", ".", movies$Rating_Public)
movies$Rating_Public <- as.numeric(movies$Rating_Public)
movies$Rating_Public[is.na(movies$Rating_Public)] <- 0
sum(is.na(movies$Rating_Public))
#convert Rating_Average in NUMERIC type
movies$Rating_Average <- gsub(",", ".", movies$Rating_Average)
movies$Rating_Average <- as.numeric(movies$Rating_Average)
# dove Rating_Average è NA, faccio la media delle altre colonne Rating 
# (solo se diverse da 0 - visto che prima ho messo a 0 i NA)
library(dplyr)
movies <- movies %>%
  mutate(
    Rating_Count = rowSums(select(., c("Rating_MyMovies", "Rating_Critic", "Rating_Public")) != 0),
    Rating_Average = ifelse(
      is.na(Rating_Average),
      ifelse(Rating_Count > 0, rowSums(select(., c("Rating_MyMovies", "Rating_Critic", "Rating_Public")), na.rm = TRUE) / Rating_Count, NA),
      Rating_Average
    )
  ) %>%
  select(-Rating_Count)
sum(is.na(movies$Rating_Average))
movies <- movies %>%
  filter(!is.na(Rating_Average))
#convert Revenue in NUMERIC type
movies$Revenue <- as.numeric(movies$Revenue)
#movies$Revenue[is.na(movies$Revenue)] <- ???
sum(is.na(movies$Revenue))
#convert Wikipedia_trends in NUMERIC type
movies$Wikipedia_trends <- as.numeric(movies$Wikipedia_trends)
movies$Wikipedia_trends[is.na(movies$Wikipedia_trends)] <- 0
sum(is.na(movies$Wikipedia_trends))
#convert Max_instafollowers in NUMERIC type
movies$Max_instafollowers <- gsub(",", ".", movies$Max_instafollowers)
movies$Max_instafollowers <- as.numeric(movies$Max_instafollowers)
sum(is.na(movies$Max_instafollowers))
movies$Max_instafollowers[is.na(movies$Max_instafollowers)] <- 0
#guardo quale valore ha l'indice massimo

# RATING AVERAGE ->binary value
movies$Rating_Average <- ifelse(movies$Rating_Average >= 3, 1, 0)
#movies$Rating_Average <- as.factor(movies$Rating_Average)
# Remove unnecessary columns
movies <- subset(movies, select = -c(Rating_MyMovies, Rating_Critic, Rating_Public,MovieName))
nrow(movies)
sum(!complete.cases(movies)) 
#ci sono 20 righe con NA, le rimuovo
movies <- movies[complete.cases(movies), ]
#to check if somethig is still character
names(movies)[sapply(movies, is.character)]
# Convert "Suggested_audienceNC.14" to binary (1 or 0)
movies$Suggested_audienceNC.14 <- ifelse(movies$Suggested_audienceNC.14 == "Suggested_audienceNC-14", 1, 0)

nrow(movies)
0.7*nrow(movies)

library(dplyr)
library(scales)
numeric_cols <- names(movies)[sapply(movies, is.numeric)]
variables_to_scale <- numeric_cols
scaled_dataset <- movies %>%
  mutate(across(all_of(variables_to_scale), rescale))
head(scaled_dataset)
movies<-scaled_dataset

set.seed(2024)
train_sample <- sample(1:nrow(movies), round(0.65*nrow(movies)))
train <- movies[train_sample, ]
test <- movies[-train_sample,]

###############
###SCALING####
#############
library(dplyr)
library(scales)

#scaling del train
numeric_cols <- names(train)[sapply(train, is.numeric)]
variables_to_scale <- numeric_cols
scaled_dataset <- train %>%
  mutate(across(all_of(variables_to_scale), rescale))
head(scaled_dataset)
train<-scaled_dataset

any(is.na(train))

#scaling del test
numeric_cols <- names(test)[sapply(test, is.numeric)]
variables_to_scale <- numeric_cols
scaled_dataset <- test %>%
  mutate(across(all_of(variables_to_scale), rescale))
head(scaled_dataset)
test<-scaled_dataset

any(is.na(test))

#tentativo di oversampling del training set

#install.packages("ROSE")
#library(ROSE)
#class_distribution <- table(balanced_data$Rating_Average)
#print(class_distribution)
#balanced_data <- ovun.sample(Rating_Average ~ ., data = train, method = "both", p = 0.5, N = 200)$data
#train<-balanced_data

# For the training set
X_train <- train[, !names(train) %in% "Rating_Average", drop=FALSE]
y_train <- train$Rating_Average

# For the test set
X_test <- test[, !names(test) %in% "Rating_Average", drop=FALSE]
y_test <- test$Rating_Average






X_numeric <- X_train[, sapply(X_train, is.numeric)]
x_matrix=as.matrix(X_numeric)
y=y_train


X_numeric_test <- X_test[, sapply(X_test, is.numeric)]
x_matrix_test=as.matrix(X_numeric_test)


#example of knn implementation
library(class)  # For k-NN implementation
# Assuming X_train and X_test are your training and testing feature matrices
# Assuming y_train contains corresponding training labels

# Train k-NN model
k <- 16  #set 2 as the number of neightbours
knn_model <- knn(train = x_matrix, test = x_matrix_test, cl = y_train, k = k)
#predict
predictions <- knn_model  # Predictions made by k-NN model


#grafico di dispersione per coppie di variabili


# Compute accuracy (for example)
accuracy <- mean(predictions == y_test)
print(accuracy)


#try knn with cross validation
library(caret)
y_train <- factor(y_train, levels = c(0, 1))
# Define cross-validation method and parameters
ctrl <- trainControl(method = "cv",   # Cross-validation method (e.g., "cv" for k-fold)
                     number = 5,      # Number of folds 3 funziona già bene
                     verboseIter = TRUE)  # Print progress during training




dim(x_matrix)

# Define the model and train using caret's train function
model <- train(x = x_matrix,                 # Features
               y = y_train,                 # Labels
               method = "knn",        # Method: k-NN
               trControl = ctrl,      # Training control
               tuneGrid = expand.grid(k = seq(2, 100)))  # Define hyperparameter grid
# Access model performance metrics (e.g., accuracy)
summary(model)

# Assuming 'model' contains the results from the train() function

library(ggplot2)

# Extract the results for plotting
results <- model$results

# Plot accuracy vs. k values
ggplot(results, aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  labs(x = "k values", y = "Accuracy") +
  ggtitle("Accuracy vs. k values")


# Make predictions on new data (if needed)
predictions <- predict(model, newdata = x_matrix_test)

conf_matrix <- table(predictions, y_test)
# Calculate accuracy, precision, recall
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score = 2 * (precision * recall) / (precision + recall)

cat("Classification Results:\n", "Accuracy:", round(accuracy, 7), "\n", "Precision:", round(precision, 7), "\n",
    "Recall", round(recall, 7), "\n","f1-score", round(f1_score, 7),  "\n\n")
conf_matrix
