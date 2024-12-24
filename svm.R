#upload the dataset given the path
movies <- read.csv("file_path", header = TRUE)
#if necessary:
movies_original <- read.csv("C:/Users/sofib/Desktop/file.csv", header = TRUE)
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

set.seed(2024)
library(dplyr)
library(scales)
numeric_cols <- names(movies)[sapply(movies, is.numeric)]
variables_to_scale <- numeric_cols
scaled_dataset <- movies %>%
  mutate(across(all_of(variables_to_scale), rescale))
head(scaled_dataset)
movies<-scaled_dataset


train_sample <- sample(1:nrow(movies), round(0.65*nrow(movies)))
train <- movies[train_sample, ]
test <- movies[-train_sample,]

# For the training set
X_train <- train[, !names(train) %in% "Rating_Average", drop=FALSE]
y_train <- train$Rating_Average

# For the test set
X_test <- test[, !names(test) %in% "Rating_Average", drop=FALSE]
y_test <- test$Rating_Average

##############
#SCALING
##########

#scaling del train
#numeric_cols <- names(train)[sapply(train, is.numeric)]
#variables_to_scale <- numeric_cols
#scaled_dataset <- train %>%
#  mutate(across(all_of(variables_to_scale), rescale))
#head(scaled_dataset)
#train<-scaled_dataset

#any(is.na(train))

#scaling del test
#numeric_cols <- names(test)[sapply(test, is.numeric)]
#variables_to_scale <- numeric_cols
#scaled_dataset <- test %>%
#  mutate(across(all_of(variables_to_scale), rescale))
#head(scaled_dataset)
#test<-scaled_dataset





####LASSO SVM######

library(sparseSVM)

X_numeric <- X_train[, sapply(X_train, is.numeric)]
x=as.matrix(X_numeric)
y=y_train

lasso.svm <- sparseSVM(x, y)
plot(lasso.svm, xvar="norm")
plot(lasso.svm, xvar="lambda")
#
#cv.svm <- cv.sparseSVM(x,y)
cv.svm <- cv.sparseSVM(x,y, nfolds = 20 )
cv.svm$lambda.min
plot(cv.svm )
#min
abline(v=log(cv.svm$lambda[cv.svm$min]), lty=2)
#1se rule

rule1se <- min(cv.svm$lambda[cv.svm$cve<=min(cv.svm$cve)+cv.svm$cvse[cv.svm$min]])
rule1se

abline(v=log(rule1se), col=3, lty=2)

#lambda min usa solo una variabile quindi per fare previsioni preferisco
#usare il lambda che minimizza il mean squared error


#coefficient with minimum lambda
coef.est <- coef(lasso.svm)[, cv.svm$lambda == cv.svm$lambda.min][coef(lasso.svm)[, cv.svm$lambda == cv.svm$lambda.min]!=0]

coef.est
#usa solo il genere commedia per prevedere la classe (NB: la classe 0 non 1)

coef(lasso.svm)[, cv.svm$lambda == rule1se][coef(lasso.svm)[, cv.svm$lambda == rule1se]!=0]


################
##PREDICTION####
################

X_test_numeric <- X_test[, sapply(X_test, is.numeric)]
X_test_matrix=as.matrix(X_test_numeric)

predictions<-predict(lasso.svm, X_test_matrix, lambda =rule1se )

# Confusion Matrix
conf_matrix <- table(predictions, y_test)
# Calculate accuracy, precision, recall
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score = 2 * (precision * recall) / (precision + recall)

cat("Classification Results:\n", "Accuracy:", round(accuracy, 7), "\n", "Precision:", round(precision, 7), "\n",
    "Recall", round(recall, 7), "\n","f1-score", round(f1_score, 7),  "\n\n")
conf_matrix

#OSS: con o senza scaling non cambia nulla
