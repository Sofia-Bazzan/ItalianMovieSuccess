#upload the dataset given the file path
#movies <- read.csv("file_path", header = TRUE)


#if necessary:
movies_original <- read.csv("file_path", header = TRUE)
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
# CORRELATION MATRIX (illeggibile)
numeric_columns <- sapply(movies, is.numeric)
numeric_data <- movies[, numeric_columns]


##########################
####PARTE NUOVA#########
########################


#Sistemo la colonna Amore e relazioni che doveva far parte di temi
#(dava problemi con i raggruppamenti)
names(movies)[names(movies) == "Amore.e.relazioni"] <- "Temi.Amore.e.relazioni"
#riordino le colonne in ordine alfabetico per poterle raggruppare facilmente
movies <- movies[, order(names(movies))]
print(movies[1:2,1:10])


#tentativo di undersampling per ottenere un dataset più bilanciato
#prop.table(table(movies$Rating_Average))

#install.packages("ROSE")
#library(ROSE)
#class_distribution <- table(movies$Rating_Average)
#print(class_distribution)
#minority_class_count <- class_distribution["0"]
#print(minority_class_count)
#balanced_movies <- ovun.sample(Rating_Average ~ ., data = movies, method = "under", N = 264)
#print(balanced_movies$data)
#movies<-balanced_movies$data
#OSS: SI POTREBBE PROVARE ANCHE L'OVERSAMPLING


##### TRAIN AND TEST SPLIT######
nrow(movies)
0.7*nrow(movies)
# train test
library(dplyr)
library(scales)

#scaling del dataset
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

#library(dplyr)

# Assuming your dataframe is named 'df', you can use the following code to scale only the numeric columns
#train <- train %>% mutate(across(where(is.numeric), scale))
#test <- test %>% mutate(across(where(is.numeric), scale))
#####################
##GROUP LASSO#######
###################


#scaling di train e test
library(dplyr)
library(scales)

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

#any(is.na(test))




library(gglasso)

# Preparo i dati per train e test
X_train <- train[, !names(train) %in% "Rating_Average", drop=FALSE]
y_train <- train$Rating_Average
X_test <- test[, !names(test) %in% "Rating_Average", drop=FALSE]
y_test <- test$Rating_Average


X_numeric <- X_train[, sapply(X_train, is.numeric)]

X=as.matrix(X_numeric)
Y=y_train
Y_numeric <- as.numeric(Y)




variable_names <-colnames(X)
options(max.print = 1500)
#funzione che assegna i gruppi in base alla parola iniziale così quelli
#che erano nella stessa variabile prima del one hot encoding 
#vengono raggruppati assieme 
get_number <- function(name) {
case_when(
  startsWith(name, "Director") ~ 3 ,
  startsWith(name, "Cast") ~ 2,
  startsWith(name, "Duration") ~ 4,
  startsWith(name, "Genre") ~ 5,
  startsWith(name, "Revenue") ~ 10,
  startsWith(name, "Nominations") ~ 7 ,
  startsWith(name, "Awards") ~ 1,
  startsWith(name, "Wikipedia_trends") ~ 14,
  startsWith(name, "Max_instafollowers") ~ 6 ,
  startsWith(name, "Time_setting") ~ 13,
  startsWith(name, "Place_setting") ~ 8,
  startsWith(name, "YTtrailer_views") ~ 15,
  startsWith(name, "Suggested_audience") ~ 11 ,
  startsWith(name, "Temi") ~ 12,
  startsWith(name, "Production") ~ 9,
  TRUE ~ NA_integer_  # If no match is found, return NA
)
}

# Apply the function to create the vector of numbers
grp <- sapply(variable_names, get_number)

#check X
print(X[1:2,1:10])


# Display the resulting vector
length(grp)
has_na <- any(is.na(grp))
na_indices <- which(is.na(grp))
print(na_indices)

#fisso lambda_seq come intervallo di lambda perché a tentativi mi sembrava 
#l'unico a non avere il minimo sull'estremo
lambda_seq <- seq(0.001, 0.02, length = 200)
fit <- gglasso(x = X, y = Y, group = grp, loss = 'ls',lambda=lambda_seq )
plot(fit)
lambda_seq <- seq(0.001, 0.02, length = 200)
#fissa il numero di folds uguale a 10
fit.cv=cv.gglasso(x=X,y=Y,group=grp,nfolds=5,lambda=lambda_seq, loss="ls")
plot(fit.cv)
lmbda=fit.cv$lambda.1se
print(lmbda)
lmbda1 <- fit.cv$lambda.min
print(lmbda1)

#utilizzo lambda.min che non si trova all'estremo dell'intervallo

plot(fit)
abline(v=log(lmbda), lty=2, col=2)
abline(v=log(lmbda1), lty=2, col=2)

refit_model <- gglasso(x = X, y = Y, group = grp, lambda = lmbda1, loss = 'ls')

coefficients <- coef(refit_model)

important_groups <- which(rowSums(abs(coefficients)) > 0)
print(important_groups)



#preparo i dati per il test
X_test_numeric <- X_test[, sapply(X_test, is.numeric)]
X_test_matrix=as.matrix(X_test_numeric)

predictions <- predict(refit_model, newx = X_test_matrix)


predicted_labels <- ifelse(predictions > 0.5, 1, 0)

# Confusion Matrix
conf_matrix <- table(predicted_labels, y_test)
conf_matrix

# Calculate accuracy, precision, recall
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score = 2 * (precision * recall) / (precision + recall)

cat("Classification Results:\n", "Accuracy:", round(accuracy, 7), "\n", "Precision:", round(precision, 7), "\n",
    "Recall", round(recall, 7), "\n","f1-score", round(f1_score, 7),  "\n\n")
conf_matrix
