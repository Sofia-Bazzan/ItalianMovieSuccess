movies <- read.csv("C:/Users/annac/OneDrive/Desktop/Modified_Movies.csv") 

#if necessary:
movies_original <- read.csv("file.csv", header = TRUE)

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
#movies$Rating_Average <- ifelse(movies$Rating_Average >= 3, 1, 0)
#movies$Rating_Average <- as.factor(movies$Rating_Average)

# Remove unnecessary columns
movies <- subset(movies, select = -c(Rating_MyMovies, Rating_Critic, Rating_Public, MovieName))

nrow(movies)

sum(!complete.cases(movies)) 
#ci sono 20 righe con NA, le rimuovo
movies <- movies[complete.cases(movies), ]

#to check if somethig is still character
names(movies)[sapply(movies, is.character)]
# Convert "Suggested_audienceNC.14" to binary (1 or 0)
movies$Suggested_audienceNC.14 <- ifelse(movies$Suggested_audienceNC.14 == "Suggested_audienceNC-14", 1, 0)


options(max.print = 80000, width = 900)


##### TRAIN AND TEST SPLIT #####

set.seed(2024)

nrow(movies)
0.7*nrow(movies)

train_sample <- sample(1:nrow(movies), round(0.65*nrow(movies)))
train <- movies[train_sample, ]
test <- movies[-train_sample,]




library(gam)

###################
##Stepwise GAM


########
unique_value_counts <- sapply(train, function(x) length(unique(x)))

# Keep only variables with at least 4 unique values
selected_variables <- names(unique_value_counts[unique_value_counts >= 4])

train_subset <- train[, c(selected_variables)]
test_subset<- test[, c(selected_variables)]

g3 <- gam(Rating_Average ~ ., data = train_subset)
train_subset
#Show the linear effects 
par(mfrow=c(3,5))
plot(g3, se=T) 

sc = gam.scope(train_subset ,arg=c("df=2","df=3","df=4", "df=5"))
g4<- step.Gam(g3, scope=sc, trace=T)
summary(g4)

AIC(g4)

par(mfrow=c(3,5))
plot(g4, se=T)

#if we want to see better some plot
par(mfrow=c(1,1))
plot(g4, se=T, ask=T)
#Prediction
p.gam <- predict(g4,newdata=test_subset)     
dev.gam <- sum((p.gam-test_subset$Rating_Average)^2)
dev.gam


######################## # Install and load the 'genlasso' package
install.packages("genlasso")
library(genlasso)
train$ReleaseDate <- as.numeric(train$ReleaseDate)

# Perform fused lasso with 1D penalty
train$ReleaseDate <- as.numeric(train$ReleaseDate)

# Perform fused lasso with 1D penalty
result <- fusedlasso1d(y = train$Revenue, gamma = 1, approx = FALSE, maxsteps = 2000,
                       minlam = 0, rtol = 1e-07, btol = 1e-07, eps = 1e-4, verbose = FALSE)

# Print the result
print(result)

# Print the result
print(result)
train$ReleaseDate<- as.matrix(train$ReleaseDate)
fusedlasso1d(train$Revenue,1, train$ReleaseDate, gamma = 0, approx = FALSE, maxsteps = 2000,
             minlam = 0, rtol = 1e-07, btol = 1e-07, eps = 1e-4,
             verbose = FALSE)

a1 = fusedlasso1d(train$Revenue)

plot(a1, lambda=1)

a2 = trendfilter(train$Revenue, ord=1)
plot(a2, lambda=10)
plot(a2, lambda=100)
plot(a2, lambda=10000)
plot(a2, lambda=100000)


a3 = trendfilter(train$Revenue, ord=2)
plot(a3, lambda=10)
plot(a3, lambda=100)
plot(a3, lambda=1000)
plot(a3, lambda=100000)

a0 = trendfilter(train$Revenue, ord=0)
plot(a0, lambda=10)
plot(a0, lambda=100)
plot(a0, lambda=1000)
plot(a0, lambda=10000)



======================================================================================
  library(dplyr)
library(glmnet)  
library(caTools)
library(caret)

set.seed(2024)

nrow(movies)
0.7*nrow(movies)
########################################ANAYSIS FOR REVENUE
train_sample <- sample(1:nrow(movies), round(0.75*nrow(movies)))
train <- movies[train_sample, ]
test <- movies[-train_sample,]
X_train<- train[,-4]
X_test<- test[,-4]
X_train<- as.matrix(X_train)
X_test<- as.matrix(X_test)
y_train<- train[4]
y_test<- test[4]
y_test<- as.numeric(y_test$Revenue)
y_train <- as.numeric(y_train$Revenue)
complete_data_train <- na.omit(cbind(y_train, X_train))
########################################################################################
########################################################################################

###############ELASTIC NET
elastic_net_cv <- cv.glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 0.3,  
  nfolds = 5,   
  parallel = TRUE,
  seed = 42
)

best_lambda <- elastic_net_cv$lambda.min


best_elastic_net <- glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 0.3,        # L1 ratio
  lambda = best_lambda
)


pred_elastic_net <- predict(best_elastic_net, newx = as.matrix(X_test))

MSE_en <- mean((y_test - pred_elastic_net)^2)
RMSE_en <- sqrt(MSE_en)
MAE_en <- mean(abs(y_test - pred_elastic_net))
R2_en <- R2(pred_elastic_net, y_test)
n <- length(y_test)
p <- ncol(X_test)

cat('Elastic Net RMSE:', RMSE_en, '\n')
cat('Elastic Net MAE:', MAE_en, '\n')
cat('Elastic Net R^2:', R2_en, '\n')


###############LASSO
lasso_cv <- cv.glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 1,   
  nfolds = 5,  
  parallel = TRUE,
  seed = 42
)


best_lambda_lasso <- lasso_cv$lambda.min


best_lasso <- glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 1,              
  lambda = best_lambda_lasso
)

pred_lasso <- predict(best_lasso, newx = as.matrix(X_test))

MSE_lasso <- mean((y_test - pred_lasso)^2)
RMSE_lasso <- sqrt(MSE_lasso)
MAE_lasso <- mean(abs(y_test - pred_lasso))
R2_lasso <- R2(pred_lasso_original_scale, y_test)
n <- length(y_test)
p <- ncol(X_test)

cat('Lasso RMSE:', RMSE_lasso, '\n')
cat('Lasso MAE:', MAE_lasso, '\n')
cat('Lasso R^2:', R2_lasso, '\n')

results_lasso <- list(
  'Model' = 'Lasso',
  'RMSE' = RMSE_lasso,
  'MAE' = MAE_lasso,
  'R-squared' = R2_lasso,
  'Adjusted R-squared' = ADJ_R2_lasso
)



########################################ANAYSIS FOR DURATION
X_train<- train[,-1]
X_test<- test[,-1]
X_train<- as.matrix(X_train)
X_test<- as.matrix(X_test)
y_train<- train[1]
y_test<- test[1]
y_test<- as.numeric(y_test$Duration)
y_train <- as.numeric(y_train$Duration)
########################################################################################
########################################################################################

###############ELASTIC NET
elastic_net_cv <- cv.glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 0.3,  
  nfolds = 5,   
  parallel = TRUE,
  seed = 42
)

best_lambda <- elastic_net_cv$lambda.min


best_elastic_net <- glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 0.3,        # L1 ratio
  lambda = best_lambda
)


pred_elastic_net <- predict(best_elastic_net, newx = as.matrix(X_test))

MSE_en <- mean((y_test - pred_elastic_net)^2)
RMSE_en <- sqrt(MSE_en)
MAE_en <- mean(abs(y_test - pred_elastic_net))
R2_en <- R2(pred_elastic_net, y_test)
n <- length(y_test)
p <- ncol(X_test)

cat('Elastic Net RMSE:', RMSE_en, '\n')
cat('Elastic Net MAE:', MAE_en, '\n')
cat('Elastic Net R^2:', R2_en, '\n')


###############LASSO
lasso_cv <- cv.glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 1,   
  nfolds = 5,  
  parallel = TRUE,
  seed = 42
)


best_lambda_lasso <- lasso_cv$lambda.min


best_lasso <- glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 1,              
  lambda = best_lambda_lasso
)

pred_lasso <- predict(best_lasso, newx = as.matrix(X_test))

MSE_lasso <- mean((y_test - pred_lasso)^2)
RMSE_lasso <- sqrt(MSE_lasso)
MAE_lasso <- mean(abs(y_test - pred_lasso))
R2_lasso <- R2(pred_lasso, y_test)
n <- length(y_test)
p <- ncol(X_test)

cat('Lasso RMSE:', RMSE_lasso, '\n')
cat('Lasso MAE:', MAE_lasso, '\n')

cat('Lasso R^2:', R2_lasso, '\n')

results_lasso <- list(
  'Model' = 'Lasso',
  'RMSE' = RMSE_lasso,
  'MAE' = MAE_lasso,
  'R-squared' = R2_lasso
)


########################################ANAYSIS FOR DURATION
X_train<- train[,-1]
X_test<- test[,-1]
X_train<- as.matrix(X_train)
X_test<- as.matrix(X_test)
y_train<- train[1]
y_test<- test[1]
y_test<- as.numeric(y_test$Duration)
y_train <- as.numeric(y_train$Duration)
########################################################################################
########################################################################################

###############ELASTIC NET
elastic_net_cv <- cv.glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 0.3,  
  nfolds = 5,   
  parallel = TRUE,
  seed = 42
)

best_lambda <- elastic_net_cv$lambda.min


best_elastic_net <- glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 0.3,        # L1 ratio
  lambda = best_lambda
)


pred_elastic_net <- predict(best_elastic_net, newx = as.matrix(X_test))

MSE_en <- mean((y_test - pred_elastic_net)^2)
RMSE_en <- sqrt(MSE_en)
MAE_en <- mean(abs(y_test - pred_elastic_net))
R2_en <- R2(pred_elastic_net, y_test)
n <- length(y_test)
p <- ncol(X_test)

cat('Elastic Net RMSE:', RMSE_en, '\n')
cat('Elastic Net MAE:', MAE_en, '\n')
cat('Elastic Net R^2:', R2_en, '\n')


###############LASSO
lasso_cv <- cv.glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 1,   
  nfolds = 5,  
  parallel = TRUE,
  seed = 42
)


best_lambda_lasso <- lasso_cv$lambda.min


best_lasso <- glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 1,              
  lambda = best_lambda_lasso
)

pred_lasso <- predict(best_lasso, newx = as.matrix(X_test))

MSE_lasso <- mean((y_test - pred_lasso)^2)
RMSE_lasso <- sqrt(MSE_lasso)
MAE_lasso <- mean(abs(y_test - pred_lasso))
R2_lasso <- R2(pred_lasso, y_test)
n <- length(y_test)
p <- ncol(X_test)

cat('Lasso RMSE:', RMSE_lasso, '\n')
cat('Lasso MAE:', MAE_lasso, '\n')

cat('Lasso R^2:', R2_lasso, '\n')

results_lasso <- list(
  'Model' = 'Lasso',
  'RMSE' = RMSE_lasso,
  'MAE' = MAE_lasso,
  'R-squared' = R2_lasso
)


########################################ANAYSIS FOR RATING AVERAGE
X_train<- train[,-3]
X_test<- test[,-3]
X_train<- as.matrix(X_train)
X_test<- as.matrix(X_test)
y_train<- train[3]
y_test<- test[3]
y_test<- as.numeric(y_test$Rating_Average)
y_train <- as.numeric(y_train$Rating_Average)
########################################################################################
########################################################################################

###############ELASTIC NET
elastic_net_cv <- cv.glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 0.3,  
  nfolds = 5,   
  parallel = TRUE,
  seed = 42
)

best_lambda <- elastic_net_cv$lambda.min


best_elastic_net <- glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 0.3,        # L1 ratio
  lambda = best_lambda
)


pred_elastic_net <- predict(best_elastic_net, newx = as.matrix(X_test))

MSE_en <- mean((y_test - pred_elastic_net)^2)
RMSE_en <- sqrt(MSE_en)
MAE_en <- mean(abs(y_test - pred_elastic_net))
R2_en <- R2(pred_elastic_net, y_test)
n <- length(y_test)
p <- ncol(X_test)

cat('Elastic Net RMSE:', RMSE_en, '\n')
cat('Elastic Net MAE:', MAE_en, '\n')
cat('Elastic Net R^2:', R2_en, '\n')


###############LASSO
lasso_cv <- cv.glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 1,   
  nfolds = 5,  
  parallel = TRUE,
  seed = 42
)


best_lambda_lasso <- lasso_cv$lambda.min


best_lasso <- glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 1,              
  lambda = best_lambda_lasso
)

pred_lasso <- predict(best_lasso, newx = as.matrix(X_test))

MSE_lasso <- mean((y_test - pred_lasso)^2)
RMSE_lasso <- sqrt(MSE_lasso)
MAE_lasso <- mean(abs(y_test - pred_lasso))
R2_lasso <- R2(pred_lasso, y_test)
n <- length(y_test)
p <- ncol(X_test)

cat('Lasso RMSE:', RMSE_lasso, '\n')
cat('Lasso MAE:', MAE_lasso, '\n')

cat('Lasso R^2:', R2_lasso, '\n')

results_lasso <- list(
  'Model' = 'Lasso',
  'RMSE' = RMSE_lasso,
  'MAE' = MAE_lasso,
  'R-squared' = R2_lasso
)

###################
########################################ANAYSIS FOR WIKIPEDIA TRENDS
X_train<- train[,-5]
X_test<- test[,-5]
X_train<- as.matrix(X_train)
X_test<- as.matrix(X_test)
y_train<- train[5]
y_test<- test[5]
y_test<- as.numeric(y_test$Wikipedia_trends)
y_train <- as.numeric(y_train$Wikipedia_trends)
########################################################################################
########################################################################################

###############ELASTIC NET
elastic_net_cv <- cv.glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 0.3,  
  nfolds = 5,   
  parallel = TRUE,
  seed = 42
)

best_lambda <- elastic_net_cv$lambda.min


best_elastic_net <- glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 0.3,        # L1 ratio
  lambda = best_lambda
)


pred_elastic_net <- predict(best_elastic_net, newx = as.matrix(X_test))

MSE_en <- mean((y_test - pred_elastic_net)^2)
RMSE_en <- sqrt(MSE_en)
MAE_en <- mean(abs(y_test - pred_elastic_net))
R2_en <- R2(pred_elastic_net, y_test)
n <- length(y_test)
p <- ncol(X_test)

cat('Elastic Net RMSE:', RMSE_en, '\n')
cat('Elastic Net MAE:', MAE_en, '\n')
cat('Elastic Net R^2:', R2_en, '\n')


###############LASSO
lasso_cv <- cv.glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 1,   
  nfolds = 5,  
  parallel = TRUE,
  seed = 42
)


best_lambda_lasso <- lasso_cv$lambda.min


best_lasso <- glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 1,              
  lambda = best_lambda_lasso
)

pred_lasso <- predict(best_lasso, newx = as.matrix(X_test))

MSE_lasso <- mean((y_test - pred_lasso)^2)
RMSE_lasso <- sqrt(MSE_lasso)
MAE_lasso <- mean(abs(y_test - pred_lasso))
R2_lasso <- R2(pred_lasso, y_test)
n <- length(y_test)
p <- ncol(X_test)

cat('Lasso RMSE:', RMSE_lasso, '\n')
cat('Lasso MAE:', MAE_lasso, '\n')

cat('Lasso R^2:', R2_lasso, '\n')

results_lasso <- list(
  'Model' = 'Lasso',
  'RMSE' = RMSE_lasso,
  'MAE' = MAE_lasso,
  'R-squared' = R2_lasso
)



###################
########################################ANAYSIS FOR Trailer
X_train<- train[,-7]
X_test<- test[,-7]
X_train<- as.matrix(X_train)
X_test<- as.matrix(X_test)
y_train<- train[7]
y_test<- test[7]
y_test<- as.numeric(y_test$YTtrailer_views)
y_train <- as.numeric(y_train$YTtrailer_views)
########################################################################################
########################################################################################

###############ELASTIC NET
elastic_net_cv <- cv.glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 0.3,  
  nfolds = 5,   
  parallel = TRUE,
  seed = 42
)

best_lambda <- elastic_net_cv$lambda.min


best_elastic_net <- glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 0.3,        # L1 ratio
  lambda = best_lambda
)


pred_elastic_net <- predict(best_elastic_net, newx = as.matrix(X_test))

MSE_en <- mean((y_test - pred_elastic_net)^2)
RMSE_en <- sqrt(MSE_en)
MAE_en <- mean(abs(y_test - pred_elastic_net))
R2_en <- R2(pred_elastic_net, y_test)
n <- length(y_test)
p <- ncol(X_test)

cat('Elastic Net RMSE:', RMSE_en, '\n')
cat('Elastic Net MAE:', MAE_en, '\n')
cat('Elastic Net R^2:', R2_en, '\n')


###############LASSO
lasso_cv <- cv.glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 1,   
  nfolds = 5,  
  parallel = TRUE,
  seed = 42
)


best_lambda_lasso <- lasso_cv$lambda.min


best_lasso <- glmnet(
  x = as.matrix(X_train),
  y = as.matrix(y_train),
  alpha = 1,              
  lambda = best_lambda_lasso
)

pred_lasso <- predict(best_lasso, newx = as.matrix(X_test))

MSE_lasso <- mean((y_test - pred_lasso)^2)
RMSE_lasso <- sqrt(MSE_lasso)
MAE_lasso <- mean(abs(y_test - pred_lasso))
R2_lasso <- R2(pred_lasso, y_test)
n <- length(y_test)
p <- ncol(X_test)

cat('Lasso RMSE:', RMSE_lasso, '\n')
cat('Lasso MAE:', MAE_lasso, '\n')

cat('Lasso R^2:', R2_lasso, '\n')

results_lasso <- list(
  'Model' = 'Lasso',
  'RMSE' = RMSE_lasso,
  'MAE' = MAE_lasso,
  'R-squared' = R2_lasso
)
