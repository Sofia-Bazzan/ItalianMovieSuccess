#++++++++++++++++++++++++++
##### PROCESSING DATA #####
#++++++++++++++++++++++++++

#upload the dataset given the file path
movies <- read.csv("file_path.csv", header = TRUE)

#if necessary:
movies_original <- read.csv("file_path.csv", header = TRUE)

#check types of the first columns
print(sapply(movies[, 1:11, drop = FALSE], class))

#there are character type...

#convert ReleaseDate in DATE type
#install.packages("lubridate")
library(lubridate)
movies$ReleaseDate <- dmy(movies$ReleaseDate)
sum(is.na(movies$ReleaseDate))
movies$ReleaseMonth <- month(movies$ReleaseDate)

#convert Rating_MyMovies in NUMERIC type
movies$Rating_MyMovies <- gsub(",", ".", movies$Rating_MyMovies)
movies$Rating_MyMovies <- as.numeric(movies$Rating_MyMovies)
movies$Rating_MyMovies[is.na(movies$Rating_MyMovies)] <- 0

#convert Rating_Critic in NUMERIC type
movies$Rating_Critic <- gsub(",", ".", movies$Rating_Critic)
movies$Rating_Critic <- as.numeric(movies$Rating_Critic)
movies$Rating_Critic[is.na(movies$Rating_Critic)] <- 0

#convert Rating_Public in NUMERIC type
movies$Rating_Public <- gsub(",", ".", movies$Rating_Public)
movies$Rating_Public <- as.numeric(movies$Rating_Public)
movies$Rating_Public[is.na(movies$Rating_Public)] <- 0

#convert Rating_Average in NUMERIC type
movies$Rating_Average <- gsub(",", ".", movies$Rating_Average)
movies$Rating_Average <- as.numeric(movies$Rating_Average)

# Where Rating_Average is NA, I calculate the average of the other Rating columns
# (only if they are different from 0 - since I set NAs to 0 earlier)
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

movies <- movies %>%
  filter(!is.na(Rating_Average))


#convert Revenue in NUMERIC type
movies$Revenue <- as.numeric(movies$Revenue)
#movies <- movies %>%
#  filter(!is.na(Revenue))

#convert Wikipedia_trends in NUMERIC type
movies$Wikipedia_trends <- as.numeric(movies$Wikipedia_trends)
movies$Wikipedia_trends[is.na(movies$Wikipedia_trends)] <- 0

#convert Max_instafollowers in NUMERIC type
movies$Max_instafollowers <- gsub(",", ".", movies$Max_instafollowers)
movies$Max_instafollowers <- as.numeric(movies$Max_instafollowers)
movies$Max_instafollowers[is.na(movies$Max_instafollowers)] <- 0


# Remove unnecessary columns
movies <- subset(movies, select = -c(Rating_MyMovies, Rating_Critic, Rating_Public, MovieName))

sum(!complete.cases(movies)) 
#if there are rows with NA, remove them
movies <- movies[complete.cases(movies), ]

#Check if somethig is still character
names(movies)[sapply(movies, is.character)]
# Convert "Suggested_audienceNC.14" to binary
movies$Suggested_audienceNC.14 <- ifelse(movies$Suggested_audienceNC.14 == "Suggested_audienceNC-14", 1, 0)

movies_pre <- movies


# RATING AVERAGE to binary value (threshold = 3)
#movies$Rating_Average <- ifelse(movies$Rating_Average >= 3.1, 1, 0)
movies$Success <- ifelse(movies$Rating_Average >= 3, 1, 0)
movies <- subset(movies, select = -c(Rating_Average))


## CORRELATION MATRIX
#numeric_columns <- sapply(movies, is.numeric)
#numeric_data <- movies[, numeric_columns]
#corr <- cor(numeric_data)
#print(corr)


#+++++++++++++++++++++++++++++++
##### TRAIN AND TEST SPLIT #####
#+++++++++++++++++++++++++++++++

set.seed(2024)

nrow(movies)
0.7*nrow(movies)

train_indices <- sample(1:nrow(movies), round(0.7*nrow(movies)))
train <- movies[train_indices, ]
test <- movies[-train_indices,]

#levels(train$Rating_Average)
levels(train$Success)

#train$Rating_Average <- factor(train$Rating_Average, levels = c(0, 1))
#test$Rating_Average <- factor(test$Rating_Average, levels = c(0, 1))
train$Success <- factor(train$Success, levels = c(0, 1))
test$Success <- factor(test$Success, levels = c(0, 1))


levels(train$Success)
summary(train$Success)
