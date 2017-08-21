library(readr)
library(caret)
library(class)
library(gmodels)

#Load and select Data
movie_metadata <- read_csv("//mac/Home/Desktop/movie_metadata.csv")
movie <- dplyr::select(movie_metadata, title_year, budget, gross, duration, facenumber_in_poster, director_facebook_likes, imdb_score)
movie <- as.data.frame(movie)

#Create a target Variable
movie$overall[movie$imdb_score  > 8.5] <- "Classic"
movie$overall[movie$imdb_score >= 6.5 &
                movie$imdb_score <= 8.5] <- "general"
movie$overall[movie$imdb_score  < 6.5] <- "Junk"

movie$overall        <- factor(movie$overall)

#Remove rows with NAs in data.frame, otherwise knn function may fail
movie <- movie[complete.cases(movie),]

str(movie)
summary(movie)

#Set X and Y Variables
Y_movie <- movie$overall
#title_year, budget, gross, duration, 
#facenumber_in_poster, director_facebook_likes, imdb_score
X_movie <- movie[, 1:7] 

#Split the data
set.seed(1)
movie_split <- createDataPartition(Y_movie, p=0.80, list=FALSE)
training_movie <- X_movie[movie_split,]
test_movie <- X_movie[-movie_split,]

training_movie_labels <- Y_movie[movie_split]
test_movie_labels <- Y_movie[-movie_split]

#kNN
set.seed(1)
movie_pred_knn <- knn(train=training_movie, test=test_movie,
                   cl=training_movie_labels, k=5, prob=TRUE)

#Evaluation cross tabs
CrossTable(x = test_movie_labels, y= movie_pred_knn,
           prop.chisq = FALSE,
           prop.r = FALSE,
           prop.c = FALSE,
           prop.t = FALSE)

#Compute FG percentage
mean(test_movie_labels == movie_pred_knn)
table(test_movie_labels, movie_pred_knn)
prop.table(table(test_movie_labels, movie_pred_knn),1)






