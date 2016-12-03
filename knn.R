# CS513
# group:
# 
# 
# 

rm(list=ls())

library(class)

# read the dataset into R
table<-read.csv("/Users/shaoanqi/Desktop/CS513A_Final Project/movie_metadata.csv")
View(table)
table1 <- na.omit(table)
View(table1)

# generate the needed dataset
critic=table1$num_critic_for_reviews
duration=table1$duration
directorFacebookLikes=table1$director_facebook_likes
actorFacebookLikes=table1$actor_1_facebook_likes
gross=table1$gross
votedUsers=table1$num_voted_users
castFacebookLikes=table1$cast_total_facebook_likes
posterFaces=table1$facenumber_in_poster
reviews=table1$num_user_for_reviews
country=table1$country
budget=table1$budget
aspectRatio=table1$aspect_ratio
movieFacebookLikes=table1$movie_facebook_likes
score=table1$imdb_score
table_new<-data.frame(critic, duration, directorFacebookLikes, actorFacebookLikes, gross, votedUsers, castFacebookLikes, posterFaces, reviews, budget, aspectRatio, movieFacebookLikes, score)
View(table_new)
str(table_new)
head(table_new)

# generate training dataset and test dataset
idx=seq(from=1,to=nrow(table_new),by=5)
test<-table_new[idx,]
training<-table_new[-idx,]
View(test)
View(training)

# use knn algorithm to build the model
predict<-knn(training[,-13],test[,-13],training[,13],k=10)

# combine the prediction with the test data and calculate the wrong rate
results<-cbind(test, predict)
table(results[,13],results[,14])
wrong<-results[,13]!=results[,14]
rate<-sum(wrong)/length(wrong)
rate

