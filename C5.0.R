###############################################################################
#  Company       : Stevens 
#  Course        : CS513A
#  Purpose       : Final project knn algorithm
#  Team remember : Yiran Li, Anqi Shao, Xuan Li
#  Date          : November 29,2016
#  Comments      :


#################################################################################

rm(list=ls())
# install package 'c50'
install.packages("C50")
require(C50)

# read the dataset into R
table<-read.csv("/Users/shaoanqi/Desktop/CS513A_Final Project/movie_metadata.csv")
table1 <- na.omit(table)

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


set.seed(9850)
g<-runif(nrow(table_new))
table_new2<-table_new[order(g),]
View(table_new2)
idx=seq(from=1,to=nrow(table_new2),by=5)
test<-table_new2[idx,]
training<-table_new2[-idx,]
training$score<- factor(training$score)
m1<-C5.0(training[,-13], training$score)
m1
minn <-min(gross)
summary(m1)

# test-how well as the algorithm does as classify
p1<-predict(m1,test)
table(test[,13], C5.0_predicted = p1)

# combine the prediction with the test data
results<-cbind(test, p1)
# combine the prediction with the test data
table(results[,13],results[,14])
wrong<-results[,13]!=results[,14]
rate<-sum(wrong)/length(wrong)
rate

