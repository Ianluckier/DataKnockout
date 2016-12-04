###############################################################################
#  Company       : Stevens 
#  Course        : CS513A
#  Purpose       : Final project knn algorithm
#  Team remember : Yiran Li, Anqi Shao, Xuan Li
#  Date          : November 29,2016
#  Comments      : ANN model for film rate prediction system.


#################################################################################

rm(list=ls())

install.packages("neuralnet")
install.packages("MASS")
install.packages("grid")
require(neuralnet)
require(MASS)
require(grid)

library(class)

# read the dataset into R
# please replace the path or put the csv in the same path
table <- read.csv(".//movie_metadata.csv")
# view the dataset
View(table)
# omit the missing data 
table1 <- na.omit(table)
# view the dataset after omit the missing data
View(table1)

# generate the needed dataset
critic = table1$num_critic_for_reviews
duration = table1$duration
directorFacebookLikes = table1$director_facebook_likes
actorFacebookLikes = table1$actor_1_facebook_likes
gross = table1$gross
votedUsers = table1$num_voted_users
castFacebookLikes = table1$cast_total_facebook_likes
posterFaces = table1$facenumber_in_poster
reviews = table1$num_user_for_reviews
country = table1$country
budget = table1$budget
aspectRatio = table1$aspect_ratio
movieFacebookLikes = table1$movie_facebook_likes
score = table1$imdb_score
table_new <- data.frame(critic, duration, directorFacebookLikes, actorFacebookLikes, gross, votedUsers, castFacebookLikes, posterFaces, reviews, budget, aspectRatio, movieFacebookLikes, score)
View(table_new)
str(table_new)
head(table_new)

# define max-min normalization function and do normalization
mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}
# normalize our dataset into new table called table_norm
table_norm <- as.data.frame (         
    cbind(critic = mmnorm(table_new[,1],min(table_new[,1]),max(table_new[,1]))
         ,duration = mmnorm(table_new[,2],min(table_new[,2]),max(table_new[,2]))
         ,directorFacebookLikes = mmnorm(table_new[,3],min(table_new[,3]),max(table_new[,3]))
         ,actorFacebookLikes = mmnorm(table_new[,4],min(table_new[,4]),max(table_new[,4]))
         ,gross = mmnorm(table_new[,5],min(table_new[,5]),max(table_new[,5]))
         ,votedUsers = mmnorm(table_new[,6],min(table_new[,6]),max(table_new[,6]))
         ,castFacebookLikes = mmnorm(table_new[,7],min(table_new[,7]),max(table_new[,7]))
         ,posterFaces = mmnorm(table_new[,8],min(table_new[,8]),max(table_new[,8]))
         ,reviews = mmnorm(table_new[,9],min(table_new[,9]),max(table_new[,9]))
         ,country = mmnorm(table_new[,10],min(table_new[,10]),max(table_new[,10]))
         ,budget = mmnorm(table_new[,11],min(table_new[,11]),max(table_new[,11]))
         ,aspectRatio = mmnorm(table_new[,12],min(table_new[,12]),max(table_new[,12]))
         ,score = mmnorm(table_new[,13],min(table_new[,13]),max(table_new[,13]))
  )
)
# view the table after normalize
View(table_norm)

# shuffle the rows
set.seed(7523)
g<-runif(nrow(table_norm))
table_shuffle<-table_norm[order(g),]

# view the table after shuffle
View(table_shuffle)

# generate training dataset and test dataset
idx = seq(from = 1,to = nrow(table_shuffle),by = 5)
test <- table_shuffle[idx,]
training <- table_shuffle[-idx,]

# use neuralnet algorithm to build the model
# hidden = 6 meanings how many hidden node we choose for this model, we have already test this model when hidden node is 1,3,6,9. Even 12.
# err.fct = "sse" that is a algorithm when error happend.
nn = neuralnet(score ~ critic + duration + directorFacebookLikes + actorFacebookLikes + gross + votedUsers + castFacebookLikes + posterFaces + reviews + country + budget + aspectRatio, data = training, hidden = 6, err.fct = "sse", linear.output = FALSE)
nn
plot(nn)
nn$net.result # overall result i.e.output
nn$weights
nn$result.matrix

nn$covariate
training$score
nn$net.result[[1]]

# calculate the traing  wrong rate,we define that when the abs different between predict and acutlly value greater than 0.1, we believe this predict result is wrong.
misClassificationError_train = mean(abs(training$score - nn$net.result[[1]]) > 0.1)
misClassificationError_train
OutputVsPred_train = cbind(training$score,nn1)
# The traing wrong rate is 
OutputVsPred_train

# test the model by test dataset
new.output = compute(nn, covariate = matrix(as.matrix(test[,-10]), byrow = TRUE, ncol = 12))

# calculate the test wrong rate, we define that when the abs different between predict and acutlly value greater than 0.1, we believe this predict result is wrong.
misClassificationError_test = mean(abs(test$score - nn$net.result[[1]]) > 0.1)
misClassificationError_test
OutputVsPred_test=cbind(test$score, nn2)
# The test wrong rate is 
OutputVsPred_test

# give you a more direct way to show the different covariates influence the target variable 
par(mfrow = c(3, 3))
gwplot(nn,selected.covariate = "critic")
gwplot(nn,selected.covariate = "duration")
gwplot(nn,selected.covariate = "directorFacebookLikes")
gwplot(nn,selected.covariate = "actorFacebookLikes")
gwplot(nn,selected.covariate = "gross")
gwplot(nn,selected.covariate = "votedUsers")
gwplot(nn,selected.covariate = "castFacebookLikes")
gwplot(nn,selected.covariate = "posterFaces")
gwplot(nn,selected.covariate = "reviews")
gwplot(nn,selected.covariate = "country")
gwplot(nn,selected.covariate = "budget")
gwplot(nn,selected.covariate = "aspectRatio")

