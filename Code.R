# Colaborative Filtering to predict movie ratings

library(data.table)
library(recommenderlab)
library(reshape2)
library(ggplot2)

train1= read.csv("Path/train_v2.csv")
head(train1)
nrow(train1)

length(unique(train1$user)) #There are 6040 unique users

#Removing column ID
train2=train1[,-1]
head(train2)

#Using acast function to rearrange the data
?acast()
t<-as.matrix(acast(train2, user ~ movie))
# Check the class of g
class(g)
head(t)

t1=as(t, "realRatingMatrix")
as(t1, "list")     # A list
as(t1, "matrix")   # A sparse matrix

head(as(t1, "list"))
head(as(t1,"data.frame")) 

# normalizing the rating matrix
r_m <- normalize(t1)
r_m
as(r_m, "list")

#Ploting raw and normalised ratings
image(t1, main="Raw Ratings")
image(r_m, main="Normlaized Ratings")


#Converting the matrix into binary matrix
r_b= binarize(t1, minRating=1)
as(r_b, "matrix")


#Building a Recommender model
#method="UBCF" is user based collaborative filtering, "IBFC" is item based collaborative filtering

rec=Recommender(t1[1:nrow(t1)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
rec1=Recommender(t1[1:nrow(t1)],method="UBCF", param=list(normalize = "Z-score",method="Jaccard",nn=5, minRating=1))
rec2=Recommender(t1[1:nrow(t1)],method="IBCF", param=list(normalize = "Z-score",method="Jaccard",minRating=1))
rec3=Recommender(t1[1:nrow(t1)],method="POPULAR")

print(rec)
names(getModel(rec))
getModel(rec)$nn

#To predict the movie ratings, type="ratings"
# We can use type="topNList" to get top-n items to predict 

recom <- predict(rec, t1[1:nrow(t1)], type="ratings")
recom


# Convert prediction into list, user-wise
as(recom, "list")
# Study and Compare the following:
as(t, "matrix")     # Has lots of NAs. 'r' is the original matrix
as(recom, "matrix") # Is full of ratings. NAs disappear
as(recom, "matrix")[,1:10] # Show ratings for all users for items 1 to 10
as(recom, "matrix")[5,3]   # Rating for user 5 for item at index 3
as.integer(as(recom, "matrix")[5,3]) # Just get the integer value
as.integer(round(as(recom, "matrix")[6039,8])) # Just get the correct integer value


#Creating submission file from the model

#Reading the test file

test=read.csv("Path/test_v2.csv")

head(test)

#Getting the rating list 
rec_list<-as(recom,"list")
head(summary(rec_list))
ratings<-NULL

# For all lines in test file, one by one
for ( u in 1:length(test[,2]))
{
  # Read userid and movieid from columns 2 and 3 of test data for each row
  userid <- test[u,2]
  movieid<-test[u,3]
  
  # Get as list & then convert to data frame all recommendations for user: userid
  u1<-as.data.frame(rec_list[[userid]])
  # Create a (second column) column-id in the data-frame u1 and populate it with row-names
  # Remember (or check) that rownames of u1 contain are by movie-ids
  # We use row.names() function
  u1$id<-row.names(u1)
  # Now access movie ratings in column 1 of u1
  x= u1[u1$id==movieid,1]
  # print(u)
  # print(length(x))
  # If no ratings were found, assign 0. You could also
  #   assign user-average
  if (length(x)==0)
  {
    ratings[u] <- 0
  }
  else
  {
    ratings[u] <-x
  }
  
}
length(ratings)
tx<-cbind(test[,1],round(ratings))
# Write to a csv file: submitfile.csv in your folder
write.table(tx,file="submitfile.csv",row.names=FALSE,col.names=FALSE,sep=',')

########################################



