
library("recommenderlab")
library(caTools)

#book rating data
book<- read.csv(file.choose())

#metadata about the variable
str(book)


#rating distribution
hist(book$Book.Rating)

#the datatype should be realRatingMatrix inorder to build recommendation engine
book_matrix <- as(book, 'realRatingMatrix')

#Popularity based 

book1 <- Recommender(book_matrix, method="POPULAR")

#Predictions for two users 
recommended_items1 <- predict(book1, book_matrix[400:420], n=10)
as(recommended_items1, "list")


## Popularity model recommends the same movies for all users , we need to improve our model using # # Collaborative Filtering

#User Based Collaborative Filtering

book2 <- Recommender(book_matrix, method="UBCF")

#Predictions for two users 
recommended_items2 <- predict(book2, book_matrix[410:415], n=10)
as(recommended_items2, "list")



book3 <- Recommender(book_matrix, method="SVD")
recommended_items3 <- predict(book3, book_matrix[410:415], n=10)
as(recommended_items3, "list")
