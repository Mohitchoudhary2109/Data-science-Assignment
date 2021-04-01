 my_movies<- read.csv(file.choose())
View(my_movies) 
 str(my_movies)
# converting everything into character format 
my_movies[] <- lapply(my_movies,as.character)
View(my_movies)
# Creating a custom fucntion to collapse all the items in a transaction into 
# a single sentence 
paste_fun <- function(i){
  return (paste(as.character(i),collapse=" "))
}

# Applying the custom function
my_movies["new_col"] <- apply(my_movies,1,paste_fun)
View(my_movies)

library(tm)
x <- Corpus(VectorSource(my_movies$new_col)) # Selecting the new column which
# contains all items of a transaction in a single sentence
x <- tm_map(x,stripWhitespace)
# Creating a TDM matrix
dtm0 <- t(TermDocumentMatrix(x))
# Converting TDM matrix to data frame
dtm0_df <- data.frame(as.matrix(dtm0))
View(dtm0_df)

# Association Rules 

library(arules)
library(arulesViz)
# Item Frequecy plot 
barplot(sapply(dtm0_df,sum),col=1:10)
# Applying apriori algorithm to get relevant rules
rules <- apriori(as.matrix(dtm0_df),parameter = list(support=0.002,confidence=0.5,minlen=2))
inspect(rules)
plot(rules)

# Sorting rules by confidence 
rules_conf <- sort(rules,by="confidence")
inspect(rules_conf)
# Sorint rules by lift ratio
rules_lift <- sort(rules,by="lift")
inspect(rules_lift)


plot(rules,method = "graph")
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "mosaic")

#changing support, confidence and minlength

rules1 <- apriori(as.matrix(dtm0_df),parameter = list(support=0.005,confidence=0.8,minlen=5))
inspect(rules1)
plot(rules1)

plot(rules1,method = "graph")
plot(rules1,method = "scatterplot")
plot(rules1,method = "grouped")
plot(rules1,method = "mosaic")
