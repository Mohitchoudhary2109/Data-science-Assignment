book <-read.csv(file.choose())
View(book)
str(book)
  
library(arules)
library(arulesViz)

rules <- apriori(as.matrix(book),parameter = list(support=0.02,confidence=0.5,minlen=5))
inspect(rules)
plot(rules)
 
##changing support confidence and minlength
rules <- apriori(as.matrix(book),parameter = list(support=0.05,confidence=0.2,minlen=2))
inspect(rules)
plot(rules)

# Sorting rules by confidence 
rules_conf <- sort(rules,by="confidence")
inspect(rules_conf)
# Sorint rules by lift ratio
rules_lift <- sort(rules,by="lift")
inspect(rules_lift)

##using different plots
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(rules,method = "mosaic")

