rm(list=ls())
# Load the libraries
library(arules)
library(arulesViz)
library(datasets)

# Load the data set via WEB / CSV
## groceries <- read.transactions("http://www.sci.csueastbay.edu/~esuess/classes/Statistics_6620/Presentations/ml13/groceries.csv", sep = ",")
## summary(groceries)

data(Groceries)

itemFrequencyPlot(Groceries,topN=20,type="absolute")

rules <- apriori(Groceries, parameter = list(supp = 0.002, conf = 0.8))
inspect(rules)

rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules)

plot(rules)
plot(rules, method="graph", control=list(type="items"))

# Lectura
# Blog Post
# http://www.r-bloggers.com/association-rule-learning-and-the-apriori-algorithm/
