rm(list=ls())

library(arules)

tr <- read.transactions(file="Clase06/01/titanic.txt", format = "basket", sep=',', rm.duplicates=TRUE)
rules <- apriori(tr, parameter = list(supp = 0.2, conf = 0.8))
inspect(rules)

rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules)
