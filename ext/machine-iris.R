iris
library(corrplot)

plot(iris$Petal.Length, iris$Petal.Width)

cor(iris$Petal.Length, iris$Petal.Width)

x=levels(iris$Species)

# Print Setosa correlation matrix
# correlation for each specie
par(mfrow = c(2, 2))
c1 <- cor(iris[iris$Species==x[1],1:4])
corrplot(c1, method = "number" , title = paste("correlation for ", x[1]),mar = c(0,0,2,0))
c2 <- cor(iris[iris$Species == x[2],1:4])
corrplot(c2, method = "number" , title = paste("correlation for ", x[2]),mar = c(0,0,2,0))
c3 <- cor(iris[iris$Species == x[3],1:4])
corrplot(c3, method = "number" , title = paste("correlation for ", x[3]),mar = c(0,0,2,0))

str(iris)

# Division of `Species`
table(iris$Species) 

# Percentual division of `Species`
round(prop.table(table(iris$Species)) * 100, digits = 1)

summary(iris[c("Petal.Width")])
boxplot(iris$Petal.Width)
