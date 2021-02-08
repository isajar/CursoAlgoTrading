rm(list=ls())

library(e1071)
data(iris)

cm_model <- cmeans(iris[,-5], 4, 100, 1, method="cmeans")

par(mfrow = c(2, 2))
plot(iris[,1], iris[,2], col=cm_model$cluster)
plot(iris[,1], iris[,3], col=cm_model$cluster)
plot(iris[,2], iris[,3], col=cm_model$cluster)


wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")}

wssplot(iris[,-5], nc = 20)

library(cluster)
sil <- silhouette(cm_model$cluster, dist(iris[,-5]))
plot(sil, col=c("red","green","blue","yellow"))



