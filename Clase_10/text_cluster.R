rm(list=ls())

library(tm) # for text mining

## make a example corpus
# make a df of documents a to i
a <- "dog dog carrot"
b <- "phone cat"
c <- "phone book dog"
d <- "cat book trees"
e <- "phone cat orange"
f <- "phone circles dog"
g <- "cat square"
h <- "dog trees"
i <- "phone carrot cat"

j <- c(a,b,c,d,e,f,g,h,i)
k <- c(1:9)
xx <- cbind(k,j)

x <- data.frame(xx)    
names(x) <- c("doc_id","text")

docs <- Corpus(DataframeSource(x))
dtm <- DocumentTermMatrix(docs)

m <- as.matrix(dtm)
d <- dist(m, method = "euclidean")

kfit <- kmeans(m, 3)
kfit

plot(kfit$cluster, col=kfit$cluster)
grid()
