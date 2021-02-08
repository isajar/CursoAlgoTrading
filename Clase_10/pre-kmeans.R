rm(list=ls())
library(tm)

### install.packages("tm.corpus.Reuters21578", repos = "http://datacube.wu.ac.at")
library(tm.corpus.Reuters21578)
data(Reuters21578)
corpus <- Reuters21578

corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeNumbers)
#corpus <- tm_map(corpus, stemDocument)
matrix_terms <- DocumentTermMatrix(corpus)

findFreqTerms(matrix_terms, 4, 1100)
findAssocs(matrix_terms, "swiss", .4)

#matrix_terms_tfxidf <- weightTfIdf(matrix_terms)

#m <- as.matrix(matrix_terms_tfxidf)

