rm(list=ls())

##Parte 1
## Se importan las librerias necesarias
library(stringr)
library(plyr)
library(e1071)
library(randomForest)

##Parte 2
## Se importan los textos con polaridad negativa y positiva.
posText <- read.delim(file='/Data/INVESTIGACION/Diciembre_2015/POS.txt', header=FALSE, stringsAsFactors=FALSE)
posText <- posText$V1
posText <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))
negText <- read.delim(file='/Data/INVESTIGACION/Diciembre_2015/NEG.txt', header=FALSE, stringsAsFactors=FALSE)
negText <- negText$V1
negText <- unlist(lapply(negText, function(x) { str_split(x, "\n") }))
#
#
ale_list <- read.delim(file="/Data/INVESTIGACION/Diciembre_2015/total_final_lexicon.csv", header=FALSE, sep=",",stringsAsFactors=FALSE)
names(ale_list) <- c('word', 'score')
ale_list$word <- tolower(ale_list$word)

##Parte 3
##Categorizamos las opiniones
vNegTerms <- ale_list$word[ale_list$score==-5 | ale_list$score==-4]
negTerms <- ale_list$word[ale_list$score==-3 | ale_list$score==-2 | ale_list$score==-1]
posTerms <- ale_list$word[ale_list$score==3 | ale_list$score==2 | ale_list$score==1]
vPosTerms <- ale_list$word[ale_list$score==4 | ale_list$score==5 ]


##Parte 4
## Comienza la funcion
sentimentScore <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms){
  final_scores <- matrix('', 0, 5)
  scores <- laply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
    initial_sentence <- sentence
    #remove unnecessary characters and split up by word 
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    sentence <- tolower(sentence)
    wordList <- str_split(sentence, '\\s+')
    words <- unlist(wordList)
    #build vector with matches between sentence and each category
    vPosMatches <- match(words, vPosTerms)
    posMatches <- match(words, posTerms)
    vNegMatches <- match(words, vNegTerms)
    negMatches <- match(words, negTerms)
    #sum up number of words in each category
    vPosMatches <- sum(!is.na(vPosMatches))
    posMatches <- sum(!is.na(posMatches))
    vNegMatches <- sum(!is.na(vNegMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
    #add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, vNegTerms, negTerms, posTerms, vPosTerms)
  return(scores)
 }    
 
##Parte 5
 ##Se crea la tabla del TRAINING
posResult <- as.data.frame(sentimentScore(posText, vNegTerms, negTerms, posTerms, vPosTerms))
negResult <- as.data.frame(sentimentScore(negText, vNegTerms, negTerms, posTerms, vPosTerms))
#neuResult <- as.data.frame(sentimentScore(neuText, vNegTerms, negTerms, posTerms, vPosTerms))
posResult <- cbind(posResult, 'positivo')
colnames(posResult) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos', 'sentiment')
negResult <- cbind(negResult, 'negativo')
colnames(negResult) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos', 'sentiment')    
#
#
results <- rbind(posResult, negResult)
########################################
########################################

##Parte 6.
### Se comienzan a desarrollar los modelos basados en Machine Learning
# Se crea un modelo basado en Naive Bayes 
modelo_bayes <- naiveBayes(results[,2:5], results[,6]) 

# Se crea un modelo basado en Random Forest
modelo_RF <- randomForest(results[,2:5], results[,6]) 

# Se crea un modelo basado en Support Vector Machines
modelo_svm <- svm(results[,6] ~ results[,2]+results[,3]+results[,4]+results[,5],data=results)


## Parte 7
##############################################################3
# Se crea la matriz de confusión para Naive Bayes
confTable_bayes <- table(predict(modelo_bayes, results), results[,6], dnn=list('predicted','actual'))
round(prop.table(confTable_bayes,1),4)

# Se crea la matriz de confusión para Random Forest
confTable_RF <- table(predict(modelo_RF, results), results[,6], dnn=list('predicted','actual'))
round(prop.table(confTable_RF,1),4)

# Se crea la matriz de confusión para Support Vector Machines
confTable_svm <- table(predict(modelo_svm, results), results[,6], dnn=list('predicted','actual'))
round(prop.table(confTable_svm,1),4)