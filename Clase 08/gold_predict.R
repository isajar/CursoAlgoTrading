rm(list = ls())
library(e1071)
library(scales)

gold_data <- read.csv(paste(getwd(),"/Clase 08/gld_price_data.csv", sep = ""),sep = ",",header = T)
gold_data <- gold_data[,-c(1,3)]
# convierto la ultima columna a factor con dos niveles
gold_data$GLD_TREND <- factor(gold_data$GLD_TREND, levels = c("SUBE","BAJA"))

# desordeno las filas para que sea mas aleatorio el entrenamiento
rand_gd <- gold_data[sample(nrow(gold_data)),]

train <-rand_gd[1:1500,]
test <- rand_gd[1501:nrow(rand_gd),]

# me fijo que proporcion quedo de cada clase (para comprobar que no se sesgue a una clase)
# Proporcion de Clases en el set de TRAIN
prop.table(table(train$GLD_TREND)) * 100
# Proporcion de Clases en el set de TEST"
prop.table(table(test$GLD_TREND)) * 100

model_nb <- naiveBayes(GLD_TREND ~.,data=train)
table(predict(model_nb, test))
test$predicted <- predict(model_nb, test)

#saco cuenta de cuantas clases acerto en promedio.
eva <- (test$GLD_TREND == test$predicted)
eva <- gsub("FALSE","ERROR",eva)
eva <- gsub("TRUE","ACIERTO",eva)
prop.table(table(eva)) * 100



## OTROS RANGOS PARA LOS DATASETS DE TRAIN Y TEST


# prueba con 1 left train para 1 set de test
testlast <- tail(gold_data,1)
train1left <- gold_data[-nrow(gold_data),]
model_nb2 <- naiveBayes(GLD_TREND ~.,data=train1left)
testlast$predicted <- predict(model_nb, testlast)
testlast

# prueba con 1 left para todo el set de test
test2<- test
eva2 <- (test$GLD_TREND == test$predicted)
model_nb2 <- naiveBayes(GLD_TREND ~.,data=train1left)
test2$predicted <- predict(model_nb, test2)

eva2 <- (test2$GLD_TREND == test2$predicted)
eva2 <- gsub("FALSE","ERROR",eva2)
eva2 <- gsub("TRUE","ACIERTO",eva2)
prop.table(table(eva2)) * 100
