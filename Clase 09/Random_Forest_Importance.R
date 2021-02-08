rm(list = ls())
library(FSelector)
library(randomForest)
library(caret)

### Ejemplo 01
SAheart <- read.csv(paste(getwd(),"/Clase 09/SAheart.csv", sep = ""),sep = ",",header = T)
head(SAheart)

#Transformo a FRACTOR, de lo contrario RandomForest intentara realizar una REGRESION!
SAheart$chd <- factor(SAheart$chd, levels=c("Si", "No"))

# genero ids de fila aleatorias para el entrenamiento
training.ids <- createDataPartition(SAheart$chd, p=0.7, list = FALSE)


#selection <- random.forest.importance(chd ~ ., SAheart)
modeloRF <- randomForest(chd ~ tobacco+age+famhist , data = SAheart[training.ids,] , ntree = 50)

# la prediccion la hago con el contrario al dataset de trainning
pred <- predict(modeloRF, SAheart[-training.ids,])
# cuento los errores en una matriz de confucion.
t<-table(SAheart[-training.ids,"chd"],pred, dnn = c("Actual", "Predicho"))
prop.table(t,margin = 1)*100
#modeloRF2 <- randomForest(chd ~ tobacco+age+famhist , data = SAheart)


#Funciones especificas de random.forest.importance
cutoff.biggest.diff(selection)
cutoff.k(selection, k = 4)

## Ejemplo 02 (Se encuentra en la documentacion de random.forest.importance)
library(mlbench)

data(HouseVotes84)
weights <- random.forest.importance(Class~., HouseVotes84, importance.type = 1)
print(weights)
subset <- cutoff.k(weights, 5)
f <- as.simple.formula(subset, "Class")
print(f)
