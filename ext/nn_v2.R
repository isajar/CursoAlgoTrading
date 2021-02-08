rm(list = ls())
library(neuralnet)
library(quantmod)

################################################################################################################################
########## Defino todos los valores parametrizables, para no tener que modificar luego el cuerpo principal######################
################################################################################################################################
start_day <- 250 # Defino una variable que luego utilizare como rango de tiempo.
start_day_if_no_entrenar <- 25 # macd 26 low => m?nimo, 53
end_date <- (Sys.Date()) # Fecha Final, en este ejemplo, el dia de hoy.
start_date <- as.Date(Sys.Date() - start_day)
acc <- as.character("ALUA.BA") # Quote de la accion segun Yahoo. En este ejemplo APPLE.
nn <- 5 # Numero de nueronas en la capa oculta, en este ejemplo, 5
stmax <- 1000000 # Variable para el parametro step_max. Maxima cantidad de iteraciones
entrenar <- TRUE # permite seleccionar si se quiere volver a entrenar la red
cDir <- getwd() # mi directorio actual
############### Fin de valores parametrizables #################################################################################
################################################################################################################################


symb <- getSymbols(acc, src = "yahoo", from = start_date, auto.assign = FALSE, header = FALSE)
colnames(symb) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")

## De todas las variables anteriores, solo seleccion Volume y Adjusted
data <- subset(symb, select = c(Volume, Adjusted))

# CALCULO DE INDICADORES
## Calculo diferentes indicadores, sobre la variable Precio de Ajuste: "Adjusted" y los redondeo a dos decimales.
## Aqui podrian seleccionar tantos indicadores como deseen y mejores resultados obtengan.
data$rsi_2 <- signif(RSI(data$Adjusted, 2), 2)
data$rsi_7 <- signif(RSI(data$Adjusted, 7), 2)
data$rsi_14 <- signif(RSI(data$Adjusted, 14), 2)
data$rsi_21 <- signif(RSI(data$Adjusted, 21), 2)

macd <- MACD(data$Adjusted, nFast = 12, nSlow = 26, nSig = 9, maType = "SMA", percent = FALSE)
## como el resultado del macd devuelve 2 columnas (macd y signal), las convierto en una sola con estos valores
## 1 comprado, 0 afuera
stratmacd <- ifelse((macd$signal < macd$macd), 1, 0)
data$stratmacd <- stratmacd[is.na(stratmacd)] <- 0

# OMITO NAS PARA SIMPLIFICAR
data <- na.omit(data)

# saco volumen y ajustado. El test siempre es la ultima fila.
test_data <- tail(data, 1)[, -c(1, 2)]

if (entrenar) {
  ## Hago un lag del dataset, creando una nueva variable con los precios "laggeados"
  data_lag <- data
  data_lag$Price_Lagged <- lag(data$Adjusted, -1)

  # CALCULO DE COLUMNA SUBE Y BAJA
  # si el precio de hoy es mayor que el de ayer entonces columna SUBE en esa celda queda en 1 y columna BAJA en 0 (contrario a SUBE)

  data_lag$SUBE <- (data_lag$Price_Lagged > data_lag$Adjusted)
  data_lag$BAJA <- !data_lag$SUBE

  # El set para entrenamiento se le sacan algunas variables y la ultima fila (que es la de testeo)
  train_data <- subset(data_lag[1:nrow(data_lag) - 1, ], select = c(-Adjusted, -Price_Lagged, -Volume))

  # Entreno la red
  neural <- neuralnet(SUBE + BAJA ~ ., data = train_data, hidden = nn, stepmax = stmax)
  # Guardo la red para no tener que volver a calcularla
  save(neural, file = paste(cDir, "neural", acc, ".rda", sep = ""))
} else {
  # cargo la red neuronal guardada.
  load(file = paste(cDir, "neural", acc, ".rda", sep = ""))
}
#######################################



## Realizo la prediccion
mypredict <- predict(neural, test_data)
# names(mypredict)[1] <- 'SUBE'
# names(mypredict)[2] <- 'BAJA'
SUBE <- mypredict[1]
BAJA <- mypredict[2]

## este comando envia el print a un archivo
sink(file = paste(cDir, "test.txt", sep = ""), append = TRUE)
cat("\n") # salto de linea
print(paste("SUBE: ", SUBE))
print(paste("BAJA: ", BAJA))
cat("\n")
print(paste("mejor opcion >>>", ifelse(SUBE > BAJA, "SUBE", "BAJA")))
cat("\n") # salto de linea
if (entrenar) print(tail(data_lag, 1))
cat("\n") # salto de linea
print(tail(test_data, 1))
cat("\n") # salto de linea
print(tail(symb, 3))
sink()
