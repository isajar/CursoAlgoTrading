rm(list = ls())
library(neuralnet)
library(quantmod)

################################################################################################################################
########## Defino todos los valores parametrizables, para no tener que modificar luego el cuerpo principal######################
################################################################################################################################
start_day <- 250 # Defino una variable que luego utilizare como rango de tiempo.
start_day_if_no_entrenar <- 25 # macd 26 low => m?nimo, 53
end_date <- (Sys.Date()) # Fecha Final, en este ejemplo, el dia de hoy.
acc <- as.character("ALUA.BA") # Quote de la accion segun Yahoo. En este ejemplo APPLE.
nn <- 5 # Numero de nueronas en la capa oculta, en este ejemplo, 5
stmax <- 1000000 # Variable para el parametro step_max. Maxima cantidad de iteraciones
entrenar <- TRUE # permite seleccionar si se quiere volver a entrenar la red
repeticiones <- 1
tiemposleep <- 15
cDir <- getwd()
############### Fin de valores parametrizables #################################################################################
################################################################################################################################

## el for est? por si se quiere calcular m?s de una vez despu?s de tiemposleep y de paso para que no siga eternamente
for (n in 1:repeticiones) {
  ## se repite para que si son pocos d?as para calcular los indicadores, repita incrementando el n?mero de d?as a tener en cuenta
  errormacd <- FALSE
  for (i in 1:100) {
    ## de ac? en m?s, cuando aparezca if(!entrenar) significa que se puede saltear porque no (!) quiero entrenar ya que tengo el
    ## modelo listo
    if (!entrenar) {
      ## start_day_if_no_entrenar es una variable temporal que modifico seg?n necesite y me sirve para asignar el valor de start_day
      start_day <- start_day_if_no_entrenar
    }
    start_date <- as.Date(Sys.Date() - start_day) # Fecha de Inicio. En este ejemplo seran start_day dias hacia atras tomando el dia de hoy.
    ## Obtengo los valores de la accion que seleccione en la variable "acc" en el paso anterior.
    ## Y nombro sus variables.
    symb <- getSymbols(acc, src = "yahoo", from = start_date, auto.assign = FALSE, header = FALSE)
    names(symb)[1] <- "Open"
    names(symb)[2] <- "High"
    names(symb)[3] <- "Low"
    names(symb)[4] <- "Close"
    names(symb)[5] <- "Volume"
    names(symb)[6] <- "Adjusted"
    ## De todas las variables anteriores, solo seleccion Volume y Adjusted
    data <- subset(symb, select = c(Volume, Adjusted))


    ## Calculo diferentes indicadores, sobre la variable Precio de Ajuste: "Adjusted" y los redondeo a dos decimales.
    ## Aqui podrian seleccionar tantos indicadores como deseen y mejores resultados obtengan.
    rsi_2 <- signif(RSI(data$Adjusted, 2), 2)
    rsi_7 <- signif(RSI(data$Adjusted, 7), 2)
    rsi_14 <- signif(RSI(data$Adjusted, 14), 2)
    rsi_21 <- try(signif(RSI(data$Adjusted, 21), 2), TRUE)

    ## si el c?lculo del rsi_21 da error por falta de d?as, incrementa los d?as para atr?s a tener en cuenta
    errorrsi <- FALSE
    if (class(rsi_21)[1] == "try-error") {
      start_day_if_no_entrenar <- start_day_if_no_entrenar + 1
      errorrsi <- TRUE
    } else {
      if (!errormacd) {
        break
      }
    }

    errormacd <- FALSE
    macd <- try(MACD(data$Adjusted,
      nFast = 12, nSlow = 26, nSig = 9,
      maType = "SMA", percent = FALSE
    ), TRUE)
    if (class(macd)[1] == "try-error") {
      if (!errorrsi) {
        start_day_if_no_entrenar <- start_day_if_no_entrenar + 1
      }
      errormacd <- TRUE
    } else {
      if (!errorrsi) {
        break
      }
    }
  }

  if (errormacd | errorrsi) {
    ## si despu?s de ir para atr?s 100 d?as, siguen con error, saltea y termina el algoritmo y muestra un error
    print("no se pudo ejecutar, error con el c?lculo del MACD")
    break
  } else {
    ## como el resultado del macd devuelve 2 columnas (macd y signal), las convierto en una sola con estos valores
    ## 1 comprado, 0 afuera
    stratmacd <- ifelse((macd$signal < macd$macd), 1, 0)
    stratmacd[is.na(stratmacd)] <- 0

    ## Uno con el comando cbind para formar el dataset con la variales que seleccione en "data" y los nuevos indicadores.
    data <- cbind(data, rsi_2, rsi_7, rsi_14, rsi_21, stratmacd)
    # data <- na.omit(data)

    if (entrenar) {
      ## Hago un lag del dataset, creando una nueva variable con los precios "laggeados"
      lag_close_price <- lag(data$Adjusted, -1)

      ## Con cbind, ahora uno el dataset anterior "data" con la nueva columna de precios "laggeados" y le pongo nombre a cada variable.
      data_lag <- cbind(data, lag_close_price)
      names(data_lag)[3] <- "RSI_2"
      names(data_lag)[4] <- "RSI_7"
      names(data_lag)[5] <- "RSI_14"
      names(data_lag)[6] <- "RSI_21"
      names(data_lag)[7] <- "STRTMACD"
      names(data_lag)[8] <- "Price_Lagged"

      ## Resto el precio "laggeado" con el valor original de ajuste, para determinar si ese dia las acciones SUBIERON o BAJARON
      ## Para ello, se crea una nueva variable "data_diff" justamente con la diferencia.

      data_diff <- (data_lag$Price_Lagged - data_lag$Adjusted)

      ## Con este loop FOR, voy a proponer que si la diferencia es NEGATIVA, entoces las acciones BAJARON, y si es POSITIVA, SUBIERON
      Signal <- 0
      data_lag <- cbind(data_lag, Signal)
      ## [] corregir de acuerdo a la columna del price_lagged + 1
      names(data_lag)[9] <- "Signal"
      for (i in 1:(nrow(data_lag) - 1)) {
        if (data_diff[i] <= 0) {
          data_lag$Signal[i] <- "BAJA"
        } else {
          data_lag$Signal[i] <- "SUBE"
        }
      }

      ## Ahora voy a crear los sets de testeo y entrenamiento
      ## Como el indicador RSI_21 toma los primeros 21 valores para realizar calculos, entre los registros 1 y 21, van a aparecer varios "NA"
      ## Para evitar estos casos, el training lo voy a formar desde el registro 23 hasta el ultimo menos 1. El ultimo lo reservo para el testing.
      ## Las variables Adjustes y Price_Lagged las quito del set de datos, y solo me quedo con los indicadores que he creado.
      ## Igualmente se puede trabajar con tantas variables como ustedes deseen. En el set de training, tambien se quita la variable "Signal", ya que esa es
      ## la variable que quiero predecir, o sea, si la accion va a SUBIR o va a BAJAR.

      ## cambiar el 27 por el indicado para que no tome las primeras filas con valores NA
      data_lag <- na.omit(data_lag)
      # train_data <- subset((data_lag[28:nrow(data_lag)-1]), select=c(-Adjusted, -Price_Lagged))
      train_data <- subset(data_lag, select = c(-Adjusted, -Price_Lagged))
      test_data <- subset((data_lag[nrow(data_lag)]), select = c(-Adjusted, -Price_Lagged, -Signal))
      nnet_train <- train_data
      nnet_train <- cbind(nnet_train, train_data$Signal == "SUBE")
      nnet_train <- cbind(nnet_train, train_data$Signal == "BAJA")
      ## se increment? la columna en 1 por el agregado del MACD
      names(nnet_train)[8] <- "SUBE"
      names(nnet_train)[9] <- "BAJA"


      ### NEW (transformaciones ya que la nueva version de neuralnet no soporta algunos tipos de datos, hay que transformar a data frame)
      ## agrear 9 por la columna macd y agrego 6 y salteo 7
      neural_train <- nnet_train[, c(2, 3, 4, 5, 6, 8, 9)]
      neural_train_final <- as.data.frame(cbind(
        neural_train$RSI_2, neural_train$RSI_7, neural_train$RSI_14, neural_train$RSI_21, neural_train$STRTMACD,
        neural_train$SUBE, neural_train$BAJA
      ))
      ## guardo la red para no tener que volver a calcularla
      neural <- neuralnet(SUBE + BAJA ~ RSI_2 + RSI_7 + RSI_14 + RSI_21 + STRTMACD, data = neural_train_final, hidden = nn, stepmax = stmax)
      save(neural, file = paste(cDir, "neural", acc, ".rda", sep = ""))
    } else {
      ## tomo la ?ltima fila del valor que devuelve symb de yahoo para usarlo como test
      test_data <- subset((data[nrow(data)]), select = c(-Adjusted))
      names(test_data)[2] <- "RSI_2"
      names(test_data)[3] <- "RSI_7"
      names(test_data)[4] <- "RSI_14"
      names(test_data)[5] <- "RSI_21"
      names(test_data)[6] <- "STRTMACD"

      ## asigno el dataframe test_data a data_lag para no modificar el c?digo que sigue ya que es el dataframe que se usa m?s adelante
      data_lag <- test_data

      ## ya estaba guardado con el siguiente comando
      load(file = paste(cDir, "neural", acc, ".rda", sep = ""))
    }
    #######################################

    neural_test <- test_data[, -1]
    neural_test_final <- as.data.frame(cbind(neural_test$RSI_2, neural_test$RSI_7, neural_test$RSI_14, neural_test$RSI_21, neural_test$STRTMACD))


    ## Realizo la prediccion
    f <- sapply(neural_test_final, as.numeric)
    mypredict <- predict(neural, rbind(f))
    # names(mypredict)[1] <- 'SUBE'
    # names(mypredict)[2] <- 'BAJA'
    SUBE <- mypredict[1]
    BAJA <- mypredict[2]

    ## este comando env?a el print a un archivo
    sink(file = paste(cDir, "test.txt", sep = ""), append = TRUE)
    print(paste("SUBE: ", SUBE))
    print(paste("BAJA: ", BAJA))
    print("")
    print(paste("mejor opci?n >>>", ifelse(SUBE > BAJA, "SUBE", "BAJA")))

    print(tail(data_lag, 1))
    print(tail(neural_test_final, 1))
    print(tail(symb, 3))
    sink()

    Sys.sleep(tiemposleep)

    ## end if error macd
  }
}

