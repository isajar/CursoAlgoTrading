rm(list = ls())
library(quantmod)
library("caret")


#FUNCTIONS
## normalization

max_min_norm <- function(df) {
  apply(df, MARGIN = 2, function(x) {
    ( x - min(x) )/ (max(x)-min(x)) 
  })
}

agregarUltimo <- function(df,fecha) {
  # obtengo el ultimo valor
  q <- getQuote('GGAL')
  # creo una copia de la ultima fila para usar la estructura
  aux <- tail(df,1)
  # le cambio la fecha
  time(aux) <- as.Date(fecha)
  # lleno de NA los valores
  aux[,] <- NA
  # agrego el adr.Open
  aux$adr.Open <- q$Open
  # lo convino con lo que tengo
  rbind(symb,aux)
}

# -------------------------------------------------------- #

#SET UP
start_day <- 60
start_date = as.Date(Sys.Date()- start_day)
end_date = (Sys.Date())
acc <- as.character("GGAL.BA")
nadr <- "GGAL"


symb <- getSymbols(acc, src = "yahoo", from = start_date,auto.assign = FALSE,header=FALSE)
gal_adr <- getSymbols(nadr, src = "yahoo", from = start_date,auto.assign = FALSE,header=FALSE)
adr.Close <- Cl(gal_adr)
adr.Open <- Op(gal_adr)
colnames(symb) <- c("Open","High","Low","Close","Vol","Adj")

# Agrego info sobre el adr
symb$adr.Close <- adr.Close
symb$adr.Open <- adr.Open

# En caso de tener info nueva la agrego
symb <- agregarUltimo(symb,"2020-10-27")
symb2 <- subset(symb,select = -Adj )

# -------------------------------------------------------- #

# PREPARO EL SET



# VARIABLES CATEGORICAS
dif <- diff(symb2,1)
##  divido en clases. -1 baja 1 sube
dif <- sign(dif)
## lagueo para combinar los valores de ayer con los de hoy
### La combinacion es adr.open de hoy con los otros valores de ayer.
dif_lag <- lag(dif[,!names(dif)=="Close"],1)
dif_lag$adr.Open.Today <- dif$adr.Open
dif_lag$Close <- dif$Close
dif_lag <- na.omit(dif_lag)
dif_lag$Close <- as.factor(dif_lag$Close)

fit <- kmeans(dif_lag,2)
confusionMatrix(data=factor(fit$cluster), reference = factor(dif_lag$Close))

# -------------------------------------------------------- #
# -------------------------------------------------------- #

# CLASIFICANDO SOLO EL Close
dif_lag2 <- symb2
## convierto la target en categorica -> suba o baja viendo el signo de la diferencia con el anterior
## 1: baja 2:suba
dif_lag2$Close <- as.factor(sign(diff(dif_lag2$Close,1)))


# Lagueo todo menos el adr.open (uso el del dia) y Close (es la que quiero predecir)
dif_lag2[,!names(symb3)=="Close" & !names(symb3)=="adr.Open"] <- lag(dif_lag2[,!names(symb3)=="Close" & !names(symb3)=="adr.Open"],1)
# adr.open no se lagea
dif_lag2$adr.Open <- symb3$adr.Open

#Saco na
ultimo <- tail(dif_lag2,1)
dif_lag2<- na.omit(dif_lag2)
dif_lag2<- rbind(dif_lag2,ultimo)

# normalizacion de datos
dif_lag2[,!names(dif_lag2)=="Close"] <- max_min_norm(dif_lag2[,!names(dif_lag2)=="Close"])

# k-means sacando la categorica y target afuera
fit2 <- kmeans(dif_lag2[,!names(dif_lag2)=="Close"],2)
confusionMatrix(data=factor(fit2$cluster), reference = factor(dif_lag2$Close))
#save(fit2, file = paste(getwd(), "k-mean", acc, ".rda", sep = ""))



