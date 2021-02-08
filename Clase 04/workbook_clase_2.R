rm(list=ls())

########################################################
############# INTRO QUANTMOD ###########################
########################################################
library(quantmod)
getSymbols("AAPL") # No se necesita guardar en objeto
class(AAPL)
head(AAPL)
tail(AAPL)

symbs <- c("AAPL","YPFD.BA","^MERV") # Declaro varios symbols al mismo tiempo
getSymbols(symbs)

ypf_final <- na.omit(YPFD.BA) # saco todos los NAs
merv_final <- na.omit(MERV)

from_dat <- as.Date("01/01/17", format="%m/%d/%y") # Siempre se recomienda crear variables 
to_dat <- as.Date("05/30/17", format="%m/%d/%y")   # con fecha de inicio y fin 
getSymbols("GGAL.BA", src="yahoo", from = from_dat, to = to_dat)
head(GGAL.BA)
tail(GGAL.BA)

aapl_month <- to.monthly(AAPL) #Convertir datos a mensuales
aapl_week <- to.weekly(AAPL) #Convertir datos a semanales
head(aapl_month)
head(aapl_week)

names(AAPL) <- c("Open","High","Low","Close","Vol","Adj") # Renombrar Variables
head(AAPL)

getQuote("AAPL") #for Real Time
getQuote("AAPL")$Last #Solo en precio en Real Time

realtime_AAPL <- getQuote("AAPL")$Last

AAPL["2016-07-11"] # Filtro por Fechas

### Escritura en Disco
write.csv(AAPL,file="/Data/CURSO_IAMC/aapl.csv") # Guardo datos en formato CSV - Reemplazar por su PATH
write.table(realtime_AAPL, "/Data/CURSO_IAMC/aapl_price.csv", sep=",", col.names=F, append=T) 

###########################################
### Comienzo del Trabajo con INDICADORES
### RSI ###
aapl_rsi_21 <- RSI(Cl(AAPL),21) # RSI 21 sobre el Valor de Cierre(Cl)
ypfd_ba_rsi_14 <- RSI(Cl(ypf_final),14) # RSI 14 sobre el Valor de Cierre(Cl)

### Practica de calculo de RSI y Data Frames
vector_01 <- c(1:30)
vector_02 <- sample(1:100,size = 30)
df_01 <- as.data.frame(cbind(vector_01,vector_02))
names(df_01) <- c("V1","V2")
df_01$RSI_21 <- round(RSI(df_01$V2,21),2)
df_01_final <- subset(df_01,df_01$RSI_21 != "NA")
df_01_final
#
#### Otros indicadores
ypfd_ba_ema_50 <- round(EMA(Cl(ypf_final),50),2) # Exponential Moving Average 50 sobre el Valor de Cierre(Cl)
ypfd_ba_sma_50 <- round(SMA(Cl(ypf_final),50),2) # Simple Moving Average 50 sobre el Valor de Cierre(Cl)
ypfd_ba_bbands <- round(BBands(Cl(ypf_final),n=20,maType = SMA),2) # Bollinger Bands de 20 periodos y SMA
### Comienzo del Trabajo con CHARTING
chart_Series(ypf_final) ## Charting
candleChart(ypf_final,multi.col=TRUE,theme='white') 
addRSI(n = 14, maType = "EMA", wilder = TRUE)
addBBands(n = 20, sd = 2, ma = "SMA", draw = 'bands', on = -1)

#####################################
### ALGORITMO MAS SIMPLE
#####################################

if(as.numeric(tail(ypfd_ba_rsi_14$rsi,1) < 30)) {
  print("COMPRAR YPF")
  } else if(as.numeric(tail(ypfd_ba_rsi_14$rsi,1) > 70)) {
    print("VENDER YPF")
  } else {
    print("NO HACER NADA")
  }