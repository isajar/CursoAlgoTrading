rm(list=ls())

########################################################
############# INTRO QUANTMOD ###########################
########################################################
library(quantmod)

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

#--------------------------------------------------------------------
#--------------------------------------------------------------------
start_date = as.Date(Sys.Date()- start_day)#### Fecha de Inicio. En este ejemplo seran 100 dias hacia atras tomando el dia de hoy.
end_date = (Sys.Date()) ####################### Fecha Final, en este ejemplo, el dia de hoy.
titulo <- as.character("YPFD.BA")
quote <- getSymbols(titulo, src = "yahoo", from = start_date,auto.assign = FALSE,header=FALSE)
names(quote)[1]='Open'
names(quote)[2]='High'
names(quote)[3]='Low'
names(quote)[4]='Close'
names(quote)[5]='Volume'
names(quote)[6]='Adjusted'
## De todas las variables anteriores, solo seleccion Volume y Adjusted
data <- subset(quote,select=c(Volume, Adjusted))
RSI(quote$Close,14)
#--------------------------------------------------------------------
#--------------------------------------------------------------------

getQuote("AAPL") #for Real Time
getQuote("AAPL")$Last #Solo en precio en Real Time

realtime_AAPL <- getQuote("AAPL")$Last

AAPL["2016-07-11"] # Filtro por Fechas

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


yahooQF()
