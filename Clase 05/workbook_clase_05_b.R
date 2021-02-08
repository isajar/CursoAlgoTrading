#EJERCICIO:
#  Conformar un dataset que tenga como variables:
#  ** Al menos 4 parametrizaciones diferentes del Indicador RSI
#  ** Al menos 2 Parametrizaciones del Indicador SMA
#  ** Al menos 2 Parametrizaciones del Indicador EMA
#  ** Estudiar el indicador MACD y pensar como incorporarlo al set de datos.
#  ** Como variable Target el “Precio de una acción”
#  ** Pensar como poner como Target el Rendimiento diario

library(quantmod)

# pensar en condicion while con fechas y tiempo
# adaptar el script para obtener varios titulos

for(t in 1:20){

rt_quote <- c("AAPL")
data_rt_quote <- getQuote(rt_quote)$Last

write.table(data_rt_quote, paste("/Data/IAMC-AT-ML/",rt_quote,sep=""), row.names=F, col.names=F, append=T) 
Sys.sleep(15)
}