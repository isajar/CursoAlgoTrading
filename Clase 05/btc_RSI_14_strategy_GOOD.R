rm(list = ls())
library(Rbitcoin)
library(quantmod)

file_histBTC <- "realTime/BTC/btc_last.txt"
file_capitalDolar <- "realTime/BTC/actual_capital.txt"
file_capitalBTC <- "realTime/BTC/total_btc.txt"
file_trades <- "realTime/BTC/btc_trades.txt"

##### DEFINO CAPITAL INICIAL
btc_last <- market.api.process(market = "bitstamp", currency_pair = c("BTC", "USD"), action = "ticker")
actual_capital <- tail(read.csv(file_capitalDolar, sep = ",", header = F), 1)
total_btc <- tail(read.csv(file_capitalBTC, sep = ",", header = F), 1)

print(paste("INIT CAPITAL BTC:", total_btc))
print(paste("INIT CAPITAL u$d:", actual_capital))
print(paste("TOTAL u$d:", actual_capital+(total_btc*btc_last$last)))

# inicio loop
for (b in 1:100) {

  #### SET UP
  # traigo cotizacion btc/dolar
  btc_last <- market.api.process(market = "bitstamp", currency_pair = c("BTC", "USD"), action = "ticker")
  # genero un archivo con las cotizaciones
  write.table(btc_last, file = file_histBTC, sep = ",", row.names = F, col.names = F, append = T)
  ####
  # leo del archivo creado
  btc_hist <- read.csv(file_histBTC, sep = ",", header = F)
  names(btc_hist) <- c("Market", "Base", "Quote", "TimeStamp", "Marke_Time", "Last", "VWAP", "Vol", "ASK", "BID")




  # guardo el capital inicial en bitcoins y en dolares

  ###############################

  ##################################
  ########## ACA COMIENZA EL ALGORITMO PROPIAMENTE DICHO
  #######################################################

  last_price_btc <- tail(btc_hist$Last, 1)

  if (nrow(btc_hist) >= 15) {
    rsi_14_btc <- round(RSI(btc_hist$Last, 14), 3)
    last_rsi_14_btc <- tail(rsi_14_btc, 1)
    
    macd_fast <- MACD(btc_hist$Last,nFast = 3, nSlow = 10, nSig = 16)
    macd_hist <- (macd_fast[,1]-macd_fast[,2])
    last_macd_hist <- macd_hist[length(macd_hist)]
    macd_buy_signal <- FALSE
    if(!is.na(last_macd_hist)){
      macd_buy_signal <- last_macd_hist > 0
    }
    

    # para comprar btc si el rsi da senal de compra y tengo capital (10% de brx)
    if ((last_rsi_14_btc < 40) & (actual_capital > (0.1 * last_price_btc)) & macd_buy_signal) {
      # genero un log de acciones
      # guardo la accion de compra, con la fecha y el monto
      op_btc <- paste(Sys.time(), "BUY 0.1 BTC at:", last_price_btc)
      write.table(op_btc, file = file_trades, sep = ",", row.names = F, col.names = F, append = T)
      # actualizo el monto disponible y lo guardo.
      actual_capital <- actual_capital - (0.1 * last_price_btc)
      total_btc <- total_btc + 0.1
      write.table(actual_capital, file =file_capitalDolar, sep = ",", row.names = F, col.names = F, append = T)
      write.table(total_btc, file = file_capitalBTC , sep = ",", row.names = F, col.names = F, append = T)

      print("BUYING 0.1 BTC")
    } else if ((last_rsi_14_btc > 60) & (total_btc >= 0.1) & !macd_buy_signal) {

      # para vender btc si el rsi da senal de venta


      op_btc <- paste(Sys.time(), "SELL 0.1 BTC at:", last_price_btc)
      # ENVIO AL MERCADO
      # RECIBO CONFIRMACION
      write.table(op_btc, file = file_trades, sep = ",", row.names = F, col.names = F, append = T)
      actual_capital <- actual_capital + (0.1 * last_price_btc)
      total_btc <- (total_btc - 0.1)

      write.table(actual_capital, file = file_capitalDolar, sep = ",", row.names = F, col.names = F, append = T)
      write.table(total_btc, file = file_capitalBTC, sep = ",", row.names = F, col.names = F, append = T)

      print("SELLING 0.1 BTC")
    } else {
      print("WAIT & SEE !!! ")
    }
  

  print(paste("Estamos Trabajando!!!", Sys.time()))
  print(paste("Actual Capital: ", actual_capital))
  print(paste("BTC Amount: ", total_btc))
  print(paste("Last RSI: ", last_rsi_14_btc))
  print(paste("Last MACDHIST: ", last_macd_hist))
  print(paste("TOTAL CAPITAL:", actual_capital + (total_btc * last_price_btc)))
  print("==============================================")
}

  # ejecuta accion cada 15 seg
  Sys.sleep(15)
}



