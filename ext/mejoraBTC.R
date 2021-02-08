rm(list = ls())
library(Rbitcoin)
library(quantmod)

# defino los files a utilizar

actual_dir <- getwd()

file_histBTC <- "/BTC/btc_last.txt"
file_capitalDolar <- "/BTC/actual_capital.txt"
file_capitalBTC <- "/BTC/total_btc.txt"
file_trades <- "/BTC/btc_trades.txt"

# check if file exist, if not -> create
if(!file.exists(file_capitalBTC)) {
  file.create(paste(actual_dir,file_capitalBTC,sep = ""))
}