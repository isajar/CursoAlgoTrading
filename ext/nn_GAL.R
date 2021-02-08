rm(list = ls())

library(neuralnet)
library(quantmod)

#SET UP
start_day <- 250 
start_date = as.Date(Sys.Date()- start_day)
end_date = (Sys.Date())
acc <- as.character("GGAL.BA")
neurons <- 10
stmax <- 1000000

symb <- getSymbols(acc, src = "yahoo", from = start_date,auto.assign = FALSE,header=FALSE)
colnames(symb) <- c("Open","High","Low","Close","Vol","Adj")
train_data <- symb


head(symb)
#Volumen mayor a la media
vmm <- symb$Vol > median(symb$Vol)
train_data$VMM <- vmm
# saco volumen y ajustado
train_data <- subset(train_data,select=c(-Vol,-Adj))
rsi_14 <- RSI(train_data$Close)
#rsi 14
train_data$RSI14 <- rsi_14
h<- MACD(train_data$Close)
train_data$macd <- h$macd
train_data$macdSig <- h$signal
train_data$ema21 <- EMA(symb$Close,n=21)
tail(train_data)

head(train_data)
train_data <- na.omit(train_data,)
# lageo el precio
lagged_price <- lag(train_data$Close, -1)
train_data$Close <- lagged_price

#uso el open de hoy
train_data$Open <- lag(train_data$Open, -1)
train_data$Open[nrow(train_data)] <- 109.95
train_data
tail(train_data)
test_data <- tail(train_data,1)
train_data <- train_data[-nrow(train_data),]
nn <- neuralnet(formula = Close ~ . , data=train_data, hidden = neurons, stepmax = stmax )
plot(nn)

predict(nn,test_data)
