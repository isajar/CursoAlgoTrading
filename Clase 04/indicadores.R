# Load packages
library(quantmod)
require(corrplot)

# DOWNLOAD DATA
#getSymbols(Symbols = "GGAL.BA", auto_assign = TRUE)
from <- Sys.Date() - 60
to <- Sys.Date()
df <- getSymbols(Symbols = "AAPL", from=from, to=to, env = NULL)
names(df)<-c("Open","High", "Low", "Close", "Volume", "Adjusted")

# INDICADORES 
df$rsi_6 <- RSI(df$Close,6)
df$rsi_11 <- RSI(df$Close,11)
df$rsi_16 <- RSI(df$Close,16)
df$rsi_21 <- RSI(df$Close,21)

df$sma_10 <- SMA(df$Close,10)
df$sma_21 <- SMA(df$Close,21)


#TRATO DE NANS
sum(is.na(df))
which(is.na(df))
apply(is.na(df), 2, which)
# para Rsi
df$rsi_6 <- na.fill(df$rsi_6,mean(f$rsi_6[7:nrow(f)]))

#Para sma
ix <- colSums(!is.na(df)) > 0
df[, ix] <- na.approx(df[, ix]); df
na.approx(df[,10:11])

#PLOTEO
plot(df$Close, main = "Cierre", grid.ticks.on = "days", major.ticks = "days", grid.col = "lightgrey")

# para pasar a data frame el xts
f<-data.frame(date=index(df), coredata(df))


#CORRELACION
pairs(f)
corr_mat<-cor(df)

corrplot(corr_mat, method = "number")

# OTRO PLOT
ggplot(f, aes(date, Close))+ geom_point() + geom_line(colour = "blue")+xlab("Year")+ ylab("Total Rec Visitors")+ggtitle("Glacier Annual Recreational Visitors")

