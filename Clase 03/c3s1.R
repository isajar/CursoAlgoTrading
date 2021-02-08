setwd("/Users/isajarspector/Desktop/algoTrading/Clase 03")
data_cor <- read.csv("data//Correlacion.csv",header=T,sep=",")
var_1 <- as.numeric(data_cor$Var_1)
var_2 <- as.numeric(data_cor$Var_2)
var_3 <- as.numeric(data_cor$Var_3)
var_4 <- as.numeric(data_cor$Var_4) 
var_5 <- as.numeric(data_cor$Var_5) 
var_6 <- as.numeric(data_cor$Var_6) 
var_7 <- as.numeric(data_cor$Var_7)

cor(var_1,var_2)

pairs(data_cor)

d<-sample(1:6,20,replace=TRUE)
hist(d)

row_names <- c("10", "100", "1K", "10K", "100K", "1000K")
col_name <- c("0", "1","2","Prob")

# crea un vector constante 
a <- rep(0,6)
# lo pasa a columna
d <- cbind(a,a,a,a)
# lo convierte en data frame
res <- data.frame(d,row.names = row_names)
res2 <- data.frame(d,row.names = row_names)
res3 <- data.frame(d,row.names = row_names)
res4 <- data.frame(d,row.names = row_names)
# renombra las columnas
colnames(res) <- col_name
colnames(res2) <- col_name
colnames(res3) <- col_name
colnames(res4) <- col_name



for(i in 1:6) {
  n <- 10 ** i
  res[i,1:3] = as.vector(table(sample(0:2,n,replace=TRUE)))
  res2[i,1:3] = as.vector(table(sample(0:2,n,replace=TRUE)))
  res3[i,1:3] = as.vector(table(sample(0:2,n,replace=TRUE)))
  res4[i,1:3] = as.vector(table(sample(0:2,n,replace=TRUE)))
  res$Prob[i] = res[i,1] / n
  res2$Prob[i] = res2[i,1] / n
  res3$Prob[i] = res3[i,1] / n
  res4$Prob[i] = res4[i,1] / n
}

res
# set the plotting area into a 2*2 array
par(mfrow=c(2,2),oma = c(0, 0, 2, 0))

#PLOT 1
plot(res$Prob,ylab = "Prob", xlab = "N sample", type = "b", main = "Try 1")
lines(x=c(0,6),y=c(1/3,1/3),co="red",lwd=2)
legend("right", legend="Target Prob",
       col="red", lty=1:2, cex=0.8)

#PLOT 2
plot(res2$Prob,ylab = "Prob", xlab = "N sample", type = "b", main =  "Try 2")
lines(x=c(0,6),y=c(1/3,1/3),co="red",lwd=2)
legend("right", legend="Target Prob",
       col="red", lty=1:2, cex=0.8)

#PLOT 3
plot(res3$Prob,ylab = "Prob", xlab = "N sample", type = "b", main = "Try 3")
lines(x=c(0,6),y=c(1/3,1/3),co="red",lwd=2)
legend("right", legend="Target Prob",
       col="red", lty=1:2, cex=0.8)
#PLOT 4
plot(res4$Prob,ylab = "Prob", xlab = "N sample", type = "b", main =  "Try 4")
lines(x=c(0,6),y=c(1/3,1/3),co="red",lwd=2)
legend("right", legend="Target Prob",
       col="red", lty=1:2, cex=0.8)

mtext("Estimación de probabilidad", outer = TRUE, cex = 1.5)



