rm(list=ls())

library("neuralnet")

x1 <- sample(0:1,10,replace=T)
x2 <- sample(0:1,10,replace=T)

x1orx2 <- as.numeric(!(x1 == x2 & x1==0))

df <- cbind(x1,x2,x1orx2)
df

train_data <- df[1:5,]
test_data <- df[5:10,]

nn_xor <- neuralnet(formula = x1orx2 ~ . , data = train_data,hidden = 1)
nn_xor
plot(nn_xor)

w0 <- nn_xor$result.matrix[7]
w1 <- nn_xor$result.matrix[5]
w2 <- nn_xor$result.matrix[6]

recta <- -w1/w2 * x1 + w0

plot(x1,x2,col="red")
lines(x1,recta,col="blue")

pred <- predict(nn_xor,test_data)

result<-data.frame(test_data,round(pred[,1],3))

names(result)[3]="y_real"
names(result)[4]="y_est"
result$error <- sqrt((result$y_real-result$y_est)**2)
format(result,width=10)


