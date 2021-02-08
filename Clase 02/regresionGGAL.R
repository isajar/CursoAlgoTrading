# Load packages
library(quantmod)

#getSymbols(Symbols = "GGAL.BA", auto_assign = TRUE)
from <- Sys.Date() - 60
to <- Sys.Date()
df <- getSymbols(Symbols = "GGAL.BA", from=from, to=to, env = NULL)
str(df)
tail(df)
chart_Series(df)
nrow(df)

train_set <- first(df,30)
test_set <- last(df,12)


x_train <- subset( train_set, select = -c(4,6) )
#y_train <- subset(train_set,select = c(4))
train_set <- subset( train_set, select = -c(6) )
x_test <- subset( test_set, select = -c(4,6) )

#train_model <- lm(y_train ~ x_train)
train_model <- lm(train_set$GGAL.BA.Close ~ . , data = train_set)
summary(train_model)
coef(train_model)


y_est <- predict(train_model,x_test)

aux <- test_set$GGAL.BA.Close
aux$GGAL.BA.Close <- unname(y_est) 

plot(test_set$GGAL.BA.Close, )

lines(aux, col= "yellow" ,lwd = 3)


