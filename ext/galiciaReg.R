# Load packages
library(quantmod)

#getSymbols(Symbols = "GGAL.BA", auto_assign = TRUE)
from <- Sys.Date() - 60
to <- Sys.Date()
df <- getSymbols(Symbols = "GGAL.BA", from=from, to=to, env = NULL)
df$prev <- df$GGAL.BA.Close * 0
df$prev[1] <- mean(df$GGAL.BA.Close)
df$prev[2:nrow(df)] = df$GGAL.BA.Close[1:(nrow(df)-1)]

head(df)

model <- lm(df$GGAL.BA.Close ~ df$prev)
model
summary(model)
p <- predict(model,df$prev)


aux <- df$GGAL.BA.Close
aux$GGAL.BA.Close <- unname(p) 

plot(df$GGAL.BA.Close)
lines(aux, col= "yellow" ,lwd = 3)


