# Borro todo
rm(list=ls())

# === 1.Realizar una Regresión Lineal con R, con ===
# los siguientes datos, los cuales representan la edad en meses de determinados niños, 
# y su promedio de altura dependiendo de la edad que tienen:

# data
edad <- 18:29
altura <- c(76.1, 77.0, 78.1, 78.2, 78.8, 79.7, 79.9, 81.1, 81.2, 81.8, 82.8,83.5)

# visualization 
plot(edad,altura)
# target:= altura
# predictor:= edad

# regression detail
model <- altura~edad
relation <- lm(altura~edad)
summary(relation)
coef(relation)


# plots
plot(model)
abline(relation)

# === 2. Estimar el promedio de altura de un niño con 27.5 meses de edad ===

# predictions
a <- data.frame(edad = 27.5)
y <- predict(relation,a)
print(y)
