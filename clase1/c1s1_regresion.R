# borrar todo
rm(list=ls())
# Mediante la letra c (componer) creo vectores
# <- flecha para asignar
# x:diametro y:longitud
diametro <- c(2.10,5.50,4.00,3.80,6.00,3.50,4.60)
longitud <- c(6.50,17.10,12.50,12.00,18.90,11.00,14.40)

#ploteo
plot(diametro,longitud)


## lm es la función que invoca el algoritmo de Regresión Lineal. lm([target variable] ~ [predictor variables], data = [data source])

# Determino la variable dependiente e independiente

## El símbolo “~” nos permite relacionar las variables en cuestión. 
## El operador ~ tilde nos permite separar el lado izquierdo del lado derecho en una formula
## En el caso de una realacion lineal del tipo "y = x + b", lo que esta a la izquierda del tilde
## es la variable dependiente (y, respuesta) y a la derecha la independiente (x, explicativa)
## (y~x) se lee como y "en funcion de" x.

# Muestro el resultado por pantalla, invocando a la variable que guardó nuestro resultado:

resultado <- lm(longitud~diametro)
resultado

#coeficientes
# (intercept) es la constante de la recta.
# la otra variable es la pendiente
coef(resultado)

# Detalles sobre la regresion
summary(resultado)

#Podemos graficar ahora la “línea de mejor ajuste”, llamando a la función “abline”
plot(longitud~diametro)
abline(resultado)

#separo los coeficientes
b1 <- coef(resultado)[1]
m1 <- coef(resultado)[2]

# obtengo aproximacion para 7.5 cm
y_ <- m1 * 7.5 + b1  
y_

# otra forma de hacer la prediccion
# creo un data frame (similar a pandas)
a <- data.frame(diametro = 7.5)
y2 <- predict(resultado, a)
print(y2)
