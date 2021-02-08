rm(list = ls())


library("neuralnet")
 
#Red Neuronal para crear una funcion "Raíz Cuadrada".
 
#Generamos 50 numeros random, con distribución uniforme, guardandolos como dataframe

traininginput <-  as.data.frame(runif(50, min=0, max=100))
trainingoutput <- sqrt(traininginput)
 
#Column bind y vector con nombres de variables
trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")
 
#Comienzo el entrenamiento de la Red Neuronal
#Comienzo con 10 neuronas en un layer oculto
net.sqrt <- neuralnet(Output~Input,trainingdata, hidden=10)
print(net.sqrt)
 
#Plot the neural network
plot(net.sqrt)
 
#Testeamos la Red Neuronal, y para ello creamos un set de testeo
#Generamos números cuadrados.
testdata <- as.data.frame((1:10)^2)
#Los corremos dentro de la red neuronal
net.results <- compute(net.sqrt, testdata)
 

#Vemos los resultados
print(net.results$net.result)
 
#Mejoramos los resultados
mejor_output <- cbind(testdata,sqrt(testdata),
                         as.data.frame(net.results$net.result))
colnames(mejor_output) <- c("Input","Expected Output","Neural Net Output")
print(mejor_output)
