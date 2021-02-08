rm(list=ls())
library(genalg)
# Creacion del dataset
# El fenotipo seria la columna de item,
dataset <- data.frame(item = c("pocketknife", "beans", "potatoes", "unions","sleeping bag", "rope", "compass"),
          survivalpoints = c(10, 20, 15, 2, 30, 10, 30), weight = c(1, 5, 10, 1, 7, 5, 1))

weightlimit <- 20

#ejemplo de codificacion
chromosome = c(1, 0, 0, 1, 1, 0, 0)
dataset[chromosome == 1, ]
cat(chromosome %*% dataset$survivalpoints)
cat(chromosome %*% dataset$weight)

# X es un cromosoma
evalFunc <- function(x) {
  current_solution_survivalpoints <- x %*% dataset$survivalpoints
  current_solution_weight <- x %*% dataset$weight
  
  if (current_solution_weight > weightlimit) 
    return(0) else return(-current_solution_survivalpoints)
}

iter = 20
#size es el numero de genes en el cromosoma
GAmodel <- rbga.bin(size = 7, popSize = 50, iters = iter, mutationChance = 0.01, elitism = T, evalFunc = evalFunc)
cat(summary(GAmodel))

solution = c(1, 1, 0, 1, 1, 1, 1)

dataset[solution == 1, ]

cat(paste(solution %*% dataset$survivalpoints, "/", sum(dataset$survivalpoints)))
