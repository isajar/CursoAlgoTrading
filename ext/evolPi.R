rm(list = ls())
library(genalg)
# intenta encontrar el numero pi por evolucion
evaluate <- function(string=c()) {
  n3 = string[9] + 2 * string[8] + string[7]
  n2 = string[6] + 2 * string[5] + 4 * string[4]
  n1 = string[3] + 2 * string[2] + 4 * string[1]
  n2 = n2/10
  n3 = n3/100
  returnVal = abs(pi - (n1 + n2 + n3))
  returnVal
}
rbga.results = rbga.bin(size=9, popSize = 100, mutationChance=0.01,
                        evalFunc=evaluate)
plot(rbga.results)

cat(summary(rbga.results))