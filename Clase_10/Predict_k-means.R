library("flexclust")
data("Nclus")

set.seed(1)
dat <- as.data.frame(Nclus)
ind <- sample(nrow(dat), 50)

dat[["train"]] <- TRUE
dat[["train"]][ind] <- FALSE

cl1 <- kcca(dat[dat[["train"]]==TRUE, 1:2], k=4, kccaFamily("kmeans"))

pred_train <- predict(cl1)
pred_test <- predict(cl1, newdata=dat[dat[["train"]]==FALSE, 1:2])

image(cl1)
points(dat[dat[["train"]]==TRUE, 1:2], col=pred_train, pch=19, cex=0.3)
points(dat[dat[["train"]]==FALSE, 1:2], col=pred_test, pch=22, bg="orange")

