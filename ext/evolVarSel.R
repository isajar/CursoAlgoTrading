rm(list = ls())
library(genalg)
library(h2o)



# USE OF H20
h2o.init()
# create the train and test h2o data frames
df <- iris
df$n1 <- sample(10,nrow(df),replace = T)
df$n2 <- sample(10,nrow(df),replace = T)
df$n3 <- sample(10,nrow(df),replace = T)
target <- "Species"
dfh2o <- h2o::as.h2o(df)
dfh2o[target] <- h2o::as.factor(dfh2o[target])



# -------------------------------------------------------- #

evaluate <- function(string=c()) {
  
  #string <- c(1,1,1,1,0,0,0,0)
  # column selection for h2o frames
  # col id
  cids <- c(1:ncol(dfh2o))
  # subtract using chromosome
  cids <- cids * string
  # get selected col names
  cname <- names(dfh2o)[cids]
  
  # early return if data dont have at least 2 features (incl. response).
  if(length(cname) < 2 | length(cname) < min_n_col | length(cname) > max_n_col ) {
    return(0)
  }
  # add target if not present on chromosome
  cname <- unique(c(cname,target))
  
  
  #slicing data frame
  dfcrom <- dfh2o[,cname]
  
  # split into train and validation
  data_splits <- h2o.splitFrame(data =  dfcrom, ratios = 0.8, seed = 1234)
  
  train_df_h2o <- data_splits[[1]]
  test_df_h2o <- data_splits[[2]]
  
  
  # Identify predictors and response
  y <- target
  x <- setdiff(cname, y)
  
  # Number of CV cross-validation folds (to generate level-one data for stacking)
  nfolds <- 5
  
  # GBM
  my_gbm <- h2o.gbm(x = x,
                    y = y,
                    training_frame = train_df_h2o,
                    nfolds = nfolds,
                    keep_cross_validation_predictions = TRUE,
                    categorical_encoding = "auto",
                    seed = 5)
  perf <- h2o.performance(my_gbm, test_df_h2o)
  
  r2 <- h2o.r2(perf)

  -r2
}

min_n_col = 3
max_n_col = 5
rbga.results = rbga.bin(size=ncol(df), popSize = 15, mutationChance=0.05,
                        evalFunc=evaluate, iters = 10)
plot(rbga.results)

cat(summary(rbga.results))

cids2 <- c(1:ncol(dfh2o))
# subtract using chromosome
cids2 <- cids2 * rbga.results$population[1,]
# get selected col names
cname2 <- names(dfh2o)[cids2]
cname2
