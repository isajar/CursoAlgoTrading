rm(list = ls())
library(lime)
library(h2o)



# USE OF H20
h2o.init()
# create the train and test h2o data frames
df <- iris
#df$n1 <- sample(10,nrow(df),replace = T)
#df$n2 <- sample(10,nrow(df),replace = T)
#df$n3 <- sample(10,nrow(df),replace = T)
target <- "Species"
dfh2o <- h2o::as.h2o(df)
dfh2o[target] <- h2o::as.factor(dfh2o[target])


set.seed(4)
index <- sample(seq_len(nrow(df)), 4)
train_obs <- df[-index, ]
local_obs <- df[index, ]

#data_splits <- h2o.splitFrame(data =  dfh2o, ratios = 0.8, seed = 1234)

#train_df_h2o <- data_splits[[1]]
#test_df_h2o <- data_splits[[2]]
train_df_h2o <- as.h2o(train_obs)

# Identify predictors and response
y <- target
x <- setdiff(names(dfh2o), y)

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

explainer <- lime(train_obs, my_gbm, bin_continuous = TRUE, quantile_bins = FALSE)

explanation<- explain(
  x = local_obs, 
  explainer = explainer, 
  n_features = 3, 
  n_labels = 1
)
plot_features(explanation)

plot_explanations(explanation)

local_obs
