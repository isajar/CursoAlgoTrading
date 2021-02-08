library(tidyverse)
library(h2o)
df <- read.csv("./Clase_12/istambul_mkt.csv", sep =";", stringsAsFactors = TRUE)
head(df)
df <- df%>%select(-date)
smp_size <- floor(0.75 * nrow(df))
train_ind <- 1:smp_size

train_df <- df[train_ind, ]
test_df <- df[-train_ind, ]

h2o.init()

# create the train and test h2o data frames

train_df_h2o<-as.h2o(train_df)
test_df_h2o<-as.h2o(test_df)

# Identify predictors and response
y <- "ISE"
x <- setdiff(names(train_df_h2o), y)

# Number of CV cross-validation folds (to generate level-one data for stacking)
nfolds <- 5

# 1. Generate a 3-model ensemble (GBM + RF + Logistic)

# Train &amp; Cross-validate a GBM
my_gbm <- h2o.gbm(x = x,
                  y = y,
                  training_frame = train_df_h2o,
                  nfolds = nfolds,
                  keep_cross_validation_predictions = TRUE,
                  seed = 5)

# Train &amp; Cross-validate a RF
my_rf <- h2o.randomForest(x = x,
                          y = y,
                          training_frame = train_df_h2o,
                          nfolds = nfolds,
                          keep_cross_validation_predictions = TRUE,
                          seed = 5)


# Train &amp; Cross-validate a LR
my_lr <- h2o.glm(x = x,
                 y = y,
                 training_frame = train_df_h2o,
                 family = c("binomial"),
                 nfolds = nfolds,
                 keep_cross_validation_predictions = TRUE,
                 seed = 5)



# Train a stacked random forest ensemble using the GBM, RF and LR above
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                metalearner_algorithm="drf",
                                training_frame = train_df_h2o,
                                base_models = list(my_gbm, my_rf, my_lr))


# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = test_df_h2o)


# Compare to base learner performance on the test set
perf_gbm_test <- h2o.performance(my_gbm, newdata = test_df_h2o)
perf_rf_test <- h2o.performance(my_rf, newdata = test_df_h2o)
perf_lr_test <- h2o.performance(my_lr, newdata = test_df_h2o)
perf_list <- list(gbm=h2o.auc(perf_gbm_test), rf=h2o.auc(perf_rf_test) , lr=h2o.auc(perf_lr_test) )

baselearner_best_auc_test <- max(unlist(perf_list))
name_best_auc <- names(perf_list)[which.max(baselearner_best_auc_test)]
ensemble_auc_test <- h2o.auc(perf)
print(paste("Best Base-Learner: ",name_best_auc , "- Best Base-learner Test AUC: ", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))
