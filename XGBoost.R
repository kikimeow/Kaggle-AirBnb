## XGBoost model used to predict one of the 12 classes of country destination
## The probability of booking from the ensemble is used as one of the input to the model

## Model 1: filtered Keywords, includes ALL avg, sec, sum, flag

## References:
#https://rpubs.com/flyingdisc/practical-machine-learning-xgboost

require(dplyr)
require(xgboost)
require(caret)
#require(Hmisc)
#require(car)

# **************************************
# Setup
# **************************************

path <- "~/Class- R Class/Kaggle- AirBnB/Solution Final - SH"
setwd(path)
#load("data/Preprocessed.RData")

# load data
load("data/X_train.rda")
load("data/X_eval.rda")
load("data/test.rda")

# Save possible predictors
predictors <- names(X_train)

# create predictor categories
flags <- grep("^flag.", predictors)

avg <- grep("^avg.", predictors)
sum <- grep("^sum.", predictors)
sec <- grep("^sec.", predictors)
month <- grep("^month.", predictors)
deviceCount <- grep("^deviceCount.", predictors)
deviceFlag <- grep("^deviceFlag.", predictors)

# Keywords related to international travel
keywordsIntl = list("translations", "languages", "translate", "curr", "countries", "country",
"locations", "localization", "destination")

# Keywords related to booking or not
keywordsBook = c("booking", "book", "coupon", "contact_host", "change_trip_characteristics", "about_host", "amenitie", "reviews",
"message", "share_click", "wishlist", "availability", "guest_cancellation_cancel", "reservations", "travel_plans", "reservations",
"unavailable", "payment", "confirmation")

keywordIntlList = c()
for (keyword in keywordsIntl){
  regExp = paste(".", keyword, ".", sep = "")
  matches <- grep(regExp, predictors)
  keywordIntlList <- c(keywordIntlList, matches)
}
keywordIntlList


keywordBookingList = c()
for (keyword in keywordsBook){
  regExp = paste(".", keyword, ".", sep = "")
  matches <- grep(regExp, predictors)
  print(matches)
  keywordBookingList <- c(keywordBookingList, matches)
}
keywordBookingList

# Pick predictors to be used in model
reduced <- predictors[-c(flags,avg, sum, sec, month, deviceCount, deviceFlag)]
addition <- predictors[c(keywordIntlList, keywordBookingList)]
modelPredictors <- c(reduced, addition)

# Prepare dataset 
#XGBoost supports only numeric matrix data. Converting all training, testing and outcome data to matrix.

# convert country_destination from factor to numeric matrix, xgboost takes multi-labels in [0, numOfClass)
y_train <- as.matrix(as.integer(X_train$country_destination)-1)
y_eval <- as.matrix(as.integer(X_eval$country_destination)-1)

# table(X_train$country_destination)
# AU    CA    DE    ES    FR    GB    IT   NDF    NL other    PT    US 
# 115   324   185   536  1070   562   732 33781   187  2765    59 15046 
# y_train
# 0     1     2     3     4     5     6     7     8     9    10    11 
# 115   324   185   536  1070   562   732 33781   187  2765    59 15046 

# table(X_eval$country_destination)
# AU    CA    DE    ES    FR    GB    IT   NDF    NL other    PT    US 
# 37   116    65   171   365   169   247 11260    60   890    24  5049 
# y_eval
# 0     1     2     3     4     5     6     7     8     9    10    11 
# 37   116    65   171   365   169   247 11260    60   890    24  5049 

# save ID data
train_id <- X_train$id
eval_id <- X_eval$id
test_id <- test$id

# Get rid of variables not used in model
X_train <- select(X_train[,modelPredictors], -c(booked, id, country_destination))
X_eval <- select(X_eval[,modelPredictors], -c(booked, id, country_destination))
test <- select(test[,modelPredictors], -c(booked, id, country_destination))

# convert data to matrix
train.matrix = as.matrix(X_train)
eval.matrix = as.matrix(X_eval)
test.matrix = as.matrix(test)

# convert from character to numeric matrix
mode(train.matrix) = "numeric" 
mode(eval.matrix) = "numeric"
mode(test.matrix) = "numeric"

# check for missing data
#na_count <-sapply(X_train, function(y) sum(is.na(y)))
#na_count <- data.frame(na_count)

# create xgb matrix for model training
train.matrix = xgb.DMatrix(train.matrix,label = y_train, missing=NA)
eval.matrix = xgb.DMatrix(eval.matrix,label = y_eval, missing=NA)

## xgboost
# cross validation (uncomment the following and run if required)
# XGBoost has a very useful function called as "cv" which performs cross-validation at each boosting iteration and thus returns the optimum number of trees required.

# XGBoost parameters
num.class = 12
param <- list("objective" = "multi:softprob",    # multiclass classification 
              "num_class" = num.class,    # number of classes 
              "eval_metric" = "merror",    # evaluation metric -merror, mlogloss
              "max_depth" = 5,    # maximum depth of tree 
              "eta" = 0.3,    # step size shrinkage 
              "gamma" = 0,    # minimum loss reduction 
              "subsample" = 0.75,    # part of data instances to grow tree 
              "colsample_bytree" = 0.5,  # subsample ratio of columns when constructing each tree 
              "min_child_weight" = 5  # minimum sum of instance weight needed in a child 
)

## Train Models (to understand features first)
# real model fit training, with full data
xgbFit <- xgboost(param=param, 
                  data=train.matrix, 
                  nrounds=200, 
                  verbose=0)

xgbFit_model1 <- xgbFit

# model scoring
pred <- predict(xgbFit, eval.matrix)
pred_matrix <- as.data.frame(matrix(pred, nrow(eval.matrix), 12, byrow=T))
head(pred_matrix)

# extracting top-5 countries
topFive <- as.data.frame(t(apply(pred_matrix, 1, function(y) order(-y)[1:5])))
head(topFive)
topOne <- topFive[,1]
topOne <- topOne - 1
assessment <- cbind.data.frame(prediction = as.integer(topOne), actual = as.integer(y_eval))
confusionMatrix(assessment$prediction, assessment$actual)
# Accuracy : 0.6957         
# 95% CI : (0.689, 0.7023)
# No Information Rate : 0.6102         
# P-Value [Acc > NIR] : < 2.2e-16      
# 
# Kappa : 0.3761  
XGB_model1_CM <- confusionMatrix(assessment$prediction, assessment$actual)

# Review variable importance
importance_matrix <- xgb.importance(names(X_train), model = xgbFit)
plot <- xgb.plot.importance(importance_matrix, numberOfClusters = c(1:10))

VarImp_model_1 <- importance_matrix


save(xgbFit_model1, file = "model output/xgbFit_model1.rda")
save(XGB_model1_CM, file = "model output/XGB_model1_CM.rda")
save(VarImp_model_1, file = "model output/VarImp_model_1.rda")




###__Code below are not checked yet______________________________________________________##########


# 5-fold cross validation
set.seed(100)
# k-fold cross validation, with timing
system.time( bst.cv <- xgb.cv(param=param, data=train.matrix, 
                              nfold=5, nrounds=200, prediction=TRUE, verbose=FALSE))
# 120 minutes (run 1)
#     user   system  elapsed 
# 27717.74    34.83  7260.05 

# user  system elapsed 41 min run 2
# 9476.75   27.94 2493.47 


# From the cross validation, choose index with minimum multiclass error rate.
head(bst.cv$dt,100)
min.merror.idx = which.min(bst.cv$dt[, test.mlogloss.mean]) #48
min.merror.idx = which.min(bst.cv$dt[, test.merror.mean]) #163
min.merror.idx  


# get CV's prediction decoding
pred.cv = matrix(bst.cv$pred, nrow=length(bst.cv$pred)/num.class, ncol=num.class)
pred.cv = max.col(pred.cv, "last")
pred.cv = pred.cv - 1
# confusion matrix
table(factor(y_train), factor(pred.cv))

pred_matrix <- as.data.frame(matrix(bst.cv$pred, nrow(X_train), num.class, byrow = T))
head(pred_matrix)
top_country <- as.data.frame(t(apply(pred_matrix, 1, function(y) order(-y)[1:5])))
top_country_1 <- top_country[,1]


confusionMatrix(top_country_1, y_train+1)

set.seed(100)
xgb_cv <- xgb.cv(data=as.matrix(X_train[,Namesx]), 
                 label=as.matrix(X_train$country_destination), 
                 objective="multi:softprob", 
                 num_class=12, 
                 nfold=5, 
                 nrounds=10, #changed from 200
                 eta=0.3,  # changed from 0.05 
                 max_depth=5, 
                 subsample=0.75, #changed from 0.9 - 0.75
                 colsample_bytree=0.5, 
                 min_child_weight=5, 
                 eval_metric='mlogloss')

# CV: 0.900348

# model building
set.seed(100)
model_xgb <- xgboost(as.matrix(X_train), 
                     as.matrix(y_train), 
                     objective="multi:softprob", 
                     num_class=12, 
                     nrounds=200, 
                     eta=0.05, 
                     max_depth=5, 
                     subsample=0.9, 
                     colsample_bytree=0.5, 
                     min_child_weight=5, 
                     eval_metric='mlogloss')

# see this link for the parameter explanations
# http://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python
# eta [default=0.3]
# min_child_weight [default=1]
# max_depth [default=6] Typical values: 3-10
# subsample [default=1] Denotes the fraction of observations to be randomly samples for each tree. Typical values: 0.5-1
# colsample_bylevel [default=1]  Denotes the subsample ratio of columns for each split, in each level.
# scale_pos_weight [default=0] A value greater than 0 can be used in case of high class imbalance as it helps in faster convergence.  scale_pos_weight = 1: Because of high class imbalance.

# model scoring
pred <- predict(model_xgb, as.matrix(X_test))

pred_matrix <- as.data.frame(matrix(pred, nrow(X_test), 12, byrow=T))

# extracting top-5 countries
submit <- as.data.frame(t(apply(pred_matrix, 1, function(y) order(-y)[1:5])))
submit$id <- test_ids

# creating submission file
submit <- melt(submit, id_vars='id')
submit <- merge(submit, data.frame("value"=seq(1,12),"country"=c("AU","CA","DE","ES","FR","GB","IT","NDF","NL","other","PT","US")))

# saving submission
submit <- submit[order(submit$variable),c("id","country")]
write.csv(submit, "./submit.csv", row.names=F)

importance_matrix <- xgb.importance(names(X_train), model = model_xgb)
gp = xgb.plot.importance(importance_matrix)


# Get the feature real names
names <- dimnames(X_train)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = model_xgb)

# generate plots
xgb.plot.importance(importance_matrix[1:10,])
xgb.plot.tree(feature_names = names, model = model_xgb, n_first_tree = 2)