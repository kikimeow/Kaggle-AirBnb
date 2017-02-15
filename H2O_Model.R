## Create an ensemble to predict if user will book or not book 
## Probability of booking is then added to the training / testing set

# Output:
# "bookingProbstackDeep"                                                
# "bookingProbstackGLM"                                                 
# "bookingProbh20Fit_GLM"                                               
# "bookingProbh20Fit_Deep"

# reference manual link: http://rpackages.ianhowson.com/cran/h2o/

# **************************************
# Setup
# **************************************

path <- "input path here"
setwd(path)
load("data/Preprocessed.RData")


library(h2oEnsemble)  # Requires version >=0.0.4 of h2oEnsemble
localH2O <- h2o.init(nthreads = -1, max_mem_size = "8G")  # Start an H2O cluster with nthreads = num cores on your machine.  Use all cores available to you (-1)
h2o.removeAll() # Clean slate - just in case the cluster was already running


# **************************************
# Save as H2O frames
# **************************************

# create h2o dataframe for model training
train_h2o <- as.h2o(X_train,  destination_frame = "train_h2o")
eval_h2o <- as.h2o(X_eval,  destination_frame = "eval_h2o")
test_h2o <- as.h2o(test,  destination_frame = "test_h2o")
class(test_h2o)

# Setup X (predictors) & Y
Namey <- "booked"
Namesx <- setdiff(names(X_train), c("id", "country_destination", Namey))

# **************************************
# GLM Model - Search Grid
# **************************************
alpha_opts = list(list(0), list(0.25), list(0.5), list(0.75), list(1))
lambda_opts = list(list(1), list(.5), list(.1), list(.01), list(.001), list(.0001), list(.00001), list(0))

hyper_parameters = list(alpha = alpha_opts, lambda = lambda_opts)

grid  <- h2o.grid("glm",
                  hyper_params = hyper_parameters,
                  y = Namey,
                  x = Namesx,
                  training_frame = train_h2o,
                  validation_frame = eval_h2o,
                  family = "binomial")

# rename grid for specific model
grid_GLM <- grid 

# Fetch grid models
#grid_models <- lapply(grid@model_ids, function(model_id) { model = h2o.getModel(model_id) })

# Get Model Ouputs of the models ran
model_ids <- grid@model_ids
models <- lapply(model_ids, function(id) { h2o.getModel(id)})  # get list of model outputs
models[1] # review best model

# print parameters and performance summary
for (i in 1:length(models)) {
  print(sprintf("regularization: %-50s  auc: %f logloss: %f",
                models[[i]]@model$model_summary$regularization,
                h2o.auc(models[[i]]),
                h2o.logloss(models[[i]])))}

rm(i)
# Get grid summary
grid@summary_table
summary(grid) # too long

# Fetch grid models
best_model <- grid@model_ids[[1]]
best_model  
best_GLM_Model <- best_model #"Grid_GLM_train_h2o_model_R_1459976840986_11610_model_36"


# **************************************
# GBM - Search Grid
# **************************************
#https://github.com/h2oai/h2o-3/blob/master/h2o-docs/src/booklets/v2_2015/PDFs/online/GBM_Vignette.pdf

ntrees_opt <- list(list(500))   # default is 50
maxdepth_opt <- seq(3, 9)  # default 5?
learnrate_opt <- seq(0.03, 0.3) # default 0.1?
hyper_parameters <- list(ntrees=ntrees_opt, 
                         max_depth=maxdepth_opt, 
                         learn_rate=learnrate_opt)
search_criteria = list(strategy = "RandomDiscrete", 
                       max_models = 10,  #max_runtime_secs=100, 
                       seed=100)

grid <- h2o.grid("gbm", 
                 hyper_params = hyper_parameters, 
                 search_criteria = search_criteria, 
                 y = Namey,
                 x = Namesx,
                 training_frame = train_h2o,
                 validation_frame = eval_h2o,
                 score_each_iteration = T,
                 sample_rate = 0.75, ## use a random 75% of the rows to fit each tree
                 col_sample_rate = 0.7, ## use 70% of the columns to fit each tree
                 distribution = "AUTO",
                 stopping_rounds = 2,
                 stopping_tolerance = 0.001,
                 stopping_metric = "logloss")

grid_GBM <- grid

# Get Model Ouputs of the models ran
model_ids <- grid@model_ids
models <- lapply(model_ids, function(id) { h2o.getModel(id)})  # get list of model outputs
models[1] # review best model

# print parameters and performance summary
for (i in 1:length(models)) {
  print(sprintf("num_trees: %f max_depth: %f  learn_rate: %f auc: %f logloss: %f",
                models[[i]]@model$model_summary$number_of_trees,
                models[[i]]@model$model_summary$max_depth,
                models[[i]]@parameters$learn_rate,
                h2o.auc(models[[i]]),
                h2o.logloss(models[[i]])))}

rm(i)

# Get grid summary
grid@summary_table
summary(grid) # too long

# Fetch grid models
best_model <- grid@model_ids[[1]]
best_model  
best_GBM_Model <- best_model #"Grid_GBM_train_h2o_model_R_1459976840986_11713_model_4"

# **************************************
# Random Forest - Search Grid
# **************************************
maxdepth_opt <- seq(10,100,10)

search_criteria = list(strategy = "RandomDiscrete", 
                       max_models = 10, 
                       stopping_metric = "AUTO", 
                       stopping_tolerance = 0.0001,
                       stopping_rounds = 3,
                       seed=123456)

hyper_parameters <- list(max_depth=maxdepth_opt)

grid  <- h2o.grid("randomForest", 
                  hyper_params = hyper_parameters, 
                  search_criteria = search_criteria, 
                  score_each_iteration = T,
                  y = Namey,
                  x = Namesx,
                  ntrees = 500,
                  training_frame = train_h2o,
                  validation_frame = eval_h2o)

grid_rf <- grid

# Get Model Ouputs of the models ran
model_ids <- grid@model_ids
models <- lapply(model_ids, function(id) { h2o.getModel(id)})  # get list of model outputs
models[1] # review best model

# Get grid summary
grid@summary_table
summary(grid) # too long

# Fetch grid models
best_model <- grid@model_ids[[1]]
best_model  
best_RF_Model <- best_model #"Grid_GLM_train_h2o_model_R_1459976840986_11610_model_36"

# review this 
param_name <- grid_rf@hyper_names[1,1]
best_param <- grid_rf@summary_table[1,1]
best_param
best_logloss <- grid_rf@summary_table[1,3]
best_logloss
grid_rf@summary_table[1,1:3]

# **************************************
# Save tuned models
# **************************************

h2o.saveModel(object = h2o.getModel(best_RF_Model), path = "model output/", force = TRUE)
h2o.saveModel(object = h2o.getModel(best_GLM_Model), path = "model output/", force = TRUE)
h2o.saveModel(object = h2o.getModel(best_GBM_Model), path = "model output/", force = TRUE)


# **************************************
# Ensemble - stack tuned models
# **************************************

# https://github.com/h2oai/h2o-3/commit/791aaedb3f4ad2c87136aab491db117750d54b0a
# https://github.com/h2oai/h2o-3/tree/master/h2o-r/ensemble
# source code: https://gist.github.com/ledell/f389ac1e9c6e7000b299

# install latest package
# library(devtools)
# install_github("h2oai/h2o-3/h2o-r/ensemble/h2oEnsemble-package")


# Re-run models with 5-fold CV. 
nfolds = 5

# AUC:  0.8046919
glmFit <- h2o.glm(
  alpha = 1,
  lambda = 0.001,
  y = Namey,
  x = Namesx,
  training_frame = train_h2o,
  validation_frame = eval_h2o,
  family = "binomial",
  nfolds = nfolds,
  fold_assignment = "Modulo",
  keep_cross_validation_predictions = TRUE) 

# AUC:  0.8387453 
gbmFit <- h2o.gbm(
  ntrees = 500,
  max_depth = 9,
  learn_rate = 0.03,
  y = Namey,
  x = Namesx,
  training_frame = train_h2o,
  validation_frame = eval_h2o,
  score_each_iteration = T,
  sample_rate = 0.75, ## use a random 75% of the rows to fit each tree
  col_sample_rate = 0.7, ## use 70% of the columns to fit each tree
  distribution = "AUTO",
  stopping_rounds = 2,
  stopping_tolerance = 0.01,
  stopping_metric = "logloss",
  nfolds = nfolds,
  fold_assignment = "Modulo",
  keep_cross_validation_predictions = TRUE,
  seed = 100)  


# AUC:  0.8228619
rfFit <- h2o.randomForest( 
  ntrees = 500,
  max_depth = 50,
  score_each_iteration = T,
  y = Namey,
  x = Namesx,
  training_frame = train_h2o,
  validation_frame = eval_h2o,
  nfolds = nfolds,
  fold_assignment = "Modulo",
  keep_cross_validation_predictions = TRUE,
  seed = 100,
  stopping_metric = "AUTO", 
  stopping_tolerance = 0.01,
  stopping_rounds = 3)

rfmodel.path = h2o.saveModel(object = rfFit, path = "model output/", force = TRUE)
glmmodel.path =h2o.saveModel(object = glmFit, path = "model output/", force = TRUE)
gbmmodel.path = h2o.saveModel(object = gbmFit, path = "model output/", force = TRUE)


# Ensemble - pick metalearner (GLM or deeplearning)
#rfmodel = h2o.loadModel(rfmodel.path)
#glmmodel = h2o.loadModel(glmmodel.path)
#gbmmodel = h2o.loadModel(gbmmodel.path)

# first run
models <- c(gbmFit, glmFit, rfFit)

# re-use model (does not work) Object 'prediction_DRF_model_R_1459976840986_21795_cv_1' not found for argument: key
#models <- c(rfmodel, glmmodel,gbmmodel)  

# GLM wrapper
stackFit_GLM <- h2o.stack( models = models, 
                           metalearner = "h2o.glm.wrapper", 
                           response_frame = train_h2o[, Namey],
                           seed = 1,
                           keep_levelone_data = TRUE)

perf_stack_GLM <- h2o.ensemble_performance(stackFit_GLM, newdata = eval_h2o)
perf_stack_GLM
# Base learner performance, sorted by specified metric:
#   learner       AUC
# 2   GLM_model_R_1460137708941_1 0.8046919
# 3 DRF_model_R_1460137708941_705 0.8228619
# 1  GBM_model_R_1460137708941_26 0.8387453
# 
# 
# H2O Ensemble Performance on <newdata>:
#   ----------------
#   Family: binomial
# 
# Ensemble performance (AUC): 0.840839636127387


# deep learning wrapper
stackFit_Deep <- h2o.stack( models = models, 
                            metalearner = "h2o.deeplearning.wrapper", 
                            response_frame = train_h2o[, Namey],
                            seed = 1,
                            keep_levelone_data = TRUE)

perf_stack_Deep <- h2o.ensemble_performance(stackFit_Deep, newdata = eval_h2o)
perf_stack_Deep
# Base learner performance, sorted by specified metric:
#   learner       AUC
# 2   GLM_model_R_1460137708941_1 0.8046919
# 3 DRF_model_R_1460137708941_705 0.8228619
# 1  GBM_model_R_1460137708941_26 0.8387453
# 
# 
# H2O Ensemble Performance on <newdata>:
#   ----------------
#   Family: binomial
# 
# Ensemble performance (AUC): 0.841346715859286


# **************************************
# Ensemble - Baseline with no tuning
# **************************************

# Specify the base learner library & the metalearner

# H20 GLM Wrapper as metalearner
learner <- c("h2o.deeplearning.wrapper",
             "h2o.glm.wrapper", 
             "h2o.randomForest.wrapper", 
             "h2o.gbm.wrapper")

#metalearner <- "h2o.deeplearning.wrapper"   
metalearner <- "h2o.glm.wrapper"  

family <- "binomial"

h2oFit_GLM <- h2o.ensemble(x = Namesx, y = Namey,   # can only use column names
                           training_frame = train_h2o, 
                           # add validation frame validation_frame = eval_h2o,
                           family = family, 
                           learner = learner, 
                           metalearner = metalearner,
                           cvControl = list(V = 5, shuffle = TRUE)) 

# Evaluate ensemble model performance
h2oFit_GLMperf <- h2o.ensemble_performance(h2oFit_GLM, newdata = eval_h2o )
h2oFit_GLMperf
# Base learner performance, sorted by specified metric:
#   learner       AUC
# 1 h2o.deeplearning.wrapper 0.7414957
# 2          h2o.glm.wrapper 0.7957895
# 3 h2o.randomForest.wrapper 0.8280383
# 4          h2o.gbm.wrapper 0.8402331
# 
# 
# H2O Ensemble Performance on <newdata>:
#   ----------------
#   Family: binomial
# 
# Ensemble performance (AUC): 0.841357074756171


# H20 Deep learning as metalearner
learner <- c("h2o.deeplearning.wrapper",
             "h2o.glm.wrapper", 
             "h2o.randomForest.wrapper", 
             "h2o.gbm.wrapper")

metalearner <- "h2o.deeplearning.wrapper"   
#metalearner <- "h2o.glm.wrapper"  

family <- "binomial"

h2oFit_Deep <- h2o.ensemble(x = Namesx, y = Namey,   # can only use column names
                            training_frame = train_h2o, 
                            # add validation frame validation_frame = eval_h2o,
                            family = family, 
                            learner = learner, 
                            metalearner = metalearner,
                            cvControl = list(V = 5, shuffle = TRUE)) 

# Evaluate ensemble model performance
h2oFit_Deepperf <- h2o.ensemble_performance(h2oFit_Deep, newdata = eval_h2o )
h2oFit_Deepperf
# Base learner performance, sorted by specified metric:
#   learner       AUC
# 1 h2o.deeplearning.wrapper 0.7195022
# 2          h2o.glm.wrapper 0.7957895
# 3 h2o.randomForest.wrapper 0.8288507
# 4          h2o.gbm.wrapper 0.8402347
# 
# 
# H2O Ensemble Performance on <newdata>:
#   ----------------
#   Family: binomial
# 
# Ensemble performance (AUC): 0.841453453241372


# **************************************
# Add Ensemble prediction to the training/testing sets
# **************************************

# Save predictions as a predictor in the train, and test set
eval_preds_stackDeep <- predict(stackFit_Deep, eval_h2o)   
train_preds_stackDeep <- predict(stackFit_Deep, train_h2o)
test_preds_stackDeep <- predict(stackFit_Deep, test_h2o)

eval_preds_stackGLM <- predict(stackFit_GLM, eval_h2o)   
train_preds_stackGLM <- predict(stackFit_GLM, train_h2o)
test_preds_stackGLM <- predict(stackFit_GLM, test_h2o)

eval_preds_h20Fit_GLM <- predict(h2oFit_GLM, eval_h2o)  
train_preds_h20Fit_GLM <- predict(h2oFit_GLM, train_h2o)
test_preds_h20Fit_GLM <- predict(h2oFit_GLM, test_h2o)

eval_preds_h20Fit_Deep <- predict(h2oFit_Deep, eval_h2o)
train_preds_h20Fit_Deep <- predict(h2oFit_Deep, train_h2o)
test_preds_h20Fit_Deep <- predict(h2oFit_Deep, test_h2o)

# probability of booking
# trainProb <- as.data.frame(train_preds$pred)[,2]
# evalProb <- as.data.frame(eval_preds$pred)[,2]
# testProb <- as.data.frame(test_preds$pred)[,2]

trainProbstackDeep <- as.data.frame(train_preds_stackDeep$pred)[,2]
evalProbstackDeep <- as.data.frame(eval_preds_stackDeep$pred)[,2]
testProbstackDeep <- as.data.frame(test_preds_stackDeep$pred)[,2]

trainProbstackGLM <- as.data.frame(train_preds_stackGLM$pred)[,2]
evalProbstackGLM <- as.data.frame(eval_preds_stackGLM$pred)[,2]
testProbstackGLM <- as.data.frame(test_preds_stackGLM$pred)[,2]

trainProbh20Fit_GLM <- as.data.frame(train_preds_h20Fit_GLM$pred)[,2]
evalProbh20Fit_GLM <- as.data.frame(eval_preds_h20Fit_GLM$pred)[,2]
testProbh20Fit_GLM <- as.data.frame(test_preds_h20Fit_GLM$pred)[,2]

trainProbh20Fit_Deep <- as.data.frame(train_preds_h20Fit_Deep$pred)[,2]
evalProbh20Fit_Deep <- as.data.frame(eval_preds_h20Fit_Deep$pred)[,2]
testProbh20Fit_Deep <- as.data.frame(test_preds_h20Fit_Deep$pred)[,2]

# add probability of booking to x_eval, x_train, test 
# X_train$bookingProb <- trainProb
# X_eval$bookingProb <- evalProb
# test$bookingProb <- testProb

X_train$bookingProbstackDeep <- trainProbstackDeep
X_eval$bookingProbstackDeep <- evalProbstackDeep
test$bookingProbstackDeep <- testProbstackDeep

X_train$bookingProbstackGLM <- trainProbstackGLM
X_eval$bookingProbstackGLM <- evalProbstackGLM
test$bookingProbstackGLM <- testProbstackGLM

X_train$bookingProbh20Fit_GLM <- trainProbh20Fit_GLM
X_eval$bookingProbh20Fit_GLM <- evalProbh20Fit_GLM
test$bookingProbh20Fit_GLM <- testProbh20Fit_GLM

X_train$bookingProbh20Fit_Deep <- trainProbh20Fit_Deep
X_eval$bookingProbh20Fit_Deep <- evalProbh20Fit_Deep
test$bookingProbh20Fit_Deep <- testProbh20Fit_Deep


# **************************************
# Save files
# **************************************
save(X_train, file = "data/X_train.rda")
save(X_eval, file = "data/X_eval.rda")
save(test, file = "data/test.rda")

# once shut down everything will be lost
h2o.shutdown()  


## *********************************
#  Performance Records
## *********************************

# H2O Ensemble Performance on <newdata>: (full set of predictors)
#   ----------------
#   Family: binomial
# 
# Ensemble performance (AUC): 0.841896157182617
# 
#             Base learner performance:
#             learner       AUC
# 1          h2o.glm.wrapper 0.7919295
# 2 h2o.randomForest.wrapper 0.8244363
# 3          h2o.gbm.wrapper 0.8399529
# 4 h2o.deeplearning.wrapper 0.7336420


# H2O Ensemble Performance on <newdata>:  (reduced predictors)
#   ----------------
#   Family: binomial
# 
# Ensemble performance (AUC): 0.841271055167855
# 
# Base learner performance:
#   learner       AUC
# 1          h2o.glm.wrapper 0.7962505
# 2 h2o.randomForest.wrapper 0.8270531
# 3          h2o.gbm.wrapper 0.8396922
# 4 h2o.deeplearning.wrapper 0.7390462

# H2O Ensemble Performance on <newdata>: (reduced predictors second run) 
#   ----------------
#   Family: binomial
# 
# Ensemble performance (AUC): 0.841524885181691
# 
# Base learner performance:
#   learner       AUC
# 1          h2o.glm.wrapper 0.7957840
# 2 h2o.randomForest.wrapper 0.8274030
# 3          h2o.gbm.wrapper 0.8402524
# 4 h2o.deeplearning.wrapper 0.7227024


# H2O Ensemble Performance on <newdata>: (reduced predictors 3rd run) 
#   ----------------
#   Family: binomial
# 
# Ensemble performance (AUC): 0.841270030390213
# 
# Base learner performance:
#   learner       AUC
# 1          h2o.glm.wrapper 0.7957858
# 2 h2o.randomForest.wrapper 0.8285065
# 3          h2o.gbm.wrapper 0.8402316

###############################################

# Base learner performance, sorted by specified metric:  (stack GLM)
#   learner       AUC
# 2   GLM_model_R_1460137708941_1 0.8046919
# 3 DRF_model_R_1460137708941_705 0.8228619
# 1  GBM_model_R_1460137708941_26 0.8387453
# 
# 
# H2O Ensemble Performance on <newdata>:
#   ----------------
#   Family: binomial
# 
# Ensemble performance (AUC): 0.840839636127387


# Base learner performance, sorted by specified metric: (stacked deep)
#   learner       AUC
# 2   GLM_model_R_1460137708941_1 0.8046919
# 3 DRF_model_R_1460137708941_705 0.8228619
# 1  GBM_model_R_1460137708941_26 0.8387453
# 
# 
# H2O Ensemble Performance on <newdata>:
#   ----------------
#   Family: binomial
# 
# Ensemble performance (AUC): 0.841346715859286


# Base learner performance, sorted by specified metric: (untuned GLM)
#   learner       AUC
# 1 h2o.deeplearning.wrapper 0.7414957
# 2          h2o.glm.wrapper 0.7957895
# 3 h2o.randomForest.wrapper 0.8280383
# 4          h2o.gbm.wrapper 0.8402331
# 
# 
# H2O Ensemble Performance on <newdata>:
#   ----------------
#   Family: binomial
# 
# Ensemble performance (AUC): 0.841357074756171

# Base learner performance, sorted by specified metric: (untuned deep)
#   learner       AUC
# 1 h2o.deeplearning.wrapper 0.7195022
# 2          h2o.glm.wrapper 0.7957895
# 3 h2o.randomForest.wrapper 0.8288507
# 4          h2o.gbm.wrapper 0.8402347
# 
# 
# H2O Ensemble Performance on <newdata>:
#   ----------------
#   Family: binomial
# 
# Ensemble performance (AUC): 0.841453453241372
