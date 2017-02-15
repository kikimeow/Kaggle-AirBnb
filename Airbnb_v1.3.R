gc()  # garbage collection

# install latest packages if default repo is incorrect
#install.packages('rmarkdown', repos='http://cran.us.r-project.org')

setwd("~/Class- R Class/Kaggle- AirBnB")
#setwd("~/Kaggle-AirBnb")

# load data
#load("Airbnb_Data_combined.RData")
#load("Airbnb_Data_raw.RData")
#load("~/Kaggle-AirBnb/Airbnb_Data_1.5.RData")
load("Airbnb_data_1.5.RData")

# list datasets
ls(all = TRUE)

# call libraries
x<-c("caret", "pROC")
#x<-c("caret", "lubridate", "Hmisc", "tabplot", "dplyr", "plyr", "pROC", "nnet")

lapply(x, FUN = function(X) {
  do.call("require", list(X)) 
})
rm(x)


#registerDoMC(8)Use for AWS only

cores <- 8

if(cores > 1) {
  library(doMC)
  registerDoMC(cores)
}


#################### Reading data and adding features ###################

# Read in Raw data - Do not have to re-load everytime
countries <- read.csv("countries.csv")
age_gender_bkts <- read.csv("~/Class- R Class/Kaggle- AirBnB/age_gender_bkts.csv")
sessions <- read.csv("sessions.csv")
test <- read.csv("test_users.csv")
train <- read.csv("train_users_2.csv")

save.image("C:/Users/moomi/Documents/Kaggle-AirBnb/Airbnb_Data.RData") # raw data- no formatting

# remove column not neede
train$date_first_booking <- NULL
test$date_first_booking <- NULL

# Format data: replace all "-unknown-" with unknown and "" with "blank"
train[] <- lapply(train, as.character)  # temporary drop as factors to replace value
train[train =="-unknown-"] <- "unknown"
train[train ==""] <- "blank"

test[] <- lapply(test, as.character)
test[test =="-unknown-"] <- "unknown"
test[test ==""] <- "blank"

# make function to substitute strings
recodeValues <- function(x)   
{
  x <- gsub("-", ".", x)
  x <- gsub("[,'\\(\\)]", "", x)
  x <- gsub(" ", ".", x)
  x <- gsub("\\(", "", x)
  x <- gsub("\\)", "", x)
  x <- gsub("/", ".", x) 
}

for (i in names(train)) train[, i] <- recodeValues(train[,i])
for (i in names(test)) test[, i] <- recodeValues(test[,i])
rm(i)

# convert date/time toPOSIXct 
train$timestamp_first_active <- ymd_hms(train$timestamp_first_active)
test$timestamp_first_active <- ymd_hms(test$timestamp_first_active)

train$date_account_created <- ymd(train$date_account_created)
test$date_account_created <- ymd(test$date_account_created)
#str(test$date_account_created)

# clean up data
# re-do age (Do not have to re-run this)
new_age_train <- as.integer(train$age)
fix <- c(1920:2010)
for (i in fix){
  new_age_train[new_age_train == i] = 2015-i
}

new_age_train[new_age_train < 15 ] = 0
new_age_train[new_age_train > 100] = 0
train$age <- new_age_train

new_age_test <- as.integer(test$age)
for (i in fix){
  new_age_test[new_age_test == i] = 2015-i
}
new_age_test[new_age_test < 15 ] = 0
new_age_test[new_age_test > 100] = 0
test$age <- new_age_test

#hist(test$age)
#summary(test$age)

rm(new_age_train)
rm(new_age_test)
rm(fix)
rm(i)

# combine the two dataset first
test$country_destination <- NA
train$set <- "train"
test$set <- "test"
alldata <- rbind(train,test)

# set back to factors
col_names <- names(train[,c(4,6:16)])  # do a str to see which columns to change to factor 
alldata[, col_names] <- lapply(alldata[, col_names], factor)

#str(alldata)
#describe(alldata)

rm(col_names)
rm(test)
rm(train)

# find which columns are regular factors or ordered factors
isOrdered <- unlist(lapply(alldata, is.ordered))
isFactored <- unlist(lapply(alldata, is.factor))
convertCols <- names(isOrdered)[isOrdered | isFactored] # |: logical OR
convertCols

# recode levels so the names are acceptable for model runs
recodeLevels <- function(x)   # make function to substitute strings
{
  x <- gsub("-", ".", x)
  x <- gsub("[,'\\(\\)]", "", x)
  x <- gsub(" ", ".", x)
  x <- gsub("\\(", "", x)
  x <- gsub("\\)", "", x)
  x <- gsub("/", ".", x) 
  factor(paste("", x, sep = "")) #  encode a vector as a factor. paste converts its arguments (via as.character) to character strings, and concatenates them (separating them by the string given by sep).
}

for (i in convertCols) alldata[, i] <- recodeLevels(alldata[,i])
#describe(alldata)
#str(alldata)
rm(convertCols)
rm(i)
rm(isOrdered)
rm(isFactored)
####
# add feature: signup_time = timestamp_first_active - date_account_created 
####
alldata$days_to_signup <- as.numeric(alldata$date_account_created- alldata$timestamp_first_active, units = "days")
alldata$days_to_signup[alldata$days_to_signup < 1 ] = 1

# Add features from the sessions dataset
sessions <- sessions[sessions$user_id != "",]  # remove observations with blank ID 

# count number of sessions per ID  (added - do not need to run all the time)
#library(plyr)
num_sessions <- count(sessions, c("user_id"))
names(num_sessions)[names(num_sessions) =="user_id"] <- "id"
names(num_sessions)[names(num_sessions) == "freq"] <- "num_sessions"
names(num_sessions)
alldata <- left_join(alldata, num_sessions, by = "id")
rm(num_sessions)

# count number of unique actions type per ID
# unique action_type
unique_action_type <- unique(sessions$action_type)
unique_action_type #11 levels

action_type_table <- table(sessions$user_id, sessions$action_type)
action_type_table <- as.data.frame.matrix(action_type_table)
#nzv <- nearZeroVar(action_type_table)  # remove all NZV at the last step together
#action_type_table <- action_type_table[, -nzv]
names(action_type_table)[names(action_type_table) =="V1"] <- "blank"
action_type_table <- cbind(id = rownames(action_type_table), action_type_table)  # rename row.name to id
rownames(action_type_table) <- NULL
colnames(action_type_table)[2:12] <- paste("action_type", colnames(action_type_table)[2:12], sep = "_")
names(action_type_table)[names(action_type_table)=="action_type_-unknown-"] <- "action_type_unknown"
names(action_type_table)

alldata <- left_join(alldata, action_type_table, by = "id")
rm(action_type_table)

# unique action
unique_actions <- unique(sessions$action)
unique_actions #360 levels

unique_action_table <- table(sessions$user_id, sessions$action)
unique_action_table <- as.data.frame.matrix(unique_action_table)
#nzv <- nearZeroVar(unique_action_table)
#nzv
#unique_action_table <- unique_action_table[, -nzv]


head(unique_action_table)
names(unique_action_table)[names(unique_action_table) =="V1"] <- "blank"
unique_action_table <- cbind(id = rownames(unique_action_table), unique_action_table)  # rename row.name to id
rownames(unique_action_table) <- NULL
colnames(unique_action_table)[2:361] <- paste("action", colnames(unique_action_table)[2:361], sep = "_")
names(unique_action_table)

alldata <- left_join(alldata, unique_action_table, by = "id")
names(alldata)


rm(unique_action_table)

# unique action_detail
unique_action_detail <- unique(sessions$action_detail)
unique_action_detail #156 levels

action_detail_table <- table(sessions$user_id, sessions$action_detail)
action_detail_table <- as.data.frame.matrix(action_detail_table)
#nzv <- nearZeroVar(action_detail_table)
#names(action_detail_table[-nzv])  # variables to keep
#names(action_detail_table[nzv]) # variables to discard

#action_detail_table <- action_detail_table[, -nzv]

head(action_detail_table)
names(action_detail_table)
names(action_detail_table)[names(action_detail_table) =="V1"] <- "blank"
action_detail_table <- cbind(id = rownames(action_detail_table), action_detail_table)  # rename row.name to id
rownames(action_detail_table) <- NULL
colnames(action_detail_table)[2:157] <- paste("action_detail", colnames(action_detail_table)[2:157], sep = "_")
names(action_detail_table)[names(action_detail_table) =="action_detail_-unknown-" ] <- "action_detail_unknown"
names(action_detail_table)

alldata <- left_join(alldata, action_detail_table, by = "id")
names(alldata)

rm(action_detail_table)

# look for total secs_elapsed
sec_elapsed <- aggregate(secs_elapsed ~ user_id, data=sessions, FUN=sum)
summary(sec_elapsed)
names(sec_elapsed)[names(sec_elapsed) =="user_id"] <- "id"
alldata <- left_join(alldata, sec_elapsed, by = "id")
rm(sec_elapsed)

# look for sessions with multiple device_type
num_device_type_used <- count(sessions, c("user_id", "device_type"))[1:2]
num_device_type_used <- count(num_device_type_used, c("user_id"))
summary(num_device_type_used)
names(num_device_type_used)[names(num_device_type_used) =="user_id"] <- "id"
names(num_device_type_used)[names(num_device_type_used) =="freq"] <- "num_device_used"
alldata <- left_join(alldata, num_device_type_used, by = "id")


# add marker as first pass of prediction:  One's didn't book (NDF) vs. Booked
alldata$booking <- alldata$country_destination
alldata$booking <- as.character(alldata$booking)
alldata$booking[alldata$booking == "NDF"] <- "No"  # ""Yes" for booking, "No" for no booking
alldata$booking[alldata$booking != "No"] <- "Yes"
alldata$booking <- as.factor(alldata$booking)   ## make levels
#table(alldata$booking)
#str(alldata$booking)


rm(unique_action_detail)
rm(unique_action_type)
rm(unique_actions)

#################### end of loading data and adding features ###################

############################################################################
# dealing with missing data
## 

#get the variables with missing data
#na_count <-sapply(alldata, function(y) sum(length(which(is.na(y)))))
Missing <- sapply(alldata, function(x) sum(is.na(x)))
var_with_missing_data <- Missing[Missing>0]
var_with_missing_data

#add a column called missing session data
alldata$missing_session <- alldata$num_sessions
notmissing <- !is.na(alldata$missing_session)
alldata$missing_session[notmissing] <- 0
missing <- is.na(alldata$missing_session)
alldata$missing_session[missing] <- 1
rm(missing)
rm(notmissing)

#table(alldata$missing_session, alldata$set)
#   test  train
#0  61668  73815
#1    428 139651

# num_device_used (if missing, then replace with 1)
missing <- is.na(alldata$num_device_used) #row, col
alldata$num_device_used[missing] <- 1
#alldata$num_device_used[missing]

# age (change NA to 0)
alldata$age <- as.numeric(alldata$age)
table(alldata$age, alldata$set, exclude=NULL) #both have lots of NA
missing <- is.na(alldata$age) #row, col
alldata$age[missing] <- 0

# since data with missing session information are stored in a variable, make the rest 0 so
# correlation can be plotted

M <- sapply(alldata, function(x) sum(is.na(x)))
var_with_missing_data <- M[M>0]
var_with_missing_data

# replace NA with 0 - action, action_type, action_detail
missing_data_cols <- names(var_with_missing_data)
#missing_data_cols
#which(names(alldata) == "booking") #91
#which(names(alldata) == "country_destination") #15
#addback_1 <- alldata[,which(names(alldata) == "booking")]
#addback_2 <- alldata[,which(names(alldata) == "country_destination") ]

for (i in missing_data_cols){
  missing <- is.na(alldata[,i])
  alldata[missing, i] <- 0
}

# double check data structure
str(alldata)
str(alldata$country_destination)

rm(M)
rm(i)
rm(var_with_missing_data)
rm(missing_data_cols)
rm(missing)
rm(num_device_type_used)


###################################################################
##    Create Dummy Variables
#####

# create dummy variables for factor variables only (exclude set, and country_destination)
factorVars <- names(alldata[,sapply(alldata,is.factor)])
factorVars <- factorVars[!is.element(factorVars, c("set","country_destination"))]

dummy_data = alldata[,factorVars]
dummies <- dummyVars(booking ~ ., data = dummy_data)
data_dummy <- as.data.frame(predict(dummies, newdata = dummy_data))

# combine existing data with data_dummy - exclude factorVars used to create dummy
booking <- alldata$booking  # add back booking
alldata_dummy <-  alldata[, !(colnames(alldata) %in% factorVars)]
alldata_dummy <- cbind(alldata_dummy, data_dummy, booking)

rm(dummy_data)
rm(booking)


######################################################################
### Create two character vectors for different predictor sets. One
### will have all the predictors (called 'fullSet').
##
### Another has some of the sparse predictors removed for models that
### require such filtering. This will be called 'reducedSet'
### (predictors without sparse or Near Zero Variance predictors). This
### set will also have predictors removed that are almost completely
### correlated with other predictors


## A function to find and remove zero-variance ("ZV") predictors
# Usage: investDOB <- noZV(investDOB)

noZV <- function(x) {
  keepers <- unlist(lapply(x, function(x) length(unique(x)) > 1))
  x[,keepers,drop = FALSE]
}



#############
#  Split Data

# split back into test(for submission) and train.  train is further broken down into training and evluation
trainingFactor <- alldata[which(alldata$set == "train"),]
trainingFactor <- trainingFactor[which(trainingFactor$missing_session == 0),]
testingFactor <- alldata[which(alldata$set == "test"),]

trainingDummy <- alldata_dummy[which(alldata_dummy$set == "train"),]
trainingDummy <- trainingDummy[which(trainingDummy$missing_session == 0),]
testingDummy <- alldata_dummy[which(alldata_dummy$set == "test"),]


# no Zero-Variance
trainingFactor <- noZV(trainingFactor)
testingFactor <- testingFactor[, names(trainingFactor)]

trainingDummy <- noZV(trainingDummy)
testingDummy <- testingDummy[, names(trainingDummy)]


### Some are extremely correlated, so remove
num <- trainingFactor[,sapply(trainingFactor,is.numeric)]  # find numeric columns only for correlation calc
str(num)
descrCor <-  cor(num)
#descrCor
#highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)
#summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .99)
highlyCorDescr
highCor_names <- names(num[,highlyCorDescr])  # list of highly correlated descriptors

## Data with Factor Variables ##
# remove high correlated names from fullSet of predictors  
fullSet <- trainingFactor[ , -which(names(trainingFactor) %in% highCor_names)]
fullSet <- names(fullSet)[!(names(fullSet) %in% c("country_destination", "id", "date_account_created", "timestamp_first_active", "set", "missing_session", "booking"))]
isNZV <- nearZeroVar(trainingFactor[,fullSet], saveMetrics = TRUE, freqCut = floor(nrow(trainingFactor)/5))
fullSet <-  rownames(subset(isNZV, !nzv))  # 331 variables

# reduced set is even smaller subset of predictors
reducedSet <- rownames(subset(isNZV, !nzv & freqRatio < floor(nrow(trainingFactor)/50))) # 254 variables

## Data with Dummy Variables ##
# remove high correlated names from fullSet of predictors  
fullSetDummy <- trainingDummy[ , -which(names(trainingDummy) %in% highCor_names)]
fullSetDummy <- names(fullSetDummy)[!(names(fullSetDummy) %in% c("country_destination", "id", "date_account_created", "timestamp_first_active", "set", "missing_session", "booking"))]
isNZV <- nearZeroVar(trainingDummy[,fullSetDummy], saveMetrics = TRUE, freqCut = floor(nrow(trainingDummy)/5))
fullSetDummy <-  rownames(subset(isNZV, !nzv))  # 421 variables

# reduced set is even smaller subset of predictors
reducedSetDummy <- rownames(subset(isNZV, !nzv & freqRatio < floor(nrow(trainingDummy)/50))) # 312 variables

# splitting data into training and evaluation set
set.seed(100)
split <- createDataPartition(trainingFactor$booking, p = .75)[[1]]
training_trainFactor <- trainingFactor[split,]
training_evaluationFactor  <- trainingFactor[ -split,]
training_trainDummy <- trainingDummy[split,]
training_evaluationDummy <- trainingDummy[-split,]

##################################################
##  Model Fitting 
#######################################

## This control object will be used across multiple models so that the
## data splitting is consistent

ctrl <- trainControl(method = "CV",
                     number = 10,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     savePredictions = "final",
                     allowParallel = TRUE)


ctrlTest <- trainControl(#method = "CV",
                         #number = 2,
                         summaryFunction = twoClassSummary,
                         classProbs = TRUE,
                         savePredictions = "final")

###
# Linear Models
###

## Linear Discriminant Analysis: Fit the model to the reduced set
set.seed(100)
ldaFit <- train(x = training_trainDummy[,reducedSetDummy], 
                y = training_trainDummy$booking,
                method = "lda",
                preProc = c("center","scale"),
                metric = "ROC",
                trControl = ctrl)
ldaFit
save(ldaFit, file = "ldaFit.rda")
ldaFit$times$everything #241.08 


#ldaCM <- confusionMatrix(ldaFit, norm = "none")
ldaCM <- confusionMatrix((predict(ldaFit, training_evaluationDummy[, reducedSetDummy])), training_evaluationDummy$booking)
ldaCM  # Accuracy : 0.7535, Kappa : 0.4606 



## Partial Least Squares Discriminant Analysis: All predictors

set.seed(100)
plsFit <- train(x = training_trainDummy[,fullSetDummy], 
                y = training_trainDummy$booking,
                method = "pls",
                tuneGrid = expand.grid(ncomp = 1:10),
                preProc = c("center","scale"),
                metric = "ROC",
                probMethod = "Bayes",
                trControl = ctrl)

plsFit
plsCM <- confusionMatrix((predict(plsFit, training_evaluationDummy[, fullSetDummy])), training_evaluationDummy$booking) 
plsCM # Accuracy : 0.7542
save(plsFit, file = "plsFit.rda")

## Partial Least Squares Discriminant Analysis: Reduced predictors
set.seed(100)
plsFit_reduced <- train(x = training_trainDummy[,reducedSetDummy], 
                        y = training_trainDummy$booking,
                        method = "pls",
                        tuneGrid = expand.grid(ncomp = 1:10),
                        preProc = c("center","scale"),
                        metric = "ROC",
                        probMethod = "Bayes",
                        trControl = ctrl)
pls_reducedCM <- confusionMatrix((predict(plsFit_reduced, training_evaluationDummy[, reducedSetDummy])), training_evaluationDummy$booking)
pls_reducedCM # Accuracy : 0.7549
save(plsFit_reduced, file = "plsFit_reduced.rda")

###
# Penalized Models
###

# The glmnet model
glmnGrid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),
                        lambda = seq(.01, .2, length = 40))

set.seed(100)
glmnFit <- train(x = as.matrix(training_trainDummy[, fullSetDummy]), 
                 y = training_trainDummy$booking,
                 method = "glmnet",
                 tuneGrid = glmnGrid,
                 preProc = c("center", "scale"),
                 metric = "ROC",
                 trControl = ctrl)

glmnFit
glmFit_pred <- predict(glmnFit, training_evaluationDummy[, fullSetDummy])
glmCM <- confusionMatrix(glmFit_pred, training_evaluationDummy$booking)
glmCM # Accuracy : 0.7517
save(glmnFit, file = "glmnFit.rda")


library(glmnet)
glmnetMode <- glmnet( x = as.matrix(training_trainDummy[, fullSetDummy]),
                      y = training_trainDummy$booking,
                      family = "binomial")

glmnet_pred <- predict(glmnetMode,
                       newx = as.matrix(training_evaluationDummy[, fullSetDummy]),
                       s = c(0.05, 0.1, 0.2), # 3 levels of regularization
                       type = "class")

head(glmnet_pred)
names(glmnet_pred)
glmnet_pred <- as.data.frame(glmnet_pred)
glmentCM <- confusionMatrix(glmnet_pred[,3], training_evaluationDummy$booking)
glmentCM #Accuracy : 0.7032,  0.6811, 0.6102 

## Sparse logistic regression 
# Error: (Error in names(resamples) <- gsub("^\\.", "", names(resamples)) : attempt to set an attribute on NULL)
set.seed(100)
spLDAFit <- train(x = training_trainDummy[,fullSetDummy], 
                  y = training_trainDummy$booking,
                  method = "sparseLDA",
                  tuneGrid = expand.grid(lambda = c(.1),
                                         NumVars = c(1:20, 50, 75, 100, 250, 500)),
                  preProc = c("center", "scale"),
                  metric = "ROC",
                  trControl = ctrl)
spLDAFit

library(sparseLDA) # Error in t.default(La.res$vt) : argument is not a matrix
sparseLDA <- sda( x = as.matrix(training_trainDummy[, fullSetDummy]),
                  y = training_trainDummy$booking,
                  lambda = 0.01,
                  stop = -6  )


###
# Non linear classification
###

# Mixture Discriminant Analysis
# Error: argument is not a matrix
set.seed(100)
mdaFit <- train(x = as.matrix(training_trainDummy[1:1000,reducedSetDummy]), 
                y = training_trainDummy$booking[1:1000],
                method = "mda",
                metric = "ROC",
                tries = 40,
                #tuneGrid = expand.grid(subclasses = 1:8),
                trControl = ctrlTest)

save(mdaFit, file = "mdaFit.rda")
mdaFit

# Neural Networks
#nnetGrid <- expand.grid(size = 1:10, decay = c(0, .1, 1, 2))
nnetGrid <- expand.grid(size = c(3, 6, 9), decay = c(.1, 1, 2))
maxSize <- max(nnetGrid$size)

set.seed(100)
nnetFit <- train(x = training_trainDummy[,reducedSetDummy], 
                 y = training_trainDummy$booking,
                 method = "nnet",
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 tuneGrid = nnetGrid,
                 trace = FALSE,
                 maxit = 2000,
                 MaxNWts = 1*(maxSize * (length(reducedSetDummy) + 1) + maxSize + 1),
                 trControl = ctrlTest)

nnetFit  #The final values used for the model were size = 3 and decay = 2.
save(nnetFit, file = "nnetFit.rda")
nnetFit$times$everything #elapsesd: 419685.88  (4.857475 days)

nnetFitCM <- confusionMatrix((predict(nnetFit, training_evaluationDummy[, reducedSetDummy])), training_evaluationDummy$booking)
nnetFitCM   #Accuracy : 0.7706, Kappa : 0.5126  


set.seed(100)
nnetFit2 <- train(x = training_trainDummy[,reducedSetDummy], 
                  y = training_trainDummy$booking,
                  method = "nnet",
                  metric = "ROC",
                  preProc = c("center", "scale", "spatialSign"),
                  tuneGrid = nnetGrid,
                  trace = FALSE,
                  maxit = 2000,
                  MaxNWts = 1*(maxSize * (length(reducedSetDummy) + 1) + maxSize + 1),
                  trControl = ctrl)
nnetFit2
save(nnetFit2, file = "nnetFit2.rda")
nnetFit2CM <- confusionMatrix((predict(nnetFit2, training_evaluationDummy[, reducedSetDummy])), training_evaluationDummy$booking)
nnetFit2CM 

nnetGrid$bag <- FALSE
set.seed(100)
nnetFit3 <- train(x = training_trainDummy[1:100,reducedSetDummy],
                  y = training_trainDummy$booking[1:100],
                  method = "avNNet",
                  metric = "ROC",
                  preProc = c("center", "scale"),
                  tuneGrid = nnetGrid,
                  repeats = 10,
                  trace = FALSE,
                  maxit = 2000,
                  MaxNWts = 10*(maxSize * (length(reducedSetDummy) + 1) + maxSize + 1),
                  allowParallel = FALSE, ## this will cause to many workers to be launched.
                  trControl = ctrl)
nnetFit3
save(nnetFit3, file = "nnetFit3.rda")
nnetFit3CM <- confusionMatrix((predict(nnetFit3, training_evaluationDummy[, reducedSetDummy])), training_evaluationDummy$booking)
nnetFit3CM 

set.seed(100)
nnetFit4 <- train(x = training_trainDummy[,reducedSetDummy],
                  y = training_trainDummy$booking,
                  method = "avNNet",
                  metric = "ROC",
                  preProc = c("center", "scale", "spatialSign"),
                  tuneGrid = nnetGrid,
                  trace = FALSE,
                  maxit = 2000,
                  repeats = 10,
                  MaxNWts = 10*(maxSize * (length(reducedSetDummy) + 1) + maxSize + 1),
                  allowParallel = FALSE, 
                  trControl = ctrl)
nnetFit4
save(nnetFit4, file = "nnetFit4.rda")
nnetFit4CM <- confusionMatrix((predict(nnetFit4, training_evaluationDummy[, reducedSetDummy])), training_evaluationDummy$booking)
nnetFit4CM 


## Flexible Discriminant Analysis
set.seed(100)
fdaFit <- train(x = training_trainDummy[,reducedSetDummy],
                  y = training_trainDummy$booking,
                  method = "fda",
                  metric = "ROC",
                  tuneGrid = expand.grid(degree = 1, nprune = 2:25),
                  trControl = ctrl)
fdaFit
save(fdaFit, file = "fdaFit.rda")
# The final values used were degree = 1, nprune = 8

fdaCM <- confusionMatrix((predict(fdaFit, training_evaluationDummy[, reducedSetDummy])), training_evaluationDummy$booking)
fdaCM # accuracy : 0.6762, kappa: 0.2499  (reasonable kappa around 0.3-0.5)

## KNN
set.seed(100)
knnFit <- train(x = training_trainDummy[,reducedSetDummy],
                y = training_trainDummy$booking,
                method = "knn",
                metric = "ROC",
                preProc = c("center", "scale"),
                tuneGrid = data.frame(k = c(4*(0:5)+1,20*(1:5)+1,50*(2:9)+1)),
                trControl = ctrl)
knnFit
save(knnFit, file = "knnFit.rda")




#  Naive Bayes
set.seed(100)
nBayesFit <- train(x = training_trainFactor[,fullSet],
                   y = training_trainFactor$booking,
                   method = "nb",
                   metric = "ROC",
                   tuneGrid = data.frame(usekernel = c(TRUE, FALSE), fL = 2),
                   trControl = ctrl)
nBayesFit
save(nBayesFit, file ="nBayesFit.rda")

nBayesCM <- confusionMatrix((predict(nBayesFit, training_evaluationFactor[, fullSet])), training_evaluationFactor$booking)
nBayesCM # accuracy : 0.6165, kappa:  0.0219 (reasonable kappa around 0.3-0.5)


# Support Vector Machines

library(kernlab)

set.seed(100)
sigmaRangeFull <- sigest(as.matrix(training_trainDummy[,reducedSetDummy]))
svmRGridFull <- expand.grid(sigma =  as.vector(sigmaRangeFull)[1],
                            C = 2^(-3:4))
set.seed(100)
svmRFitReduced <- train(x = training_trainDummy[,reducedSetDummy], 
                     y = training_trainDummy$booking,
                     method = "svmRadial",
                     metric = "ROC",
                     preProc = c("center", "scale"),
                     tuneGrid = svmRGridFull,
                     trControl = ctrl)
svmRFitReduced
save(svmRFitReduced, file =" svmRFitReduced.rda")

svmRFitReduced$times$everything  # elapsed 379850.17,  4.396414 days


# Tuning parameter 'sigma' was held constant at a value of 0.0007664126
# ROC was used to select the optimal model using  the largest value.
# The final values used for the model were sigma = 0.0007664126 and C = 8. 

svmRFitReducedCM <- confusionMatrix((predict(svmRFitReduced, training_evaluationDummy[, reducedSetDummy])), training_evaluationDummy$booking)
svmRFitReducedCM # accuracy: Error , kappa: Error   (reasonable kappa around 0.3-0.5)


# Error in binom.test(sum(diag(x)), sum(x)) : 
#   'n' must be a positive integer >= 'x'
# In addition: Warning message:
#   In method$predict(modelFit = modelFit, newdata = newdata, submodels = param) :
#   kernlab class prediction calculations failed; returning NAs


####  Basic Classification Trees

# rpart
set.seed(100)
rpartFit <- train(x = training_trainDummy[,reducedSetDummy], 
                  y = training_trainDummy$booking,
                  method = "rpart",
                  tuneLength = 30,
                  metric = "ROC",
                  trControl = ctrl)
rpartFit  #The final value used for the model was cp = 0.0001853482.
save(rpartFit, file = "rpartFit.rda")
rpartFit$times$everything #272.34 

rpartFitCM <- confusionMatrix((predict(rpartFit, training_evaluationDummy[, reducedSetDummy])), training_evaluationDummy$booking)
rpartFitCM # accuracy : 0.772, kappa: 0.5121  (reasonable kappa around 0.3-0.5)

set.seed(100)
rpartFactorFit <- train(x = training_trainFactor[,fullSet], 
                        y = training_trainFactor$booking,
                        method = "rpart",
                        tuneLength = 30,
                        metric = "ROC",
                        trControl = ctrl)
rpartFactorFit
save(rpartFactorFit, file = "rpartFactorFit.rda")
rpartFactorFit$times$everything #284.74
rpartFactorFitCM <- confusionMatrix((predict(rpartFactorFit, training_evaluationFactor[, fullSet])), training_evaluationFactor$booking)
rpartFactorFitCM # accuracy : 0.7719, kappa: 0.5103  (reasonable kappa around 0.3-0.5)


## J48
set.seed(100)  # failed.. no RWeka.. java installion
j48FactorFit <- train(x = training_trainFactor[,fullSet],
                      y = training_trainFactor$booking,
                      method = "J48",
                      metric = "ROC",
                      trControl = ctrl)
j48FactorFit
save(j48FactorFit, file = "j48FactorFit.rda")
j48FactorFit$times$everything #1908.98
j48FactorFitCM <- confusionMatrix((predict(j48FactorFit, training_evaluationFactor[, fullSet])), training_evaluationFactor$booking)
j48FactorFitCM # accuracy: 0.7478  , kappa: 0.4607   (reasonable kappa around 0.3-0.5)

set.seed(100) # errors 
j48Fit <- train(x = training_trainDummy[,fullSetDummy], 
                y = training_trainDummy$booking,
                method = "J48",
                metric = "ROC",
                trControl = ctrl)
save(j48Fit, file = "j48FactorFit.rda")
j48Fit$times$everything #
j48FitCM <- confusionMatrix((predict(j48Fit, training_evaluationFactor[, fullSet])), training_evaluationFactor$booking)
j48FitCM # accuracy : , kappa:   (reasonable kappa around 0.3-0.5)

# 6: In eval(expr, envir, enclos) :
#   model fit failed for Fold09: C=0.25 Error in .jarray(x) : java.lang.OutOfMemoryError: Java heap space
# 
# 7: In nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,  :
#                              There were missing values in resampled performance measures.

#### Rule-Based Models
set.seed(100)
partFit <- train(x = training_trainDummy[,fullSetDummy], 
                 y = training_trainDummy$booking,
                 method = "PART",
                 metric = "ROC",
                 trControl = ctrl)
partFit
#Tuning parameter 'threshold' was held constant at a value of 0.25
#Tuning parameter 'pruned' was held constant at a value of yes
save(partFit, file = "partFit.rda")
partFit$times$everything #64501.28, 17 hrs
partFitCM <- confusionMatrix((predict(partFit, training_evaluationDummy[, fullSetDummy])), training_evaluationDummy$booking)
partFitCM # accuracy 0.7082 : , kappa:  0.3838  (reasonable kappa around 0.3-0.5)


# tree bag  # cannot run due to memory maximum reached
set.seed(100)
treebagFit <- train(x = training_trainDummy[,reducedSetDummy],  
                    y = training_trainDummy$booking,
                    method = "treebag",
                    nbagg = 50,
                    metric = "ROC",
                    trControl = ctrl)
treebagFit
save(treebagFit, file = "treebagFit.rda")


# Random Forest
mtryValues <- c(10, 100, 250, 500, 1000)
set.seed(100)
rfFit <- train(x = training_trainDummy[,fullSetDummy], 
               y = training_trainDummy$booking,
               method = "rf",
               ntree = 500,
               tuneGrid = data.frame(mtry = mtryValues),
               importance = TRUE,
               metric = "ROC",
               trControl = ctrl)
rfFit

#ROC was used to select the optimal model using  the largest value.
#The final value used for the model was mtry = 100. 

save(rfFit, file = "rfFit.rda")
rfFit$times$everything   # 7.125 days
rm(rfFit)
load("rfFit.rda")
rfFitCM <- confusionMatrix((predict(rfFit, training_evaluationDummy[, fullSetDummy])), training_evaluationDummy$booking)
rfFitCM # accuracy: 0.7728, kappa:0.5164   (reasonable kappa around 0.3-0.5)
varImp(rfFit)
caret:::getTrainPerf(rfFit)


# XGBoost
cv.ctrl <- trainControl(method = "CV", 
                        number = 10, 
                        #method = "repeatedCV", repeats = 1, number = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE,
                        savePredictions = "final",
                        allowParallel=TRUE)

xgb.grid <- expand.grid(nrounds = c(1000),  # number of trees
                        eta = c(0.01),  # learning rate
                        max_depth = c(6),
                        gamma = 0,
                        colsample_bytree = c(0.5),  # .3 - .5
                        min_child_weight = c(1.58))  # 1/ sqrt(event rate)


# if over fitting observed, reduce step size "eta" and increase nround at the same time


#The final values used for the model were nrounds = 1000, max_depth = 6, eta = 0.01, gamma = 0, colsample_bytree = 0.5 and min_child_weight = 1.58.
# tuning reference: http://www.slideshare.net/odsc/owen-zhangopen-sourcetoolsanddscompetitions1
# xgb.grid <- expand.grid(nrounds = c(100,1000),  # number of trees
#                         eta = c(0.01, 0.05, 0.1),  # learning rate
#                         max_depth = c(6, 100),
#                         gamma = 0,
#                         colsample_bytree = c(0.3, 0.5),  # .3 - .5
#                         min_child_weight = c(1.58, 3)  # 1/ sqrt(event rate)
# )


set.seed(100)
xgbFit <- train(x = training_trainDummy[,fullSetDummy],
               y = training_trainDummy$booking,
               method = "xgbTree",
               trControl=cv.ctrl,
               tuneGrid=xgb.grid,
               verbose=T,
               metric="Kappa"
               #metric = "ROC",
               nthread =3)
xgbFit

save(xgbFit, file = "xgbFit.rda")
xgbFit$times$everything # 2.584306 hr
#The final values used for the model were nrounds = 1000, max_depth = 6, eta = 0.01, gamma = 0, colsample_bytree = 0.5 and min_child_weight = 1.58.
xgbFitCM <- confusionMatrix((predict(xgbFit, training_evaluationDummy[, fullSetDummy])), training_evaluationDummy$booking)
xgbFitCM # accuracy: 0.7817 , kappa:0.5353  (reasonable kappa around 0.3-0.5)





############ 
# submission
############

# look at exisitng data 
# top 5 (NDF, US, other, FR, IT, GB, ES)
sort(table(trainingDummy$country_destination)/ nrow(trainingDummy) *100)


#  prepare testing set 
testingDummy <- testingDummy[, names(trainingDummy)]
names(testingDummy)

# RF prediction
load("rfFit.rda")
rfPrediction <- predict(rfFit, testingDummy[, fullSetDummy])
head(rfPrediction)  # Levels: No (NDF) Yes (US)

# NNET prediction
load("nnetFit.rda")
nnetPrediction <- predict(nnetFit, test_preds)
head(nnetPrediction)  # Levels: No (NDF) Yes (US)


## Rpart prediction
load("rpartFit.rda")
rpartPrediction <- predict(rpartFit, testingDummy[, reducedSetDummy])
head(rpartPrediction)  # Levels: No (NDF) Yes (US)


## XGBoost prediction
load("xgbFit.rda")
xgbPrediction <- predict(xgbFit, testingDummy[, fullSetDummy])
head(xgbPrediction)  # Levels: No (NDF) Yes (US)

# probability (for ensemble)
rfProbability <- predict(rfFit, testingDummy[, fullSetDummy], type =  "prob" )
nnetProbability <- predict(nnetFit, testingDummy[, reducedSetDummy], type = "prob")
rpartProbability <- predict(rpartFit, testingDummy[, reducedSetDummy], type = "prob")
xgbProbability <- predict(xgbFit, testingDummy[, fullSetDummy], type = "prob")
head(rfProbability)
head(nnetProbability)
head(rpartProbability)
head(xgbProbability)

modelAvg <- cbind(rfProbability$No, nnetProbability$No, rpartProbability$No, xgbProbability$No)
modelAvg <- data.frame(modelAvg)
names(modelAvg)[1] <- "rf"
names(modelAvg)[2] <- "nnet"
names(modelAvg)[3] <- "rpart"
names(modelAvg)[4] <- "xgboost"
modelAvg$average <- rowMeans(subset(modelAvg, select = c("rf", "nnet", "rpart", "xgboost")), na.rm = TRUE)
modelAvg$prediction <- ifelse(modelAvg$average >=0.5, "NDF", "US")
head(modelAvg)


# submission format (id,country)  - re-use code for each submission
submission <- read.csv("submission.csv", header = TRUE)
head(submission)
submission$id <- NULL
submission$country <- NULL
submission$id <- testingDummy$id
#submission$country <- rfPrediction
#submission$country <- nnetPrediction
#submission$country <- rpartPrediction
#submission$country <- xgbPrediction
submission$country <-  modelAvg$prediction

# Rename the levels of "No, Yes" by name
levels(submission$country)[levels(submission$country)=="No"] <- "NDF"
levels(submission$country)[levels(submission$country)=="Yes"] <- "US"

submissionNDF <- submission[which(submission$country == "NDF"),]
submissionUS <- submission[which(submission$country == "US"),]

# top 5 (NDF, US, other, FR, IT, GB, ES)
#NDF Set <- NDF, US, other, FR, IT
NDF_US <- submissionNDF
NDF_US$id <- submissionNDF$id
NDF_US$country <- "US"

NDF_other <- submissionNDF
NDF_other$id <- submissionNDF$id
NDF_other$country <- "other"

NDF_FR <- submissionNDF
NDF_FR$id <- submissionNDF$id
NDF_FR$country <- "FR"

NDF_IT <- submissionNDF
NDF_IT$id <- submissionNDF$id
NDF_IT$country <- "IT"

#US Set <- US, NDF, other, FR, IT
US_NDF <- submissionUS
US_NDF$id <- submissionUS$id
US_NDF$country <- "NDF"

US_other <- submissionUS
US_other$id <- submissionUS$id
US_other$country <- "other"

US_FR <- submissionUS
US_FR$id <- submissionUS$id
US_FR$country <- "FR"

US_IT <- submissionUS
US_IT$id <- submissionUS$id
US_IT$country <- "IT"

# submission output
submissionFinal <- rbind(submission, NDF_US, NDF_other, NDF_FR, NDF_IT, US_NDF, US_other, US_FR, US_IT)

#write.csv(submissionFinal, "submission_rfFit.csv", quote=FALSE, row.names = FALSE) # Kaggle: 0.87618
#write.csv(submissionFinal, "submission_nnetFit.csv", quote=FALSE, row.names = FALSE) # Kaggle: 0.87360
#write.csv(submissionFinal, "submission_rpartFit.csv", quote=FALSE, row.names = FALSE) # Kaggle: 0.87592
#write.csv(submissionFinal, "submission_xgbFit.csv", quote=FALSE, row.names = FALSE) # Kaggle: 0.88364
#write.csv(submissionFinal, "submission_avg_rf_nnet_rpart.csv", quote=FALSE, row.names = FALSE) # Kaggle: 0.87737
write.csv(submissionFinal, "submission_avg_rf_nnet_rpart_xgb.csv", quote=FALSE, row.names = FALSE) # Kaggle: 0.88313

##############
# multi-class training
##############

# train a smaller subset (no NDF)
trainingNoNDF <- trainingDummy[trainingDummy$booking == "Yes",]
trainingNoNDF$country_destination <- factor(trainingNoNDF$country_destination)  # drop unused factor levels (NA, NDF)
names(trainingNoNDF)
levels(trainingNoNDF$country_destination)
round(table(trainingNoNDF$country_destination)/nrow(trainingNoNDF)*100,2)
str(trainingNoNDF$country_destination)
levels(trainingNoNDF$country_destination)
# 1:"AU" 2:"CA" 3:"DE" 4:"ES" 5:"FR" 6:"GB" 7:"IT" 8:"NL"  9:"other" 10:"PT" 11:"US"   

#library(car)
#trainingNoNDF$country_destination <- recode(trainingNoNDF$country_destination,"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")

#trainingNoNDF$country_destination <- as.numeric(trainingNoNDF$country_destination)
table(trainingNoNDF$country_destination)

# create a upSample training set
library(caret)
trainingUpSample <- upSample(trainingNoNDF, trainingNoNDF$country_destination , list = FALSE, yname = "Class")
trainingUpSample$country_destination <- factor(trainingUpSample$country_destination)  # drop unused factor levels (NA, NDF)
trainingUpSample$Class <- NULL
table(trainingUpSample$country_destination)


## xgbFit multiclass
cv.ctrl <- trainControl(
  method = "repeatedCV", repeats = 1, number = 3,
  classProbs = TRUE,
  savePredictions = "final",
  allowParallel=TRUE)

xgbMultiFit <- train(x = trainingNoNDF[,fullSetDummy],
                     y =trainingNoNDF$country_destination,
                     method = "xgbTree",
                     trControl=cv.ctrl,
                     tuneGrid=xgb.grid,
                     verbose=T,
                     metric="Kappa",
                     nthread =3)
xgbMultiFit

# min_child_weight  Accuracy   Kappa       Accuracy SD   Kappa SD    
# 0.50              0.6987211  0.01089670  0.0004537622  0.0004910653
# 1.00              0.6985473  0.01036969  0.0005628442  0.0010273574
# 1.58              0.6989643  0.01118722  0.0005320956  0.0011983176





# C50
ctrl_multi <- trainControl(method = "CV",
                     number = 10,
                     classProbs = TRUE,
                     savePredictions = "final")


c50Grid <- expand.grid(trials = c(1:9, (1:10)*10),
                       model = c("tree", "rules"),
                       winnow = c(TRUE, FALSE))
set.seed(100)
c50Fit <- train(trainingNoNDF[,fullSetDummy], trainingNoNDF$country_destination,
                method = "C5.0",
                tuneGrid = c50Grid,
                metric = "ROC",
                verbose = FALSE,
                trControl = ctrl_multi)
c50Fit
save(c50Fit, file = "c50Fit.rda")
c50Predict <- predict(c50Fit,trainingNoNDF[,fullSetDummy] )
table(c50Predict,trainingNoNDF$country_destination )

#The final values used for the model were trials = 1, model = rules and winnow = TRUE
# accuracy: 0.6976440 kappa:0.014300332

###
##  C5.0 Up sample predictions
###

ctrl_multi_upsample <- trainControl(
                                    classProbs = TRUE,
                                    savePredictions = "final")


#c50Grid2 <- expand.grid(trials = c(1,(1:3)*3,(1:5)*20),
#                       model = c("rules"),
#                       winnow = c(TRUE))


c50Grid2 <- expand.grid(trials = 50,
                        model = c("rules"),
                        winnow = c(TRUE))


c50Grid2
c50UpSampleFit2 <- train(trainingNoNDF[,fullSetDummy], trainingNoNDF$country_destination,
                method = "C5.0",
                tuneGrid = c50Grid2,
                metric = "ROC",
                verbose = FALSE,
                trControl = ctrl_multi_upsample)

c50UpSampleFit2
save(c50UpSampleFit2, file = "c50UpSampleFit2.rda")


# > c50UpSampleFit
# C5.0 
# 
# 28774 samples
# 421 predictor
# 11 classes: 'AU', 'CA', 'DE', 'ES', 'FR', 'GB', 'IT', 'NL', 'other', 'PT', 'US' 
# 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 28774, 28774, 28774, 28774, 28774, 28774, ... 
# Resampling results
# 
# Accuracy   Kappa       Accuracy SD  Kappa SD   
# 0.6047118  0.02956567  0.007019887  0.007321655
# 
# Tuning parameter 'trials' was held constant at a value of 1
# Tuning parameter 'model' was held constant at a value of rules
# Tuning parameter 'winnow' was held constant at a value
# of TRUE

# > c50UpSampleFit2
# C5.0 
# 
# 28774 samples
# 421 predictor
# 11 classes: 'AU', 'CA', 'DE', 'ES', 'FR', 'GB', 'IT', 'NL', 'other', 'PT', 'US' 
# 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 28774, 28774, 28774, 28774, 28774, 28774, ... 
# Resampling results
# 
# Accuracy  Kappa        Accuracy SD  Kappa SD   
# 0.693452  0.007593709  0.002470687  0.002391798
# 
# Tuning parameter 'trials' was held constant at a value of 50
# Tuning parameter 'model' was held constant at a value of rules
# Tuning parameter 'winnow' was held constant at a value
# of TRUE


c50UpSamplePredict <- predict(c50UpSampleFit,trainingNoNDF[,fullSetDummy] )
table(c50UpSamplePredict,trainingNoNDF$country_destination )


c50UpSamplePredict2 <- predict(c50UpSampleFit2,trainingNoNDF[,fullSetDummy] )
table(c50UpSamplePredict2,trainingNoNDF$country_destination )



##############################
## Model Comparison
##############################
x <- c("xgbFit.rda", "svmRFitReduced.rda", "rpartFit.rda", "rpartFactorFit.rda", "rfFit.rda",
       "partFit.rda", "nnetFit.rda", "nBayesFit.rda", "ldaFit.rda", "j48FactorFit.rda",
       "glmnFit.rda", "fdaFit.rda")

lapply(x, FUN = function(X) {
  do.call("load", list(X)) 
})
rm(x)


resamps <- resamples(list(XGBoost = xgbFit,
                          SVM = svmRFitReduced,
                          Rpart = rpartFit,
                          RpartFactor = rpartFactorFit,
                          RF = rfFit,
                          Part = partFit,
                          nBayes = nBayesFit,
                          lda = ldaFit,
                          j48 = j48Fit))
                      
                          
#glm = glmnFit,
#c50 = c50Fit))
#NNET = nnetFit3,
#fda = fdaFit
resamps
summary(resamps)


bwplot(resamps, layout = c(3, 1))

varImp(xgbFit)
varImp(svmRFitReduced)
varImp(rpartFit)
varImp(rpartFactorFit)
varImp(rfFit)
varImp(partFit) # don't work
varImp(nBayesFit)
varImp(ldaFit)
varImp(j48Fit)

svmRFitReduced###
######### this part don't work well ############
###
x = c("xgbFit", "svmRFitReduced", "rpartFit","rpartFactorFit", "rfFit", "partFit", "nBayesFit", "ldaFit", "j48Fit")

xgbFitSummary <- caret:::getTrainPerf(xgbFit)
svmRFitReducedSummary <- caret:::getTrainPerf(svmRFitReduced)
rpartFitSummary <- caret:::getTrainPerf(rpartFit)
rpartFactorFitSummary <- caret:::getTrainPerf(rpartFactorFit)
rfFitSummary <- caret:::getTrainPerf(rfFit)
partFitSummary <- caret:::getTrainPerf(partFit)
nBayesFitSummary <- caret:::getTrainPerf(nBayesFit)
ldaFitSummary <- caret:::getTrainPerf(ldaFit)
j48FitSummary <- caret:::getTrainPerf(j48Fit)


trainPerf <- rbind(xgbFitSummary, svmRFitReducedSummary, svmRFitReducedSummary, rpartFactorFitSummary, rpartFactorFitSummary, rfFitSummary,
                   partFitSummary, nBayesFitSummary, ldaFitSummary, j48FitSummary)


library(lattice)
library(reshape2)
trainPerf <- melt(trainPerf)
trainPerf$metric <- "ROC"
trainPerf$metric[grepl("Sens", trainPerf$variable)] <- "Sensitivity"
trainPerf$metric[grepl("Spec", trainPerf$variable)] <- "Specificity"

dotplot(method ~ value|metric,
        data = trainPerf,
        horizontal = TRUE,
        auto.key = list(columns = 1),
        between = list(x = 1),
        xlab = "")

##########################################

######
##Using H2O to run
######

library(h2oEnsemble)  # Requires version >=0.0.4 of h2oEnsemble
localH2O <- h2o.init(nthreads = -1)  # Start an H2O cluster with nthreads = num cores on your machine

train_h2o <- h2o.importFile()
test_h2o <- h2o.importFile("http://www.stat.berkeley.edu/~ledell/data/higgs_test_5k.csv")
y <- "C1"
x <- setdiff(names(train), y)
family <- "binomial"

training_trainDummy$date_account_created = as.character(training_trainDummy$date_account_created)
training_trainDummy$timestamp_first_active = as.character(training_trainDummy$timestamp_first_active)

training_evaluationDummy$date_account_created = as.character(training_evaluationDummy$date_account_created)
training_evaluationDummy$timestamp_first_active = as.character(training_evaluationDummy$timestamp_first_active)

testingDummy <- testingDummy[, names(trainingDummy)]  # to correct different # of variables from load("Airbnb_data_1.5.RData")
levels(testingDummy$booking) <- levels(trainingDummy$booking) # to correct levels in testing set to match training set
testingDummy$date_account_created = as.character(testingDummy$date_account_created)
testingDummy$timestamp_first_active = as.character(testingDummy$timestamp_first_active)
str(testingDummy$booking)
testingDummy$booking[1:10] = "No"

train_h2o <- as.h2o(training_trainDummy,  destination_frame = "train_h2o")
class(train_h2o)

evaluation_h2o <- as.h2o(training_evaluationDummy,  destination_frame = "evaluation_h2o")
class(evaluation_h2o)

test_h2o <- as.h2o(testingDummy,  destination_frame = "test_h2o")
class(test_h2o)
str(test_h2o$booking)


# Specify the base learner library & the metalearner
learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper", 
             "h2o.gbm.wrapper", "h2o.deeplearning.wrapper")
metalearner <- "h2o.deeplearning.wrapper"
family <- "binomial"

# Train the ensemble using 5-fold CV to generate level-one data
# More CV folds will take longer to train, but should increase performance
fit <- h2o.ensemble(x = reducedSetDummy, y = "booking", 
                    training_frame = train_h2o, 
                    family = family, 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 2, shuffle = TRUE))

perf <- h2o.ensemble_performance(fit, newdata = evaluation_h2o )
perf


# H2O Ensemble Performance on <newdata>:
#   ----------------
#   Family: binomial
# 
# Ensemble performance (AUC): 0.842187959529432
# 
# Base learner performance:
#   learner       AUC
# 1          h2o.glm.wrapper 0.8218792
# 2 h2o.randomForest.wrapper 0.8354707
# 3          h2o.gbm.wrapper 0.8406871
# 4 h2o.deeplearning.wrapper 0.8126837

# Ensemble test set AUC
perf$ensemble@metrics$AUC


# If desired, you can generate predictions on the test set
# This is useful if you need to calculate custom performance metrics in R (not provided by H2O)

test_preds <- predict(fit, test_h2o)
test_preds

# $pred
# predict        No        Yes
# 1      No 0.7737861 0.22621386
# 2      No 0.9583251 0.04167491
# 3      No 0.9317004 0.06829963
# 4      No 0.9312338 0.06876620
# 5     Yes 0.1673589 0.83264110
# 6     Yes 0.4869392 0.51306076



predictions <- as.data.frame(test_preds$pred)[,2] # probability of "No"
head(predictions)

# probability (for ensemble)
modelAvg <- cbind(predictions)
modelAvg <- data.frame(modelAvg)

modelAvg$prediction <- ifelse(predictions >=0.5, "NDF", "US")
head(modelAvg)

h2o.shutdown()
# submission format (id,country)  - re-use code for each submission
submission <- read.csv("submission.csv", header = TRUE)
head(submission)
submission$id <- NULL
submission$country <- NULL
submission$id <- testingDummy$id
#submission$country <- rfPrediction
#submission$country <- nnetPrediction
#submission$country <- rpartPrediction
#submission$country <- xgbPrediction
submission$country <-  modelAvg$prediction

submissionNDF <- submission[which(submission$country == "NDF"),]
submissionUS <- submission[which(submission$country == "US"),]

# top 5 (NDF, US, other, FR, IT, GB, ES)
#NDF Set <- NDF, US, other, FR, IT
NDF_US <- submissionNDF
NDF_US$id <- submissionNDF$id
NDF_US$country <- "US"

NDF_other <- submissionNDF
NDF_other$id <- submissionNDF$id
NDF_other$country <- "other"

NDF_FR <- submissionNDF
NDF_FR$id <- submissionNDF$id
NDF_FR$country <- "FR"

NDF_IT <- submissionNDF
NDF_IT$id <- submissionNDF$id
NDF_IT$country <- "IT"

#US Set <- US, NDF, other, FR, IT
US_NDF <- submissionUS
US_NDF$id <- submissionUS$id
US_NDF$country <- "NDF"

US_other <- submissionUS
US_other$id <- submissionUS$id
US_other$country <- "other"

US_FR <- submissionUS
US_FR$id <- submissionUS$id
US_FR$country <- "FR"

US_IT <- submissionUS
US_IT$id <- submissionUS$id
US_IT$country <- "IT"

# submission output
submissionFinal <- rbind(submission, NDF_US, NDF_other, NDF_FR, NDF_IT, US_NDF, US_other, US_FR, US_IT)

#write.csv(submissionFinal, "submission_rfFit.csv", quote=FALSE, row.names = FALSE) # Kaggle: 0.87618
#write.csv(submissionFinal, "submission_nnetFit.csv", quote=FALSE, row.names = FALSE) # Kaggle: 0.87360
#write.csv(submissionFinal, "submission_rpartFit.csv", quote=FALSE, row.names = FALSE) # Kaggle: 0.87592
#write.csv(submissionFinal, "submission_xgbFit.csv", quote=FALSE, row.names = FALSE) # Kaggle: 0.88364
#write.csv(submissionFinal, "submission_avg_rf_nnet_rpart.csv", quote=FALSE, row.names = FALSE) # Kaggle: 0.87737
#write.csv(submissionFinal, "submission_avg_rf_nnet_rpart_xgb.csv", quote=FALSE, row.names = FALSE) # Kaggle: 0.88313
write.csv(submissionFinal, "submission_h20.csv", quote=FALSE, row.names = FALSE) # Kaggle: 0.88343
	






