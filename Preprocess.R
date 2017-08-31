## Preprocess Data##

# **************************************
# Setup
# **************************************

# set your working directory
path <- "fill in path"
setwd(path)

load("data/Preprocessed.RData")
load("./data/Preprocessed.RData")
load("data/H2ORunwithData.RData")

require(data.table)
require(bit64)
require(plyr)
require(dplyr)
require(caret)

# **************************************
# loading and viewing data
# **************************************

sessions <- fread("data/sessions.csv")
test <- fread("data/test_users.csv")
train <- fread("data/train_users_2.csv")

# Keep sessions with user ID in train or test set (from 10567737 to 10533241 )
sessions <- subset(sessions, user_id %in% c(train$id, test$id))

# keeping users present in sessions data for training set (most of 2014 onward for date_account_created)
# about 400 users in test set does not have sessions data (from 213451 to 73815)
train <- subset(train, id %in% (sessions$user_id)) 

# add country_destination to test set 
test$country_destination <- NA

# make markers 
train_ids <- train$id
test_ids <- test$id
destination_id <- subset(train, select = c("id","country_destination"))

# view data
library(Hmisc)
describe(train)
describe(test)
head(sessions, 50)


# **************************************
# Cleaning data - Main Dataset
# **************************************

# clean formatting
train[train =="-unknown-"] <- "unknown"
test[test =="-unknown-"] <- "unknown"
train[train =="Other/Unknown"] <- "unknown"
test[test =="Other/Unknown"] <- "unknown"

# Clean Age
train$age[is.na(train$age)] <- -1
test$age[is.na(test$age)] <- -1

for (i in 1920:2014){
  train$age[train$age == i] <- 2014 - train$age[train$age == i]
  test$age[test$age == i] <- 2014 - test$age[test$age == i]
}

rm(i)

train$age[ train$age < 17 | train$age > 90 ] <- NA
test$age[test$age < 17 | test$age > 90 ] <- NA
train$age[train$age == -1] <- NA
test$age[test$age == -1] <- NA

# Aggregate similar browsers
train$first_browser[train$first_browser %in% c("Arora","Avant Browser","Camino","CometBird","Comodo Dragon","Conkeror","CoolNovo","Crazy Browser","Epic","Flock","Google Earth","Googlebot","IBrowse","IceDragon","IceWeasel","Iron","Kindle Browser","Maxthon","Nintendo Browser","NetNewsWire","OmniWeb","Outlook 2007","Pale Moon","Palm Pre web browser","PS Vita browser","RockMelt","SeaMonkey","SiteKiosk","SlimBrowser","Sogou Explorer","Stainless","TenFourFox","TheWorld Browser","UC Browser","wOSBrowser","Yandex.Browser")] <- "Other"
test$first_browser[test$first_browser %in% c("Arora","Avant Browser","Camino","CometBird","Comodo Dragon","Conkeror","CoolNovo","Crazy Browser","Epic","Flock","Google Earth","Googlebot","IBrowse","IceDragon","IceWeasel","Iron","Kindle Browser","Maxthon","Nintendo Browser","NetNewsWire","OmniWeb","Outlook 2007","Pale Moon","Palm Pre web browser","PS Vita browser","RockMelt","SeaMonkey","SiteKiosk","SlimBrowser","Sogou Explorer","Stainless","TenFourFox","TheWorld Browser","UC Browser","wOSBrowser","Yandex.Browser")] <- "Other"

train$first_browser[train$first_browser %in% c("Chrome","Chrome Mobile","Chromium")] <- "Chrome"
test$first_browser[test$first_browser %in% c("Chrome","Chrome Mobile","Chromium")] <- "Chrome"

train$first_browser[train$first_browser %in% c("Firefox","Mobile Firefox","Mozilla")] <- "Firefox"
test$first_browser[test$first_browser %in% c("Firefox","Mobile Firefox","Mozilla")] <- "Firefox"

train$first_browser[train$first_browser %in% c("IE","IE Mobile")] <- "IE"
test$first_browser[test$first_browser %in% c("IE","IE Mobile")] <- "IE"

train$first_browser[train$first_browser %in% c("Mobile Safari","Safari")] <- "Safari"
test$first_browser[test$first_browser %in% c("Mobile Safari","Safari")] <- "Safari"

train$first_browser[train$first_browser %in% c("Opera","Opera Mini","Opera Mobile")] <- "Opera"
test$first_browser[test$first_browser %in% c("Opera","Opera Mini","Opera Mobile")] <- "Opera"

# Aggregate affiliate provider
train$affiliate_provider[train$affiliate_provider %in% c("other", "craigslist","daum","meetup", "gsp", "vast", "yandex", "baidu")] <- "other"
test$affiliate_provider[test$affiliate_provider %in% c("other", "craigslist","daum","meetup", "gsp", "vast", "yandex", "baidu")] <- "other"

train$affiliate_provider[train$affiliate_provider %in% c("facebook","facebook-open-graph")] <- "facebook"
test$affiliate_provider[test$affiliate_provider %in% c("facebook","facebook-open-graph")] <- "facebook"


# **************************************
# Adding features - Main Dataset
# **************************************

##
# Add features from date_account_created and timestamp_first_active (from main dataset)
##
train <- as.data.table(train)
test <- as.data.table(test)

train <- train[, ':='(
                      month_account = as.integer(substr(date_account_created,6,7)), 
                      day_account = as.integer(substr(date_account_created,9,10)),
                      weekday_account = weekdays(as.Date(paste0(substr(date_account_created,1,4),"-",substr(date_account_created,6,7),"-",substr(date_account_created,9,10)))),
                      
                      month_active = as.integer(substr(timestamp_first_active,5,6)),
                      day_active = as.integer(substr(timestamp_first_active,7,8)),
                      weekday_active = weekdays(as.Date(paste0(substr(timestamp_first_active,1,4),"-",substr(timestamp_first_active,5,6),"-",substr(timestamp_first_active,7,8))))
                      )
              ]

test <- test[, ':='(
                      month_account = as.integer(substr(date_account_created,6,7)), 
                      day_account = as.integer(substr(date_account_created,9,10)),
                      weekday_account = weekdays(as.Date(paste0(substr(date_account_created,1,4),"-",substr(date_account_created,6,7),"-",substr(date_account_created,9,10)))),
                      
                      month_active = as.integer(substr(timestamp_first_active,5,6)),
                      day_active = as.integer(substr(timestamp_first_active,7,8)),
                      weekday_active = weekdays(as.Date(paste0(substr(timestamp_first_active,1,4),"-",substr(timestamp_first_active,5,6),"-",substr(timestamp_first_active,7,8))))
                    )
            ]

# **************************************
# Convert to factor variables for the main data set
# **************************************

# remove columns not needed for prediction
train$date_account_created <- NULL
train$timestamp_first_active <- NULL
train$date_first_booking <- NULL

test$date_account_created <- NULL
test$timestamp_first_active <- NULL
test$date_first_booking <- NULL

# Make factor variables
convertCols <- c("gender", "signup_method", "signup_flow", "language", "affiliate_channel", "affiliate_provider", "first_affiliate_tracked"
                 ,"signup_app", "first_device_type", "first_browser", "country_destination", "month_account", "day_account", "weekday_account",
                 "month_active", "day_active", "weekday_active")
require(plyr)
train <- as.data.frame(train)
test <- as.data.frame(test)
combined <- rbind(train, test)
combined[, convertCols]  <- colwise(as.factor)(combined[, convertCols])

# recode country_destination
#library(car)
#combined$country_destination <- recode(combined$country_destination,"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'IT'=4; 'GB'=5; 'ES'=6; 'CA'=7; 'DE'=8; 'NL'=9; 'AU'=10; 'PT'=11")

# Verify that the factors are not ordered
unlist(lapply(combined, is.ordered))

train <- subset(combined, id %in% train$id)
test <- subset(combined, id %in% test$id)
booked <- ifelse(train$country_destination == "NDF", 0, 1)  #1 = Booked, #0 = NDF
country_destination <- combined$country_destination

# **************************************
# Make dummy variables for main dataset 
# **************************************

# create dummy variables for factor variables
factorVars <- names(combined[,sapply(combined,is.factor)])

require(caret)
factorData = combined[,factorVars]
dummiesModel <- dummyVars(country_destination ~ ., data = factorData)
factorData <- as.data.frame(predict(dummiesModel, newdata = factorData))  # country destination not in factorData

# combine non-factor variables with dummy factor variables
notFactorData <-  combined[, !(colnames(combined) %in% factorVars)] # not id or age
combined <- cbind(notFactorData, factorData)

rm(factorData)
rm(notFactorData)
rm(convertCols)
rm(dummiesModel)
rm(factorVars)


# clean names
names(combined) <- gsub("-", "",names(combined))
names(combined) <- gsub("/", "",names(combined))
names(combined) <- gsub("\\(", "",names(combined))
names(combined) <- gsub("\\)", "",names(combined))
names(combined) <- gsub(" ", "",names(combined))

# add back country destination
combined$country_destination <- country_destination

# separate into two datasets
train <- subset(combined, id %in% train_ids)
test <- subset(combined, id %in% test_ids)

rm(combined)

## **************************************
#  Add features from the sessions dataset
## **************************************

# clean dataset
names(sessions)[1] <- "id"
sessions[sessions =="-unknown-"] <- "unknown"

##
# action/action_type/action_detail 
##

# Review Sessions Dataset
sessions_count <- sessions %>%
  dplyr::summarise_each(funs(n_distinct))
# id        action      action_type   action_detail device_type   secs_elapsed
# 135483    360         11            156           14            337189

# Check number of different kind of actions (457 action combos)
session_combos <- sessions %>%
  dplyr::select(-id, -device_type, -secs_elapsed) %>%
  dplyr::distinct() %>%
  dplyr::arrange(action_type)
session_combos
rm(session_combos)

# create new field
sessions$count <- 1
sessions <- sessions %>%
  dplyr:: mutate(actionCombo = paste(action_type, action_detail, action, sep = "_"))


# Check distribution of action combos to see if any are rare
distinctActionCombos <- sessions %>%
  dplyr:: count(actionCombo) %>%
  dplyr:: arrange(n) %>%
  dplyr:: mutate(logN = log(n))
distinctActionCombos
rm(distinctActionCombos)
  
# keep actionCombos that have more than 20 observations (narrow from 457 to 355 variables)
PredictorsActionCombo <- distinctActionCombos %>%
  dplyr::filter( n > 20) %>%
  dplyr::select(actionCombo)
PredictorsActionCombo <- as.list(PredictorsActionCombo)

# make sessions dataset excluding rows with rare action Combos
sessionsTrim <- subset(sessions, actionCombo %in% PredictorsActionCombo$actionCombo)

##
# create new features from actionCombos
##

# Flag for unique actionCombo per ID (only for actionsCombos that have more than 20 observations)
actionCombo_flag <- dcast(sessionsTrim, id ~ actionCombo, mean, value.var="count") #135483 obs. of  458 variables
names(actionCombo_flag)[2:ncol(actionCombo_flag)] <- paste("flag", names(actionCombo_flag)[2:ncol(actionCombo_flag)], sep = "_")

# Count number of occurances per unique actionCombo per ID
actionCombo_sum <- dcast(sessionsTrim, id ~ actionCombo, sum, value.var="count")
names(actionCombo_sum)[2:ncol(actionCombo_sum)] <- paste("sum", names(actionCombo_sum)[2:ncol(actionCombo_sum)], sep = "_")

# Secs elapsed of individual actions, action type, action detail
actionCombo_secs <- dcast(sessionsTrim, id ~ actionCombo, sum, value.var="secs_elapsed")
names(actionCombo_secs)[2:ncol(actionCombo_secs)] <- paste("sec", names(actionCombo_secs)[2:ncol(actionCombo_secs)], sep = "_")

# Average time elapsed of individual actions, action type, action detail
actionCombo_avg <- sessionsTrim[,list(secs_elapsed_mean = mean(secs_elapsed, na.rm=T)),
                                by=list(id, actionCombo)]
actionCombo_avg <- dcast(actionCombo_avg, id ~ actionCombo, value_var = "secs_elapsed_mean")
names(actionCombo_avg)[2:ncol(actionCombo_avg)] <- paste("avg", names(actionCombo_avg)[2:ncol(actionCombo_avg)], sep = "_")

# Total number of sessions per user (including the rare events)
sessions_total_flag <- sessions[,list(sessions_total_flag_count = sum(count, na.rm=T)),
                                by=list(id)]

# Total number of secs_elapsed per user (including the rare events)
sessions_total_secs <- sessions[,list(sessions_total_secs_elapsed = sum(secs_elapsed, na.rm=T)),
                                by=list(id)]

# Average sec_elapsed per action for each ID (including the rare events)
sessions_total <- sessions_total_flag %>%
  dplyr::left_join(sessions_total_secs, by = "id") 

sessions_total <- sessions_total %>%
  dplyr::mutate(sessions_sec_per_action = sessions_total_secs_elapsed / sessions_total_flag_count) 

# Free up memory
rm(sessions_total_secs)
rm(sessions_total_flag)


##
# create new features from Device Types
##

# Unique device used per ID
device_per_id <- sessions %>%
  dplyr::select(id, device_type) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise_each(funs(n_distinct))

names(device_per_id) <- c("id", "device_per_id") 

# device type flag
device_type_flag <- dcast(sessions, id ~ device_type, mean, value.var="count")
names(device_type_flag)[2:ncol(device_type_flag)] <- paste("deviceFlag", names(device_type_flag)[2:ncol(device_type_flag)], sep = "_")

# device usage in secs elapsed
device_type_secs <- dcast(sessions, id ~ device_type, sum, value.var="secs_elapsed")
names(device_type_secs)[2:ncol(device_type_secs)] <- paste("deviceSecs", names(device_type_secs)[2:ncol(device_type_secs)], sep = "_")

# device usage in number of sessions
device_type_count <- dcast(sessions, id ~ device_type, sum, value.var="count")
names(device_type_count)[2:ncol(device_type_count)] <- paste("deviceCount", names(device_type_count)[2:ncol(device_type_secs)], sep = "_")

## **************************************
#  merging new features from sessions with main dataset
## **************************************

# create table for all device data
device_summary <- device_per_id %>%
  dplyr::left_join(device_type_count, by = "id") %>%
  dplyr::left_join(device_type_flag, by = "id") %>%
  dplyr::left_join(device_type_secs, by = "id") 

# create table for all sessions data
sessions_summary <- actionCombo_avg %>%
  dplyr::left_join(actionCombo_flag, by = "id") %>%
  dplyr::left_join(actionCombo_secs, by = "id") %>%
  dplyr::left_join(actionCombo_sum, by = "id") %>%
  dplyr::left_join(sessions_total, by = "id")

# clean-up column name  
names(device_summary) <- gsub("-", "",names(device_summary))
names(device_summary) <- gsub("/", "",names(device_summary))
names(device_summary) <- gsub(" ", "",names(device_summary))

names(sessions_summary) <- gsub("-", "",names(sessions_summary))
names(sessions_summary) <- gsub("/", "",names(sessions_summary))
names(sessions_summary) <- gsub(" ", "",names(sessions_summary))

# change all of NA's to 0
device_summary[is.na(device_summary)] <- 0
sessions_summary[is.na(sessions_summary)] <- 0

# check data prior to merging with main dataset
library(Hmisc)
describe(device_summary)
describe(sessions_summary)

# Merge device_summary and sessions_summary from Sessions with main dataset
train <- dplyr::left_join(train, device_summary, by = "id")
test <- dplyr::left_join(test, device_summary, by = "id")

train <- dplyr::left_join(train, sessions_summary, by = "id")
test <- dplyr::left_join(test, sessions_summary, by = "id")

# Free up memory
#ls(all = TRUE)
rm(device_per_id)
rm(device_type_secs)
rm(device_type_flag)
rm(device_type_count)
rm(actionCombo_avg)
rm(actionCombo_secs)
rm(actionCombo_sum)
rm(actionCombo_flag)
rm(sessions_total)
rm(sessions)
rm(sessionsTrim)
rm(device_summary)
rm(sessions_summary)
gc()
memory.size()

## ***********************************
#  Split Training Data into Train and Evaluation set
## ***********************************

# add variable "booked" 
# modify testing to have identical variables and factor levels as training set
train <- cbind(train, booked)
test$booked[1:nrow(test)] <- train$booked[1:nrow(test)]
train$booked <- as.factor(train$booked)  #NDF:0, Booked:1 0 (45041, 61%) 1 (28774, 39%)
train$booked <- factor(train$booked,
                      levels = c(0,1),
                      labels = c("NDF", "Booked"))
test$booked <- factor(test$booked,
                    levels = c(0,1),
                    labels = c("NDF", "Booked"))

test$country_destination[1:nrow(test)] <- train$country_destination[1:nrow(test)]
levels(test$country_destination)
levels(train$country_destination)
levels(test$booked)
levels(train$booked)

require(caret)
set.seed(100)
split <- createDataPartition(train$booked, p = .75)[[1]]
X_train <- train[split,]
X_eval  <- train[-split,]
y_train <- X_train$booked
y_eval <- X_eval$booked

## ***********************************
#  Save
## ***********************************

save.image("Preprocessed.RData")
