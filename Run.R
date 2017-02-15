
# **************************************
# Setup
# **************************************

# set your working directory
path <- "~/Class- R Class/Kaggle- AirBnB/Solution Final - SH"
setwd(path)

# create directory
dir.create(paste0("data")) # Please put the download data into this folder
dir.create(paste0("model output"))
dir.create(paste0("script"))

# load libraries
# x<-c("caret", "dplyr", "data.table", "bit64")
# lapply(x, FUN = function(X) {
#   do.call("require", list(X)) 
# })
# rm(x)



# **************************************
# Functions
# **************************************




# **************************************
# run scripts
# **************************************

source("script/Preprocess.R")
source("script/H2O_model.R")