
# **************************************
# Setup
# **************************************

# set your working directory
path <- "input path here"
setwd(path)

# create directory
dir.create(paste0("data")) # Please put the download data into this folder
dir.create(paste0("model output"))
dir.create(paste0("script"))

# **************************************
# run scripts
# **************************************

source("script/Preprocess.R")
source("script/H2O_model.R")
