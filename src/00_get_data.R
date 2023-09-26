# This script does the following:
#
# 1) Creates the necessary file folders
# 2) Details the data sources


# 1. Create folders -------------------------------------------------------

#Create data folder
folder <- "data"

if (file.exists(folder)) {
  
  cat("The folder already exists")
  
} else {
  
  dir.create(folder)
  
}

#Create results folder
folder <- "results"

if (file.exists(folder)) {
  
  cat("The folder already exists")
  
} else {
  
  dir.create(folder)
  
}


# 2. Get data -------------------------------------------------------------

#Data for this project are available at:

# Culpepper and Yang, 2023 - EDI or something

#Raw data can be generated using the following script in Google Earth Engine:

# Give the script link

# Verificaiton data are available from the following publications:


