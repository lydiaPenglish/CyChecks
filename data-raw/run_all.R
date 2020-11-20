# Constructs all package data.frames by running scripts in order
# except that it doesn't (currently) create ALL the data.frames
#--this doesn't actually work, and hasn't been tested 11/20/2020

source("departments.R")
source("salaries.R")
source("affiliation.R")
source("professors/00_run-all.R")
