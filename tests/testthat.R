library(testthat)
library(gemma.R)
library(dplyr)

# Prevent certificate issues for GitHub actions
options(gemma.SSL = FALSE)
# get gemma api path if it's set in the environment
options(gemma.API = Sys.getenv('API_PATH','https://gemma.msl.ubc.ca/rest/v2/'))
test_check("gemma.R")


