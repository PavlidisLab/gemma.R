library(testthat)
library(gemmaAPI)
library(dplyr)

# Prevent certificate issues for GitHub actions
options(ssl.verify = FALSE)

test_check("gemmaAPI")
