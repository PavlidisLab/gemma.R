library(testthat)
library(gemmaAPI)
library(dplyr)

# Prevent certificate issues for GitHub actions
options(gemma.SSL = FALSE)

test_check("gemmaAPI")
