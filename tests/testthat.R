library(testthat)
library(gemma.R)
library(dplyr)

# Prevent certificate issues for GitHub actions
options(gemma.SSL = FALSE)

test_check("gemma.R")
