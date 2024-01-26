library(testthat)
library(dplyr)

# Prevent certificate issues for GitHub actions
options(gemma.SSL = FALSE)
# get gemma api path if it's set in the environment
gemma.R:::setGemmaPath('prod')
print(gemma.R:::gemmaPath())
test_check("gemma.R")


