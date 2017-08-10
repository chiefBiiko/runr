Sys.setenv(R_TESTS="")

library(testthat)
library(runr)

test_check("runr")
