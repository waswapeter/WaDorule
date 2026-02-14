# Extracted from test-myfunction.R:5

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "WaDorule", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(WaDorule)

# test -------------------------------------------------------------------------
expect_equal(my_function(2), 4)
