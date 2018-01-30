context("TempEnv")

describe("TempEnv creation and retrieval", {
  tenv <- TempEnv()

  it("must be an environment attached to the search path", {
    expect_is(tenv, "environment")
    expect_false(is.na(match("SciViews:TempEnv", search())))
  })
  it("always provides the same environment on further calls", {
    expect_identical(tenv, TempEnv())
  })
})

describe("Variables management in TempEnv", {
  temp_var <- "test_variable___"

  it("indicates when temporary variables are in TempEnv or not", {
    expect_false(existsTemp(temp_var))
    assignTemp(temp_var, 1:3)
    expect_true(existsTemp(temp_var))
    expect_identical(getTemp(temp_var), 1:3)
    expect_error(existsTemp(), "argument \"x\" is missing, with no default", fixed = TRUE)
  })
  it("does replace or not existing temporary variables depending on replace.existing =", {
    assignTemp(temp_var, 4:5, replace.existing = FALSE)
    expect_identical(getTemp(temp_var), 1:3)
    assignTemp(temp_var, 4:5, replace.existing = TRUE)
    expect_identical(getTemp(temp_var), 4:5)
  })
  it("removes temporary variables from TempEnv, and proceed silently if variable not found", {
    expect_true(rmTemp(temp_var))
    expect_false(existsTemp(temp_var))
    expect_false(rmTemp("non_existing_variable___"))
  })
})


# Old code - svUnit version -----------------------------------------------

## In ../test-with-svUnit.R:
## Note that the message in case of error is not explicit enough
## Moreover, nothing is printed in case of success, while we would like a succinct report
## TODO: check if warnings are not converted into errors during R CMD check tests
#library(svUnit)
#library(svMisc)
#clearLog()
#res <- capture.output(runTest(svSuite("package:svMisc"), "svMisc"))
#errorLog()


## Run tests using svUnit
## This is a wrapper to run these from within testthat
## so that R Studio's Test function and devtools::test() can be used too
#pkgname <- "svMisc"
#
#test_svUnit <- function(pkgname) {
#  library(svUnit)
#  clearLog()
#
#  # Current dir is /tests/testthat
#  basedir <- dirname(dirname(getwd()))
#
#  # Look for our tests in /unitTests or /inst/unitTests
#  testdir <- file.path(basedir, "unitTests")
#  if (!dir.exists(testdir)) {
#    # We must be in a development environment, look at /inst/unitTests
#    testdir <- file.path(basedir, "inst", "unitTests")
#  }
#  if (!dir.exists(testdir)) {
#    # We must be in a checking environment, look at /<pkgname>/unitTests
#    testdir <- file.path(basedir, pkgname, "unitTests")
#  }
#  if (!dir.exists(testdir)) {
#    # No test directory found
#    stop("No svUnit test directories found for ", basename(basedir))
#  }
#
#  res <- capture.output(runTest(svSuite(paste0("dir:", testdir)), pkgname))
#  summary(Log())
#}
#test_svUnit(pkgname)
