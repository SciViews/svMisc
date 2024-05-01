test_that("temp_env() creates and retrieves the temporary environment", {
  tenv <- temp_env()
  expect_type(tenv, "environment")
  expect_false(is.na(match("SciViews:TempEnv", search())))
  expect_identical(tenv, temp_env())
})

test_that("Variables management in temp_env()", {
  temp_var <- "test_variable___"
  expect_false(exists_temp(temp_var))
  assign_temp(temp_var, 1:3)
  expect_true(exists_temp(temp_var))
  expect_identical(get_temp(temp_var), 1:3)
  expect_error(exists_temp(),
    "argument \"x\" is missing, with no default", fixed = TRUE)

  # Replace or not variables in TempEnv
  assign_temp(temp_var, 4:5, replace.existing = FALSE)
  expect_identical(get_temp(temp_var), 1:3)
  assign_temp(temp_var, 4:5, replace.existing = TRUE)
  expect_identical(get_temp(temp_var), 4:5)

  # Remove variables in TempEnv (silently, if not there)
  expect_true(delete_temp(temp_var))
  expect_false(exists_temp(temp_var))
  expect_false(delete_temp("non_existing_variable___"))
})
