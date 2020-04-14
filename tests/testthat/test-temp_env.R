context("temp_env")

describe("temp_env() creation and retrieval", {
  tenv <- temp_env()

  it("must be an environment attached to the search path", {
    expect_is(tenv, "environment")
    expect_false(is.na(match("SciViews:TempEnv", search())))
  })

  it("always provides the same environment on further calls", {
    expect_identical(tenv, temp_env())
  })
})

describe("Variables management in temp_env()", {
  temp_var <- "test_variable___"

  it("indicates when temporary variables are in temp_env() or not", {
    expect_false(exists_temp(temp_var))
    assign_temp(temp_var, 1:3)
    expect_true(exists_temp(temp_var))
    expect_identical(get_temp(temp_var), 1:3)
    expect_error(exists_temp(), "argument \"x\" is missing, with no default", fixed = TRUE)
  })

  it("does replace or not existing temporary variables depending on replace.existing =", {
    assign_temp(temp_var, 4:5, replace.existing = FALSE)
    expect_identical(get_temp(temp_var), 1:3)
    assign_temp(temp_var, 4:5, replace.existing = TRUE)
    expect_identical(get_temp(temp_var), 4:5)
  })

  it("removes temporary variables from temp_env(), and proceed silently if variable not found", {
    expect_true(delete_temp(temp_var))
    expect_false(exists_temp(temp_var))
    expect_false(delete_temp("non_existing_variable___"))
  })
})
