test_that("capture_all() produces correct outputs", {
  expr1 <- parse(text = "1+1")
  res1 <- "[1] 2\n"
  expect_true(is.na(capture_all(NA)))
  expect_error(capture_all(1),
    "expr must be an expression or NA", fixed = TRUE)
  expect_error(capture_all("1+1"),
    "expr must be an expression or NA", fixed = TRUE)
  expect_error(capture_all(TRUE),
    "expr must be an expression or NA", fixed = TRUE)
  expect_error(capture_all(NULL),
    "argument is of length zero", fixed = TRUE)
  expect_error(capture_all(logical(0)),
    "argument is of length zero", fixed = TRUE)
})

#test_that("capture_all() splits outputs with split = TRUE", {
  #expect_identical(capture_all(expr1, echo = FALSE, split = TRUE), res1)
  #expect_identical(capture_all(expr1, echo = FALSE, split = FALSE), res1)
  #expect_identical(capture_all(expr1, echo = FALSE, split = c(TRUE, FALSE)),
  #  res1)
  #expect_identical(capture_all(expr1, echo = FALSE, split = logical(0)), res1)
  #expect_identical(capture_all(expr1, echo = FALSE, split = NULL), res1)
  #expect_identical(capture_all(expr1, echo = FALSE, split = "TRUE"), res1)
  #expect_identical(capture_all(expr1, echo = FALSE, split = 1), res1)
  #expect_identical(capture_all(expr1, echo = FALSE, split = NA), res1)
#})
