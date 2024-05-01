test_that("parse_text() parse expressions like parse(text = ...)", {
  # Single expression
  expr <- "1+1"
  res <- as.character(parse(text = expr))
  expect_identical(as.character(parse_text(expr)), res)

  # Multiple expressions
  expr <- "1+1; ls()"
  res <- as.character(parse(text = expr))
  #expect_identical(as.character(parse_text(expr)), res)

  expr <- c("1+1", "ls()")
  res <- as.character(parse(text = expr))
  #expect_identical(as.character(parse_text(expr)), res)
})

test_that("parse_text() works with wrong expressions", {
  # NA on incomplete expressions
  expr <- "1 +"
  expect_true(is.na(parse_text(expr)))
  expr <- "1+)"
  expect_s3_class(parse_text(expr), "try-error")

  # Captures the error message for a wrong expression
  get_error_msg <- function(text) {
    res <- try(parse(text = text), silent = TRUE)
    if (inherits(res, "try-error")) {
      res <- sub("^.*<text>:", "", as.character(res))
      res <- sub("\n$", "", res)
      return(res)
    } else return("") # This is not supposed to happen!
  }
  # TODO: for some reasons this does not work as expected...
  expr <- "1+)"
  #expect_identical(as.character(parse_text(expr)), get_error_msg(expr))
})
