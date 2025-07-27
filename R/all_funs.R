#' Find all functions in an Expression
#'
#' @description
#' Return a character vector containing the name of all the functions in an
#' expression or a call.
#'
#' @param expr an expression or call from which the name of the function are to
#' be extracted.
#' @param max.names the maximum number of names to be returned. -1 indicates no limit (other than vector size limits).
#' @param unique a logical value which indicates whether duplicate names should be removed from the value.
#'
#' @details
#' The c code is adapted from base R code `do_allnames()` (the later one allows
#' to extract either variables, or variables + functions, but not functions
#' alone).
#'
#' @returns A character vector with the extracted function names.
#' @export
#'
#' @examples
#' ff <- ~z(x, y, z, TRUE, "test", l = 4) + (y(z, x(l)) + y(2))
#' all.vars(ff)
#' all.names(ff, unique = TRUE)
#' all_funs(ff)
all_funs <- function(expr, max.names = -1L, unique = TRUE) {
  #max.names <- as.integer(max.names)[1]
  #unique <- isTRUE(unique)
  .Call(allfuns, expr, max.names, unique, PACKAGE = "svMisc")
}
