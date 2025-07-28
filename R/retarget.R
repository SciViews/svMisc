#' Retarget a formula to a different environment
#'
#' @description
#' Change the environment of a formula to a specified environment.
#'
#' @param formula A formula (or a quosure)
#' @param env The environment to which the formula should be retargeted.
#' Defaults to the parent frame.
#'
#' @returns The formula with the new environment set.
#' @export
#'
#' @examples
#' # A rather inefficient way to build a formula... for the sake of the demo!
#' make_formula <- function(x)
#'   as.formula(x)
#' f <- make_formula("x ~ log(y) + z")
#' f
#' f <- retarget(f)
#' f
#' # OK, but the environment associated to this formula is...
#' # the environment of the function:
#' rlang::f_env(f)
retarget <- function(formula, env = parent.frame()) {
  f_env(formula) <- env
  formula
}
