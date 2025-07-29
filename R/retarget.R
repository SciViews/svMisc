#' Retarget a formula to a different environment
#'
#' @description
#' Change the environment of a formula to a specified environment.
#'
#' @param x A formula (or a quosure), or a list of formulas.
#' @param env The environment to which the formula(s) should be retargeted.
#' Defaults to the parent frame.
#'
#' @returns The formula or list of formulas with the new environment set.
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
#'
#' # With a list of formulas (local() creates a new environment):
#' fl <- local(list(y ~x^2, z~ sqrt(cos(x^2) + sin(x^2))))
#' fl # Not in GlobalEnv
#' retarget(fl) # Retargeted to GlobalEnv
retarget <- function(x, env = parent.frame()) {
  if (is.list(x)) {
    if_formula_f_env <- function(x, env) {
      if (is_formula(x))
        f_env(x) <- env
      x
  }
    x <- lapply(x, if_formula_f_env, env)

  } else {# Not a list
    f_env(x) <- env
  }
  x
}
