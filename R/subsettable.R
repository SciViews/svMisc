#' Define a function as being 'subsettable' using $ operator
#'
#' @description In case a textual argument allows for selecting the result, for
#' instance, if `plot()` allows for several charts that you can choose with a
#' `type=` or `which=`, making the function 'subsettable' also allows to
#' indicate `fun$variant()`. See examples.
#' @export
#' @name subsettable
#' @param x A `subsettable_type` function.
#' @param name The value to use for the `type=` argument.
#' @method $ subsettable_type
#' @keywords utilities
#' @concept create 'subsettable' functions
#' @examples
#' foo <- structure(function(x, type = c("histogram", "boxplot"), ...) {
#'   type <- match.arg(type, c("histogram", "boxplot"))
#'   switch(type,
#'     histogram = hist(x, ...),
#'     boxplot = boxplot(x, ...),
#'     stop("unknow type")
#'   )
#' }, class = c("function", "subsettable_type"))
#' foo
#'
#' # This function can be used as usual:
#' foo(rnorm(50), type = "histogram")
#' # ... but also this way:
#' foo$histogram(rnorm(50))
#' foo$boxplot(rnorm(50))
`$.subsettable_type` <- function(x, name)
  function(...) x(type = name, ...)

#' @export
#' @rdname subsettable
#' @method $ subsettable_which
`$.subsettable_which` <- function(x, name)
  function(...) x(which = name, ...)
