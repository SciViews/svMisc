#' Create an alias (also known as) for an object whose comment and/or original help page can be viewed with .?obj.
#'
#' @description When a function or object is renamed, the link to its original
#' help page is lost in R. Using `aka()` (also known as) with correct `alias=`
#' allows to keep track of the original help page and get it with `.?obj`.
#' Moreover, one can also add a short comment message that is displayed at the
#' same time in the R console.
#'
#' @param obj An R object.
#' @param alias The full qualified name of the alias object whose help
#' page should be retained as `pkg::name`. If `NULL` (by default), use `obj`.
#' @param comment A comment to place in `obj` (will also be printed when calling
#' `.?obj`).
#'
#' @return The original `obj` with the `comment` attribute set or replaced with
#' `comment =` plus a `src` attribute set to `alias =`
#' @export
#'
#' @examples
#' # Say you prefer is.true() similar to is.na() or is.null()
#' # but R provides isTRUE().
#' library(svMisc)
#' is.true <- aka(isTRUE)
#' # This way, you still got access to the right help page for is.true()
#' \dontrun{
#' .?is.true
#'}
aka <- function(obj, alias = NULL, comment = "") {
  if (is.null(alias))
    alias <- deparse(substitute(obj))
  attr(comment, "src") <- alias
  comment(obj) <- comment
  obj
}
