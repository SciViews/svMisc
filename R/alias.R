#' Create an alias for an object whose help page can be viewed with .?obj.
#'
#' @param obj An R object.
#' @param full.name The full qualified name of the original object whose help
#' page should be retained as `pkg::name`. If `NULL` (by default), use `obj`.
#' @param comment A comment to place in `obj` (will also be printed when calling `.?obj`).
#'
#' @return The original `obj` with the `comment` attribute set or replaced with `comment =` plus a `src`attribute set to `full.name =`
#' @export
#'
#' @examples
#' # Say you prefer is.true() similar to is.na() or is.null()
#' # but R provides isTRUE().
#' is.true <- alias(isTRUE)
#' # This way, you still got access to the right help page for is.true()
#' \dontrun{
#' library(svMisc)
#' .?is.true
#'}
alias <- function(obj, full.name = NULL, comment = "") {
  if (is.null(full.name))
    full.name <- deparse(substitute(obj))
  attr(comment, "src") <- full.name
  comment(obj) <- comment
  obj
}
