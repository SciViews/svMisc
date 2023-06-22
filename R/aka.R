#' Create an alias (also known as) for an object whose a short help page and/or original help page can be viewed with .?obj.
#'
#' @description When a function or object is renamed, the link to its original
#' help page is lost in R. Using `aka()` (also known as) with correct `alias=`
#' allows to keep track of the original help page and get it with `.?obj`.
#' Moreover, one can also populate a short man page with description, seealso and example or add a short comment message that is displayed at the
#' same time in the R console.
#'
#' @param obj An R object.
#' @param alias The full qualified name of the alias object whose help
#' page should be retained as `pkg::name`. If `NULL` (by default), use `obj`.
#' @param comment A comment to place in `obj` (will also be printed when calling
#' `.?obj`).
#' @param description A description of the function for the sort man page.
#' @param seealso A character string of functions in the form `fun` or `pkg::fun`.
#' @param example A character string with code for a short example.
#'
#' @return The original `obj` with the `comment` attribute set or replaced with
#' `comment =` plus a `src` attribute set to `alias =` ans `description`, `seealso` and `example` attributes also possibly populated.
#' @export
#'
#' @examples
#' # Say you prefer is.true() similar to is.na() or is.null()
#' # but R provides isTRUE().
#' library(svMisc)
#' # Also defining a short man page
#' is.true <- aka(isTRUE, description = "Check if an object is TRUE.",
#'   seealso = c("is.false", "logical"), example = c("is.true(TRUE)", "is.true(FALSE)", "is.true(NA)"))
#' # This way, you still got access to the right help page for is.true()
#' \dontrun{
#' .?is.true
#'}
aka <- function(obj, alias = NULL, comment = "", description = NULL,
seealso = NULL, example = NULL) {
  if (is.null(alias))
    alias <- deparse(substitute(obj))
  attr(comment, "src") <- alias
  if (!is.null(description))
    attr(comment, "description") <- as.character(description)
  if (!is.null(seealso))
    attr(comment, "seealso") <- as.character(seealso)
  if (!is.null(example))
    attr(comment, "example") <- as.character(example)
  comment(obj) <- comment
  obj
}
