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
#' @param url An http or https URL pointing to the help page for the function
#' on the Internet.
#'
#' @return The original `obj` with the `comment` attribute set or replaced with
#' `comment =` plus a `src` attribute set to `alias =` and `description`,
#' `seealso`, `example`, and `url` attributes also possibly populated. If the
#' object is a function, its class becomes **aka** and **function**.
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
seealso = NULL, example = NULL, url = NULL) {
  if (is.null(alias))
    alias <- deparse(substitute(obj))
  attr(comment, "src") <- alias
  if (!is.null(description))
    attr(comment, "description") <- as.character(description)
  if (!is.null(seealso))
    attr(comment, "seealso") <- as.character(seealso)
  if (!is.null(example))
    attr(comment, "example") <- as.character(example)
  if (!is.null(url))
    attr(comment, "url") <- url
  comment(obj) <- comment
  if (is.function(obj))
    class(obj) <- c("aka", "function")
  obj
}

# Internal function to cache the type of hyperlink that can be used
.hyperlink_type <- function() {
  if (rlang::is_interactive() && cli::ansi_has_hyperlink_support()) {
    types <- cli::ansi_hyperlink_types()
    if (isTRUE(types$help)) {
      res <- "help"
    } else if (isTRUE(types$href)) {
      res <- "href"
    } else {
      res <- "none"
    }
  } else {
    res <- "none"
  }
  if (is.null(getOption("hyperlink_type")))
    options(hyperlink_type = res)
  res
}

#' @export
#' @rdname aka
#' @param x An aka object
#' @param hyperlink_type The type of hyperlink supported. The default value
#' should be ok. Use `"none"` to suppress hyperlink, `"href"` for http(s)://
#' link that opens a web page, or `"help"` in RStudio to open the corresponding
#' help page in the Help tab.
#' @param ... Further arguments (not used yet)
#' @method print aka
print.aka <- function(x, hyperlink_type = getOption("hyperlink_type",
default = .hyperlink_type()), ...) {
  src <- attr(comment(x), "src")
  link <- switch(hyperlink_type,
    help = {
      # src must be package::helpage
      pkg_page <- strsplit(src, "::", fixed = TRUE)[[1]]
      # Assume package = base if not provided
      if (length(pkg_page) == 1L)
        pkg_page <- c("base", pkg_page)
      cli::style_hyperlink(src, "ide:help",
        params = c(package = pkg_page[1], topic = pkg_page[2]))
    },
    href = {
      url <- attr(comment(x), "url")
      if (is.null(url)) {
        src
      } else {
        cli::style_hyperlink(src, url)
      }
    },
    src)

  cat(cli::col_blue("\b = ", link, "()"))
  invisible(x)
}

#' @export
#' @rdname aka
#' @param object An aka object
#' @method str aka
str.aka <- function(object, ...) {
  cat("aka ", attr(comment(object), "src"), "()\n", sep = "")
}
