#' A very silent and multipackage require() function
#'
#' @description This function loads one or several R packages as silently as
#' possible and it returns `TRUE` only if all packages are loaded successfully.
#' If at least one loading fails, a short message is printed.
#'
#' @param ... The name of one or several R packages to load (character strings).
#' @param warn If `TRUE`, issue a warning if one or several packages are not
#' loaded.
#' @return `TRUE` if all packages are loaded correctly, \code{FALSE} otherwise.
#' This function is designed to concisely and quitely indicate package
#' requirements in GUI menu or other GUI actions.
#' @export
#' @seealso [require()], [library()]
#' @keywords utilities
#' @concept package requirement and loading
#' @examples
#' # This should work...
#' if (all(package("tools", "methods"))) message("Fine!")
#' # ... but this not
#' if (!all(package("tools", "badname", warn = FALSE))) message("Not fine!")
package <- function(..., warn = TRUE) {
  # A multiple require proceeding as silently as possible
  # Suppress packages messages as much as possible
  owarn <- getOption("warn")
  options(warn = -1)  # Suppress warnings
  on.exit(options(warn = owarn))
  args <- unlist(list(...))
  l <- length(args)
  check <- rep(TRUE, l)
  if (l > 0)
    for (i in 1:l)
      check[i] <- suppressPackageStartupMessages(require(args[i],
        quietly = TRUE, character.only = TRUE, warn.conflicts = FALSE))
  if (!all(check) && isTRUE(warn)) {
    bads <- args[!check]
    options(warn = owarn)
    if (length(bads) == 1) {
      warning("Unable to load package ", bads, "!\n")
    } else {
      warning("Unable to load package(s): ",
        paste(bads, collapse = ", "), "!\n")
    }
  }
  invisible(check)
}
