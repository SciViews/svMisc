#' A (possibly) very silent and multi-package library()/require() function
#'
#' @description This function loads one or several R packages as silently as
#' possible (with `warn/message = FALSE`) and it returns `TRUE` only if all
#' packages are loaded successfully. If at least one loading fails, a short
#' message is printed, by default. For all packages that were not found, an
#' entry is recorded in `.packages_to_install` in `SciViews:TempEnv`, and that
#' list can be automatically used by [Install()].
#'
#' @param ... The name of one or several R packages to load (character strings).
#' @param stop If `TRUE`, issue an error in case the package(s) cannot be
#' loaded.
#' @param message Do we display introductory message of the package? If a
#' package displays such a message, there is often a good reason. So, it is
#' **not** a good idea to disable it in _interactive_ sessions. However, in
#' other contexts, like in non-interactive use, inside an R Markdown document,
#' etc., it is more convenient not to display it.
#' @param warn.conflicts As for [library()]: "logical. If TRUE, warnings are
#' printed about conflicts from attaching the new package. A conflict is a
#' function masking a function, or a non-function masking a non-function.
#' @param pos As for [library()]: "the position on the search list at which to
#' attach the loaded namespace. Can also be the name of a position on the
#' current search list as given by [search()]". Only one position can be
#' provided here, even if several packages, and they will be all inserted one
#' after the other at the given position.
#' @param lib.loc As for [library()]: "a character vector describing the
#' location of \R library trees to search through, or `NULL`. The default value
#' of `NULL` corresponds to all libraries currently known to [.libPaths()].
#' Non-existent library trees are silently ignored".
#' @param verbose A logical indicating if additional diagnostic messages are
#' printed.
#' @return `TRUE` if all packages are loaded correctly, `FALSE` otherwise, with
#' a `details` attribute indicating which package was loaded or not.
#' @note This function is designed to concisely and possibly quietly (with
#' `warn = FALSE`) load packages and attach them to the search path. Also, on
#' the contrary to [library()], or [require()], it is **not** possible to use
#' unquoted names of the packages. This is cleaner, and avoids the contrived
#' work-around to pass name(s) of packages as a variable with an arguments
#' `character.only = TRUE`!
#'
#' If several packages are provided, they are loaded and attached in reverse
#' order, so that the order in the search path is the same one as the order in
#' the provided vector.
#'
#' The `library(help = ...)` version is not implemented here.
#' @export
#' @seealso [require()], [library()], [Install()]
#' @keywords utilities
#' @concept package requirement and loading
#' @examples
#' # This should work...
#' if (package('tools', 'methods', stop = FALSE)) message("Fine!")
#' # ... but this not (note that there are no details here!)
#' if (!package('tools', 'badname', stop = FALSE)) message("Not fine!")
#' \dontrun{
#' # Get an error
#' package('badname')
#' }
package <- function(..., stop = TRUE, message = stop, warn.conflicts = message,
pos = 2L, lib.loc = NULL, verbose = getOption("verbose")) {

  owarn <- getOption("warn")
  options(warn = -1)  # Suppress warnings
  on.exit(options(warn = owarn))

  pkgs <- unlist(list(...))
  l <- length(pkgs)
  if (l < 1)
    return(library()) # Same as library invoked without arguments (list of pkgs)

  if (length(pos) > 1) {
    warning("more than one 'pos' provided; Using only the first one")
    pos <- pos[1]
  }

  check <- rep(TRUE, l)
  names(check) <- pkgs

  if (message) suppressPackageStartupMessages <- function(expr) expr

  for (i in l:1)
    check[i] <- suppressPackageStartupMessages(suppressWarnings(
      tryCatch(
        library(pkgs[i], pos = pos, lib.loc = lib.loc,
          character.only = TRUE, logical.return = TRUE,
          warn.conflicts = warn.conflicts, quietly = TRUE, verbose = verbose)
        , error = function(e) e)))
  res <- structure(all(check), details = check)

  if (!res) {
    bads <- pkgs[!check]
    # Record the list of packages that were not found for easier Install()
    to_install <- get_temp('.packages_to_install', default = character(0))
    to_install <- unique(c(bads, to_install))
    assign_temp('.packages_to_install', to_install, replace.existing = TRUE)

    options(warn = owarn)
    if (stop) {
      if (length(bads) == 1) {
        stop("Unable to load package '", bads,
          "'!\nUse `Install()` to make it available...")
      } else {
        stop("Unable to load package(s): '",
          paste(bads, collapse = "', '"),
          "'!\nUse `Install()` to make them available...")
      }
    }
  }
  invisible(res)
}
