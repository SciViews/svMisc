#' Get a system file or directory
#'
#' @description Get system files or directories, in R subdirectories, in package
#' subdirectories, or elsewhere on the disk (including executables that are
#' accessible on the search path).
#'
#' @param ... One or several executables if `exec = TRUE`, or subpath to a file
#' or dir in a package directory if `package != NULL`, or a list of paths and
#' subpaths for testing the existence of a file on disk, or a list of directory
#' components to retrieve in 'temp', 'sysTemp', 'user', 'home', 'bin', 'doc',
#' 'etc' and/or 'share' to retrieve special system directories.
#' @param exec If `TRUE` (default) search for executables on the search path.
#' It superseedes all other arguments.
#' @param package The name of one package to look for files or subdirs in its
#' main directory (use `exec = FALSE` to search inside package dirs).
#' @param lib.loc A character vector with path names of \R libraries or `NULL`
#' (search all currently known libraries in this case).
#' @return A string with the path to the directories or files, or `""` if they
#' are not found, or of the wrong type (a dir for `system_file()` or or a file
#' for `system_dir()`).
#' @note These function aggregate the features of several \R functions in
#' package base: [system.file()], [R.home()], [tempdir()], [Sys.which()], and
#' aims to provide a unified and convenient single interface to all of them. We
#' make sure also to check that returned components are respectively directories
#' and files for `system_dir()` and `system_file()`.
#' @export
#' @seealso [file_edit()], [file.path()], [file.exists()]
#' @keywords utilities
#' @concept system files and directories
#' @examples
#' system_file("INDEX", package = "base")
#' system_file("help", "AnIndex", package = "splines")
#' system_file(package = "base")  # This is a dir, not a file!
#' system_file("zip", exec = TRUE)
#' system_file("ftp", "ping", "zip", "nonexistingexe", exec = TRUE)
#' system_dir("temp")             # The R temporary directory
#' system_dir("sysTemp")          # The system temporary directory
#' system_dir("user")             # The user directory
#' system_dir("home", "bin", "doc", "etc", "share")  # Various R dirs
#' system_dir("zip", exec = TRUE) # Look for the dir of an executable
#' system_dir("ftp", "ping", "zip", "nonexistingexe", exec = TRUE)
#' system_dir(package = "base")   # The root of the 'base' package
#' system_dir(package = "stats")  # The root of package 'stats'
#' system_dir("INDEX", package = "stats") # This is a file, not a dir!
#' system_dir("help", package = "splines")
system_file <- function(..., exec = FALSE, package = NULL, lib.loc = NULL) {
  # First look if exec is TRUE
  if (isTRUE(exec)) {
    res <- Sys.which(as.character(unlist(list(...))))
    if (length(res) == 1) res <- as.character(res)
  } else if (!is.null(package)) {
    # A file in a package
    res <- system.file(..., package = package, lib.loc = lib.loc)
    # Check that this is a directory
    if (!file_test("-f", res)) res <- ""
  } else {
    # Look if this file exists and is a file
    file <- as.character(unlist(list(...)))
    file <- file.path(file)
    if (file_test("-f", file)) res <- normalizePath(file) else res <- ""
  }
  res
}

#' @export
#' @rdname system_file
system_dir <- function(..., exec = FALSE, package = NULL, lib.loc = NULL) {
  # First look if exec is TRUE
  if (isTRUE(exec)) {
    files <- Sys.which(as.character(unlist(list(...))))
    # Note: Sys.which() does not always return "" for items not found!
    res <- dirname(files)
    res[res == "."] <- ""
    if (length(res) > 1) names(res) <- names(files)
  } else if (!is.null(package)) {
    # A directory in a package
    res <- system.file(..., package = package, lib.loc = lib.loc)
    # Check that this is a directory
    if (!file_test("-d", res)) res <- ""
  } else {
    # A predefined directory
    which <- as.character(unlist(list(...)))

    # This is a specific directory
    get_dir <- function(which = c("temp", "sysTemp", "user", "home", "bin",
      "doc", "etc", "share")) {
      which = match.arg(which)
      switch(which,
        "temp" = tempdir(),
        "sysTemp" = if (!isWin() && file_test("-d", "/tmp")) "/tmp" else
          dirname(tempdir()),
        "user" = file_path_as_absolute("~"), # From tools package
        "home" = R.home("home"),
        "bin" = R.home("bin"),
        "doc" = R.home("doc"),
        "etc" = R.home("etc"),
        "share" = R.home("share"))
    }

    if (is.null(which) || length(which) == 0) {
      return(character(0))
    } else {
      res <- character(length(which))
      if (length(which) > 1) names(res) <- which
      for (i in seq_along(which))
        res[i] <- get_dir(which[i])
    }
  }
  res
}

# Backward compatibility

#' @export
#' @rdname system_file
systemFile <- system_file

#' @export
#' @rdname system_file
systemDir <- system_dir
