#' Get textual help on function or function arguments, or get a call tip
#'
#' @description Textual help on functions or their arguments is extracted for
#' text online help for a given function. By default, all arguments from the
#' online help are returned for `describe_args()`. If the file contains help for
#' several functions, one probably gets also some irrelevant information. Use of
#' 'args' to limit result is strongly encouraged. `args_tip()` provides a
#' human-readable textual description of function arguments in a better way than
#' `args()` does. It is primarily intended for code tips in GUIs. `call_tip()`
#' has a similar purpose to show how some code could be completed.
#'
#' @param fun A character string with the name of a function (several functions
#' accepted too for `describe_function()`.
#' @param args Either `NULL` (by default) to return the description of all
#' arguments from the corresponding man page, or a character vector with names
#' of the arguments to search for.
#' @param package A character string with the name of the package that contains
#' `fun`, or `NULL` for searching in all loaded packages.
#' @param lib.loc A character vector of directory names of \R libraries, or
#' `NULL`. The default value of `NULL` corresponds to all libraries currently
#' known. If the default is used, the loaded packages are searched before the
#' libraries.
#' @param name A string with the name of a function.
#' @param code A fraction of R code ending with the name of a function,
#' eventually followed by '('.
#' @param only.args Do we return only arguments of the function
#' (`arg1, arg2 = TRUE, ...`), or the full call, like
#' (`myfun(arg1, arg2 = TRUE, ...)`).
#' @param width Reformat the tip to fit to fit in that width, except if
#' `width = NULL`.
#' @param location If `TRUE` then the location (in which package the function
#' resides) is appended to the calltip between square brackets.
#' @param description If `TRUE` then a short description of the function is
#' added to the call_tip (in fact, the title of the corresponding help page, if
#' it exists).
#' @param methods If `TRUE` then a short message indicating if this is a generic
#' function and that lists, in this case, available methods.
#' @return A string with the description of the function or of its arguments, or
#' the calling syntax of the function, plus additional information depending on
#' the flags used. If the man page is not found, a vector of empty strings is
#' returned. Empty strings are also returned for arguments that are not found in
#' the man page.
#' @note `args_tip()` is supposed to display S3 and S4 methods, and primitives
#' adequately,... but this is not implemented yet in the current version! For
#' `call_tip()`, the use of `methods = TRUE` slows down the execution of the
#' function, especially for generic functions that have many methods like
#' `print()` or `summary()`.
#' @export
#' @seealso [completion()], [args()], [argsAnywhere()]
#' @keywords utilities
#' @concept graphical user interface (GUI) control
#' @examples
#' describe_function("ls", "base")
#' describe_function("library", "base")
#' describe_function("descFun", "svMisc")
#' describe_function("descArgs")
#'
#' describe_args("ls")
#' describe_args("library", args = c("package", "pos"))
#'
#' args_tip("ls")
#'
#' call_tip("myvar <- lm(")
describe_function <- function(fun, package, lib.loc = NULL) {
  if (!length(fun))
    return("")
  fun <- as.character(fun)
  l <- length(fun)
  if (missing(package) || is.null(package)) package <- ""
  package <- rep(package, length.out = l)

  # Create a vector of results
  res <- rep("", l)

  # Collect help for each function
  for (i in 1:l) {
    # Get location of the help file
    # We cannot just call help normally because otherwise it thinks
    # we are looking for package "package" so we create a call and eval it
    help_call <- call("help", fun[i], lib.loc = lib.loc, help_type = "text")
    if (package[i] != "") help_call[["package"]] <- package[i]
    file <- as.character(eval(help_call))
    if (length(file) > 0) {
      # Read the Rd file and get the title section out of it
      Rdoc <- getNamespace("utils")$.getHelpFile(file[1L])
      # Look for the \title tag
      j <- 0
      for (j in seq_along(Rdoc))
        if (attr(Rdoc[[j]], "Rd_tag") == "\\title") break
      if (j > 0) {
        desc <- as.character(Rdoc[[j]][[1]])
        desc <- sub("^[ \t]+", "", desc)
        desc <- sub("[ \t]+$", "", desc)
        res[i] <- desc
      }
    }
  }
  res
}

#' @export
#' @rdname describe_function
describe_args <- function(fun, args = NULL, package = NULL, lib.loc = NULL) {
  # We cannot just call help normally because otherwise it thinks
  # we are looking for package "package" so we create a call and eval it
  help_call <- call("help", fun, lib.loc = lib.loc, help_type = "text")
  if (!is.null(package)) help_call[["package"]] <- package
  file <- eval(help_call)

  # This is borrowed from utils::print.help_files_with_topic
  path <- dirname(file)
  dirpath <- dirname(path)
  pkgname <- basename(dirpath)
  RdDB <- file.path(path, pkgname)

  if (!file.exists(paste(RdDB, "rdx", sep = ".")))
    return(character(length(args)))

  rd <- getNamespace("tools")$fetchRdDB(RdDB, basename(file))

  # This is not exported from tools
  rd_tags <- function(Rd) {
    res <- sapply(Rd, attr, "Rd_tag")
    if (!length(res)) res <- character(0)
    return(res)
  }
  tags <- gsub("\\", "", rd_tags(rd), fixed = TRUE)

  if (!any(tags == "arguments"))
    return(character(length(args)))

  arguments <- rd[[which(tags == "arguments")[1]]]
  items <- arguments[rd_tags(arguments) == "\\item"]
  descriptions <- do.call(rbind, lapply(items, function(item) {
    names <- try(strsplit(item[[1]][[1]], "\\s*,\\s*", perl = TRUE)[[1]],
      silent = TRUE)
    if (inherits(names, "try-error")) {
      # This happens with the "..." argument
      names <- "..."
    }
    content <- paste(rapply(item[-1], as.character), collapse = "")
    cbind(names, rep.int(content, length(names)))
  }))

  if (is.null(args)) {
    structure(descriptions[, 2], names = descriptions[, 1])
  } else {
    sapply(args, function(a) {
      if (a %in% descriptions[, 1]) {
        descriptions[which(descriptions[, 1] == a)[1] , 2]
      } else ""
    })
  }
}

#' @export
#' @rdname describe_function
args_tip <- function(name, only.args = FALSE, width = getOption("width")) {
  # TODO: handle primitives and S3/S4 methods for generic functions
  ret <- try(res <- eval(parse(text = paste0("argsAnywhere(", name, ")"))),
    silent = TRUE)
  if (inherits(ret, "try-error") || is.null(res))
    return("")  # Function 'name' not found
  res <- deparse(res)
  res <- paste(res[-length(res)], collapse = "\n")
  if (isTRUE(only.args)) {
    res <- sub("^function *[(]", "", res)
    res <- sub(" *[)] *$", "", res)
  } else {
    res <- sub("^function *", name, res)
    res <- sub(" *$", "", res)
  }
  # Reflow the tip
  if (!is.null(width))
    res <- paste(strwrap(res, width = width, exdent = 4), collapse = "\n")
  res
}

#' @export
#' @rdname describe_function
call_tip <- function(code, only.args = FALSE, location = FALSE,
description = FALSE, methods = FALSE, width = getOption("width")) {
  code <- attr(completion(code, types = NA, description = FALSE), "fguess")
  if (is.null(code) || !length(code) || code == "")
    return("")

  # Get the corresponding call_tip
  ctip <- args_tip(code, only.args = only.args, width = NULL) # Reflow later!
  if (is.null(ctip))
    return("")

  # Do we need to append an indication about where this function is located?
  if (isTRUE(location)) {
    where <- res <-
      eval(parse(text = paste0("getAnywhere(", code, ")")))$where[1]
    if (!is.na(where) && where != ".GlobalEnv")
      ctip <- paste0(ctip, " [", sub("^package:", "", where), "]")
  }

  # Reflow the tip now
  if (!is.null(width))
    ctip <- paste(strwrap(ctip, width = width, exdent = 4), collapse = "\n")

  # Do we add the description of this function?
  if (isTRUE(description)) {
    desc <- describe_function(code)
    if (!is.null(desc) && length(desc) && desc != "") {
      if (!is.null(width))
        desc <- paste(strwrap(desc, width = width), collapse = "\n")
      ctip <- paste0(ctip, "\n\n", desc)
    }
  }

  # Do we add a short mention of available methods if the function is generic?
  if (isTRUE(methods)) {
    mets <- list_methods(code)
    if (length(mets)) {
      # How many 25 char strings can we put on width and 5 lines max?
      # Note: we use two space each time as separator, except for last
      # line => take this into account in the calculation
      if (is.null(width)) nitems <- 3 else nitems <- (width + 2) %/% 27
      if (nitems < 1) nitems <- 1

      # Make sure the list is not too long: restrict to nitems * 5 entries
      if (length(mets) > nitems * 5) mets <- c(mets[1:(nitems * 5)], "...")

      # Make sure each method description is not longer than 25 characters
      n <- nchar(mets)
      # Cut entries that are too long
      tooLong <- n > 25
      mets[tooLong] <- paste0(substr(mets[tooLong], 1, 22), "...")

      # Paste strings together
      mets <- paste0(format(mets, width = 25), c(rep("  ", nitems - 1), "\n"),
        collapse = "")
      # Add this info to the call_tip
      ctip <- paste0(ctip,
        "\n\nGeneric function with methods for the following classes:\n", mets)
    }
  }
  ctip
}


# Backward compatibility

#' @export
#' @rdname describe_function
descFun <- describe_function

#' @export
#' @rdname describe_function
descArgs <- describe_args

#' @export
#' @rdname describe_function
argsTip <- args_tip

#' @export
#' @rdname describe_function
callTip <- call_tip
