#' List all methods associated with a generic function or a class, or all types
#' associated with a method
#'
#' @description List all S3 and/or S4 methods for a generic function or for a
#' class. List all types for a method; types are variants for a given method
#' defined in a way it is easy to add other variants dynamically (on the
#' contrary to a usual `type =` or `which =` argument, like in [plot.ts()] or
#' [plot.lm()], respectively.
#'
#' @param f The name of the generic function (character string), used only when
#' `class = NULL`.
#' @param method The method name.
#' @param class The name of a class.
#' @param S3 If `TRUE`, list of S3 methods.
#' @param S4 If `TRUE`, list of S4 methods.
#' @param mixed If `TRUE`, S3 and S4 methods are mixed together in a character
#' vector, otherwise, S3 and S4 methods are reported separately in a list.
#' @param filter A list of methods to consider when listing class methods. Only
#' classes in this list that are defined for the class are returned. Store the
#' list of methods you want in the options `"svGUI.methods"`. The package
#' proposes a reasonable starting point on loading if this option is not defined
#' yet.
#' @param strict Do we list only types for the class (\code{TRUE}), or all
#' possible types, including for inherited objects, and default ones `FALSE`,
#' by default)?
#' @return For `list_methods()`, if `mixed = TRUE`, a list with components:
#' - `S3` The S3 methods for the generic function or the class, or
#' `character(0)` if none
#' - `S4` The S4 methods for the generic function or the class, or
#' `character(0)` if none.
#'
#' Otherwise, a character vector with the requested methods.
#'
#' For `list_types()`, a vector with character strings with methods' type names.
#' @note `list_types()` is only useful for special generic functions with type
#' argument like `view`, `copy` or `export`. These functions offer a mechanism
#' to easily add custom types, and the present function list them. For S3
#' objects a type is simply a function whose name is : <method>_<type>.<class>.
#' So, adding new <type>s for <method> is very easy to implement.
#' @export
#' @seealso [obj_menu()]
#' @keywords utilities
#' @concept classes, objects and methods
#' @examples
#' # Generic functions
#' list_methods("t.test")               # S3
#' list_methods("show", mixed = FALSE)	# S4
#' list_methods("ls") # None, not a generic function!
#'
#' # Classes
#' # Only the following methods are considered
#' getOption("gui.methods")
#' list_methods(class = "data.frame")
#' list_methods(class = "lm")
#'
#' # List method types
#' list_types("view")  # All default view types currently defined
#' list_types("view", "data.frame")
#' list_types("view", "data.frame", TRUE) # None, except if you defined custom views!
list_methods <- function(f = character(), class = NULL, S3 = TRUE, S4 = TRUE,
mixed = TRUE, filter = getOption("svGUI.methods")) {
  # Given a function, if it is generic then return a list of its methods
  # or given a class name, return all methods for this class

  if (!inherits(f, "character"))
    stop("'f' must be a character string!")

  if (!is.null(class)) {
    class <- as.character(class)[1]
    res <- list()

    # S3 version
    if (isTRUE(S3)) {
      s3 <- unclass(methods(class = class))
      attr(s3, "info") <- NULL
      # Do we have to filter the methods?
      if (!is.null(filter))
        s3 <- s3[s3 %in% paste(filter, class, sep = ".")]
      res$S3 <- sub(paste(".", class, sep = ""), "", s3)
    }

    # S4 version
    if (isTRUE(S4)) {
      if (is.null(filter)) filter <- character()
      s4 <- capture.output(showMethods(filter, classes = class,
        inherited = FALSE, showEmpty = FALSE))
      # I need to filter this output to get only function names
      res$S4 <- sub("^.*\\: +([^ ]+).*$", "\\1",
        s4[regexpr("Function:", s4) == 1])
    }
    if (isTRUE(mixed)) res <- sort(unique(c(res$S3, res$S4)))
    return(res)

  } else {# List all methods for one generic function
    # Keep only first item if a vector is provided
    f <- f[1]
    res <- list()

    # S3 version
    if (isTRUE(S3)) {
      # Does the function exists somewhere?
      if (length(findFunction(f, where = .GlobalEnv)) > 0) {
        s3 <-  unclass(suppressWarnings(methods(f)))
        attr(s3, "info") <- NULL
        # Rework this to match presentation for S4 methods
        arg <- names(formals(eval(parse(text =
        paste("getAnywhere(", f, ")", sep = "")))[1]))[1]
        s3 <- sub(paste("^", f, ".", sep = ""), "", s3)
        if (length(s3) == 0 || s3 == "") {
          res$S3 <- character(0)
        } else {
          # Check all possible methods in turn, to verify them
          for (i in 1:length(s3))
            if (inherits(try(getS3method(f, s3[i]), silent = TRUE),
              "try-error"))
              s3[i] <- ""
          s3 <- s3[s3 != ""]
          if (length(s3) == 0) res$S3 <- character(0) else
            res$S3 <- paste(arg, "=\"", s3, "\"", sep = "")
        }
      } else {# Not found
        res$S3 <- character(0)
      }
    }

    # S4 version
    if (isTRUE(S4)) {
      # Is it an S4 generic function?
      if (isGeneric(f, where = .GlobalEnv)) {
        s4 <- capture.output(showMethods(f,
          inherited = FALSE, showEmpty = FALSE))
        res$S4 <- s4[-c(1, length(s4))]
      } else {
        res$S4 <- character(0)
      }
    }
    if (isTRUE(mixed)) res <- sort(unique(c(res$S3, res$S4)))
    return(res)
  }
}

#' @export
#' @rdname list_methods
list_types <- function(method, class = "default", strict = FALSE) {
  # List all custom functions for a method and for a given class
  # For instance, a custom view is a function as 'view_<customview>.class'

  make_list <- function(method, class) {
    pat <- paste("^", method, "_([^.]+)\\.", class, "$", sep = "")
    sub(pat, "\\1", apropos(pat, ignore.case = FALSE, mode = "function"))
  }

  method <- as.character(method[1])
  class <- as.character(class)
  types <- make_list(method, class[1])
  if (!isTRUE(strict)) {
    # Also include views for inherited classes
    if (l <- length(class) > 1)
      for (i in 2:l)
        types <- c(types, make_list(method, class[i]))
      # Also include default views
      if (class != "default")
        types <- c(types, make_list(method, "default"))

      types <- sort(unique(types))
  }
  types
}


# Backward compatibility

#' @export
#' @rdname list_methods
listMethods <- list_methods

#' @export
#' @rdname list_methods
listTypes <- list_types
