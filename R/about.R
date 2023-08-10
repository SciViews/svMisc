#' Get information and help about \R objects
#'
#' @description Help obtained with this function is wider than with [help()]. If
#' a man page is not found, it suggests related topics. If an object is an S3
#' generic function, it also lists all its known methods. Also, one can track
#' the help page of an object even if its name is changed, by using the `src` or
#' `srcfile` attribute of the object's comment. By the way, if the object has a
#' comment, it is also displayed. This can be used as a quick and dirty way to
#' provide short hints to custom objects. Finally, it is possible to track down
#' the source of an object into a file with the `srcfile` attribute of its
#' comment. In this case, it is the source file that is displayed. So, you can
#' also further document your custom objects easily in their source files!
#'
#' @param topic The name of an object, or the topic to search for, if this is
#' not the name of a known object.
#' @param ... Further arguments passed to [help()].
#' @return A string with the location of all objects named `topic` are found is
#' returned invisibly.
#' @export
#' @seealso [help()], [help.search()], [apropos()]
#' @keywords utilities
#' @concept help and information about objects
#' @examples
#' \dontrun{
#' about("nonexisting") # Not found on search path, but help pages
#' about("htgdsfgfdsgf") # Not found anywhere
#' #library(tidyverse)
#' #about("group_by") # Just one page
#' #about("filter") # Several items
#' about("stats::filter") # OK
#' #about("dplyr::filter") # OK too
#' about("base::filter") # Not found there
#' # Objects with comment: print comment
#' vec <- structure(1:10, comment = "A simple vector")
#' about("vec")
#' # If there is a srcfile attribute in the comment, also display the file
#' # Hint: integrate some help in the header!
#' #library(data)
#' #(iris <- read(data_example("iris.csv")))
#' #about("iris")
#' # If the comment has a src attribute, change the topic to that one
#' #urchin <- read("urchin_bio", package = "data")
#' #about("urchin")
#' .?filter
#' .?stats::filter
#' }
about <- function(topic, ...) {
  if (!is.character(topic) || length(topic) != 1 || nchar(topic) < 1)
    stop("topic must be a single character string")

  # Is topic like pkg::topic?
  pkg_topic <- strsplit(topic, "::", fixed = TRUE)[[1]]
  if (length(pkg_topic) == 1) {
    package <- NULL
    where <- find(topic)
  } else {# topic provided as pkg::topic
    package <- pkg_topic[1]
    topic <- pkg_topic[2]
    where <- paste0("package:", package)
    if (!exists(topic, where = where, inherits = FALSE))
      where <- character(0)
  }

  nitems <- length(where)
  if (!nitems) {# Nothing found... use apropos() and help.search()
    # Look for similar objects as topic using apropos()
    found <- apropos(topic, where = TRUE)
    if (length(found)) {
      if (is.null(package)) {
        message("'", topic, "' not found, do you mean?")
      } else {
        message("'", topic, "' not found in package '", package,
          "', do you mean?")
      }
      locations <- search()[as.numeric(names(found))]
      locations[!grepl("^package:", locations)] <- ""
      locations <- sub("^package:(.+)$", "\\1::", locations)
      message(paste0(locations, found, collapse = ", "))
    } else {
      if (is.null(package)) {
        message("'", topic, "' not found")
      } else {
        message("'", topic, "' not found in package '", package, "'")
      }
    }
    message("Searching keyword in all R help pages for '", package, "'...")
    print(help.search(topic, package = package, ...))

  } else {# At least one found
    if (nitems > 1) {
      message("'", topic, "' was found multiple times in:")
      message(paste0(where, collapse = ", "))
      message("Hint: use 'pkg::topic' to be more accurate")
    }

    # Is there comments, and is it a 'src' attribute too to substitute topic?
    obj <- get(topic)
    info <- comment(obj)
    # Note: hard-coded for now, but these strings should be trnaslated in a regular way!
    lang <- Sys.getenv("language", unset = "en")
    description <- attr(info, "description")
    if (!is.null(description))
      writeLines((description))
    seealso <- attr(info, "seealso")
    if (!is.null(seealso)) {
      cat("\n")
      if (lang == "fr") {
        cat("- Voir aussi : ")
      } else {
        cat("- See also: ")
      }
      cat(paste(seealso, collapse = ", "), "\n", sep = "")
    }
    example <- attr(info, "example")
    if (!is.null(example)) {
      cat("\n")
      if (lang == "fr") {
        cat("- Exemples (taper `ex` pour les lancer) :\n")
      } else {
        message("- Examples (type `ex` to run them):\n")
      }
      assign_temp(".last.example", example)
      writeLines(example)
      cat("\n")
    }
    if (!is.null(info)) {
      if (length(info) != 1 || info != "") {
        cat("\n")
        if (lang == "fr") {
          cat("- Commentaire :\n")
        } else {
          cat("- Comment:\n")
        }
        writeLines(info)
      }
      # Is there a 'src_file' attribute?
      src_file <- attr(info, "srcfile")
      if (!is.null(src_file)) {
        message("'", topic, "' comes from '", src_file, "'. ")
        message("Displaying that file...")
        file.show(src_file, title = topic)
        return(invisible(character(0))) # The object has no R help page

      } else {# Look for a different help page in 'src' attribute
        src_topic <- attr(info, "src")
        if (!is.null(src_topic)) {
          message("Matching help page is '", src_topic, "'")
          topic <- src_topic
          pkg_topic <- strsplit(topic, "::", fixed = TRUE)[[1]]
          if (length(pkg_topic) == 1) {
            package <- NULL
          } else {# topic provided as pkg::topic
            package <- pkg_topic[1]
            topic <- pkg_topic[2]
          }
        }
      }
    }

    # Is there an help page for this topic?
    if (do.call(is_help, list(topic, package = package))[["help"]]) {
      meths <- try(methods(topic), silent = TRUE)
      if (!inherits(meths, "try-error") && length(meths)) {
        message("Possible methods for '", topic, "':")
        print(meths)
      }
      message("Displaying help page...")
      print(do.call(help, list(topic, package = package, ...)))
    } else {# Search R help for it
      message("Searching all R help pages...")
      print(help.search(topic, ...))
    }
  }
  invisible(where)
}

#' @export
#' @rdname about
#' @param type First argument to `?`. If it is a dot, like `.?topic`, the second
#' argument is a topic passed to the `about()` function. Otherwise, it is the
#' first argument to restrict help pages, like `class`, `methods`, or `method`.
#' See examples for how to use it.
`?` <- function(type, topic) {
  type <- substitute(type)
  if (missing(topic)) {
    do.call(utils::`?`, list(type))
  } else {
    topic <- substitute(topic)
    if (type == ".") {
      about(deparse(topic))
    } else {
      do.call(utils::`?`, list(type, topic))
    }
  }
}

#' @rdname about
#' @export
ex <- structure(function() {
  ex <- get_temp(".last.example")
  if (is.null(ex))
    return()
  source(textConnection(ex), echo = TRUE)
}, class = c("runnable", "function"))

#' @rdname about
#' @param x The name of a function.
#' @export
print.runnable <- function(x, ...) {
  x()
  invisible(x)
}
