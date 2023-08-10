#' Create a section in a list (collection of functions and other objects).
#'
#' @description A section tags a list to sort its items. It is particularly
#' useful when you create a collection of function (or other objects) to ease
#' the access to these functions. Sections are displayed in printed and "str"ed
#' versions of the list and are also functions that cut the list to the section
#' content only. `get_section()` is the workhorse function that does the section
#' extraction.
#'
#' @param obj A list object.
#' @param title The title of the section. It must match the name of the list
#' item. For a title "My section title", the name must be "0__MY_SECTION_NAME__"
#' that is both a syntactically correct name and something that emphasizes the
#' entry as a title.
#'
#' @return A function that is able to extract the corresponding section from the
#' list.
#' @export
#'
#' @examples
#' #TODO...
section <- function(obj, title) {
  structure(function(x = obj) get_section(x, title),
    title = title, class = c("section", "function"))
}

#' @export
#' @rdname section
#' @param x A list containing the section
#' @param ... Further arguments (not used yet)
#' @method print section
print.section <- function(x, ...) {
  title <- attr(x, "title")
  # TODO...
  # This is for RStudio. In terminal, use: cat("XXXXXXXXXXX\n\033[1A\033[KY\n")
  back <- rep("\b", nchar(title) + 7L)
  cat(back, cli::col_red("o  ", title, "   "), sep = "")
  invisible(x)
}

#' @export
#' @rdname section
#' @param object A list to use for section extraction
#' @method str section
str.section <- function(object, ...) {
  cat("section\n")
}

#' @export
#' @rdname section
get_section <- function(x, title) {
  stopifnot(is.list(x), is.character(title), length(title) == 1L)

  # We need tp rework title, so that it matches a section name
  # Section title -> o__SECTION_TITLE__
  title <- toupper(title)
  title <- gsub(" ", "_", title, fixed = TRUE)
  title <- paste0("o__", title, "__")

  # Search the section in the list
  names <- names(x)
  l <- length(names)
  start <- which(names == title)
  if (!length(start)) # The section title is not found -> return an empty list
    return(list())
  end <- which(startsWith(names[(start + 1):l], "o__"))
  if (length(end)) {
    end <- end[1] + start - 1
  } else {
    end <- l
  }
  # Truncate the list
  sel <- start:end
  x[sel]
}
