#' Convert R object to and from RJSON specification
#'
#' @description RJSON is an object specification that is not unlike JSON, but
#' better adapted to represent \R objects (i.e., richer than JSON). It is also
#' easier to parse and evaluate in both \R and JavaScript to render the objects
#' in both languages. RJSON objects are used by SciViews to exchange data
#' between \R and SciViews GUIs like Komodo/SciViews-K.
#'
#' @param x Any \R object to be converted into RJSON (do not work with objects
#' containing C pointers, environments, promises or expressions, but should
#' work with almost all other \R objects).
#' @param attributes If `FALSE` (by default), a simple object is created by
#' ignoring all attributes. This is usally the suitable option to transfer data
#' to another language, like JavaScript that do not understand R attributes
#' anyway. With `attributes = TRUE`, the complete information about the object
#' is written, so that the object could be recreated (almost) identical when
#' evaluated in \R (but prefer [save()] and [load()] to tranfer objects between
#' \R sessions!).
#' @param rjson A string containing an object specified in RJSON notation. The
#' specification is evaluated in \R... and it can contain also R code. There is
#' no protection provided against execution of bad code. So, you must trust the
#' source!
#' @return For `to_rjson()`, a character string vector with the RJSON
#' specification of the argument.
#'
#' For `eval_rjson()`, the corresponding \R object in case of a pure RJSON
#' object specification, or the result of evaluating the code, if it contains \R
#' commands (for instance, a RJSONp -RJSON with padding- item where a RJSON
#' object is an argument of an \R function that is evaluated. In this case, the
#' result of the evaluation is returned).
#'
#' For `list_to_json()`, correct (standard) JSON code is generated if `x` is a
#' list of character strings, or lists.
#' @details JSON (JavaScript Object Notation) allows to specify fairly complex
#' objects that can be rather easily exchanged between languages. The notation
#' is also human-readable and not too difficult to edit manually (although not
#' advised, of course). However, JSON has too many limitations to represent \R
#' objects (no `NA` versus `NaN`, no infinite numbers, no distinction between
#' lists and objects with attributes, or S4 objects, etc.). Moreover, JSON is
#' not very easy to interpret in \R and the existing implementations can convert
#' only specified objects (simple objects, lists, data frames, ...).
#'
#' RJSON slighly modifies and enhances JSON to make it: (1) more complete to
#' represent almost any \R object (except objects with pointers, environments,
#' ..., of course), and (2) to make it very easy to parse and evaluate in both
#' \R and JavaScript (and probably many other) languages.
#'
#' With `attributes = FALSE`, factors and Dates are converted to their usual
#' character representation before encoding the RJSON object. If
#' `attributes = TRUE`, they are left as numbers and their attributes (class,
#' -and levels for factor-) completely characterize them (i.e., using
#' `eval_rjson()` and such objects recreate factors or Dates, respectively).
#' However, they are probably less easy to handle in JavaScript of other
#' language where you import the RJSON representation.
#'
#' Note also that a series of objects are not yet handled correctly. These
#' include: complex numbers, the different date flavors other that Date,
#' functions, expressions, environments, pointers. Do not use such items in
#' objects that you want to convert to RJSON notation.
#'
#' A last restriction: you cannot have any special characters like linefeed,
#' tabulation, etc. in names. If you want to make your names most compatible
#' with JavaScript, note that the dot is not allowed in syntactically valid
#' names, but the dollar sign is allowed.
#' @export
#' @seealso [parse_text()]
#' @keywords utilities
#' @concept object serialization, JavaScript Object Notation
#' @examples
#' # A complex R object
#' obj <- structure(list(
#'   a = as.double(c(1:5, 6)),
#'   LETTERS,
#'   c = c(c1 = 4.5, c2 = 7.8, c3 = Inf, c4 = -Inf, NA, c6 = NaN),
#'   c(TRUE, FALSE, NA),
#'   e = factor(c("a", "b", "a")),
#'   f = 'this is a "string" with quote',
#'   g = matrix(rnorm(4), ncol = 2),
#'   `h&$@` = data.frame(x = 1:3, y = rnorm(3),
#'     fact = factor(c("b", "a", "b"))),
#'   i = Sys.Date(),
#'   j = list(1:5, y = "another item")),
#'   comment = "My comment",
#'   anAttrib = 1:10,
#'   anotherAttrib = list(TRUE, y = 1:4))
#'
#' # Convert to simplest RJSON, without attributes
#' rjson1 <- to_rjson(obj)
#' rjson1
#' eval_rjson(rjson1)
#'
#' # More complex RJSON, with attributes
#' rjson2 <- to_rjson(obj, TRUE)
#' rjson2
#' obj2 <- eval_rjson(rjson2)
#' obj2
#' # Numbers near equivalence comparison (note: identical(Robj, Robj2) is FALSE)
#' all.equal(obj, obj2)
#'
#' rm(obj, obj2, rjson1, rjson2)
to_rjson <- function(x, attributes = FALSE) {
  # TODO: complex => character + how to restore complex numbers with
  # attributes = TRUE?
  # TODO: check dates, and manage other date formats than Date!
  # TODO: convert functions, expressions into string, and how to include JS
  # code? or R code?
  # TODO: allow for special characters \b, \n, \r, \f, \t, \" in names!
  # TODO: environment and proto
  # This is derived from dput()
  file <- file()
  on.exit(close(file))
  # Martin Maechler suggested 'niceNames' used fro R >= 3.5.0
  opts <- c(if (getRversion() >= "3.5") "niceNames",
    if (isTRUE(attributes)) "showAttributes", "S_compatible")

  # Non-named list items are not allowed => make sure we give names to these
  # Also if attributes == FALSE, we use the string representation of factors
  rework <- function(x, attributes = FALSE) {
    if (is.list(x) && length(x)) {
      # Make sure all items have names, and use [[x]] for unnamed items
      i <- paste("[[", 1:length(x), "]]", sep = "")
      n <- names(x)
      if (is.null(n)) {
        n <- i
      } else {
        no_names <- n == ""
        n[no_names] <- i[no_names]
      }
      # Flag names with leading and trailing sequence (unlikely elsewhere)
      n <- paste0("@&#&&", n, "&&#&@")
      # Change names of x
      names(x) <- n
      # If we don't use attributes, convert factors and Dates to characters
      if (!isTRUE(attributes))
        x <- rapply(x, as.character, classes = c("factor", "Date"),
          how = "replace")

      # Do this recursively
      for (item in names(x))
        x[[item]] <- rework(x[[item]], attributes)
    } else if (!isTRUE(attributes) && inherits(x, c("factor", "Date"))) {
      x <- as.character(x)
    }

    # Process also all attributes
    if (isTRUE(attributes)) {
      a <- attributes(x)
      if (!is.null(a)) {
        n <- names(x)
        a$.Names <- NULL
        a$names <- NULL
        na <- names(a)
        if (length(na)) {
          for (item in na)
            a[[item]] <- rework(a[[item]], attributes)
          # Tag attributes names and translate a few special ones
          specials <- c(".Dim", ".Dimnames", ".Tsp", ".Label")
          replace <- c("dim", "dimnames", "tsp", "levels")
          m <- match(na, specials)
          ok <- (!is.na(m) & m)
          na[ok] <- replace[m[ok]]
          names(a) <- paste0("@&#&&", na, "&&#&@")
        }
        attributes(x) <- a
        names(x) <- n
      }
    }
    x
  }

  # FIXME eventually: In R 3.5.0,  dput() works for S4 objects
  # Is this an S4 object => process each slot separately
  if (isS4(x)) {
    cat('list("Class_" := "', class(x), '"\n', file = file, sep = "")
    for (n in slotNames(x)) {
      cat('    , "', n, '" := ', file = file)
      dput(rework(slot(x, n), attributes), file = file, control = opts)
    }
    cat(")\n", file = file)
    invisible()
  } else {
    dput(rework(x, attributes), file = file, control = opts)
  }

  # Now read content from the file
  res <- readLines(file)

  # dput() indicates sequences of integers with x:y that JavaScript cannot
  # process... replace these by the equivalent code seq(x, y)
  res <- gsub("(-?[0-9]+):(-?[0-9]+)", "seq(\\1, \\2)", res)

  # Convert '.Names = ' into '"names" := '
  res <- gsub(".Names = ", '"names" := ', res, fixed = TRUE)
  # We need to replace special characters
  # TODO: do so only inside `@&#&&...&&#&@`
# TODO: all this does not work!!!
#  res <- gsub('(`@&#&&.*)\b(.*&&#&@`)', '\\1\\\\b\\2', res)
#  res <- gsub('(`@&#&&.*)\t(.*&&#&@`)', '\\1\\\\t\\2', res)
#  res <- gsub('(`@&#&&.*)\n(.*&&#&@`)', '\\1\\\\n\\2', res)
#  res <- gsub('(`@&#&&.*)\f(.*&&#&@`)', '\\1\\\\f\\2', res)
#  res <- gsub('(`@&#&&.*)\r(.*&&#&@`)', '\\1\\\\r\\2', res)
#  res <- gsub('(`@&#&&.*)\"(.*&&#&@`)', '\\1\\\\"\\2', res)
  #res <- gsub('\t', '\\t', res, fixed = TRUE)
  #res <- gsub('\n', '\\n', res, fixed = TRUE)
  #res <- gsub('\f', '\\f', res, fixed = TRUE)
  #res <- gsub('\r', '\\r', res, fixed = TRUE)
  #res <- gsub('\"', '\\"', res, fixed = TRUE)
  # Convert `@&#&& into ", and &&#&@` = into " :=
  res <- gsub('"?`@&#&&', '"', res)
  res <- gsub('&&#&@`\"? =', '" :=', res)
  # Convert "@&#&&[[d]]&&#&@" to "" (non-named items)
  res <- gsub('"@&#&&\\[\\[[1-9][0-9]*]]&&#&@"', '""', res)
  # Convert "@&#&& into " and &&#&@" into "
  res <- gsub('"@&#&&', '"', res, fixed = TRUE)
  res <- gsub('&&#&@"', '"', res, fixed = TRUE)
  # No unnamed items, so, convert 'structure(' into 'list("Data_" := '
  res <- gsub("([^a-zA-Z0-9._])structure\\(", '\\1list("Data_" := ', res)
  res <- sub("^structure\\(", 'list("Data_" := ', res)
  # Old code!
  # Convert 'list(' into 'hash('
  #res <- gsub("([^a-zA-Z0-9._])list\\(", "\\1hash(", res)
  #res <- sub("^list\\(", "hash(", res)

  # Return  the no quoted results
  noquote(res)
}

#' @export
#' @rdname to_rjson
eval_rjson <- function(rjson) {
  # Our list() manages to create list() but also new() or structure() items
  list <- function(Class_, Data_, ...) {
    # If there is a "Class_" argument, create new S4 object
    # Note that "Data_" is ignored in this case!
    if (!missing(Class_))
      return(new(Class_, ...))
    # If there is a "_Data_" argument, create a structure
    if (!missing(Data_))
      return(structure(Data_, ...))
    # otherwise, create a list
    base::list(...)
	}

  # To convert RJSON data into a R object, simply evaluate it
  # Note: RJSONp objects will be evaluated correctly too
  # providing the <callback>() exists and can manage a single
  # argument (being the RJSOn object converted to R)

  # We need first to convert all ':=' into '='
  eval(parse(text = gsub(":=", "=", rjson, fixed = TRUE)))
}

#' @export
#' @rdname to_rjson
list_to_json <- function(x) {
  # Simple JSON for lists containing character strings
  if (!is.list(x) && length(x) == 1L)
    return(encodeString(x, quote = '"'))
  x <- lapply(x, list_to_json)
  x <- if (is.list(x) || length(x) > 1L) {
    nms <- names(x)
    if (is.null(nms)) {
      paste0('[', paste(x, collapse = ','), ']')
    } else {
      paste0("{",
        paste(
          paste0(encodeString(make.unique(nms, sep = '#'), quote = '"'),
            ":", x),
          collapse = ","),
        "}")
    }
  }
  x
}

# Backward compatibility

#' @export
#' @rdname to_rjson
toRjson <- to_rjson

#' @export
#' @rdname to_rjson
evalRjson <- eval_rjson

#' @export
#' @rdname to_rjson
listToJson <- list_to_json
