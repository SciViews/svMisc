#' Parse a character string as if it was a command entered at the command line
#'
#' @description Parse R instructions provided as a string and return the
#' expression if it is correct, or an object of class 'try-error' if it is an
#' incorrect code, or `NA` if the (last) instruction is incomplete.
#'
#' @param text The character string vector to parse into an R expression.
#' @param firstline The index of first line being parsed in the file. If this is
#' larger than `1`, empty lines are added in front of `text` in order to match
#' the correct position in the file.
#' @param srcfilename A character string with the name of the source file.
#' @param encoding Encoding of `text``, as in [parse()].
#' @return Returns an expression with the parsed code or `NA` if the last
#' instruction is correct but incomplete, or an object of class 'try-error' with
#' the error message if the code is incorrect.
#' @note On the contrary to `parse()`, `parse_text()` recovers from incorrect
#' code and also detects incomplete code. It is also easier to use in case you
#' pass a character string to it, because you don't have to name the argument
#' explicitly (`text = ...`).
#' @export
#' @seealso [parse()], [capture_all()]
#' @keywords IO
#' @examples
#' parse_text("1 + 1")
#' parse_text("1 + 1; log(10)")
#' parse_text(c("1 + 1", "log(10)"))
#'
#' # Incomplete instruction
#' parse_text("log(")
#'
#' # Incomplete strings
#' parse_text("text <- \"some string")
#' parse_text("text <- 'some string")
#'
#' # Incomplete names (don't write backtick quoted names on several lines!)
#' # ...but just in case
#' parse_text("`myvar")
#'
#' # Incorrect expression
#' parse_text("log)")
parse_text <- function(text, firstline = 1, srcfilename = NULL,
encoding = "unknown") {
  # Parse R instructions provided as a string and return the expression if it
  # is correct, or a 'try-error' object if it is an incorrect code, or NA if
  # the (last) instruction is incomplete
  text <- paste(text, collapse = "\n")
  # if firstline is higher than 1, "align" code by prepending empty codes
  firstline <- as.integer(firstline)[1]
  if (firstline > 1)
    text <- paste(c(rep("", firstline - 1), text), collapse = "\n")
  if (is.null(srcfilename)) srcfilename <- "<text>"
  res <- tryCatch(parse(text = text, srcfile = srcfilecopy(srcfilename, text),
    encoding = encoding), error = identity)

  if (inherits(res, "error")) {
    # Check if this is incomplete code
    msg <- conditionMessage(res)

    # Incomplete string
    if (regexpr(gettext("INCOMPLETE_STRING", domain = "R"), msg) > 0)
      return(NA)

    # Incomplete instruction
    if (regexpr(gettext("end of input", domain = "R"), msg) > 0)
      return(NA)

    # This should be incorrect R code
    # Rework the message a little bit... keep line:col position in front

    # TODO: from SciViews-K-dev:
    # This reformats the message as it would appear in the CLI:
    #errinfo <-
    #  strsplit(sub("(?:<text>:)?(\\d+):(\\d+): +([^\n]+)\n([\\s\\S]*)$",
    #    "\\1\n\\2\n\\3\n\\4", msg, perl = TRUE), "\n", fixed = TRUE)[[1]]

    #errpos <- as.numeric(errinfo[1:2])
    #err <- errinfo[-(1:3)]
    #rx <- sprintf("^%d:", errpos[1])
    #errcode <- sub(rx, "", err[grep(rx, err)])
    #err <- simpleError(sprintf("%s in \"%s\"", errinfo[3], errcode))

    # -or-

    err <- res
    err$message <- res <- sub("^<.*>:", "", msg)
    # Call is from instructions in "text"... but from the corresponding line
    err$call <- strsplit(text, "\n")[[1]][as.integer(
      sub("^[^0-9]*([0-9]+):.*$", "\\1", res))]

    # ... until here...

    # Return a try-error object to remain compatible with previous versions
    # TODO: from SciViews-K-dev:
    #res <- .makeMessage(res)
    class(res) <- "try-error"
    attr(res, 'error') <- err
  }

  res
}

# Backward compatibility

#' @export
#' @rdname parse_text
parseText <- parse_text
