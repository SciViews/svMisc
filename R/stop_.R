#' Enhanced stop() and warning()
#'
#' @description
#' `stop_()` is a wrapper around `cli::cli_abort()` that provides more control
#' on the stop message and also provides nice formatting and glue interpolation.
#' This version uses [gettext()] to translate the message, on the contrary to
#' [cli::cli_abort()]. `warning_()` is similar to [warning()] but uses
#' `call. = FALSE` by default. Finally, `stop_top_call()` allows to tag from
#' where an error should be reported (see examples).
#'
#' @param ... One or more character strings with the error or warning message(s).
#'   Name them '*' =, 'i' =, 'v' =, 'x' = or '!' = to format message items.
#'   First message item is considered to be '!' by default.
#' @param call. Logical, whether to include the call in the warning message. Not
#'   used for `stop_()`.
#' @param domain see [gettext()]. If NA, messages will not be translated.
#' @param class The subclass of the error condition message
#' @param call The execution environment of a currently running function where
#'   the error should be reported from.
#' @param envir The environment where to evaluate the glue expressions.
#'
#' @returns `stop_()` and `warning_()` are invoked for their side-effects, but
#'   `stop_()` actually stops execution of the current code. `stop_top_call()`
#'   return the top call to be used for stop condition messages.
#' @export
#' @seealso [stop()], [warning()], [cli::cli_abort()], [gettext()]
#'
#' @examples
#' # If you want to include the error messages in the translation strings in
#' # your package, you have to rename `stop_()` into `stop()` and `warning_()`
#' # into `warning()`, because [tools::xgettext2pot()] will only pick up the
#' # later ones.
#' stop <- stop_
#' warning <- warning_
#'
#' # Call not integrated by default now
#' warning("just a test")
#' warning("just a test", call. = TRUE)
#'
#' if (FALSE) {# Avoid running code that generates errors automatically
#' # Correctly formatted stop messages
#' n <- "some text"
#' stop("{.var n} must be a numeric vector",
#'   x = "You've supplied a {.cls {class(n)}} vector.")
#'
#' n <- 1:18
#' stop("{.var n} must be a scalar numeric:",
#'   i = "There {?is/are} {length(n)} element{?s}.",
#'   x = "Indicate a single numeric, not: {n}.")
#'
#' # When issued from within a function, the function call is used in the error
#' test1 <- function(x) {
#'   stop("{.var n} must be a scalar numeric:",
#'     i = "There {?is/are} {length(x)} element{?s}.")
#' }
#' test1(1:3)
#'
#' # If another function calls `test1()`, error is still reported from test1:
#'
#' test2 <- function(x) {
#'   test1(x)
#' }
#' test2(1:3)
#'
#' # In such a case, it is better to report the error from `test2()`.
#' # You can do that by stating `._top_call_. <- TRUE` in the body of `test2()`.
#' test2 <- function(x) {
#'   .__top_call__. <- TRUE
#'   test1(x)
#' }
#' test2(1:3)
#' }# End of if(FALSE)
#'
#' rm(stop, warning)
stop_ <- function(..., call. = FALSE, domain = NULL, class = NULL,
  call = stop_top_call(2L), envir = parent.frame()) {
  # Note that call. is not use here!
  message <- gettext(..., domain = domain, trim = TRUE)
  cli_abort(message, class = class, call = call, .envir = envir)
}

#' @rdname stop_
#' @export
#' @param immediate. Logical, whether to issue the warning immediately even if
#' `getOption("warn") <= 0`. Note that this is not respected for condition
#' objects.
#' @param noBreaks. logical, indicating as far as possible the message should be
#'   output as a single line when `options(warn = 1)`.
warning_ <- function(..., call. = FALSE, immediate. = FALSE, noBreaks. = FALSE,
  domain = NULL) {
  base::warning(..., call. = call., immediate. = immediate.,
    noBreaks. = noBreaks., domain = domain)
}

#' @rdname stop_
#' @export
#' @param nframe The number of frames to go up the call stack to find the top
#' call for stop condition messages.
stop_top_call <- function(nframe = 2L) {
  env <- call <- parent.frame(nframe)
  max_frames <- sys.nframe()
  if (max_frames > nframe) {
    for (i in (nframe + 1):max_frames) {
      call <- env
      env <- parent.frame(i)
      if (!isTRUE(env$`.__top_call__.`))
        break
    }
  }
  call
}
