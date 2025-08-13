#' Prepare a data_dot function
#'
#' @description Prepare a function that uses the data-dot mechanism. In case the
#' argument (usually, `.data = (.)`) is missing or is not a data frame in a call
#' to a "data-dot" function, it is recalled after injection `.` as first
#' argument.
#'
#' @param x An argument to check.
#' @param y A second argument.
#' @param is_top_call A logical indicating if this is a top-level call (`TRUE` by
#' default) that should be focused in the call stack in case of an error.
#' @param call A call object, usually a function call. Could be omitted, and in
#' this case, `sys.call()` is invoked.
#' @param arg The name of the argument to inject, usually '.data' (default). For
#' `prepare_data_dot2()`, it is `x` by default
#' @param arg2 The name of the second argument, `y` by default.
#' @param value The value to inject, usually the symbol `.` (default).
#' @param env The environment where the evaluation of the data-dot-injected call
#'  should be evaluated (by default, `parent.frame(2L)`, should rarely be
#'  changed).
#' @param abort_msg The message to use in case the '.data' argument is wrong.
#' @param abort_msg2 An additional message to append to the error message in
#'   case data-dot-injection is not permitted (when
#'   `.SciViews.implicit.data.dot != TRUE`, see example).
#' @param abort_frame The environment to use for the error message, by default,
#'   the caller environment (should rarely be changed).
#'
#' @details
#' The call is **not** checked if it is a correct function call. When called
#' from within a function, passing [sys.call()] as `call`, it should be always
#' correct.
#' `prepare_data_dot2()` and `recall_with_data_dot2()` are similar, but for
#' functions that have two first arguments that must be data frames, generally
#' called `x`and `y`.
#'
#' @returns `TRUE` if the preparation is correct for `prepare_dzata_dot()`,
#' `FALSE` otherwise. The result from evaluating the data-dot-injected call for
#' `recall_with_data_dot()`.
#' @export
#'
#' @examples
#' # Here is how you create a data-dot function
#' my_subset <- function(.data = (.), i, j) {
#'   # This makes it a data-dot function
#'   if (!prepare_data_dot(.data))
#'     return(recall_with_data_dot())
#'
#'   # Code of the function
#'   # Second argument (i here) must not be a data.frame to avoid confusion
#'   message(".env has ", paste(names(.env), collapse = ", "))
#'   .data[i, j]
#' }
#' dtf1 <- data.frame(x = 1:3, y = 4:6)
#' my_subset(dtf1, 1, 'y')
#' # If .data is in '.', it can be omitted
#' .= dtf1
#' my_subset(1, 'y')
#'
#' # This mechanism is potentially confusing. You can inactivate it anywhere:
#' .SciViews.implicit.data.dot <- FALSE
#' # This time next call is wrong
#' try(my_subset(1, 'y'))
#' # You must indicate '.' explicitly in that case:
#' my_subset(., 1, 'y')
#' rm(.SciViews.implicit.data.dot) # Reactivate it
#' my_subset(1, 'y') # Implicit again
#' # Note that, if you have not defined '.' and try to use it, you got
#' # an error:
#' rm(.)
#' try(my_subset(1, 'y'))
prepare_data_dot <- function(x, is_top_call = TRUE) {
  if (!missing(x) && inherits(x, 'data.frame')) {
    pf <- parent.frame()
    # Make sure . is usable inside the function
    pf$. <- x
    # Also install .env
    pf$.env <- pf
    # Set a placeholder for our stop() calls
    if (isTRUE(is_top_call))
      pf$.__top_call__. <- TRUE
    TRUE
  } else {
    FALSE
  }
}

#' @rdname prepare_data_dot
#' @export
prepare_data_dot2 <- function(x, y, is_top_call = TRUE) {
  if (!missing(x) && inherits(x, 'data.frame') &&
      !missing(y) && inherits(y, 'data.frame')) {
    pf <- parent.frame()
    # Make sure . and .. are usable inside the function
    pf$. <- x
    pf$.. <- y
    # Also install .env
    pf$.env <- pf
    # Set a placeholder for our stop() calls
    if (isTRUE(is_top_call))
      pf$.__top_call__. <- TRUE
    TRUE
  } else {
    FALSE
  }
}

#' @rdname prepare_data_dot
#' @export
recall_with_data_dot <- function(call, arg = '.data', value = as.symbol('.'),
  env = parent.frame(2L),
  abort_msg = gettextf("`%s` must be a 'data.frame'.", arg),
  abort_msg2 = gettext("Implicit data-dot (.) not permitted"),
  abort_frame = parent.frame()) {

  if (missing(call))
    call <- sys.call(-1L)

  if (isFALSE(.SciViews.implicit.data.dot))
    abort(c(abort_msg, i = abort_msg2), .frame = abort_frame)

  if (any(names(call) == arg))
    abort(abort_msg, .frame = abort_frame)

  # Inject arg = value in first position
  l <- length(call)
  if (l == 1) {
    call <- call[1:2] # Add a new argument in pos 2
  } else {
    call <- call[c(1, l + 1, 2:l)] # Add a new argument in pos 2
  }
  call[[2]] <- value
  names(call)[2] <- arg

  eval_bare(call, env)
}

#' @rdname prepare_data_dot
#' @export
recall_with_data_dot2 <- function(call, arg = 'x', arg2 = 'y',
  value = as.symbol('.'), env = parent.frame(2L),
  abort_msg = gettextf("`%s` and `%s` must both be 'data.frame'.", arg, arg2),
  abort_msg2 = gettext("Implicit data-dot (.) not permitted"),
  abort_frame = parent.frame()) {

  if (missing(call))
    call <- sys.call(-1L)

  if (isFALSE(.SciViews.implicit.data.dot))
    abort(c(abort_msg, i = abort_msg2), .frame = abort_frame)

  if (any(names(call) == arg))
    abort(abort_msg, .frame = abort_frame)

  # Inject arg = value in first position
  l <- length(call)
  if (l == 1) {
    call <- call[1:2] # Add a new argument in pos 2
  } else {
    call <- call[c(1, l + 1, 2:l)] # Add a new argument in pos 2
  }
  call[[2]] <- value
  names(call)[2] <- arg

  eval_bare(call, env)
}
