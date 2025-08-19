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
#' @param abort_msg3 An error message when `.` is not found.
#' @param abort_msg4 An additional explanation when `.`is not found.
#' @param abort_msg5 A hint to read the documentation of the data-dot mechanism.
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
#' @seealso [data_dot_mechanism]
#'
#' @examples
#' # Use this (rename) function to get extra-info in the error message about the
#' # data-dot mechanism automatically
#' stop <- stop_
#'
#' # Here is how you create a data-dot function
#' my_subset <- function(.data = (.), i, j) {
#'   if (!prepare_data_dot(.data))
#'     return(recall_with_data_dot())
#'
#'   if (!is.numeric(i))
#'     stop_("{.arg i} must be numeric") # Use this function
#'   if (i == 1)
#'     message(".env has ", paste(names(.env), collapse = ", "))
#'   .data[i, j]
#' }
#'
#' dtf1 <- data.frame(x = 1:3, y = 4:6)
#' # The message shows the objects available in the function environment
#' my_subset(dtf1, 1, 'y')
#' # This is wrong
#'try(my_subset(dtf1, 'y'))
#' .= dtf1
#' my_subset(2, 'y')
#' # Error message with indication that the data-dot mechanism is activated
#' try(my_subset('y'))
#' # Data-dot mechanism not activate, but dot object used
#' try(my_subset(., 'y'))
#' # Wrong .data=
#' try(my_subset(.data = 'y'))
#'
#' # When data-dot is not permitted...
#' .SciViews.implicit.data.dot <- FALSE
#' try(my_subset(2, 'y'))
#' rm(.SciViews.implicit.data.dot)
#'
#' # When `.` is not found...
#' rm(.)
#' try(my_subset(2, 'y'))
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
recall_with_data_dot <- function(call, arg = '.data', value = quote((.)),
  env = parent.frame(2L),
  abort_msg = gettextf("`%s` must be a 'data.frame'.", arg),
  abort_msg2 = gettext("Implicit data-dot (.) not permitted"),
  abort_msg3 = gettext("Data-dot mechanism activated, but no `.` object found."),
  abort_msg4 = gettextf("Define `.` before calling this function, or provide `%s=` explicitly.", arg),
  abort_msg5 = gettextf("See {.help svMisc::data_dot_mechanism} for more infos."),
  abort_frame = parent.frame()) {

  if (missing(call))
    call <- sys.call(-1L)

  if (isFALSE(.SciViews.implicit.data.dot))
    abort(c(abort_msg, x = abort_msg2), .frame = abort_frame)

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

  # Check that "." exists
  if (!exists('.')) {
    cli_abort(c(abort_msg3, i = abort_msg4, i = abort_msg5),
      .frame = abort_frame)
  }

  # Indicate that the data-dot mechanisms was triggered
  pf <- parent.frame()
  pf$._data_dot_. <- TRUE

  Exec(call, env)
}

#' @rdname prepare_data_dot
#' @export
recall_with_data_dot2 <- function(call, arg = 'x', arg2 = 'y',
  value = quote((.)), env = parent.frame(2L),
  abort_msg = gettextf("`%s` and `%s` must both be 'data.frame'.", arg, arg2),
  abort_msg2 = gettext("Implicit data-dot (.) not permitted"),
  abort_msg3 = gettext("Data-dot mechanism activated, but no `.` object found."),
  abort_msg4 = gettextf("Define `.` before calling this function, or provide `%s=` explicitly.", arg),
  abort_msg5 = gettextf("See {.help svMisc::data_dot_mechanism} for more infos."),
  abort_frame = parent.frame()) {

  if (missing(call))
    call <- sys.call(-1L)

  if (isFALSE(.SciViews.implicit.data.dot))
    abort(c(abort_msg, x = abort_msg2), .frame = abort_frame)

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

  # Check that "." exists
  if (!exists('.')) {
    cli_abort(c(abort_msg3, x = abort_msg4, i = abort_msg5),
      .frame = abort_frame)
  }

  Exec(call, env)
}

#' The data-dot mechanism
#'
#' @name data_dot_mechanism
#' @description The **data-dot** mechanism injects automatically `.data = .` in
#' the call to a function when it detects it is necessary (most of the time,
#' when `.data=` is missing, or a unnamed first argument is not suitable as
#' `.data`, i.e., it is not a **data.frame**).
#' This is useful to avoid having to avoid writing `.` "everywhere" in your
#' functions when you use the explicit pipe operator `%>.%`, or with `.= ...`
#' constructs.
#' The data-dot mechanism may fail with an error message if it cannot inject
#' `.` as `.data=`, or when `.` is not found. It may also be prohibited if the
#' variable `.SciViews.implicit.data.dot` is set to
#' ,0 `FALSE` (see examples).
#' @seealso [prepare_data_dot()]
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
NULL
