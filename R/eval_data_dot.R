# This is the function that injects data = . as first argument in a call
# In case it cannot do it, it returns NULL
#' Inject first argument in a call, usually data = (.)
#'
#' @description In case the `data = (.)` is missing in a call to a "data-dot"
#' function (a function that defines that its first argument `data=` could be
#' missing, and it this case, it is considered to be `.`), this function does
#' the injection of the first argument in the call and evaluate it.
#'
#' @param call A call object, usually a function call.
#' @param arg The name of the argument to inject, usually 'data' or '.data'.
#' @param value The value to inject, usually the symbol '.'.
#' @param env The environment where the evaluation of the data-dot-injected call
#'  should be evaluated (by default, `parent.frame(2L)`, should rarely be
#'  changed).
#' @param abort_msg The message to use in case the 'data' argument is wrong.
#' @param abort_msg2 An additional message to append to the error message in
#'   case data-dot-injection is not permitted (when
#'   `.SciViews.implicit.data.dot != TRUE`)
#' @param abort_frame The environment to use for the error message, by default,
#'   the caller environment (should rarely be changed).
#'
#' @details
#' The call is **not** checked if it is a correct function call. When called
#' from within a function, passing [sys.call()] as `call`, it should be always
#' correct.
#'
#' @returns The result from evaluating the data-dot-injected call.
#' @export
#'
#' @examples
#' # Here is how you create a data-dot function
#' my_subset <- function(data = (.), i, j) {
#'   if (!is.data.frame(data))
#'     return(eval_data_dot(sys.call()))
#'
#'   # Code of the function
#'   # Second argument (i here) must not be a data.frame to avoid confusion
#'   data[i, j]
#' }
#' dtf1 <- data.frame(x = 1:3, y = 4:6)
#' my_subset(dtf1, 1, 'y')
#' # If data is in '.', it can be omitted
#' .= dtf1
#' my_subset(1, 'y')
#'
#' # This mechanism is potentially confusing. You can inactivate it anywhere:
#' .SciViews.implicit.data.dot <- FALSE
#' # This time next call is wrong
#' #my_subset(1, 'y')
#' rm(.SciViews.implicit.data.dot) # Reactivate it
#' my_subset(1, 'y')
#' # Note that, if you have not defined '.' and try to use it, you got
#' # an error: uncomment next line to see it
#' #get_temp(".")
eval_data_dot <- function(call, arg = 'data', value = as.symbol('.'),
  env = parent.frame(2L), abort_msg = gettextf("Wrong '%s' argument.", arg),
  abort_msg2 = gettext("\nImplicit data-dot (.) not permitted"),
  abort_frame = caller_env()) {

  if (!isTRUE(.SciViews.implicit.data.dot))
    abort(paste(abort_msg, abort_msg2), .frame = abort_frame)

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
