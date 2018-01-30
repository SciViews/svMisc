#' Define a vector of a given mode and length (possibly filling it with default
#' values)
#'
#' @description This function makes sure that a vector of a given mode and
#' length is returned. If the value provided is `NULL`, or empty, the default
#' value is used instead. If `length.out = NULL`, the length of the vector is
#' not constrained, otherwise, it is fixed (possibly cutting or recycling
#' `value`).
#'
#' @param value The value to pass with default.
#' @param default The default value to use, in case of `NULL`, or
#' `length(value) == 0`.
#' @param mode The mode of the resulting object: 'character', 'logical',
#' 'numeric' (and, if you want to be more precise: 'double', 'integer' or
#' 'single') or 'complex'. Although not being a mode by itself, you can also
#' specify 'factor' to make sure the result is a factor (thus, of mode
#' 'numeric', storage mode 'integer', with a levels attribute). Other modes are
#' ignored, and `value` is NOT coerced (silently) in this case, i.e., if you
#' don't want to force coercion of the resulting object, use anything else.
#' @param length.out The desired length of the returned vector; use
#' `length.out = NULL` (default) if you don't want to change the length of the
#' vector.
#' @return A vector of given mode and length, with either `value` or `default`.
#' @export
#' @seealso [mode()], [rep()], [temp_env()]
#' @keywords utilities
#' @concept coercion and default values
#' @examples
#' def(1:3, length.out = 5)                      # Convert into character and recycle
#' def(0:2, mode = "logical")                    # Numbers to logical
#' def(c("TRUE", "FALSE"), mode = "logical")     # Text to logical
#' def(NULL, "default text")                     # Default value used
#' def(character(0), "default text")             # Idem
#' def(NA, 10, mode = "numeric", length.out = 2) # Vector of two numbers
def <- function(value, default = "", mode = "character", length.out = NULL) {
  # Ensure we got a value of a given mode, and if not, use default
  # If length.out is provided, make sure that the returned vector has
  # that length (if needed, cut or recycle 'value')
  # If either NULL or something of length == 0 is in 'value', then,
  # return default
  if (!length(value)) value <- default

  # Coerce to mode...
  res <- switch(as.character(mode[1]),
    logical = as.logical(value),
    character = as.character(value),
    numeric = as.numeric(value),
    double = as.double(value),
    integer = as.integer(value),
    single = as.single(value),
    factor = as.factor(value),
    complex = as.complex(value),
    value)	# This is for unrecognized modes!

  # If length.out is provided, make sure the vector has this length
  if (!is.null(length.out)) {
    if (length(length.out) == 0) length.out <- 1 else
      length.out <- round(as.numeric(length.out[1]))
    res <- rep(res, length.out = length.out)
  }
  res
}
