#' Get an arbitrary name for a temporary variable
#'
#' @description This function ensures that the variable name is cryptic enough
#' and is not already used.
#'
#' @param pattern The prefix for the variable (the rest is a random number).
#' @return A string with the name of a variable.
#' @export
#' @seealso [tempfile()]
#' @keywords utilities
#' @concept temporary variables
#' @examples
#' temp_var()
temp_var <- function(pattern = ".var") {
  # Similar to base::tempfile() but for temporary variables
  repeat {
    var <- paste0(pattern, as.integer(runif(1) * 100000))
    if (!exists(var, where = 1, inherits = TRUE)) break()
  }
  var
}

# Backward compatibility

#' @export
#' @rdname temp_var
tempvar <- temp_var
