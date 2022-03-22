#' Pass first argument as dot to run code in second argument for pipe operators that do not natively support dot-replacement scheme (base R pipe operator)
#'
#' @param x Object to pass to `code` as dot (`.`).
#' @param code Code to execute, containing `.` as a placeholder.
#'
#' @return The result from executing `code` in the parent environment.
#' @details The function has a side-effect to assign `x` as `.` in the calling environment. Therefore, make sure you do not use `.` there for something else. In case `code` fails in the middle of a series of chained pipes, you can inspect `.` or possibly rerun a modified version of the instruction that failed on it for easier debugging purpose.
#' @export
#' @examples
#' # The function is really supposed to be use in a pipe instruction
#' # This example only runs on R >= 4.1
#' \dontrun{
#' # lm has data = as second argument, which does not fit well with the pipe |>
#' # In R 4.1, one should write:
#' iris |> \(.)(lm(data = ., Sepal.Length ~ Petal.Length + Species))()
#' # which is not very elegant ! With ._() it is more concise and straighforward
#' iris |> ._(lm(data = ., Sepal.Length ~ Petal.Length + Species))
#' }
._ <- function(x, code) {
  assign('.', x, envir = parent.frame(2))
  code
}
