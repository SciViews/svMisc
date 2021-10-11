#' Run a function in batch mode
#'
#' @description A function can be run in batch mode if it never fails (replace
#' errors by warnings) and returns `TRUE` in case of success, or `FALSE` otherwise.
#'
#' @param items The items (usually, arguments vector of character strings) on
#' which to apply `fun` sequentially.
#' @param fun The function to run (must return `TRUE` or `FALSE` and issue only
#' warnings or messages to be a good candidate, batchable, function).
#' @param ... Further arguments to pass the `fun`.
#' @param show.progress Do we show progression as item x on y... message? This
#' uses the [progress()] function.
#' @param suppress.messages Are messages from the batchable function suppressed?
#' Only warnings will be issued. Recommended if `show.progress = TRUE`.
#' @param verbose Display start and end messages if `TRUE` (default).
#' @return Returns invisibly the number of items that were correctly processed
#' with attributes `items` and `ok` giving more details.
#' @export
#' @seealso [progress()]
#' @keywords utilities
#' @concept batch processing
#' @examples
#' \dontrun{
#' # Here is a fake batchable process
#' fake_process <- function(file) {
#'   message("Processing ", file, "...")
#'   flush.console()
#'   Sys.sleep(0.5)
#'   if (runif(1) > 0.7) { # Fails
#'     warning("fake_process was unable to process ", file)
#'     invisible(FALSE)
#'   } else invisible(TRUE)
#' }
#'
#' # Run it in batch mode on five items
#' files <- paste0("file", 1:5)
#' batch(files, fake_process)
#' }
batch <- function(items, fun, ..., show.progress = !is_aqua() && !is_jgr(),
suppress.messages = show.progress, verbose = TRUE) {
  if (!is.function(fun))
    stop("'fun' must be a function")

  # Preparation of the batch process...
  owarn <- options(warn = 1) # Issue warnings immediatelly!
  on.exit(options(owarn))
  verbose <- isTRUE(as.logical(verbose))
  if (verbose) message("Running the batch process with ",
    deparse(substitute(fun)), "...")
  cat("\n")
  n <- length(items)
  if (n < 1) {
    warning("No items to process!")
    return(invisible(structure(FALSE, items = items, ok = logical(0))))
  }
  ok <- rep(NA, n) # A vector with results

  # Do we show progression?
  if (!isTRUE(as.logical(show.progress)))
    progress <- function(...) NULL # Fake progress() function
  if (!isTRUE(as.logical(suppress.messages)))
    suppressMessages <- function(x) return(x) # Fake suppressMessages() fun

  # Run fun() for each item
  flush.console()
  for (i in 1:n) {
    progress(i, n)
    item <- items[i]
    ok[i] <- as.logical(suppressMessages(fun(item, ...)))[1]
    # Go to a new line in case of error and prevent progess() to erase text
    if (!ok[i]) {
      cat("\n")
      rm_temp(".progress")
    }

    flush.console()
  }
  progress(n + 1, n) # Cancel progression message
  if (verbose) {
    cat("\n")
    message("Processed successfully ", sum(ok, na.rm = TRUE),
    " items on ", n, " (see .last.batch)")
  }
  # Record .last.batch variable in SciViews:TempEnv
  last_batch <- structure(sum(ok, na.rm = TRUE) == n, items = items, ok = ok)
  assignTemp(".last.batch", last_batch)
  invisible(last_batch)
}
