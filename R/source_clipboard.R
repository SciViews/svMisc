#' Source code from the clipboard
#'
#' @description This function reads R code from the clipboard, and then source
#' it. Clipboard is managed correctly depending on the OS (Windows, MacOS, or
#' *nix)
#'
#' @param primary Only valid on *nix: read the primary (or secondary) clipboard.
#' @param ... Further parameters passed to [source()].
#' @return Same result as [source()].
#' @export
#' @seealso [source()], [file()]
#' @keywords IO
#' @concept Source code from clipboard
source_clipboard <- function(primary = TRUE, ...) {
  # Source data from clipboard, manage clipboard correctly depending on the OS
  if (is_win()) {
    data <- file("clipboard")
  } else if (is_mac()) {
    data <- pipe("pbpaste")
  } else {# Must be Linux/Unix
    if (primary) {
      data <- file("X11_clipboard")
    } else {
      data <- file("X11_secondary")
    }
  }
  on.exit(close(data))
  # Invoke source() with the data from the clipboard
  invisible(source(data, ...))
}

# Backward compatibility

#' @export
#' @rdname source_clipboard
sourceClipboard <- source_clipboard
