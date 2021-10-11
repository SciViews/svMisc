#' Display progression of a long calculation at the R console and/or in a GUI
#'
#' @description Display progression level of a long-running task in the console.
#' Two mode can be used: either percent of achievement (55\%), or the number of
#' items or steps done on a total (1 file on 10 done...). This is displayed
#' either through a message, or through a text-based "progression bar" on the
#' console, or a true progression bar widget in a GUI.
#'
#' @param value Current value of the progression (use a value higher than
#' `max.value` to dismiss the progression indication automatically.
#' @param max.value Maximum value to be achieved.
#' @param progress.bar Should we display a progression bar on the console? If
#' `FALSE`, a temporary message is used.
#' @param char The character to use to fill the progress bar in the console. not
#' used for the alternate display, or for GUI display of progression.
#' @param init Do we have to initialize the progress bar? It is usually done the
#' first time the function is used, and the default value `init = (value == 0)`
#' is correct most of the time. You must specify `init = TRUE` in two cases:
#' (1) if the first value to display is different from zero, and (2) if your
#' code issues some text on screen during progression display. Hence, you must
#' force redraw of the progression bar.
#' @param console Do we display progression on the console?
#' @param gui Do we display progression in a dialog box, or any other GUI widget?
#' See "details" and "examples" hereunder to know how to implement your own GUI
#' progression indicator.
#'
#' @return This function returns `NULL` invisibly. It is invoked for its side
#' effects.
#'
#' @details The function `progress()` proposes different styles of progression
#' indicators than the standard [txtProgressBar()] in package 'utils'.
#'
#' The function uses backspace (\\8) to erase characters at the console.
#'
#' With `gui = TRUE`, it looks for all functions defined in the `.progress` list
#' located in the `SciViews:TempEnv` environment. Each function is executed in
#' turn with following call: `the_gui_function(value, max.value)`. You are
#' responsible to create `the_gui_function()` and to add it as an element in
#' the `.progress` list. See also example (5) hereunder.
#'
#' If your GUI display of the progression offers the possibility to stop
#' calculation (for instance, using a 'Cancel' button), you are responsible to
#' pass this info to your code doing the long calculation and to stop it there.
#' Example (5) shows how to do this.
#' @export
#' @seealso [batch()], [txtProgressBar()]
#' @keywords utilities
#' @concept graphical user interface (GUI) long process progression and feedback
#' @examples
#' # 1) A simple progress indicator in percent
#' for (i in 0:101) {
#'   progress(i)
#'   Sys.sleep(0.01)
#'   if (i == 101) message("Done!")
#' }
#'
#' \dontrun{
#' # 2) A progress indicator with 'x on y'
#' for (i in 0:31) {
#'   progress(i, 30)
#'   Sys.sleep(0.02)
#'   if (i == 31) message("Done!")
#' }
#'
#' # 3) A progress bar in percent
#' for (i in 0:101) {
#'   progress(i, progress.bar = TRUE)
#'   Sys.sleep(0.01)
#'   if (i == 101) message("Done!")
#' }
#'
#' # 4) A progress indicator with 'x on y'
#' for (i in 0:21) {
#'   progress(i, 20, progress.bar = TRUE)
#'   Sys.sleep(0.03)
#'   if (i == 21) message("Done!")
#' }
#' }
#'
#' # 5) A progression dialog box with Tcl/Tk
#' \dontrun{
#' if (require(tcltk)) {
#'   gui_progress <- function(value, max.value) {
#'     # Do we need to destroy the progression dialog box?
#'     if (value > max.value) {
#'       try(tkdestroy(get_temp("gui_progress_window")), silent = TRUE)
#'       delete_temp(c("gui_progress_state", "gui_progress_window",
#'         "gui_progress_cancel"))
#'       return(invisible(FALSE))
#'     } else if (exists_temp("gui_progress_window") &&
#'       !inherits(try(tkwm.deiconify(tt <- get_temp("gui_progress_window")),
#'         silent = TRUE), "try-error")) {
#'       # The progression dialog box exists
#'       # Focus on it and change progress value
#'       tkfocus(tt)
#'       state <- get_temp("gui_progress_state")
#'       tclvalue(state) <- value
#'     } else {
#'       # The progression dialog box must be (re)created
#'       # First, make sure there is no remaining "gui_progress_cancel"
#'       delete_temp("gui_progress_cancel")
#'       # Create a Tcl variable to hold current progression state
#'       state <- tclVar(value)
#'       assign_temp("gui_progress_state", state)
#'       # Create the progression dialog box
#'       tt <- tktoplevel()
#'       assign_temp("gui_progress_window", tt)
#'       tktitle(tt) <- "Waiting..."
#'       sc <- tkscale(tt, orient = "h", state = "disabled", to = max.value,
#'         label = "Progress:", length = 200, variable = state)
#'       tkpack(sc)
#'       but <- tkbutton(tt, text = "Cancel", command = function() {
#'         # Set a flag telling to stop running calculation
#'         assign_temp("gui_progress_cancel", TRUE) # Content is not important!
#'         tkdestroy(tt)
#'       })
#'       tkpack(but)
#'     }
#'     invisible(TRUE)
#'   }
#'   # Register it as function to use in progress()
#'   change_temp(".progress", "gui_progress", gui_progress,
#'     replace.existing = TRUE)
#'   rm(gui_progress) # Don't need this any more
#'   # Test it...
#'   for (i in 0:101) {
#'     progress(i) # Could also set console = FALSE for using the GUI only
#'     Sys.sleep(0.05)
#'     # The code to stop long calc when user presses "Cancel"
#'     if (exists_temp("gui_progress_cancel")) {
#'       progress(101, console = FALSE) # Make sure to clean up everything
#'       break
#'     }
#'     if (i == 101) message("Done!")
#'   }
#'   # Unregister the GUI for progress
#'   change_temp(".progress", "gui_progress", NULL)
#' }
#' }
progress <- function(value, max.value = NULL, progress.bar = FALSE, char = "|",
init = (value == 0), console = TRUE, gui = TRUE) {
  if (!is.numeric(value))
    stop("'value' must be numeric!")

  if (is.null(max.value)) {
    max.value <- 100
    percent <- TRUE
  } else percent <- FALSE

  if (!is.numeric(max.value))
    stop("'max.value' must be numeric or NULL!")

  # If value is higher than max.value, we erase the message
  erase_only <- (value > max.value)

  # Get the saved data associated with this function
  cmd_progress <- get_temp(".progress", default = list(), mode = "list")

  if (console & progress.bar) {
    # The progress bar consists in two lines:
    # first is a "scale" (only drawn when init == TRUE),
    # second is filled with char according to the actual progression
    if (erase_only) {
      cat("\n")
      cmd_progress$pos <- NULL
      cmd_progress$scale <- NULL
      assign_temp(".progress", cmd_progress)
    } else {
      if (init || is.null(cmd_progress$pos)) {
        msg1 <- gettext("Progress:")
        l1 <- nchar(msg1)
        if (percent) {
          scale_bar <- " 0%---------25%---------50%---------75%--------100%\n"
          cmd_progress$scale <- 2
        } else {
          # Calculate best scale
          w <- getOption("width") - l1
          cmd_progress$scale <- (max.value %/% w) + 1
          sl <- round(max.value / cmd_progress$scale)
          max_val <- as.character(round(max.value))
          ticks <- sl - 1 - nchar(max_val)
          if (ticks > 0) {
             scale_ticks <- paste(rep("-", ticks), collapse = "")
          } else scale_ticks <- "-"
          scale_bar <- paste0(" 0", scale_ticks, max_val, "\n")
        }
        cat(rep(" ", l1), scale_bar, msg1, " ", sep = "")
        pos1 <- 0
        cmd_progress$pos <- 0
        assign_temp(".progress", cmd_progress)
      } else pos1 <- cmd_progress$pos
      pos2 <- round(value / cmd_progress$scale)
      if (pos2 > pos1) {
        cmd_progress$pos <- pos2
        assign_temp(".progress", cmd_progress)
        cat(rep(as.character(char[1]), pos2 - pos1))
      }
    }
    # Under Windows or MacOS, make sure the message is actualized
    flush.console()
  } else if (console & !progress.bar) {
    # A progress indicator in the R console
    # We work only with integer part of the values
    # and transform them into strings of same length
    max.value <- as.character(round(max.value))
    l <- nchar(max.value)
    value <- formatC(round(value), width = l)
    msg1 <- gettext("Progress:")
    l1 <- nchar(msg1)
    msg2 <- gettext("on")
    #l2 <- nchar(msg2)
    l3 <- def(cmd_progress$msglength, 0, mode = "numeric", length.out = 1)
    if (l3 < 0) l3 <- 0
    cmd_progress$msglength <- NULL  # Avoid using twice same data
    backspaces <- paste(rep("\b", l3), collapse = "")
    if (erase_only) {
      message <- ""
      cat(backspaces, rep(" ", l3), sep = "")
    } else {
      # Treatment is different if it is 'x%' or 'x on y' display type
      if (percent) {
        message <- paste(msg1, " ", value, "%  ", sep = "", collapse = "")
      } else {
        message <- paste(msg1, " ", value, " ", msg2, " ",
          max.value, "  ", sep = "", collapse = "")
      }
    }
    cat(backspaces, message, sep = "")
    cmd_progress$msglength <- nchar(message)
    assign_temp(".progress", cmd_progress)
    # Under Windows or MacOS, make sure the message is actualized
    flush.console()
  }

  # An additional, graphical display of progression may be implemented too
  # using custom functions as items in .progress in SciViews:TempEnv...
  # Here we look for and trigger them...
  if (gui && length(cmd_progress) > 1) {
    # Execute each item of the list that is a function
    for (i in 1:length(cmd_progress))
      if (mode(cmd_progress[[i]]) == "function")
        cmd_progress[[i]](value, max.value)
  }
  invisible(NULL)
}
