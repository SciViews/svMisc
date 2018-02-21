#' Run an R expression and capture output and messages in a similar way as it
#' would be done at the command line
#'
#' @description This function captures results of evaluating one or several R
#' expressions the same way as it would be issued at the prompt in a R console.
#' The result is returned in a character string. Errors, warnings and other
#' conditions are treated as usual, including the delayed display of the
#' warnings if `options(warn = 0)`.
#'
#' @param expr A  valid R expression to evaluate (names and calls are also
#'   accepted).
#' @param split Do we split output, that is, do we also issue it at the R
#' console too, or do we only capture it silently?
#' @param echo Do we echo each expression in front of the results (like in the
#' console)? In case the expression spans on more than 7 lines, only first and
#' last three lines are echoed, separated by `[...]`.
#' @param file A file, or a valid opened connection where output is sinked. It
#' is closed at the end, and the function returns `NULL` in this case. If
#' `file = NULL` (by default), a `textConnection()` captures the output and it
#' is returned as a character string by the function.
#' @param markStdErr If `TRUE`, stderr is separated from sddout by STX/ETX
#' characters.
#' @return Returns a string with the result of the evaluation done in the user
#' workspace.
#' @note If the expression is provided as a character string that should be
#' evaluated, and you need a similar behaviour as at the prompt for incomplete
#' lines of code (that is, to prompt for more), you should not parse the
#' expression with `parse(text = "<some_code>")` because it returns an error
#' instead of an indication of an incomplete code line. Use
#' `parse_text("<some_code>")` instead, like in the examples bellow.
#' Of course, you have to deal with incomplete line management in your GUI/CLI
#' application... the function only returns `NA` instead of a character string.
#' @export
#' @seealso [parse()], [expression()], [capture.output()]
#' @keywords IO
#' @concept capturing output for GUI clients
#' @examples
#' writeLines(capture_all(expression(1 + 1), split = FALSE))
#' writeLines(capture_all(expression(1 + 1), split = TRUE))
#' writeLines(capture_all(parse_text("search()"), split = FALSE))
#' \dontrun{
#' writeLines(capture_all(parse_text('1:2 + 1:3'), split = FALSE))
#' writeLines(capture_all(parse_text("badname"), split = FALSE))
#' }
#'
#' # Management of incomplete lines
#' capt_res <- capture_all(parse_text("1 +")) # Clearly an incomplete command
#' if (is.na(capt_res)) cat("Incomplete line!\n") else writeLines(capt_res)
#' rm(capt_res)
capture_all <- function(expr, split = TRUE, echo = TRUE, file = NULL,
markStdErr = FALSE) {
  # Inspired by 'capture.output' and the old .try_silent in utils package
  # Requires: R >= 2.13.0

  if (is.null(expr))
    stop("argument is of length zero")
  if (!is.expression(expr)) {
    if (is.na(expr)) return(NA) else
      stop("expr must be an expression or NA")
  }
  # TODO: support for 'file'
  # markStdErr: if TRUE, stderr is separated from sddout by STX/ETX character

  last.warning <- list()
  Traceback <- list()
  n_frame_offset <- sys.nframe() + 23L	# frame of reference (used in traceback)
  # + length of the call stack when a condition occurs
  # Note: if 'expr' is a call, not an expression, 'n_frame_offset' is lower by 2
  # (i.e. 24): -1 for lapply, -1 for unwrapping 'expression()'

  # This may change in course of evaluation, so must be retrieved dynamically
  get_warn_level <- function() options('warn')[[1L]]

  ret_val <- NULL
  conn <- textConnection("ret_val", "w", local = TRUE)
  split <- isTRUE(split)
  if (split) {
    # This is required to print error messages when we are, say, in a
    # browser() environment
    sink(stdout(), type = "message")
  } else {
    # This is the conventional way to do it
    sink(conn, type = "message")
  }
  sink(conn, type = "output", split = split)
  #sink(conn, type = "message")
  on.exit({
    sink(type = "message")
    sink(type = "output")
    close(conn)
  })

  in_stdout <- TRUE

  if (isTRUE(markStdErr)) {
    put_mark <- function(to_stdout, id) {
      if (in_stdout) {
        if (!to_stdout) {
          cat("\x03")
          in_stdout <<- FALSE
        }
      } else {# in StdErr stream
        if (to_stdout) {
          cat("\x02")
          in_stdout <<- TRUE
        }
      }
      #cat("<", id, in_stdout, ">")
    }
  } else {
    put_mark <- function(to_stdout, id) {}
  }

  eval_vis <- function(x) {
    # Do we print the command? (note that it is reformatted here)
    if (isTRUE(echo)) {
      # Reformat long commands... and possibly abbreviate them
      cmd <- deparse(x)
      l <- length(cmd)
      if (l > 7) cmd <- c(cmd[1:3], "[...]", cmd[(l - 2):l])
      cat(":> ", paste(cmd, collapse = "\n:+ "), "\n", sep = "")
    }
    res <- withVisible(eval(x, .GlobalEnv))
    # Do we have result to print?
    if (inherits(res, "list") && res$visible) {
      # Printing is veeery slow on Windows when split = TRUE
      # => unsplit temporarily, and print twice instead!
      #print(res$value)

      if (split) {
        sink(type = "message")
        sink(type = "output")
        # Print first to the console
        try(print(res$value), silent = TRUE)
        sink(conn, type = "message")
        sink(conn, type = "output", split = FALSE)
        # Print a second time to the connection
        try(print(res$value), silent = TRUE)
        # Resink with split = TRUE
        sink(type = "message")
        sink(type = "output")
        sink(stdout(), type = "message")
        sink(conn, type = "output", split = TRUE)
      } else {
        # This is the conventional way to do it
        print(res$value)
      }
    }

    res
  }

  fomat_message <- function(msg) {
    # For some reasons, 'Error: ' and 'Error in ' are not translated,
    # although the rest of the message is correctly translated
    # This is a workaround for this little problem
    res <- .makeMessage(msg)
    res <- sub("^Error: ", ngettext(1, "Error: ", "Error: ", domain = "R"), res)
    sub("^Error in ", ngettext(1, "Error in ", "Error in ", domain = "R"), res)
  }

  restart_error <- function(e, calls) {
    # Remove call (eval(expr, envir, enclos)) from the message
    ncls <- length(calls)

    #DEBUG: cat("n calls: ", ncls, "n_frame_offset: ", n_frame_offset, "\n")
    if (isTRUE(all.equal(calls[[n_frame_offset]], e$call,
      check.attributes = FALSE)))
      e$call <- NULL

    Traceback <<- rev(calls[-c(seq.int(n_frame_offset), (ncls - 1L):ncls)])

    #> cat(captureAll(expression(1:10, log(-1),log(""),1:10)), sep="\n")
    #Error in calls[[n_frame_offset]]: subscript out of bounds
    #Warning message:
    #In log(-1) : NaNs produced

    put_mark(FALSE, 1)
    cat(fomat_message(e))
    if (get_warn_level() == 0L && length(last.warning) > 0L)
      cat(ngettext(1, "In addition: ", "In addition: ", domain = "R"))
  }

  res <- tryCatch(
    withRestarts(
      withCallingHandlers(
        {
          # TODO: allow for multiple expressions and calls (like in
          # 'capture.output'). The problem here is how to tell 'expression'
          # from 'call' without evaluating it?
          #list(eval_vis(expr))
          lapply(expr, eval_vis)
        },

        error = function(e) invokeRestart("grmbl", e, sys.calls()),

        warning = function(e) {
          # Remove call (eval(expr, envir, enclos)) from the message
          if (isTRUE(all.equal(sys.call(n_frame_offset), e$call,
            check.attributes = FALSE)))
            e$call <- NULL

          last.warning <<- c(last.warning, structure(list(e$call),
            names = e$message))

          if (get_warn_level() != 0L) {
            put_mark(FALSE, 2)
            .signalSimpleWarning(conditionMessage(e), conditionCall(e))
              put_mark(TRUE, 3)
          }
          invokeRestart("muffleWarning")
        }
      ),

      # Restarts:

      # Handling user interrupts. Currently it works only from within R.
      #TODO: how to trigger interrupt via socket connection?
      abort = function(...) {
        put_mark(FALSE, 4)
        cat("<aborted!>\n") #DEBUG
      },

      interrupt = function(...)
        cat("<interrupted!>\n"), #DEBUG: this does not seem to be ever called.

      muffleWarning = function() NULL,

      grmbl = restart_error
    ),

    error = function(e) { # This is called if warnLevel == 2
      put_mark(FALSE, 5)
      cat(fomat_message(e))
      e #identity
	  },

    finally = {}
  )

  if (get_warn_level() == 0L) {
    n_warn <- length(last.warning)
    assign("last.warning", last.warning, envir = baseenv())

    if (n_warn > 0L) put_mark(FALSE, 6)
    if (n_warn <= 10L) {
      print.warnings(last.warning)
    } else if (n_warn < 50L) {
      # This is buggy and does not retrieve a translation of the message!
      #cat(gettextf("There were %d warnings (use warnings() to see them)\n",
      #  n_warn, domain = "R"))
      msg <- ngettext(1,
        "There were %d warnings (use warnings() to see them)\n",
        "There were %d warnings (use warnings() to see them)\n",
        domain = "R")
      cat(sprintf(msg, n_warn))
    } else {
      cat(ngettext(1,
        "There were 50 or more warnings (use warnings() to see the first 50)\n",
        "There were 50 or more warnings (use warnings() to see the first 50)\n",
        domain = "R"))
    }
  }

  put_mark(TRUE, 7)

  sink(type = "message")
  sink(type = "output")
  close(conn)
  on.exit()

  # Allow for tracebacks of this call stack:
  assign(".Traceback", lapply(Traceback, deparse), envir = baseenv())

  # Make sure last line ends up with \n
  l <- length(ret_val)
  if (l) ret_val[l] <- paste(ret_val[l], "\n", sep = "")
  ret_val
}

# Backward compatibility

#' @export
#' @rdname capture_all
captureAll <- capture_all
