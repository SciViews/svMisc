#' Execute a command in the GUI client
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions are not intended to be used at the command line
#' (except for debugging purposes). They executes a command string into a
#' (compatible) GUI client.
#'
#' @param command The command string to execute in the GUI client.
#' @param ... Parameters provided for the command to execute in the GUI client.
#' @return The result of the command if it succeed, or `NULL` if the command
#' cannot be run (i.e., `.guiCmd()` is not defined in `SciViews:TempEnv`).
#' @details You must define a function `.guiCmd()` in the `SciViews:TempEnv`
#' environment that will take first argument as the name of the command to
#' execute (like `source`, `save`, `import`, etc.), and ... with arguments to
#' the command to send. Depending on your GUI, you should have code that
#' delegates the GUI elements (ex: display a dialog asking for a .Rdata file to
#' source) and then, execute the command in \R with the selected file as
#' attribute.
#' @export
#' @seealso [get_temp()]
#' @keywords misc
#' @concept graphical user interface (GUI) control
gui_cmd <- function(command, ...) {
  lifecycle::deprecate_soft(when = "1.5.0",
    what = "gui_cmd()", with = NULL)

  # This function sends a command to the GUI client
  # The actual code is a custom function named .guiCmd in SciViews:TempEnv
  cmd_fun <- get_temp(".guiCmd", mode = "function")
  if (!is.null(cmd_fun)) {
    cmd_fun(command, ...)
  } else {
    NULL
  }
}

#' @export
#' @rdname gui_cmd
gui_load <- function(...) {
  lifecycle::deprecate_soft(when = "1.5.0",
    what = "gui_load()", with = NULL)

  # Ask the GUI client to select a .Rdata file to load()
  gui_cmd("load", ...)
}

#' @export
#' @rdname gui_cmd
gui_source <- function(...){
  lifecycle::deprecate_soft(when = "1.5.0",
    what = "gui_source()", with = NULL)

  # Ask the GUI client to select a .R file to source()
  gui_cmd("source", ...)  # TODO: should use sys.source() here
}

#' @export
#' @rdname gui_cmd
gui_save <- function(...){
  lifecycle::deprecate_soft(when = "1.5.0",
    what = "gui_save()", with = NULL)

  # Ask the GUI client for a file where to save some data
  gui_cmd("save", ...)
}

#' @export
#' @rdname gui_cmd
gui_import <- function(...){
  lifecycle::deprecate_soft(when = "1.5.0",
    what = "gui_import()", with = NULL)

  # Ask the client to display a dialog for importing some data
  gui_cmd("import", ...)
}

#' @export
#' @rdname gui_cmd
gui_export <- function(...) {
  lifecycle::deprecate_soft(when = "1.5.0",
    what = "gui_export()", with = NULL)

  # Ask the client to display a dialog for exporting some data
  gui_cmd("export", ...)
}

#' @export
#' @rdname gui_cmd
gui_report <- function(...) {
  lifecycle::deprecate_soft(when = "1.5.0",
    what = "gui_report()", with = NULL)

  # Ask the client to display a dialog for reporting data (send a view...)
  gui_cmd("report", ...)
}

#' @export
#' @rdname gui_cmd
gui_setwd <- function(...) {
  lifecycle::deprecate_soft(when = "1.5.0",
    what = "gui_setwd()", with = NULL)

  # Ask the GUI client to select a directory to set as active
  gui_cmd("setwd", ...)
}

# Backward compatibility

#' @export
#' @rdname gui_cmd
guiCmd <- gui_cmd

#' @export
#' @rdname gui_cmd
guiLoad <- gui_load

#' @export
#' @rdname gui_cmd
guiSource <- gui_source

#' @export
#' @rdname gui_cmd
guiSave <- gui_save

#' @export
#' @rdname gui_cmd
guiImport <- gui_import

#' @export
#' @rdname gui_cmd
guiExport <- gui_export

#' @export
#' @rdname gui_cmd
guiReport <- gui_report

#' @export
#' @rdname gui_cmd
guiSetwd <- gui_setwd
