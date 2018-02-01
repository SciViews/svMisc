#' Execute a command in the GUI client
#'
#' @description This function is not intended to be used at the command line
#' (except for debugging purposes). It executes a command string to a
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
  # Ask the GUI client to select a .Rdata file to load()
  gui_cmd("load", ...)
}

#' @export
#' @rdname gui_cmd
gui_source <- function(...){
  # Ask the GUI client to select a .R file to source()
  gui_cmd("source", ...)  # TODO: should use sys.source() here
}

#' @export
#' @rdname gui_cmd
gui_save <- function(...){
  # Ask the GUI client for a file where to save some data
  gui_cmd("save", ...)
}

#' @export
#' @rdname gui_cmd
gui_import <- function(...){
  # Ask the client to display a dialog for importing some data
  gui_cmd("import", ...)
}

#' @export
#' @rdname gui_cmd
gui_export <- function(...) {
  # Ask the client to display a dialog for exporting some data
  gui_cmd("export", ...)
}

#' @export
#' @rdname gui_cmd
gui_report <- function(...) {
  # Ask the client to display a dialog for reporting data (send a view...)
  gui_cmd("report", ...)
}

#' @export
#' @rdname gui_cmd
gui_setwd <- function(...) {
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
