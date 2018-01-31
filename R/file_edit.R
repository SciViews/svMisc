#' Invoke an external text editor for a file
#'
#' @description Edit a text file using an external editor. Possibly wait for the
#' end of the program and care about creating the file (from a template) if it
#' does not exists yet.
#'
#' @param ... Path to one or more files to edit.
#' @param title The title of the editor window (not honoured by all editors,
#' most external editors only display the file name or path).
#' @param editor Editor to use. Either the name of the program, or a string
#' containing the command to run, using \%s as replacement tag where to place
#' the filename in the command, or a function with 'file', 'title' and 'wait'
#' arguments to delegate process of the files.
#' @param file.encoding Encoding of the files. If `""` or `native.enc`, the
#' files are considered as being already in the right encoding.
#' @param template One or more files to use as template if files must be
#' created. If `NULL`, an empty file is created. This argument is recycled for
#' all files to edit.
#' @param replace Force replacement of files if `template=` is not null.
#' @param wait Wait for edition to complete. If more than one file is edited,
#' the program waits sequentially for each file to be edited in turn (with a
#' message in the R console).
#' @return The function returns `TRUE` if it was able to edit the files or
#' `FALSE` otherwise, invisibly. Encountered errors are reported as warnings.
#' @note The default editor program, or the command to run is in the
#' `fileEditor` option (use `getOption("fileEditor")` to retrieve it, and
#' `options(fileEditor = "<my_own_editor>")` to change it). Default values are
#' determined automatically.
#'
#' On Unixes, "gedit", "kate" and "vi" are looked for in that order. Note that
#' there is a gedit plugin to submit code directly to R:
#' <http://rgedit.sourceforge.net/>. Since, gedit natively supports a lot of
#' different syntax highlighting, including R, and is lightweight but feature
#' rich, it is recommended as default text editor for `file_edit()` on Unixes.
#' If JGR is run and the editor is "vi" or "internal", then the internal JGR
#' editor is used, otherwise, the provided editor is choosen.
#'
#' On MacOS, if the "bbedit" program exists, it is used (it is the command line
#' program installed by BBEdit, see <http://www.barebones.com/products/>, a much
#' more capables text editor than the default TextEdit program), otherwise, the
#' default text editor used by MacOS is choosen (default usually to TextEdit).
#' BBEdit can be configured to highlight and submit R code.It features also
#' several tools that makes it a much better choice than TextEdit for
#' `file_edit()` on MacOS. Specify "bbedit" to force using it. The default value
#' is "textedit", the MacOS default text editor, but on R.app, and with
#' `wait = FALSE`, the internal R.app editor is used instead in that case. If
#' RStudio or JGR is run, and the editor is "textedit", "internal" or "vi", then,
#' the RStuiod or JGR internal editor is used instead. If `wait = TRUE` with an
#' RStudio editor, it is enough to switch to another editor to continue.
#'
#' On Windows, if Notepad++ is installed in its default location, it is used,
#' otherwise, the default "notepad" is used in Rterm and the internal editors
#' are choosen for Rgui. Notepad++ is a free text editor that is much better
#' suited to edit code or text files that the default Windows' notepad
#' application, in particular because it can handle various line end types
#' (Unix, Mac or Windows) and encodings. It also supports syntax highlighting,
#' code completion and much more. So, it is strongly recommended to install it
#' (see <http://notepad-plus-plus.org/>) and use it with `file-edit()`. There is
#' also a plugin to submit code to R directly from Notepad++:
#' <http://sourceforge.net/projects/npptor/>.
#'
#' Of course, you can use your own text editor, just indicate it in the
#' `fileEditor` option. Note, however, that you should use only lighweight and
#' fast starting programs. Also, for the `wait = TRUE` argument of
#' `file_edit()`, you must check that R waits for the editor to be closed
#' before further processing code. In some cases, a little command line program
#' is used to start a larger application (like for Komodo Edit/IDE), or the
#' program delegates to an existing instances and exits immediatelly, even if the
#' file is still edited. Such editors are not recommended at all for
#' `file_edit()`.
#'
#' If you want to use files that are compatibles between all platforms supported
#' by R itself, you should think about using ASCII encoding as much as possible
#' and the Windows style of line-ending. That way, you ensure that all the
#' default editors will handle those files correctly, including the broken
#' default editor on Windows, notepad, which does not understand at all MacOS
#' or Unix line ending characters!
#' @export
#' @seealso [system_file()], [file.path()], [file.edit()]
#' @keywords utilities
#' @concept file edition
#' @examples
#' \dontrun{
#' # Create a template file in the tempdir...
#' template <- tempfile("template", fileext = ".txt")
#' cat("Example template file to be used with file_edit()", file = template)
#'
#' # ... and edit a new file, starting from that template:
#' new_file <- tempfile("test", fileext = ".txt")
#' file_edit(new_file, template = template, wait = TRUE)
#'
#' message("Your file contains:")
#' readLines(new_file)
#'
#' # Eliminate both the file and template
#' unlink(new_file)
#' unlink(template)
#' }
file_edit <- function(..., title = files, editor = getOption("fileEditor"),
file.encoding = "", template = NULL, replace = FALSE, wait = FALSE) {
  # Rework files, title and template
  files <- c(...)
  lf <- length(files)
  if (!lf) {
    warning("You must provide at least one file path")
    return(invisible(FALSE))
  }
  title <- rep(as.character(title), len = lf)
  if (length(template))
    template <- rep(as.character(template), len = lf)

  # If the file(s) do not exist or must be replaced,
  # create them (possibly from template)
  to_replace <- (isTRUE(as.logical(replace)) | !file.exists(files))
  if (length(to_replace)) {
    new_files <- files[to_replace]
    if (length(template)) template <- template[to_replace]
    for (i in 1:length(new_files)) {
      if (!length(template) || !nzchar(template[i])) {
        file.create(new_files[i])
      } else if (file.exists(template[i])) {
        file.copy(template[i], new_files[i], overwrite = TRUE,
          copy.mode = FALSE)
      } else {# Template file not found!
        warning("Template file '", template[i],
          '" not found, starting from an empty file')
        file.create(new_files[i])
      }
    }
  }
  files <- normalizePath(files)

  # Manage file encoding
  if (nzchar(file.encoding) && file.encoding != "native.enc") {
    tfile <- file
    for (i in seq_along(file)) {
      tfile <- tempfile()
      con <- file(file[i], encoding = file.encoding)
      writeLines(readLines(con), tfile)
      close(con)
      file[i] <- tfile
    }
  }

  # There are a few shortcuts for editors that need to be expanded
  if (length(editor) && is.character(editor))
    editor <- switch(tolower(editor),
      textedit = "open -e -n -W \"%s\"",
      textwrangler = "bbedit --wait --resume \"%s\"",
      bbedit = "bbedit --wait --resume \"%s\"",
      editor)

  # Fallback to "editor", in case no fileEditor is provided
  if (!length(editor)) {
    editor <- getOption("editor")
  } else if (!is_win() && is.character(editor) && !grepl("%s", editor)) {
    cmd <- paste('which ', '"', editor, '"', sep = "")
    if (!length(system(cmd, intern = TRUE))) {
      # Fall back to the default editor (if any)
      editor <- getOption("editor")
    }
  }

  # If not in interactive mode, or expressly no editor provided
  # We don't edit!
  if (!interactive() || !length(editor) ||
    (!is.function(editor) && !nzchar(editor))) {
    # Do nothing, issue, a warning!
    warning("Cannot edit files: no editor or not in interactive mode")
    return(invisible(FALSE))
  }

  # Special cases... where we prefer the internal editor
  # Note: just change editor a little bit to make sure to avoid internal!
  wait <- isTRUE(as.logical(wait))
  if (is.character(editor) &&
    editor %in% c("notepad", "internal", "vi", "open -e -n -W \"%s\"")) {
    done <- FALSE

    # 1) JGR
    if (is_jgr()) {
      for (i in 1:lf)
        .file_edit_jgr(files[i], title = title[i], wait = wait)
      done <- TRUE

    # 2) Windows Rgui
    } else if (is_rgui()) {
      for (i in 1:lf)
        .file_edit_rgui(files[i], title = title[i], wait = wait)
      done <- TRUE

    # 3) R.app and wait == FALSE (we cannot wait the end of edition using
    #    the internal R.app editor!)
    } else if (is_aqua() && !wait) {
      # Note that, here, the editor in use is the one defined in the
      # R.app preference dialog box!
      for (i in 1:lf)
        file.edit(files[i], title = title[i], fileEncoding = "")
      done <- TRUE

    # 4) RStudio and wait == FALSE (note that we should use rstudioapi here,
    #    but we don't want to add a dependency to it here... and
    #    .rs.api.navigateToFile should be directly available under RStudio).
  } else if (is_rstudio()) {
    for (i in 1:lf)
      .file_edit_rstudio(files[i], title = title[i], wait = wait)
    done <- TRUE
  }
    if (done) return(invisible(TRUE))
  }

  # In any other case, we use the defined editor
  if (is.function(editor)) {
    # Here, we need a special editor function that is able to wait!
    res <- try(editor(file = file, title = title, wait = wait), silent = TRUE)
  } else {
    # Construct the command...
    if (grepl("%s", editor)) {
      cmds <- sprintf(editor, files)
    } else {
      cmds <- paste('"', editor, '" "', files, '"', sep = "")
    }
    if (is_mac()) msg <- "'... Close the editor (Cmd-Q) to continue!" else
      msg <- "'... Close the editor to continue!"
    for (i in 1:length(cmds)) {
      if (wait) message("Editing the file '", basename(files[i]), msg)
      flush.console()
      if (is_win()) {
        res <- try(system(cmds[i], ignore.stdout = TRUE,
          ignore.stderr = TRUE, wait = wait, minimized = FALSE,
          invisible = FALSE, show.output.on.console = FALSE),
          silent = TRUE)
      } else {
        res <- try(system(cmds[i], ignore.stdout = TRUE,
          ignore.stderr = TRUE, wait = wait), silent = TRUE)
      }
      if (inherits(res, "try-error")) break
    }
  }
  if (inherits(res, "try-error")) {
    warning(as.character(res))  # Transform the error into a warning
    invisible(FALSE)
  } else invisible(TRUE)
}

.file_edit_rstudio <- function(file, title = file, wait = FALSE) {
  # Note that title is not used here!
  file <- as.character(file)
  if (length(file) != 1)
    stop("Only one item for 'file' is accepted")

  # Check that RStudio is running
  if (!is_rstudio()) {
    message(".file_edit_rstudio() cannot be used outside RStudio.\n")
    return(invisible(NULL))
  }

  # Create a new editor window and open the file in it
  open_file <- get0(".rs.api.navigateToFile")
  if (is.null(open_file))
    stop("impossible to get .rs.api.navigateToFile() function")
  editor <- open_file(file)

  # Do we wait that the file is edited?
  if (isTRUE(as.logical(wait))) {
    get_editor_context <- get0(".rs.api.getSourceEditorContext")
    if (is.null(get_editor_context))
      warning("impossible to get editor context... cannot honor wait = TRUE")
    Sys.sleep(0.5)
    path <- get_editor_context()$path
    message("Editing file '", basename(file),
      "'... Close the editor, or switch to another one to continue!")
    while (get_editor_context()$path == path) {
      Sys.sleep(0.3)
    }
  }
  invisible(editor)
}

.file_edit_jgr <- function(file, title = file, wait = FALSE) {
  file <- as.character(file)
  if (length(file) != 1)
    stop("Only one item for 'file' is accepted")
  title <- as.character(title)
  if (length(title) != 1)
    stop("Only one item for 'title' is accepted")

  # Check that JGR is running
  if (!is_jgr()) {
    message(".file_edit_jgr() cannot be used outside JGR.\n")
    return(invisible(NULL))
  }

  # Create a new editor window and open the file in it
  # Note that, if JGR is loaded, rJava is there too. So .jnew is available!
  editor <- rJava::.jnew('org/rosuda/JGR/editor/Editor', as.character(file)[1])
  # Set the title
  if (title != file) editor$setTitle(title)

  # Do we wait that the file is edited?
  if (isTRUE(as.logical(wait))) {
    message("Editing file '", basename(file),
      "'... Close the editor to continue!")
    while (editor$isVisible()) {
      editor$setState(0L)   # Make sure it is not iconized
      editor$toFront()      # Make the editor the frontmost window
      Sys.sleep(0.3)
    }
  }
  invisible(editor)
}

.file_edit_rgui <- function(file, title = file, wait = FALSE) {
  # Avoid errors in R CMD check about missing getWindowsHandles() function
  if (!is_win()) getWindowsHandles <- function(...) NULL

  file <- as.character(file)
  if (length(file) != 1)
    stop("Only one item for 'file' is accepted")
  title <- as.character(title)
  if (length(title) != 1)
    stop("Only one item for 'title' is accepted")

  # Check if we are in RGui
  if (!is_rgui()) {
    message(".file_edit_rgui() cannot be used outside Rgui.\n")
    return(invisible(NULL))
  }

  # Edit file in an Rgui internal editor and track its existence
  # if wait == TRUE
  hdl <- getWindowsHandles()
  file.edit(file, title = title, editor = "internal", fileEncoding = "")
  hdl2 <- getWindowsHandles()
  editor <- hdl2[!hdl2 %in% hdl]

  # Do we wait that the file is edited?
  if (isTRUE(as.logical(wait)) && length(editor) == 1) {
    message("Editing file '", basename(file),
      "'... Close the editor to continue!")
    flush.console()
    while (editor %in% getWindowsHandles(minimized = TRUE))
      Sys.sleep(0.3)
  }
  invisible(editor)
}

# Backward compatibility

#' @export
#' @rdname file_edit
fileEdit <- file_edit
