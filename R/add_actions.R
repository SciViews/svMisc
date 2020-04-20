#' Add GUI elements like actions (menu items), icons, or methods in a predefined
#' list
#'
#' @description Manage lists of GUI actions, icons and methods.
#'
#' @param obj The name of the object in `SciViews:TempEnv` to manipulate.
#' @param text The text of actions to add (label on first line, tip on other
#' lines).
#' @param code The R code of actions to add.
#' @param state The default (initial) state of an action, as a succession of
#' letters: `c` = checked, `u` = unchecked (default); `d` = disabled,
#' `e` = enabled (default); `h` = hidden, `v` = visible (default). Default
#' values are optional. Ex: `udv` means: unchecked - disabled - visible and
#' it equals to simply `d`, given the defaults for the other properties.
#' @param options A character vector with other options to pass to the graphical
#' toolkit for this action.
#' @param replace Do we replace existing items in 'x'?
#' @param icons The description of the icons to add.
#' @param methods The list of methods to add (character string).
#' @return The modified object is returned invisibly.
#' @export
#' @seealso [add_items()], [obj_menu()], [temp_env()]
#' @keywords utilities
#' @concept list of GUI elements
#' @examples
#' # This is useful to add actions, icons, descriptions, shortcuts or methods
#' # TODO: examples and use for functions add_actions(), add_icons() and
#' # add_methods()
add_actions <- function(obj = get_actions(), text = NULL, code = NULL,
state = NULL, options = NULL, replace = TRUE) {
  dat <- get_temp(obj, default = list())
  if (!inherits(dat, "list"))
    stop("'obj' should inherit from 'list'")

  # Make sure we return an svActions object
  class(dat) <- unique(c("svActions", class(dat)))

  # Add new actions characteristics to dat; make sure newdata are correct
  add_data <- function(x, new_data, replace) {
    new_names <- names(new_data)
    if (is.null(new_names))
      stop("Data you add in actions must be a named character vector")
    new_data <- as.character(new_data)
    names(new_data) <- new_names
    add_items(x, new_data, replace = replace)
	}
	if (!is.null(text)) dat$text <- add_data(dat$text, text, replace)
	if (!is.null(code)) dat$code <- add_data(dat$code, code, replace)
	if (!is.null(state)) dat$state <- add_data(dat$state, state, replace)
	if (!is.null(options)) dat$options <- add_data(dat$options, options, replace)

	## Reassign the modified values
	assign_temp(obj, dat)
	invisible(dat)
}

#' @export
#' @rdname add_actions
get_actions <- function(){
  if (!exists_temp(".svActions")) {
    # Create .svActions if it does not exists yet
    .svActions <- list()
    class(.svActions) <- unique(c("svActions", class(.svActions)))
    assign_temp(".svActions", .svActions, replace.existing = FALSE)

    # Define actions we need for the object browser menus
    add_temp(".svActions", "text", c(
      load =      gettext("Load...\nLoad R objects"),
      source =    gettext("Source...\nSource R code"),
      save =      gettext("Save as...\nSave to a file"),
      import =    gettext("Import...\nImport data in R"),
      export =    gettext("Export...\nExport data to a file"),
      report =    gettext("Report...\nPrepare a report for this object"),
      setwd =     gettext("Set Working dir...\nChange current R working directory"),
      print =     gettext("Print or show\nPrint or show the content of the object"),
      generic =   gettext("<<<fun>>>()\nApply method <<<fun>>>() to the object"),
      names =     gettext("Names\nNames of variables contained in the object"),
      str =       gettext("Str\nCompact str() representation of an object"),
      help =      gettext("Help\nHelp on an object"),
      example =   gettext("Example\nRun examples for this object"),
      edit =      gettext("Edit\nEdit an object"),
      fix =       gettext("Fix\nFix an R object"),
      pkg =       gettext("Load package(s)\nLoad one or several R packages"),
      remove =    gettext("Remove\nRemove (permanently!) one or several objects from memory"),
      require =   gettext("Require <<<pkg>>>\nRequire the package <<<pkg>>>"),
      attach =    gettext("Attach\nAttach an object to the search path"),
      detach =    gettext("Detach\nDetach an object or package from the search path"),
      detachUnload = gettext("Detach and unload\nDetach a package from the search path and unload it"),
      reattach =  gettext("Reattach\nReattach an object to the search path"),
      pkgInfo =   gettext("Package info\nShow detailed information for this package"),
      viewDef =   gettext("View (default)\nDefault view for this object"),
      view =      gettext("View <<<type>>>\nDisplay a '<<<type>>>' view for this object"),
      copyDef =   gettext("Copy (default)\nCopy this object to the clipboard (default format)"),
      copy =      gettext("Copy <<<type>>>\nCopy this object to the clipboard in '<<<type>>>' format"),
      Functions = gettext("Functions\nGeneric functions and methods"),
      View =      gettext("View\nView the object"),
      Copy =      gettext("Copy\nCopy the object to the clipboard")
    ), replace = replace)

    add_temp(".svActions", "code", c(
      load =     "guiLoad([[[pos = \"<<<envir>>>\"]]])",
      source =   "guiSource([[[pos = \"<<<envir>>>\"]]])",
      save =     "guiSave(<<<obj>>>[[[, pos = \"<<<envir>>>\"]]])",
      import =   "guiImport()",
      export =   "guiExport(<<<obj>>>)",
      report =   "guiReport(<<<obj>>>)",
      setwd =    "guiSetwd([[[<<<dir>>>]]])",
      print =    "<<<obj>>>",
      generic =  "[[[<<<var>>>> <- ]]]<<<fun>>>(<<<obj>>>)",
      names =    "names(<<<obj>>>)",
      str =      "str(<<<obj>>>)",
      help =     "help(<<<obj>>>)",
      example =  "example(<<<obj>>>)",
      edit =     "<<<obj>>> <- edit(<<<obj>>>)",
      fix =      "fix(<<<obj>>>)",  # There is no guarantee we fix the right one!
      pkg =      "[[[<<<res>>> <- ]]]pkg(\"<<<pkgs>>>\")",
      remove =   "rm(<<<obj>>>[[[, pos = \"<<<envir>>>\"]]])",
      require =  "[[[<<<res>>> <- ]]]require(<<<pkg>>>)",
      attach =   "attach(<<<obj>>>)",
      detach =   "detach(<<<envir>>>)",
      detachunload = "detach(<<<envir>>>, unload = TRUE)",
      reattach = "detach(<<<obj>>>); attach(<<<obj>>>)",
      pkgInfo =  "<<<H>>>library(help = <<<package>>>)",
      viewDef =  "view(<<<obj>>>)",
      view =	   "view(<<<obj>>>, type = \"<<<type>>>\")",
      copyDef =  "copy(<<<obj>>>)",
      copy =	   "copy(<<<obj>>>, type = \"<<<type>>>\")"
    ), replace = replace)

    add_temp(".svActions", "state", c(
      viewDef = "d",
      copyDef = "d"
    ), replace = replace)

    add_temp(".svActions", "options", c(
      generic = ""
    ), replace = replace)
  }

  get_temp(".svActions")
}

#' @export
#' @rdname add_actions
add_icons <- function(obj = ".svIcons", icons, replace = TRUE) {
  # Get the list of icons
  icn <- get_temp(obj, default = character())
  if (!inherits(icn, "character"))
    stop("'obj' should inherit from 'character'")

  # Check that new icons are correctly formatted
  nicons <- names(icons)
  if (is.null(nicons))
    stop("Icons map you add must be a named character vector")
  icons <- as.character(icons)
  names(icons) <- nicons

  # Add new icons to it
  icn <- add_items(icn, icons, replace = replace)

  # Make sure we return an svIcons object
  class(icn) <- unique(c("svIcons", class(icn)))

  # Reassign the modified values
  assign_temp(obj, icn)
  invisible(icn)
}

#' @export
#' @rdname add_actions
add_methods <- function(methods) {
  # Get the list of methods
  met <- getOption("svGUI.methods")
  if (!is.null(met))
    methods <- add_items(met, methods, use.names = FALSE)
  options(svGUI.methods = sort(methods))
  invisible(methods)
}


# Backward compatibility

#' @export
#' @rdname add_actions
addActions <- add_actions

#' @export
#' @rdname add_actions
addIcons <- add_icons

#' @export
#' @rdname add_actions
addMethods <- add_methods
