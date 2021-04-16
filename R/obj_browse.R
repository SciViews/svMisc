#' Functions to implement an object browser
#'
#' @description These functions provide features required to implement a
#' complete object browser in a GUI client.
#'
#' @param id The id of the object browser (you can run several ones
#' concurrently, providing you give them different ids).
#' @param envir An environment, or the name of the environment, or the position
#' in the [search()] path.
#' @param all.names Do we display all names (including hidden variables starting
#' with '.')?
#' @param pattern A pattern to match for selecting variables.
#' @param group A group to filter.
#' @param sep Separator to use between items (if path is not `NULL`).
#' @param path The path where to write a temporary file with the requested
#' information. Set to NULL (default) if you don't pass this data to your GUI
#' client by mean of a file.
#' @param regenerate Do we force to regenerate the information?
#' @param object Name of the object selected in the object browser,
#' components/arguments of which should be listed.
#' @param objects A list with selected items in the object browser.
#' @param all.info Do we return all the information (envir as first column or
#' not (by default).
#' @param compare If TRUE, result is compared with last cached value and the
#' client is updated only if something changed.
#' @param x Object returned by `obj_list()`.
#' @param eol Separator to use between object entries, default is to list each
#' item in a separate line.
#' @param header If `TRUE`, two-line header is printed, of the form: \cr
#'   Environment = environment name \cr
#'   Object = object name \cr
#' Default is not to print header if `all.info == TRUE`.
#' @param raw.output If `TRUE`, a compact, better suited for parsing output is
#' produced.
#' @param ... Further arguments, passed to [write.table()].
#' @return Depending on the function, a list, a string, a reference to an
#' external, temporary file or `TRUE` in case of success or `FALSE` otherwise
#' is returned invisibly.
#' @details `obj_browse()` does the horse work. `obj_dir()` gets the temporary
#' directory where exchange files with the GUI client are stored, in case you
#' exchange data through files. You can use a better way to communicate with
#' your GUI (you have to provide your code) and disable writing to files by
#' using `path = NULL`.
#'
#' `obj_list()` lists objects in a given environment, elements of a recursive
#' object or function argument.
#'
#' `obj_search()` lists the search path.
#'
#' `obj_clear()` clears any reference to a given object browser.
#'
#' `obj_info()` computes a tooltip info for a given object.
#'
#' obj_menu()` computes a context menu for selected object(s) in the object
#' explorer managed by the GUI client.
#'
#' `print.objList()` print method for `objList` objects.
#' @author Philippe Grosjean <phgrosjean@sciviews.org> &
#' Kamil Barton <kamil.barton@uni-wuerzburg.de>
#' @export
#' @seealso [completion()], [call_tip()]
#' @keywords misc
#' @examples
#' # Create various context menus
#' data(iris)
#' (obj_info(object = "iris"))
#' data(trees)
#' # For one object
#' (obj_menu(objects = "iris"))
#' # For multiple objects
#' (obj_menu(objects = c("iris", "trees")))
#' # For inexistant object (return "")
#' (obj_info(object = "noobject"))
#' (obj_menu(objects = "noobject"))
#' rm(iris, trees)
#'
#' # For environments
#' (obj_info(envir = ".GlobalEnv"))
#' (obj_menu(envir = ".GlobalEnv"))
#' (obj_info(envir = "SciViews:TempEnv"))
#' (obj_menu(envir = "SciViews:TempEnv"))
#' (obj_info(envir = "package:datasets"))
#' (obj_menu(envir = "package:datasets"))
#' # For an environment that does not exist on the search path (return "")
#' (obj_info(envir = "noenvir"))
#' (obj_menu(envir = "noenvir"))
obj_browse <- function(id = "default", envir = .GlobalEnv, all.names = NULL,
pattern = NULL, group = NULL, sep = "\t", path = NULL, regenerate = FALSE) {
  # Maintain files for remote Object Browser
  # If four first parameters are NULL, use cached version of these parameters,
  # or default values

  # Format envir as character (use only first item provided!)
  if (is.environment(envir)) envir <- deparse(substitute(envir))
  if (is.numeric(envir)) envir <- search()[envir[1]]
  envir <- as.character(envir)[1]
  # Get the current position in the search path for envir
  pos <- match(envir, search(), nomatch = -1)
  if (pos < 1) {
    pos <- 1  # NOT FOUND, use .GlobalEnv
    envir = ".GlobalEnv"
  }

  if (!is.null(path)) {
    # Does the directory exists?
    if (path == "") path <- objDir()
    if (!file.exists(path) || !file.info(path)$isdir) {
      #unlink(path)
      if (!dir.create(path))
        stop("Impossible to create the Object Browser 'path' directory!")
    }
  }

  # Control that 'Search.txt' is up-to-date
  changed_search <- obj_search(path = path, compare = !regenerate)

  # Make sure id is character
  id <- as.character(id)[1]
  if (id == "") id <- "default"

  # Get the five parameters pos, envir, all.names, pattern & group
  all_pars <- get_temp(".guiObjParsCache", default = NULL)
  if (!is.null(all_pars)) {
    pars <- all_pars[[id]]
  } else {
    pars <- list(pos = 1, envir = ".GlobalEnv", all.names = FALSE,
      pattern = "", group = "")
    assign_temp(".guiObjParsCache", list())	# Create the list
  }
  if (is.null(pars))
    pars <- list(pos = 1, envir = ".GlobalEnv", all.names = FALSE,
      pattern = "", group = "")
  # Possibly change some parameters (and make sure they are valid!)
  pars_changed <- FALSE
  if (!is.null(pos)) {
    pars_changed <- TRUE
    pars$pos <- as.integer(pos[1])
    pars$envir <- envir
    if (pars$pos < 1) {
      pars_changed <- TRUE
      pars$pos <- 1
      pars$envir <- ".GlobalEnv"
    }
  }
  if (pars$pos > length(search())) {
    pars_changed <- TRUE
    pars$pos <- 1
    pars$envir <- ".GlobalEnv"
  }
  # Track possible changes in the search path
  if (is.na(match(pars$envir, search()))) {
    pars_changed <- TRUE
    pars$pos <- 1
    pars$envir <- ".GlobalEnv"
  }
  if (match(pars$envir, search()) != pars$pos) {
    pars_changed <- TRUE
    pars$pos <- match(pars$envir, search())
  }
  # Track changes in the options
  if (!is.null(all.names)) {
    pars_changed <- TRUE
    pars$all.names <- as.logical(all.names[1])
  }
  if (!is.null(pattern)) {
    pars_changed <- TRUE
    pars$pattern <- as.character(pattern[1])
  }
  if (!is.null(group)) {
    pars_changed <- TRUE
    pars$group <- as.character(group[1])
  }
  # Write a cached version of these parameters in SciViews:TempEnv
  all_pars <- get_temp(".guiObjParsCache", default = list())
  all_pars[[id]] <- pars
  assign_temp(".guiObjParsCache", all_pars)

  # Control that 'List_<id>.txt' is up-to-date, but only if pos == 1 or
  # envir is not a package or regenerate or pars or Search have changed
  # to limit the work done on workspaces that are unlikely to have change
  if (pars_changed || regenerate || (changed_search != "") || (pars$pos == 1) ||
      (regexpr("^package:", envir) == -1)) {
    changed_list <- objList(id = id, envir = pars$pos,
      all.names = pars$all.names, pattern = pars$pattern,
      group = pars$group, path = path, compare = !regenerate,
      sep = sep)

    changed_list <- if (!is.null(nrow(changed_list)) && nrow(changed_list) > 0) {
      apply(changed_list, 1, paste, collapse = sep)
    } else ""

  } else changed_list <- ""

  # We return the data, or indication that the data have changed to the client
  res <- ""
  if (length(changed_search) > 1 || changed_search != "") {
    res <- "<<<search>>>\n"
    if (is.null(path)) res <- paste0(res,
      paste(changed_search, collapse = sep), "\n")
  }
  if (length(changed_list) > 1 || changed_list != "") {
    res <- paste(res, "<<<list>>>", sep = "")
    if (is.null(path)) res <- paste(c(res, changed_list),
      collapse = "\n")
  }

  # Possibly call a .guiObjBrowse function to pass the res to the GUI client
  cmd_fun <- get_temp(".guiObjBrowse", mode = "function")
  if (!is.null(cmd_fun)) cmd_fun(id = id, data = res)

  invisible(res)
}

#' @export
#' @rdname obj_browse
obj_clear <- function(id = "default") {
  # Clear any reference to a given 'id' object browser
  id <- as.character(id)[1]  # Make sure id is character
  if (id == "") id <- "default"
  pars <- get_temp(".guiObjParsCache", default = list())
  pars[[id]] <- NULL
  assign_temp(".guiObjParsCache", pars)
  list_cache <- get_temp(".guiObjListCache", default = list())
  list_cache[[id]] <- NULL
  assign_temp(".guiObjListCache", list_cache)
  # Also delete corresponding files
  root <- obj_dir()

  pars_file = file.path(root, paste0("Pars_", id, ".txt"))
  if (file.exists(pars_file)) unlink(pars_file)

  list_file = file.path(root, paste0("List_", id, ".txt"))
  if (file.exists(list_file)) unlink(list_file)

  menu_file = file.path(root, paste0("Menu_", id, ".txt"))
  if (file.exists(menu_file)) unlink(menu_file)

  invisible(TRUE)
}

#' @export
#' @rdname obj_browse
obj_dir <- function()
  file.path(tempdir(), "svObjBrowser")

#' @export
#' @rdname obj_browse
obj_info <- function(id = "default", envir = .GlobalEnv, object = "",
path = NULL) {
  # Get a tooltip information for an object (for mouseover method)

  # Format envir as character (use only first item provided!)
  if (is.environment(envir)) envir <- deparse(substitute(envir))
  if (is.numeric(envir)) envir <- search()[envir[1]]
  envir <- as.character(envir)[1]

  # Possibly call a custom function .objInfo() in SciViews:TempEnv
  cmd_fun <- get_temp(".objInfo", mode = "function")
  if (!is.null(cmd_fun)) {  # We call a custom function
    info <- cmd_fun(id = id, envir = envir, object = object)
  } else if (object == "") {  # An environment...
    info <- switch(envir,
      .GlobalEnv = paste(c("Global environment\n", capture.output(gc())),
        collapse = "\n"),
      `SciViews:TempEnv` = "SciViews temporary variables environment",
      RcmdrEnv = "R Commander temporary variables environment",
      `tools:RGUI` = "R.app tools environment",
      `tools:rstudio` = "RStudio tools environment",
      Autoloads = "R autoloading objects environment",
      if (regexpr("^package:", envir) > -1) {
        pkg <- sub("^package:", "", envir)
        paste(library(help = pkg, character.only = TRUE)$info[[1]],
          collapse = "\n")
      } else if (envir %in% search()) {
        paste0("'", envir, "' environment")
      } else ""
    )
  } else {# An object...
    if (!exists(object, where = envir))
      return(invisible(""))
    obj <- get(object, pos = envir)
    # The info is simply a str() representation of the object
    # We need to capture output
    info <- capture.output(str(obj))
    # Add estimation of size for this object, if it is not a function
    if (!inherits(obj, "function")) {
      size <- object.size(obj)
      if (size > 1024 * 1024) {
        size <- paste("Estimated size:",
          format(size / 1024 / 1024, digits = 3), "Mb")
      } else if (size > 1024) {
        size <- paste("Estimated size:",
          format(size / 1024, digits = 3), "kb")
      } else size <- paste("Estimated size:",
        format(size, digits = 3), "bytes")
      info[length(info) + 1] <- size
    }
  }

  if (!is.null(path)) {
    # Save the data in a file
    if (path == "") path <- obj_dir()
    info_file <- file.path(path, paste("Info_", id, ".txt", sep = ""))
    cat(info, collapse = "\n", file = info_file)
  }

  # Possibly call a .guiObjInfo function to pass the data to the GUI client
  cmd_fun <- get_temp(".guiObjInfo", mode = "function")
  if (!is.null(cmd_fun)) cmd_fun(id = id, data = info)

  # Return the info tooltip invisibly
  invisible(info)
}

#' @export
#' @rdname obj_browse
obj_list <- function(id = "default", envir = .GlobalEnv, object = NULL,
all.names = FALSE, pattern = "", group = "", all.info = FALSE, sep = "\t",
path = NULL, compare = TRUE, ...) {
  # Make sure that id is character
  id <- as.character(id)[1]
  if (id == "") id <- "default"
  ename <- NA

  # Format envir as character (use only first item provided!)
  if (!is.environment(envir)) {
    if (is.numeric(envir) && envir > 0)
      envir <- search()[envir]

    if (is.character(envir)) {
      ename <- envir
      envir <- tryCatch(as.environment(envir), error = function(e) NULL)
      if (is.null(envir) || inherits(envir, "error")) {
        envir <- NULL
        ename <- ""
      }
    }
  }

  # base and .GlobalEnv do not have name attribute
  if (!is.null(attr(envir, "name"))) ename <- attr(envir, "name")
    else if (is.na(ename)) ename <- deparse(substitute(envir))
  if (ename %in% c("baseenv()", ".BaseNamespaceEnv"))
    ename <- "package:base"

  # Object to return in case of empty data
  #nothing <- data.frame(Envir = character(0), Name = character(0),
  #	Dims = character(0), Group = character(0), Class = character(0),
  #	Recursive = logical(0), stringsAsFactors = FALSE)
  #if (!isTRUE(all.info)) nothing <- nothing[, -1]
  #attr(nothing, "all.info") <- all.info
  #attr(nothing, "envir") <- ename
  #attr(nothing, "object") <- object
  #class(nothing) <- c("objList", "data.frame")

  # This is ~15x faster:
  nothing <- structure(list(Name = character(0),
    Dims = character(0), Group = character(0), Class = character(0),
    Recursive = logical(0), stringsAsFactors = FALSE),
    class = c("objList", "data.frame"),
    all.info =  all.info, envir = ename, object = object
  )
  if (isTRUE(all.info))
    nothing <- cbind(Envir = character(0), nothing)

  if (is.null(envir))
    return(nothing)

  if (!missing(object) && is.character(object) && object != "") {
    res <- .ls_obj(envir = envir, objname = object)
  } else {
    # Get the list of objects in this environment
    items <- ls(envir = envir, all.names = all.names, pattern = pattern)
    if (length(items) > 0) {
      # Get characteristics of all objects
      describe <- function(name, all.info = FALSE) {
        # Get a vector with five items:
        # Name, Dims, Group, Class and Recursive
        obj <- envir[[name]]
        res <- c(
          Name = name,
          Dims = if (is.null(Dim <- dim(obj))) length(obj) else
            paste(Dim, collapse = "x"),
          Group = mode(obj),
          Class = class(obj)[1],
          Recursive = is.recursive(obj) || mode(obj) == "S4"
        )
        return(res)
      }
      res <- data.frame(t(sapply(items, describe, all.info = all.info)),
        stringsAsFactors = FALSE)

      # Quote non-syntactic names
      nsx <- res$Name != make.names(res$Name)
      res$Full.name[!nsx] <- res$Name[!nsx]
      res$Full.name[nsx] <- paste("`", res$Name[nsx], "`", sep = "")
      res <- res[, c(1, 6, 2:5)]
    } else res <- nothing

    # No, because if rm(list = ls()), we must reactualize the objects
    # browser anyway
    if (NROW(res) == 0) return(nothing)

    if (isTRUE(all.info)) res <- cbind(Envir = ename, res)

    v_mode <- groups <- res$Group
    v_class <- res$Class

    # Recalculate groups into meaningful ones for the object explorer
    # 1) Correspondance of typeof() and group depicted in the browser
    groups[groups %in% c("name", "environment", "promise", "language", "char",
      "...", "any", "(", "call", "expression", "bytecode", "weakref",
      "externalptr")] <- "language"

    groups[groups == "pairlist"] <- "list"

    # 2) All groups not being language, function or S4 whose class is
    #    different than typeof are flagged as S3 objects
    groups[!(groups %in% c("language", "function", "S4")) &
      v_mode != v_class] <- "S3"

    # 3) Integers of class factor become factor in group
    groups[v_class == "factor"] <- "factor"

    # 4) Objects of class 'data.frame' are also group 'data.frame'
    groups[v_class == "data.frame"] <- "data.frame"

    # 5) Objects of class 'Date' or 'POSIXt' are of group 'DateTime'
    groups[v_class == "Date" | v_class == "POSIXt"] <- "DateTime"

    # Reaffect groups
    res$Group <- groups

    # Possibly filter according to group
    if (!is.null(group) && group != "")
      res <- res[groups == group, ]
  }

  # Determine if it is required to refresh something
  Changed <- TRUE
  if (isTRUE(compare)) {
    allList <- get_temp(".guiObjListCache", default = list())

    if (identical(res, allList[[id]])) Changed <- FALSE else {
      # Keep a copy of the last version in SciViews:TempEnv
      allList[[id]] <- res
      assign_temp(".guiObjListCache", allList)
    }
  }

  ## Create the 'objList' object
  attr(res, "all.info") <- all.info
  attr(res, "envir") <- ename
  attr(res, "object") <- object
  attr(res, "changed") <- Changed
  attr(res, "class") <- c("objList", "data.frame")

  if (is.null(path)) {  # Return results or "" if not changed
    return(if (Changed) res else nothing)
  } else if (Changed) {  # Write to files in this path
    return(write.objList(res, path = path, sep = sep, ...))
  } else {
    return(nothing)  # Not changed
  }
}

#' @export
#' @rdname obj_browse
write.objList <- function(x, path, sep = "\t", ...) {
  id <- attr(x, "id")
  list_file <- file.path(path, sprintf("List_%s.txt", id))
  pars_file <- file.path(path, sprintf("Pars_%s.txt", id))

  write.table(as.data.frame(x), row.names = FALSE, col.names = FALSE,
    sep = sep, quote = FALSE, file = list_file)

  ## Write also in the Pars_<id>.txt file in the same directory
  cat(sprintf("envir=%s\nall.names=%s\npattern=%s\ngroup=%s",
    attr(x, "envir"), attr(x, "all.names"), attr(x, "pattern"),
    attr(x, "group")), file = pars_file, append = FALSE)

  invisible(list_file)
}

#' @export
#' @rdname obj_browse
print.objList <- function(x, sep = NA, eol = "\n",
header = !attr(x, "all.info"), raw.output = !is.na(sep), ...) {
  if (!inherits(x, "objList"))
    stop("x must be an 'objList' object")

  empty <- NROW(x) == 0

  if (!raw.output)
    cat(if (empty) "An empty objects list\n" else "Objects list:\n")

  if (header) {
    header_fmt <- if (raw.output) "Env=%s\nObj=%s\n" else
      "\tEnvironment: %s\n\tObject: %s\n"

    objname <- if (is.null(attr(x, "object"))) {
      if (raw.output) "" else "<All>"
    } else attr(x, "object")

    cat(sprintf(header_fmt,  attr(x, "envir"), objname))
  }

  if (!empty) {
    if (is.na(sep)) {
      print(as.data.frame(x))
    } else if (!is.null(nrow(x)) && nrow(x) > 0) {
      write.table(x, row.names = FALSE, col.names = FALSE, sep = sep,
        eol = eol, quote = FALSE)
    }
  }
  invisible(x)
}

# Called by obj_list() when object is provided
.ls_obj <- function(objname, envir, ...) {
  obj <- try(eval(parse(text = objname), envir = as.environment(envir)),
    silent = TRUE)
  if (inherits(obj, "try-error"))
    return(NULL)

  if (is.environment(obj))
    obj <- as.list(obj)

  if (mode(obj) == "S4") {
    ret <- .ls_obj_s4(obj, objname)
  } else if (is.function(obj)) {
    ret <- .ls_obj_function(obj, objname)
  } else {# S3
    if (!(mode(obj) %in% c("list", "pairlist")) || length(obj) == 0)
      return(NULL)

    item_names <- full_names <- names(obj)
    if (is.null(item_names)) {
      item_names <- seq_along(obj)
      full_names <- paste(objname, "[[", item_names, "]]", sep = "")
    } else {
      w_names <- item_names != ""
      i_names <- item_names[w_names]
      nsx <- i_names != make.names(i_names)  # Non-syntactic names
      i_names[nsx] <- paste0("`", i_names[nsx], "`")
      full_names[w_names] <- paste0(objname, "$", i_names)
      full_names[!w_names] <- paste0(objname, "[[",
        seq_along(item_names)[!w_names], "]]")
    }

    ret <- t(sapply(seq_along(obj), function(i) .obj_desc(obj[[i]])))
    ret <- data.frame(itemnames = item_names, fullnames = full_names, ret,
      stringsAsFactors = FALSE)
  }
  if (!is.null(ret))
    names(ret) <- c("Name", "Full.name", "Dims/default", "Group", "Class",
      "Recursive")
  ret
}

# Called by .ls_obj for functions
.ls_obj_function <- function(obj, objname = deparse(substitute(obj))){
  # formals(obj) returns NULL if only arg is ..., try: formals(expression)
  obj <- formals(args(obj))
  objname <- paste("formals(args(", objname, "))", sep = "")

  if (length(obj) == 0)
    return(NULL)

  item_names <- full_names <- names(obj)
  nsx <- item_names != make.names(item_names) # non-syntactic names
  item_names[nsx] <- paste("`", item_names[nsx], "`", sep = "")
  full_names <- paste(objname, "$", item_names, sep = "")

  ret <- t(sapply(seq_along(obj), function(i) {
    x <- obj[[i]]
    lang <- is.language(obj[[i]])
    obj_class <- class(obj[[i]])[1]
    obj_mode <- mode(obj[[i]])

    d <- deparse(obj[[i]])
    if (lang && obj_class == "name") {
      obj_class <- ""
      obj_mode <- ""
    }

    ret <- c(paste(d, collapse = "x"), obj_class,	obj_mode, FALSE)
    return(ret)
  }))

  data.frame(itemnames = item_names, fullnames = full_names, ret,
    stringsAsFactors = FALSE)
}

# Called by .ls_obj in S4 case
.ls_obj_s4 <- function(obj, objname = deparse(substitute(obj))) {
  item_names <- full_names <- slotNames(obj)
  nsx <- item_names != make.names(item_names)
  item_names[nsx] <- paste("`", item_names[nsx], "`", sep = "")
  full_names <- paste(objname, "@", item_names, sep = "")

  ret <- t(sapply(item_names, function(i) .obj_desc(slot(obj, i))))

  data.frame(itemnames = item_names, fullnames = full_names,
    ret, stringsAsFactors = FALSE)
}

# Returns a *character* vector with elements: dims, mode, class, rec(ursive)
.obj_desc <- function(x) {
  d <- dim(x)
  if (is.null(d)) d <- length(x)

  c(dims = paste(d, collapse = "x"),
    mode = mode(x), class = class(x)[1],
    rec = mode(x) == "S4" || is.function(x) ||
      (is.recursive(x) && !is.language(x) && sum(d) != 0))
}

#' @export
#' @rdname obj_browse
obj_search <- function(sep = "\t", path = NULL, compare = TRUE) {
  new_search <- search()
  if (isTRUE(compare)) {
    old_search <- get_temp(".guiObjSearchCache", default = "")
    # Compare both versions
    if (length(new_search) != length(old_search) ||
      !all(new_search == old_search)) {
      # Keep a copy of the last version in SciViews:TempEnv
      assign_temp(".guiObjSearchCache", new_search)
      is_changed <- TRUE
    } else is_changed <- FALSE
  } else is_changed <- TRUE

  if (is.null(path)) {# Return result, as a single character string with sep
    if (is_changed) {
      if (!is.null(sep)) new_search <- paste(new_search, collapse = sep)
      return(new_search)
    } else return("")
  } else {# Write to a file called 'Search.txt' in this path
    file <- file.path(path, "Search.txt")
    if (is_changed) {
      if (is.null(sep)) sep <- "\n"
      cat(new_search, sep = sep, file = file)
    }
    invisible(is_changed)
  }
}

#' @export
#' @rdname obj_browse
obj_menu <- function(id = "default", envir = .GlobalEnv, objects = "",
sep = "\t", path = NULL) {
  # TODO: look also in .required (now .Depends) in .GlobalEnv to determine
  # if one can detach a package
  # TODO: copy name to clipboard, send name to editor in menu

  # Get a context menu  for given object(s) or environment
  # It returns a matrix with the following columns:
  # - widget: "menu", "item", "sep" or "space"
  # - value: label of the menu or the item, or "-" for a separator
  # - tip: a short description of this menu entry
  # - code: the command to issue. Precede it with <<<h>>> to run the command
  #   silently and by <<<H>>> to run it silently and disconnect immediately
  #   Use a substitution placeholder for replacement in value, tip and code.
  #   For instance, <<<obj>>>, <<<objname>>>, <<<pos>>>, <<<envir>>> and
  #   add an argument returning the string to place there in the call to
  #   .addStripbar()
  # - icon: the URL of the icon to use in the menu
  # - checked: if the menu entry should present a check mark
  # - disabled: if the menu entry is disabled
  # - hidden: if the menu entry is hidden
  # - options: further options that the GUI client can interpret
  # Notes:
  # - Selecting environments and objects inside environments is not allowed
  # - Selecting multiples environments is not supported
  # - Selecting items in different environments is not allowed

  popup <- .create_stripbar("popupbar")

  # Format envir as character (use only first item provided!)
  if (is.environment(envir)) envir = deparse(substitute(envir))
  if (is.numeric(envir)) envir <- search()[envir[1]]
  envir <- as.character(envir)[1]
  # Get the current position in the search path for envir
  pos <- match(envir, search(), nomatch = -1)
  if (pos < 1)
    return(invisible(popup))  # Not found!

  # Do we replace envir or not?
  if (envir == ".GlobalEnv") Envir <- "<<<envir>>>" else Envir <- envir

  # Look at 'objects'
  if (length(objects) > 1) {  # We have a multiple selection
    obj_type <- "multiple"
    # Add 'save' and 'remove' menu entries
    objs_par <- paste(objects, collapse = ", ")
    popup <- .add_stripbar(popup, c("save", "remove"), obj = objs_par,
      envir = Envir)

  } else {# Only one item is selected
    if (objects == "") {# This is a menu for the environment
      # The menu is different depending if we are in .GlobalEnv or not
      if (envir == ".GlobalEnv") {
        obj_type <- ".GlobalEnv"
        obj_par <- NULL  # Nothing special to propose
        popup <- .add_stripbar(popup, c("load", "source", "import", "sep_",
          "detach (d)"), envir = envir)

      } else if (regexpr("^package:", envir) > -1) {# A package envir
        obj_type <- "package"
        obj_par <- sub("^package:", "", envir)
        req <- getOption("required.packages")
        if (is.null(req))  # Use list of known sensible environments
          req <- c("package:base", "package:methods", "package:datasets",
            "package:utils", "package:grDevices", "package:graphics",
            "package:stats", "package:tcltk", "package:svMisc",
            "package:svGUI", "package:svSocket", "package:svViews",
            "package:svIO", "package:svIDE", "package:svDialogs",
            "package:svWidgets", "package:tcltk2")
        # Make sure that "package:base" is in the list
        req <- c("package:base", req)
        detachable <- !(envir %in% req)
        if (detachable) state <- "" else state <- " (d)"
        popup <- .add_stripbar(popup, c("pkgInfo", "sep_",
          paste("detach", state, sep = ""),
          paste("detachUnload", state, sep = "")
          ), package = obj_par, envir = Envir)

      } else {# Another environment than .GlobalEnv, but not a package
        obj_type <- "environment"
        req <- getOption("required.environments")
        if (is.null(req))  # Use list of known sensible environments
          req <- c("SciViews:TempEnv", "RcmdrEnv", "Autoloads", "tools:RGUI")
        detachable <- !(envir %in% req)
        if (detachable) state <- "" else state <- " (d)"
        obj_par <- detachable
        popup <- .add_stripbar(popup, c(paste("detach", state, sep = "")),
          envir = Envir)
      }

    } else {# This is a menu for an object
      if (!exists(objects, where = envir))
        return(invisible(popup))

      obj_type <- "object"
      obj <- get(objects, pos = envir)
      obj_par <- class(obj)

      # Look if there is an help file and examples for this object
      hlp <- is_help(objects)
      names(hlp) <- NULL
      # Then we add 'help' and 'example' at the top of the menu
      popup <- .add_stripbar(popup, c(
        ifelse(hlp, c("help", "example"),
        c("help (d)", "example (d)")), "sep_"), obj = objects)

      # Add a series of standard functions/methods for this object
      mets <- list_methods(class = obj_par)
      popup <- .add_stripbar(popup, c("Functions_.", "._print",
        ifelse("summary" %in% mets, "._generic", "._generic (d)")
        ), obj = objects, fun = "summary")
      popup <- .add_stripbar(popup, c(
        ifelse("plot" %in% mets, "._generic", "._generic (d)"),
        ifelse(!is.null(names(obj)), "._names", "._names (d)"), "._str"
        ), obj = objects, fun = "plot")
      # Eliminate 'print', 'show', 'summary' and 'plot' from mets
      mets <- mets[!(mets %in% c("print", "show", "summary", "plot"))]
      # Add the other methods from mets in the menu
      if (length(mets) > 0) {
        popup <- .add_stripbar(popup, "._sep_")
        for (met in mets)
          popup <- .add_stripbar(popup, "._generic", obj = objects, fun = met)
      }

      # Add a &View menu and at least one entry: &Default view
      enabled_views <- "svViews" %in% search()	# We need this package
      if (enabled_views) {
        popup <- .add_stripbar(popup, c("View_.", "._viewDef"
          ), obj = objects)
        # Complete the '&View' submenu with more entries
        view_types <- list_types("view", obj_par)
        for (view_type in view_types)
          popup <- .add_stripbar(popup, "._view", obj = objects,
            type = view_type)
        popup <- .add_stripbar(popup, "report", obj = objects)
      } else {# Give the opportunity to load the svViews package
        popup <- .add_stripbar(popup, c("View_.", "._viewDef (d)", "._require",
          "report (d)"), obj = objects, pkg = "svViews")
      }

      # Add &edit/fix and &save
      popup <- .add_stripbar(popup, c(
        ifelse(envir == ".GlobalEnv", "edit", "fix"), "save"
        ), obj = objects, envir = Envir)

      # If we are in .GlobalEnv and the object is a data.frame or a list,
      # then I can attach/detach it
      if (envir == ".GlobalEnv" && inherits(obj, "list")) {
        # Is this object already attached (present in the search path)
        if (objects %in% search()) {
          popup <- .add_stripbar(popup, c("detach", "reattach", "sep_"),
            obj = objects)
        } else {# The object is not attached yet...
          popup <- .add_stripbar(popup, c("attach", "sep_"), obj = objects)
        }
      }

      # Create the '&copy' submenu with all possible entries
      # plus &Export and &Remove
      enabled_copy <- "svIO" %in% search()	# We need this package
      if (enabled_copy) {
        popup <- .add_stripbar(popup, c("Copy_.", "._copyDef"
          ), obj = objects)
        # Complete the '&Copy' submenu with more entries
        copy_types <- list_types("copy", obj_par)
        for (copy_type in copy_types)
          popup <- .add_stripbar(popup, "._copy", obj = objects,
            type = copy_type)
          popup <- .add_stripbar(popup, c("export", "sep_", "remove"
            ), obj = objects, envir = Envir)
      } else {# Give the opportunity to load the svIO package
        popup <- .add_stripbar(popup, c("Copy_.", "._copyDef (d)", "._require",
          "export (d)", "sep_", "remove"
          ), obj = objects, envir = Envir, pkg = "svIO")
      }
    }
  }# Done (menu construction)

  # Possibly call a custom function .objMenu() in SciViews:TempEnv
  # to finalize the menu
  # Depending on obj_type, we send something we already calculated to avoid
  # doing it twice:
  # obj_type   -> obj_par
  # .GlobalEnv   NULL
  # package      Package name
  # environment  is it detachable or not?
  # object       class of the object
  # multiple     string with the multiple objects separated by ', '
  cmd_fun <- get_temp(".objMenu", mode = "function")
  if (!is.null(cmd_fun))
    popup <- cmd_fun(popup, id = id, envir = envir, pos = pos,
      objects = objects, path = path, objType = obj_type, objPar = obj_par)

  if (!is.null(path)) {
    # Save the data in a file
    if (path == "") path <- obj_dir()
    menu_file <- file.path(path, paste("Menu_", id, ".txt", sep = ""))
    write.table(popup, file = menu_file, sep = sep, row.names = FALSE,
      col.names = FALSE)
  }

  # TODO: make this more flexible
  # Possibly call a .guiObjMenu function to pass the data to the GUI client
  cmd_fun <- get_temp(".guiObjMenu", mode = "function")
  if (!is.null(cmd_fun))
    cmd_fun(id = id, data = popup)

  # Return the menu specification invisibly
  invisible(popup)
}

.create_stripbar <- function(type = c("menubar", "popupbar", "toolbar",
  "buttonbar", "statusbar")) {
  type <- match.arg(type)
  strp <- data.frame(widget = character(), value = character(),
    tip = character(), code = character(), icon = character(),
    checked = logical(), disabled = logical(), hidden = logical(),
    options = character(), stringsAsFactors = FALSE)
  class(strp) <- unique(c("svStripbar", "svStrip", class(strp)))
  attr(strp, "type") <- type
  strp
}

.add_stripbar <- function(strip, widgets, gui = getOption("svGUI.name"),
  actions = NULL, ..., icons = NULL) {

  if (!inherits(strip, "svStripbar"))
    stop("'strip' must be a 'svStripbar' object")

  strip_type <- attr(strip, "type")
  if (is.null(strip_type))
    strip_type <- "menubar"	# Default value

  # Extract possible state information from widgets
  pos <- regexpr(" *[(][cCuUdDeEhHvV]+[)] *$", widgets)
  pos[pos == -1] <- 1000000
  state <- substr(widgets, pos, 1000000)

  # Clean up state to keep only digits
  state <- tolower(sub("^ *[(]([cCuUdDeEhHvV]+)[)] *$", "\\1", state))
  widgets <- substr(widgets, 1, pos - 1)

  # If widgets is a named character vector, just check it is
  # 'menu', 'item', 'sep' or 'space', otherwise, compile name = widget
  wnames <- names(widgets)
  if (is.null(wnames)) {
    wnames <- widgets
    # Determine widgets according to wnames
    widgets <- rep("item", length.out = length(wnames))
    widgets[regexpr("_[.]$", wnames) > -1] <- "menu"
    widgets[regexpr("_$", wnames) > -1] <- "sep"
    widgets[regexpr("__$", wnames) > -1] <- "space"
    names(widgets) <- wnames
  } else {
    # Name is provided => check content for 'menu', 'item', 'sep' or 'space'
    if (!all(widgets %in% c("menu", "item", "sep", "space")))
      stop("'widgets' must be 'menu', 'item', 'sep' or 'space'")
  }

  # Get tree hierarchy of the menus being the number of dots before '_'
  tree <- sub("^([.]+)_.*$", "\\1", wnames)
  tree[regexpr("^[.]+$", tree) == -1] <- ""
  tree <- gsub("[.]", "|", tree)

  # Clean up widget names
  wnames <- sub("^[.]+_", "", wnames)
  wnames <- sub("_[.|_]{0,1}$", "", wnames)

  # Guess some of the values from the names
  value_def <- wnames
  value_def[widgets == "sep"] <- "-"
  value_def[widgets == "space"] <- "<->"

  # Collect together 'text', 'code', 'state' and 'options' from actions,
  # .svActions.[gui] and .svActions
  if (is.null(gui)) gui <- "___"	# Default value if no gui exists
  act_gui <- get_temp(paste(".svActions", gui, sep = "."),
    default = structure(list(), class = c("svActions", "list")))
  act <- get_actions()
  # Collect together items
  def_text <- c(actions$text, act_gui$text, act$text)
  def_code <- c(actions$code, act_gui$code, act$code)
  def_state <- c(actions$state, act_gui$state, act$state)
  def_options <- c(actions$options, act_gui$options, act$options)

  # Do the same for icons
  def_icons <- c(icons,
    get_temp(paste(".svIcons", gui, sep = "."), default = character()),
    get_temp(".svIcons", default = character()))

  # The function used to replace placeholders in text and code
  replace <- function(x, ...) {
    # Do replacement for ... arguments
    args <- list(...)
    largs <- length(args)
    if (length(args) > 0) {
      nargs <- names(args)
      for (i in 1:largs)
        if (nargs[i] != "")
          x <- gsub(paste("<<<", nargs[i], ">>>", sep = ""), args[[i]], x)
    }
    # Eliminate optional parts where no replacement occured
    x <- gsub("\\[\\[\\[.*<<<.*>>>.*\\]\\]\\]", "", x)
    # Eliminate triple square brackets for optional parts we keep
    x <- gsub("\\[\\[\\[", "", x)
    x <- gsub("\\]\\]\\]", "", x)
    x
  }

  # Compute the different elements we need
  # - text => value (first line) and tip (the rest)
  text <- replace(def_text[wnames], ...)
  text[is.na(text)] <- ""	# TODO: a better guess for menus, items and sep/space
  names(text) <- wnames
  pos <- regexpr("\n", text)
  pos[pos == -1] <- 1000000
  value <- substr(text, 1, pos - 1)
  value[value == ""] <- value_def[value == ""]
  tip <- substr(text, pos + 1, 1000000)

  # Indicate menu hierarchy in value
  value <- paste(tree, value, sep = "")

  # - code
  code <- replace(def_code[wnames], ...)
  code[is.na(code)] <- ""
  names(code) <- wnames

  # - icon
  icon <- def_icons[wnames]
  icon[is.na(icon)] <- ""
  names(icon) <- wnames

  # - options
  options <- def_options[wnames]
  options[is.na(options)] <- ""
  names(options) <- wnames

  # Compute state => checked, disabled and hidden
  state2 <- def_state[wnames]
  state2[is.na(state2)] <- ""
  state <- paste(state, tolower(state2))
  checked <- ifelse(regexpr("c", state) > -1, TRUE, FALSE)
  disabled <- ifelse(regexpr("d", state) > -1, TRUE, FALSE)
  hidden <- ifelse(regexpr("h", state) > -1, TRUE, FALSE)

  # Create the data frame containing the data
  add_strip <- data.frame(widget = widgets, value = value, tip = tip,
    code = code, icon = icon, checked = checked, disabled = disabled,
    hidden = hidden, options = options, stringsAsFactors = FALSE)
  snames <- rownames(strip)
  # Add it to strip and return it
  strip <- rbind(strip, add_strip)
  rownames(strip) <- make.names(c(snames, wnames), unique = TRUE)
  # Make sure class and type are kept
  class(strip) <- unique(c("svStripbar", "svStrip", class(strip)))
  attr(strip, "type") <- strip_type
  strip
}

#test <- c(
#	"load",
#	"sep_",
#	"File_.",
#	"._import (cdh)",
#	"._sep_",
#	"._export (h) ",
#	"._View_.",
#	".._viewDef (ch)",
#	".._view",
#	"._report",
#	"attach  (d)  "
#)
#pop <- .create_stripbar("popupbar")
#.add_stripbar(pop, test, obj = "testobj", type = "mytype")


# Backward compatibility

#' @export
#' @rdname obj_browse
objBrowse <- obj_browse

#' @export
#' @rdname obj_browse
objClear <- obj_clear

#' @export
#' @rdname obj_browse
objDir <- obj_dir

#' @export
#' @rdname obj_browse
objInfo <- obj_info

#' @export
#' @rdname obj_browse
objList <- obj_list

#' @export
#' @rdname obj_browse
objSearch <- obj_search

#' @export
#' @rdname obj_browse
objMenu <- obj_menu
