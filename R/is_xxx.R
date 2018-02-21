#' Check for the existence of an help file, or some context
#'
#' @description For `is_help()`, determine if 'topic' has a help file and
#' example to run. For `is_win()` and `is_mac()`, determine if the platform is
#' Windows or MacOS. For `is_aqua()`, is the R UI is AQUA, the standard R GUI
#' under Macintosh? For `is_rgui()`, determine if the default Rgui under Windows
#' is in use, and with `is_sdi()` in this case, you can check if it is in SDI
#' (single-document interface) _versus_ MDI (multi-document interface, by
#' default). `is_rstudio()` and `is_rstudio_server()` check if R is run under
#' RStudio (server), and `is_jgr()` indicate if the R GUI is JGR.
#'
#' @param topic Name or literal character string: the online help topic to
#' look for.
#' @param package A character vector giving the package names to look into for
#' help or example code, or `NULL`. By default, all packages in the search
#' path are used.
#' @param lib.loc A character vector of directory names of \R libraries, or
#' `NULL`. The default value of `NULL` corresponds to all libraries currently
#' known. If the default is used, the loaded packages are searched before the
#' libraries.
#' @return All these functions return either `TRUE` or `FALSE` depending on the
#' tested item, except for `is_help()`, which returns a logical vector with two
#' elements. The first one indicating if there is a help file, and the second
#' one indicating if there are examples associated with this help file.
#' @note The code of `is_help()` is largely inspired from the first part of
#' `example()`.
#' @note Under **Rgui**, to switch fro MDI to SDI more, go to the menu entry
#' 'Edit' -> 'GUI preferences' to change the Rgui mode, or start Rgui with the
#' '--SDI' argument line parameter. Under another platform than Windows or if it
#' is not Rgui, then `is_sdi()` always returns `FALSE`.`
#' @export
#' @seealso [example()], [help()], [capabilities()]
#' @keywords utilities
#' @concept help and example, and OS system platform
#' @examples
#' is_help("help")		# Help and example
#' is_help("Rtangle")	# Help but no example
#' is_help("notopic")	# No help or example
#'
#' is_win()
#' is_mac()
#'
#' is_aqua()
#' is_rgui()
#' is_sdi()
#' is_rstudio()
#' is_rstudio_desktop()
#' is_rstudio_server()
#' is_jgr()
is_help <- function(topic, package = NULL, lib.loc = NULL) {
  # Code taken from example(), but here we don't run the example!
  topic <- substitute(topic)
  if (!is.character(topic))
    topic <- deparse(topic)[1L]

  pkg_paths <- find.package(package, lib.loc, verbose = FALSE)
  utils_ns <- getNamespace("utils")
  file <- utils_ns$index.search(topic, pkg_paths, TRUE)
  if (!length(file))
    return(c(help = FALSE, example = FALSE))

  encoding <- NULL
  temp_file <- tempfile("Rex")
  on.exit(unlink(temp_file))
  encoding <- "UTF-8"
  tools::Rd2ex(utils_ns$.getHelpFile(file), temp_file)
  if (!file.exists(temp_file)) {
    c(help = TRUE, example = FALSE)
  } else c(help = TRUE, example = TRUE)
}

#' @export
#' @rdname is_help
is_win <- function()
  (.Platform$OS.type == "windows")

#' @export
#' @rdname is_help
is_rgui <- function()
  (.Platform$GUI[1] == "Rgui")

#' @export
#' @rdname is_help
is_sdi <- function() {
  # This function is specific to Windows, but it is defined everywhere
  # so that we don't have to test the platform before use!
  # Check if Rgui was started in SDI mode (needed by some GUI clients)

  # First, is it Rgui?
  if (!is_rgui()) return(FALSE)
  # RGui SDI mode: returns "R Console", in MDI mode: returns "RGui"
  if (utils::getIdentification() == "R Console") TRUE else FALSE
}

#' @export
#' @rdname is_help
is_mac <- function()
  grepl("darwin", R.version$os) # According to what's done in R sources
#(grepl("^mac", .Platform$pkgType))

#' @export
#' @rdname is_help
is_aqua <- function()
  (.Platform$GUI[1] == "AQUA")

#' @export
#' @rdname is_help
is_rstudio <- function()
  !is.null(get0("RStudio.Version"))

#' @export
#' @rdname is_help
is_rstudio_desktop <- function() {
  rstudio_version <- get0("RStudio.Version")
  if (is.null(rstudio_version) || !is.function(rstudio_version))
    return(FALSE)
  (rstudio_version()$mode == "desktop")
}


#' @export
#' @rdname is_help
is_rstudio_server <- function() {
  rstudio_version <- get0("RStudio.Version")
  if (is.null(rstudio_version) || !is.function(rstudio_version))
    return(FALSE)
  (rstudio_version()$mode == "server")
}

#' @export
#' @rdname is_help
is_jgr <- function() {
  # Search for .jgr.works on the whole search path, starting from GlobalEnv
  if (exists(".jgr.works", envir = .GlobalEnv, inherits = TRUE)) {
    get(".jgr.works", envir = .GlobalEnv, inherits = TRUE)
  } else FALSE  # JGR is probably not (correctly) installed
}


# Backward compatibility

#' @export
#' @rdname is_help
isHelp <- is_help

#' @export
#' @rdname is_help
isWin <- is_win

#' @export
#' @rdname is_help
isRgui <- is_rgui

#' @export
#' @rdname is_help
isSDI <- is_sdi

#' @export
#' @rdname is_help
isMac <- is_mac

#' @export
#' @rdname is_help
isAqua <- is_aqua

#' @export
#' @rdname is_help
isJGR <- is_jgr

# One could define this too:
#`%is%` <- function(x, what)
#  inherits(x, what)
