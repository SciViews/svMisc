#' SciViews - Miscellaneous functions
#'
#' Miscellaneous functions for SciViews or general use. The svMisc package
#' collects together a series of functions that are shared with svXXX packages.
#'
#' @section Important functions:
#'
#' - [temp_env()] for unsing a temporary environment attached to the search path,
#'
#' - [temp_var()] create the name of temporary variables,
#'
#' - [capture_all()] to capture R output, errors, warnings and messages,
#'
#' - [parse_text()] to parse any R expression, including partial or incorrect
#'   ones (fails gracefully).
#'
#' @docType package
#' @name svMisc-package
#'
#' @importFrom utils RSiteSearch apropos available.packages browseURL
#'   capture.output compareVersion file.edit file_test flush.console
#'   getCRANmirrors getS3method install.packages installed.packages methods
#'   object.size packageDescription remove.packages str tail write.table ? help
#'   help.search apropos find
#' @importFrom methods findFunction existsFunction new getMethods isGeneric
#'   showMethods slot slotNames
#' @importFrom stats runif
#' @importFrom tools file_path_as_absolute
NULL
