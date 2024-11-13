#' Miscellaneous Functions for 'SciViews::R'
#'
#' The \{svMisc\} package is of general use among SciViews::R, a layer on top of
#' R, and the tidyverse. This package collects together a series of general
#' functions to manage a centralized environment for temporary variables, a
#' progress bar and batch analysis mode, etc.
#'
#' @section Important functions:
#'
#' - [temp_env()] for using a temporary environment attached to the search path,
#'
#' - [temp_var()] create the name of temporary variables,
#'
#' - [capture_all()] to capture R output, errors, warnings and messages,
#'
#' - [parse_text()] to parse any R expression, including partial or incorrect
#'   ones (fails gracefully).

## usethis namespace: start
#' @importFrom utils RSiteSearch apropos available.packages browseURL
#'   capture.output compareVersion file.edit file_test flush.console
#'   getCRANmirrors getS3method install.packages installed.packages methods
#'   object.size packageDescription remove.packages str tail write.table ? help
#'   help.search apropos find txtProgressBar setTxtProgressBar
#' @importFrom methods findFunction existsFunction new isGeneric
#'   showMethods slot slotNames
#' @importFrom stats cor fft quantile rnorm runif
#' @importFrom tools file_path_as_absolute
#' @importFrom rlang is_interactive
## usethis namespace: end
"_PACKAGE"
