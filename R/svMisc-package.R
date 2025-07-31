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
#' @importFrom cli cli_abort
#' @importFrom compiler cmpfun
#' @importFrom methods findFunction existsFunction new isGeneric
#' @importFrom rlang abort caller_env eval_bare f_env<- is_formula is_interactive
#' @importFrom stats cor fft quantile rnorm runif
#' @importFrom tools file_path_as_absolute
#' @importFrom utils RSiteSearch apropos available.packages browseURL
#' @importFrom methods showMethods slot slotNames
#' @importFrom utils capture.output compareVersion file.edit file_test find flush.console getCRANmirrors getS3method ? help help.search install.packages installed.packages methods object.size packageDescription remove.packages setTxtProgressBar str tail txtProgressBar write.table
#' @useDynLib svMisc, .registration = TRUE
## usethis namespace: end
"_PACKAGE"
