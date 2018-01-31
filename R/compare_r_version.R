#' Compare current R version with a specified one
#'
#' @description Determine if R is older (return -1), or not (return 0 if equal,
#' or 1 if newer) than a given version number.
#'
#' @param version A string defining the version to compare to, like '2.0.0' or
#' '1.9.1'.
#' @return -1 if R is older, 0 if equal, 1 if newer. Take care: if you specify
#' version as "2.11", and R is version "2.11.0", then the function will return 1
#' (newer)!
#' @export
#' @seealso [compareVersion()], [R.version()]
#' @keywords utilities
#' @concept version comparison
#' @examples
#' compare_r_version("2.11.0") # Note that we strongly advise to use R > 2.11.0!
compare_r_version <- function(version) {
  # This is similar to compareVersion, but works for R version comparison
  compareVersion(paste(R.version$major, R.version$minor, sep = "."), version)
}

# Backward compatibility

#' @export
#' @rdname compare_r_version
compareRVersion <- compare_r_version
