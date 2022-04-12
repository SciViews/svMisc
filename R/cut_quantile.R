#' Convert numeric to factor with intervals of equal number of items by using quantiles
#'
#' @description `cut_quantile()` is like [cut()], but it calculates intervals
#' from quantiles such that each interval has approximately the same number of
#' items from the original vector. `x` must have both [quantile()] and [cut()]
#' methods implemented.
#'
#' @param x An R object, usually a numeric vector.
#' @param breaks A single integer with the number of breaks to use.
#' @param labels Labels for the resulting category or `NULL` (by default) to
#' construct them automatically like "(a,b]". If `labels = FALSE`, simple
#' integer codes are returned instead of factor.
#' @param ... Further arguments passed to [cut()].
#'
#' @return A [factor()] is returned, unless `labels = FALSE` (in this case, a
#' integer vector is obtained).
#' @export
#'
#' @examples
#' # Transform a numeric vector into a factor with 5 levels of same item numbers
#' vec <- rnorm(20)
#' fact <- cut_quantile(vec, breaks = 5)
#' fact
#' table(fact)
cut_quantile <- function(x, breaks, labels = NULL, ...) {
  stopifnot(is.numeric(x) && length(x) > 1)
  stopifnot(length(breaks) == 1 && breaks > 0)
  m <- breaks + 1
  cuts <- quantile(x, seq(0, 1, length.out = m), na.rm = TRUE)
  # Extend range by 0.1%
  ext <- (cuts[m] - cuts[1]) * 0.0005
  cuts[1] <- cuts[1] - ext
  cuts[m] <- cuts[m] + ext
  cut(x, breaks = cuts, labels = labels, ...)
}
