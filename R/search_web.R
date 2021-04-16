#' Search web documents about R and R functions
#'
#' @description Retrieve web documents, or search with Google for `what` string.
#'
#' @param what The string(s) to search. In case of several strings, or several
#' words, any of these words are searched.
#' @param type The search engine, or location to use.
#' @param browse Do we actually show the page in the Web browser? If
#' `type = "R"`, this argument is ignored and the result is always displayed in
#' the Web browser.
#' @param msg Do we issue a message indicating that a page should be displayed
#' shortly in the Web browser? If `type = "R"`, this argument is ignored and a
#' message is always displayed.
#' @param ... Further arguments to format the result page in case of
#' `type = "R"`. These are the same arguments as for [RSiteSearch()].
#' @return Returns the URL used invisibly (invoked for its side effect of
#' opening the Web browser with the search result, when `browse = TRUE`).
#' @note The [RSiteSearch()] function in the 'utils' package is used when
#' `type = "R"`.
#' @export
#' @seealso [RSiteSearch()], [help.search()]
#' @keywords utilities
#' @examples
#' \dontrun{
#' search_web("volatility")                    # R site search, by default
#' search_web("volatility", type = "google")   # Google search
#' }
search_web <- function(what, type = c("R", "google"),
browse = TRUE, msg = browse, ...) {
  what <- paste0(what, collapse = " ")
  what <- gsub(" ", "+", what)
  type <- match.arg(type)
  search_url <- switch(type,
    "R" = RSiteSearch(what, ...),
    "google" = paste0("http://www.google.com/search?sitesearch=r-project.org&q=",
      what),
    stop("'type' could be only 'R', or 'google', currently!"))
  if (type != "R") {
    if (isTRUE(browse))
      browseURL(search_url)
    if (isTRUE(msg)) {
      cat(gettext("A search query has been submitted"), "\n")
      cat(gettext("The results page should open in your browser shortly\n"))
    }
  }
  invisible(search_url)
}


# Backward compatibility

#' @export
#' @rdname search_web
helpSearchWeb <- search_web
