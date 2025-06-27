.get_text_lang_factory <- function() {
  # This function makes necessary checkings only once and keeps the results
  # Also, it keeps memory of languages already used and languages that failed
  # to speed up switch from one language to another

  # Is natural language supported?
  no_nls <- (!capabilities("NLS") || is.na(.popath) ||
      Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))

  # The (growing) list of supported languages (each time a successful switch
  # to such a language is done, it is recorded here)
  # Obviously, current language is already supported
  known_lang <- Sys.getenv("LANGUAGE", unset = NA)
  if (is.na(known_lang) || !nzchar(known_lang))
    known_lang <- "en"

  # The growing list of failed languages (e.g., strings that do not match any
  # known language)
  failed_lang <- character(0)

  # Run Sys.setLanguage() on current language to make sure everything is fine
  if (!no_nls) {
    res <- attr(Sys.setLanguage(known_lang[1]), "ok")
    # If it fails, consider no_nls with a warning
    if (!res) {
      no_nls <- TRUE
      warning("Natural language support is available on this system, ",
        "but unable to properly switch language.")
    }
  }

  # Make sure current language is properly defined in the LANGUAGE envir var
  Sys.setenv(LANGUAGE = known_lang[1])

  # The function that retrieves one or more translated character strings in a
  # given language (may be different to current R language)
  get_text_lang_fun <- compiler::cmpfun(
    function(..., lang = getOption("data.io_lang",
      default = Sys.getenv("LANGUAGE", unset = "en")), domain = NULL,
      trim = TRUE) {

      # If no_nls or same as current language, or failed lang, just run gettext()
      lang <- as.character(lang)[1] # Use only first string, without warning
      cur_lang <- Sys.getenv("LANGUAGE", unset = "en")
      if (no_nls || lang == cur_lang || any(failed_lang == lang)) {
        #message("Optimisation #1!")
        return(gettext(..., domain = domain, trim = trim))
      }

      # If the  language is already known, switch faster
      if (any(known_lang == lang)) {
        #message("Optimisation #2!")
        Sys.setenv(LANGUAGE = lang)
        on.exit(Sys.setenv(LANGUAGE = cur_lang))
      } else {
        # Use the slower Sys.setLanguage() to switch language properly
        # the first time, then, record it in known_lang if it works
        cur_lang <- Sys.setLanguage(lang)
        on.exit(Sys.setenv(LANGUAGE = cur_lang))
        if (attr(cur_lang, "ok")) {
          known_lang <<- c(known_lang, lang) # Record the new language
        } else {# Failed to switch to this lang
          failed_lang <<- c(failed_lang, lang) # Record the failed language
          warning("Unable to switch to language '", lang, "'. ",
            "Using current language '", cur_lang, "' instead ",
            "(displayed only once per session).")
          return(gettext(..., domain = domain, trim = trim))
        }
      }

      # Flush the cache of translations since we switch to another language
      bindtextdomain(NULL)

      res <- gettext(..., domain = domain, trim = trim)

      # Flush the cache of translations again before switching back to the
      # current language (done in on.exit)
      bindtextdomain(NULL)

      # This is for debugging purposes only
      #message(no_nls)
      #message(paste0(known_lang, collapse = ", "))
      #message(paste0(failed_lang, collpase = ", "))

      res
    })

  # Return the function
  get_text_lang_fun
}

#' Translate text messages in a different language than the one currently defined in the R session
#'
#' Translation messages are obtained with [base::gettext()]. But, there is no way to specify that one needs translated messages in a different language than the current one in R. This function adds a `lang=` argument that allows to do so.
#'
#' @param ... one of more character vectors
#' @param lang the desired language (usually in two letters, e.g., "en", "fr", "de", etc.)
#' @param domain the 'domain' for the translation, a character string or `NULL`; see [base::gettext()] for more details.
#' @param trim logical indicating if the white space trimming should happen. `trim = FALSE` may be needed for compiled code (C / C++) messages which often end with `\n`.
#'
#' @returns a character vector with translated messages
#' @export
#'
#' @examples
#' old_lang <- Sys.setLanguage("fr") # Switch to French for R language
#'
#' # Retreive strings in same language
#' get_text_lang("empty model supplied", "incompatible dimensions",
#'  domain="R-stats", lang = "fr")
#'
#' # Retrieve strings in different languages
#' get_text_lang("empty model supplied", "incompatible dimensions",
#'   domain="R-stats", lang = "en")
#' get_text_lang("empty model supplied", "incompatible dimensions",
#'   domain="R-stats", lang = "de")
#'
#' # Try to get strings translated in an unknown language (just return the strings)
#' get_text_lang("empty model supplied", "incompatible dimensions",
#'   domain="R-stats", lang = "xx")
#'
#' Sys.setLanguage(old_lang) # Restore original language
#' rm(old_lang)
get_text_lang <- .get_text_lang_factory()
