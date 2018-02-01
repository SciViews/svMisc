#' Get a completion list for a R code fragment
#'
#' @description Returns names of objects/arguments/namespaces matching a code
#' fragment.
#'
#' @param code A partial R code to be completed.
#' @param pos The position of the cursor in this code.
#' @param min.length The minimal length in characters of `code` required before
#' the completion list is calculated.
#' @param print Logical, print result and return invisibly. See details.
#' @param types A named list giving names of types. Set to \code{NA} to give
#' only names. See details.
#' @param addition Should only addition string be returned?
#' @param sort Do we sort the list of completions alphabetically?
#' @param what What are we looking for? Allow to restrict search for faster
#' calculation.
#' @param description Do we describe items in the completion list
#' (could be slow)?
#' @param max.fun In the case where we describe items, the maximum number of
#' functions to process (if longer, no description is returned for function)
#' because it can be very slow otherwise.
#' @param skip.used.args Logical, if completion is within function arguments,
#' should the already used named arguments be omitted?
#' @param sep The separator to use between returned items.
#' @param field.sep Character string to separate fields for each entry.
#' @return If `types == NA` and `description = FALSE`, a character vector giving
#' the completions, otherwise a data frame with two columns: 'completion', and
#' 'type' when `description = FALSE`, or with four columns: 'completion',
#' 'type', 'desc' and 'context' when `description = TRUE`.\cr
#' Attributes:\cr
#' `attr(, "token")` - a completed token.\cr
#' `attr(, "triggerPos")` - number of already typed characters.\cr
#' `attr(, "fguess")` - name of guessed function.\cr
#' `attr(, "isFirstArg")`` - is this a first argument?
#' @details The completion list is context-dependent, and it is calculated as if
#' the code was entered at the command line.
#'
#' If the code ends with `$` or `[[`, then the function look for items in a list
#' or data.frame whose name is the last identifier.
#'
#' If the code ends with `@`, then the function look for slots of the
#' corresponding S4 object.
#'
#' If the code ends with `::`, then it looks for objects in a namespace.
#'
#' If the code ends with a partial identifier name, the function returns all
#' matching keywords visible from .GlobalEnv.
#'
#' If the code is empty or parses into an empty last token, the list of objects
#' currently in the global environment is returned.
#'
#' @note Take care: depending on the context, the completion list could be
#' incorrect (but it should work for code entered at the command line). For
#' instance, inside a function call, the context is very different, and
#' arguments and local variables should be returned instead. This may be
#' implemented in the future, but for now, we focus on completion that should be
#' most useful for novice useRs that are using R expressions entered one after
#' the other at the R console or in a script (and considering the script is run
#' or sourced line after line in R).
#'
#' There are other situations where the completion can be calculated, see the
#' help of [rc.settings()].
#'
#' If `print == TRUE`, results are returned invisibly, and printed in a form:
#' triggerPos<newline>completions separated by `sep`.
#'
#' If `types` are supplied, a completion will consist of name and type,
#' separated by `type.sep`. `types` may me a vector of length 5, giving the type
#' codes for "function", "variable", "environment", "argument" and "keyword".
#' If `types == "default"`, above type names are given; `types == "scintilla"`
#' will give numeric codes that can be used with "scintilla.autoCShow" function
#' (e.g., with the SciViews-K Komodo Edit plugin).
#' @author Philippe Grosjean <phgrosjean@sciviews.org> &
#' Kamil Barton <kamil.barton@uni-wuerzburg.de>
#' @export
#' @seealso [rc.settings()]
#' @keywords utilities
#' @concept graphical user interface (GUI) control, completion
#' @examples
#' # A data frame
#' data(iris)
#' completion("item <- iris$")
#' completion("item <- iris[[")
#'
#' # An S4 object
#' setClass("track", representation(x = "numeric", y = "numeric"))
#' t1 <- new("track", x = 1:20, y = (1:20)^2)
#' completion("item2 <- t1@")
#'
#' # A namespace
#' completion("utils::", description = TRUE)
#'
#' # A partial identifier
#' completion("item3 <- va", description = TRUE)
#'
#' # Otherwise, a list with the content of .GlobalEnv
#' completion("item4 <- ")
#'
#' # TODO: directory and filename completion!
#' rm(iris, t1)
completion <- function(code, pos = nchar(code), min.length = 2,
print = FALSE, types = c("default", "scintilla"), addition = FALSE, sort = TRUE,
what = c("arguments", "functions", "packages"), description = FALSE,
max.fun = 100, skip.used.args = TRUE, sep = "\n", field.sep = "\t") {

  finalize <- function(completions) {
    # Construct a data frame with completions
    ret <- data.frame(completion = completions, stringsAsFactors = FALSE)

    # Do we add types?
    if (isTRUE(add_types)) {
      tl <- numeric(length(completions))
      tl[grep(" = $", completions)] <- 4L
      tl[grep("::$", completions)] <- 3L
      tl[grep("<-$", completions)] <- 1L
      tl[completions %in% .reserved_words] <- 5L
      tl[!tl] <- ifelse(sapply(completions[!tl],
        function(x) existsFunction(x, where = .GlobalEnv)), 1L, 2L)
      tl <- factor(tl, levels = 1:5, labels = types)
      ret <- cbind(ret, data.frame(type = tl, stringsAsFactors = FALSE))
    }

    # Do we add descriptions?
    if (isTRUE(description)) {
      ret <- cbind(ret, data.frame(desc = rep("", nrow(ret)),
        context = rep("", nrow(ret)), stringsAsFactors = FALSE))

      # Deal with packages (completions ending with ::)
      if (length(test_pack <- grep("::$", completions))) {
        describe_package <- function(pkg) {
          # This is to deal with completion of :, ::, ::: in pkg base
          if (grepl(":$", pkg)) return("") else
            return(packageDescription(pkg, fields = "Description"))
        }
        ret[test_pack, "desc"] <- sapply(sub(":{2,3}$", "",
          completions[test_pack]), describe_package)
      }

      # Deal with argument completions (ending with " = ")
      if (length(test_arg <- grep(" = ", completions))) {
        fun <- getNamespace("utils")$.CompletionEnv[["fguess"]]
        ret[test_arg, "context"] <- fun
        ret[test_arg, "desc"] <- descArgs(fun,
          sub(" = $", "", completions[test_arg]))
      }

      # Deal with completions with "$" (excluding things like base::$)
      if (length(test_dollar <- grep("[^:]\\$", completions))) {
        elements <- completions[test_dollar]
        object <- gsub("\\$.*$", "", completions)[1]
        items <- gsub("^.*\\$", "", completions)
        pack <- .find_multiple(object)
        ret[test_dollar, "context"] <- pack
        ret[test_dollar, "desc"] <- .describe_data(object, items,
          package = pack)
      }

      # Deal with completions with "@" (excluding things like base::$)
      if (length(test_slot <- grep("[^:]@", completions))) {
        elements <- completions[test_slot]
        object <- gsub("@.*$", "", completions)[1]
        slots <- gsub("^.*@", "", completions)
        pack <- .find_multiple(object)
        ret[test_slot, "context"] <- pack
        ret[test_slot, "desc"] <- .describe_slots(object, slots,
          package = pack)
      }

      # Deal with completions with "["
      if (length(test_square <- grep("\\[", completions))) {
        ret[test_square, "desc"] <- .describe_square(completions[test_square],
          package = pack)
      }

      # TODO: do not know what to do with these?
      test_others <- grep(" ", completions)
      # TODO: are there other kind of completions I miss here?

      # Deal with function completions
      test_fun <- setdiff(1:length(completions), c(test_arg, test_pack,
        test_others, test_dollar, test_slot, test_square))
      if (length(test_fun)) {
        funs <- completions[test_fun]
        # If we have nmspace::fun, or nmspace:::fun, split it
        test_nms <- grep(".+::.+", funs)
        packs <- rep("", length(funs))
        if (length(test_nms)) {
          packs[test_nms] <- sub(":{2,3}[^:]+$", "", funs[test_nms])
          funs[test_nms] <- sub("^.+:{2,3}", "", funs[test_nms])
          packs[-test_nms] <- .find_multiple(funs[-test_nms])
        } else packs <- .find_multiple(funs)
        desc_fun <- rep("", length(packs))
        # Do not try to find description for functions in those envs
        is_pack <- !packs %in% c("", ".GlobalEnv", "SciViews:TempEnv",
          "Autoloads", "tools:RGUI")
        # The following code is too slow for many function
        # (it takes 6-7sec for the 1210 base::XXXX functions)
        # So, do it only if less than max.fun
        # Note, without descriptions, it takes 0.3sec on my MacBook Pro
        if (length(is_pack) < max.fun)
          desc_fun[is_pack] <- descFun(funs[is_pack], packs[is_pack])
        ret[test_fun, "context"] <- packs
        ret[test_fun, "desc"] <- desc_fun
      }
    }

    # Do we sort results alphabetically?
    if (isTRUE(sort)) ret <- ret[order(completions), ]

    # Add metadata as attributes
    attr(ret, "token") <- token
    attr(ret, "triggerPos") <- triggerPos
    attr(ret, "fguess") <- fguess
    attr(ret, "funargs") <- funargs
    attr(ret, "isFirstArg") <- isFirstArg

    if (isTRUE(print)) {
      if (is.null(ret$desc)) {
        cat(triggerPos, paste(ret$completion, ret$type, sep = field.sep),
          sep = sep)
      } else {
        cat(triggerPos, paste(ret$completion, ret$type, ret$desc, ret$context,
          sep = field.sep), sep = sep)
      }
      if (sep != "\n") cat("\n")
      invisible(ret)
    } else ret
  }

  # Do we return the type of the entry, and if yes, in which format?
  if (is.character(types[1L])) {
    types <- switch(match.arg(types),
      default = .default_completion_types,
      scintilla = .scintilla_completion_types,
      .default_completion_types)
  }
  add_types <- as.logical(!is.na(types[1L]))

  # Default values for completion context
  token <- ""
  triggerPos <- 0L
  fguess <- ""
  funargs <- list()
  isFirstArg <- FALSE

  # Is there some code provided?
  code <- paste(as.character(code), collapse = "\n")
  if (is.null(code) || !length(code) || code == "" ||
    nchar(code, type = "chars") < min.length) {
    # Just return a list of objects in .GlobalEnv
    # TODO: look if we are inside a function and list
    # local variables (code analysis is required!)
    return(finalize(ls(envir = .GlobalEnv)))
  }

  # If code ends with a single [, then look for names in the object
  if (regexpr("[^[][[]$", code) > 0) {
    # TODO: look for object names... currently, return nothing
    return(invisible(""))
  }

  # If code ends with a double [[, then, substitute $ instead and indicate
  # to quote returned arguments (otherwise, [[ is not correctly handled)!
  if (regexpr("[[][[]$", code) > 0) {
    code <- sub("[[][[]$", "$", code)
    dblBrackets <- TRUE
  } else dblBrackets <- FALSE

  # Save funarg.suffix and use " = " locally
  utils <- getNamespace("utils")
  completion_env <- utils$.CompletionEnv
  opts <- completion_env$options
  funarg.suffix <- opts$funarg.suffix
  on.exit({
    opts$funarg.suffix <- funarg.suffix
    completion_env$options <- opts
  })
  opts$funarg.suffix <- " = "
  completion_env$options <- opts

  # Calculate completion with standard R completion tools
  utils$.assignLinebuffer(code)
  utils$.assignEnd(pos)
  utils$.guessTokenFromLine()
  # The standard utils:::.completeToken() is replaced by our own version:
  .complete_token_ext()
  completions <- utils$.retrieveCompletions()
  triggerPos <- pos - completion_env[["start"]]
  token <- completion_env[["token"]]

  # If token is empty, we complete by using objects in .GlobalEnv by default
  if (!length(completions) && token == "") {
    triggerPos <- nchar(code, type = "chars")
    # TODO: look if we are inside a function and list
    # local variables (code analysis is required!)
    return(finalize(ls(envir = .GlobalEnv)))
  }

  # For tokens like "a[m", the actual token should be "m"
  # completions are modified accordingly
  rx <- regexpr("[[]+", completion_env$token)
  if (rx > 0) {
    # Then we need to trim out whatever is before the [ in the completion
    # and the token
    start <- rx + attr(rx, "match.length")
    completion_env$token <- substring(completion_env$token, start)
    completions <- substring(completions, start)
  }
  if (!length(completions)) return(invisible(""))

  # Remove weird object names (useful when the token starts with ".")
  i <- grep("^[.]__[[:alpha:]]__", completions)
  if (length(i) > 0)
    completions <- completions[-i]
  if (!length(completions))
    return(invisible(""))

  # Restrict completion for which information is gathered (speed things up)
  if (!"arguments" %in% what)
    completions <- completions[regexpr("=$", completions) < 0]
  if (!length(completions))
    return(invisible(""))

  if (!"packages" %in% what)
    completions <- completions[regexpr("::$", completions) < 0]
  if (!length(completions))
    return(invisible(""))

  if (!"functions" %in% what)
    completions <- completions[regexpr("(::|=)$", completions) > 0]
  if (!length(completions))
    return(invisible(""))

  # Eliminate function arguments that are already used
  fguess <- completion_env$fguess
  if (skip.used.args && length(fguess) && nchar(fguess))
    completions <- completions[!(completions %in% completion_env$funargs)]
  if (!length(completions))
    return(invisible(""))

  # Eliminate function names like `names<-`
  i <- grep("<-.+$", completions)
  if (length(i) > 0)
    completions <- completions[-i]

  # Do we return only additional strings for the completion?
  if (isTRUE(addition) && triggerPos > 0L)
    completions <- substring(completions, triggerPos + 1)

  # In case of [[, restore original code
  if (dblBrackets) {  # Substitute var$name by var[["name"
    completions <- sub("[$](.+)$", '[["\\1"', completions)
    token <- sub("[$]$", "[[", token)
    triggerPos <- triggerPos + 1
  }

  # Finalize processing of the completion list
  funargs <- completion_env$funargs
  isFirstArg <- completion_env$isFirstArg
  finalize(completions)
}

.reserved_words <- c("if", "else", "repeat", "while", "function", "for", "in",
  "next", "break", "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_",
  "NA_real_", "NA_complex_", "NA_character_")

.default_completion_types <- list(fun = "function", var = "variable",
  env = "environment", args = "arg", keyword = "keyword")

.scintilla_completion_types <- list(fun = "1", var = "3",
  env = "8", args = "11", keyword = "13")

.describe_data <- function(data, columns, package = NULL, lib.loc = NULL)
  character(length(columns))

.describe_slots <- function(object, slots, package = NULL, lib.loc = NULL)
  character(length(slots))

.describe_square <- function(completions, package = NULL)
  character(length(completions))

# Modified utils:::inFunction()
# (checked equivalent with R 2.11.1)
# Only difference: it also gets current arguments list (if applicable).
# They are assigned to utils:::.CompletionEnv$funargs
.in_function_ext <- function(line, cursor) {
  utils <- getNamespace("utils")
  if (missing(line))
    line <- utils$.CompletionEnv[["linebuffer"]]
  if (missing(cursor))
    cursor <- utils$.CompletionEnv[["start"]]

  parens <- sapply(c("(", ")"), function(s)
    gregexpr(s, substr(line, 1L, cursor), fixed = TRUE)[[1L]], simplify = FALSE)

  parens <- lapply(parens, function(x) x[x > 0])
  temp <- data.frame(i = c(parens[["("]], parens[[")"]]),
    c = rep(c(1, -1), sapply(parens, length)))

  if (nrow(temp) == 0)
    return(character(0L))

  temp <- temp[order(-temp$i), , drop = FALSE]
  wp <- which(cumsum(temp$c) > 0)
  if (length(wp)) {
    index <- temp$i[wp[1L]]
    prefix <- substr(line, 1L, index - 1L)
    suffix <- substr(line, index + 1L, cursor + 1L)
    if ((length(grep("=", suffix, fixed = TRUE)) == 0L) &&
      (length(grep(",", suffix, fixed = TRUE)) == 0L))
      utils$setIsFirstArg(v = TRUE)
    if ((length(grep("=", suffix, fixed = TRUE))) && (length(grep(",",
      substr(suffix, tail(gregexpr("=", suffix, fixed = TRUE)[[1L]],
      1L), 1000000L), fixed = TRUE)) == 0L)) {
      return(character(0L))

    } else {
      # This is the code added to utils:::inFunction()
      wp2 <- rev(cumsum(temp$c[-(wp[1L]:nrow(temp))]))
      suffix <- sub("^\\s+", "", suffix, perl = TRUE)
      # TODO: simplify this:
      if (length(wp2)) {
        funargs <- strsplit(suffix,	"\\s*[\\(\\)][\\s,]*", perl = TRUE)[[1]]
        funargs <- paste(funargs[wp2 == 0], collapse = ",")
      } else {
        funargs <- suffix
      }
      funargs <- strsplit(funargs, "\\s*,\\s*", perl = TRUE)[[1]]
      funargs <- unname(sapply(funargs, sub, pattern = "\\s*=.*$",
        replacement = utils$.CompletionEnv$options$funarg.suffix,
        perl = TRUE))
      assign("funargs", funargs, utils$.CompletionEnv)
      # TODO: how to take non named arguments into account too?
      # ... addition ends here

      possible <- suppressWarnings(strsplit(prefix, utils$breakRE,
        perl = TRUE))[[1L]]
      possible <- possible[possible != ""]
      if (length(possible)) {
        return(tail(possible, 1))
      } else {
        return(character(0L))
      }
    }
  } else {
    return(character(0L))
  }
}

# Modified utils:::.completeToken()
# (checked equivalent with R 2.11.1)
# Main difference is that calls .in_function_ext instead of utils:::inFunction
# and it also makes sure completion is for Complete in 'Complete("anova(", )'!
.complete_token_ext <- function() {
  utils <- getNamespace("utils")
  completion_env <- utils$.CompletionEnv
  text <- completion_env$token
  linebuffer <- completion_env$linebuffer
  st <- completion_env$start

  if (utils$isInsideQuotes()) {
    probably_not_filename <- (st > 2L &&
      (substr(linebuffer, st - 1L, st - 1L) %in% c("[", ":", "$")))
    if (completion_env$settings[["files"]]) {
      if (probably_not_filename) {
        completion_env[["comps"]] <- character(0L)
      } else {
        completion_env[["comps"]] <- utils$fileCompletions(text)
      }
      utils$.setFileComp(FALSE)
    } else {
      completion_env[["comps"]] <- character(0L)
      utils$.setFileComp(TRUE)
    }
  } else {
    # Completion does not a good job when there are quoted strings,
    # e.g for linebuffer = "Complete("anova(", )" would give arguments for
    # anova rather than for Complete.
    # Replace quoted strings with sequences of "_" of the same length.
    # This is a temporary solution though, there should be a better way...
    mt <- gregexpr('(?<!\\\\)(["\']).*?((?<!\\\\)\\1|$)', linebuffer,
      perl = TRUE)[[1]]
    if (mt[1L] != -1) {
      ml <- attr(mt, "match.length")
      y <- sapply(lapply(ml, rep, x = "a"), paste, collapse = "")
      for (i in seq_along(mt))
        substr(linebuffer, mt[i], mt[i] + ml[i]) <- y[i]
    }
    # ... additions until here

    utils$.setFileComp(FALSE)
    utils$setIsFirstArg(FALSE)
    guessed_function <- ""
    if (completion_env$settings[["args"]]) {
      # Call of .in_function_ext() instead of utils:::inFunction()
      guessed_function <- .in_function_ext(linebuffer, st)
    } else {
      guessed_function <- ""
    }

    assign("fguess", guessed_function, completion_env)
    farg_comps <- utils$functionArgs(guessed_function, text)

    if (utils$getIsFirstArg() && length(guessed_function) &&
      guessed_function %in% c("library", "require", "data")) {
      assign("comps", farg_comps, completion_env)
      return()
    }
    last_arith_op <- tail(gregexpr("[\"'^/*+-]", text)[[1L]], 1)
    if (have_arith_op <- (last_arith_op > 0)) {
      prefix <- substr(text, 1L, last_arith_op)
      text <- substr(text, last_arith_op + 1L, 1000000L)
    }
    spl <- utils$specialOpLocs(text)
    if (length(spl)) {
      comps <- utils$specialCompletions(text, spl)
    } else {
      append_function_suffix <- !any(guessed_function %in%
        c("help", "args", "formals", "example", "do.call",
        "environment", "page", "apply", "sapply", "lapply",
        "tapply", "mapply", "methods", "fix", "edit"))
      comps <- utils$normalCompletions(text,
        check.mode = append_function_suffix)
    }
    if (have_arith_op && length(comps))
      comps <- paste(prefix, comps, sep = "")
    comps <- c(comps, farg_comps)
    assign("comps", comps,  completion_env)
	}
}

# Similar to "find" but `what` can be a vector
# also, this one only searches in packages (position of the search path
# matching '^package:') and only gives one result per what
.find_multiple <- function(what) {
  stopifnot(is.character(what))
  sp <- grep( "^package:", search(), value = TRUE)
  out <- rep( "" , length(what))
  for (i in sp) {
    ok <- what %in% ls(i, all.names = TRUE) & out == ""
    out[ok] <- i
    if (all(out != "")) break
  }
  names(out) <- what
  sub("^package:", "", out)
}
