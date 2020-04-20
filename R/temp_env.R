#' Get an environment dedicated to temporary variables (and create it if needed)
#'
#' @description Create and manage a temporary environment `SciViews:TempEnv`
#' low enough on the search path so that all loaded packages (except **base**)
#' could easily access objects there.
#'
#' @param x The vector to add items to for `add_items()` or any object. for
#'   `delete_temp()`, it is the name of the variable (character string), or a
#'   vector of characters with the name of all variables to remove from
#'   `SciViews:TempEnv`.
#' @param y The vector of which we want to inject missing items in 'x'.
#' @param use.names Use names of items to determine which one is unique,
#'   otherwise, the selection is done on the items themselves.
#' @param replace Do we replace existing items in 'x'?
#' @param item The item to add data to in the list.
#' @param value The value to add in the item, it must be a named vector and
#'   element matching is done according to name of items.
#' @param replace.existing Do we replace an existing variable?
#' @param mode The mode of the seek variable
#' @param  default The default value to return, in case the variable or the item
#'   does not exist.
#' @return The temporary environment for `temp-env()`, the value assigned, added
#'   or changed for `assign_temp()`, `add_temp()`, `change_temp()`, or
#'   `get_temp()`. `TRUE` or `FALSE` for `exists_temp()`, `delete_temp()` or
#'   `rm_temp()`.
#' @details The temporary environment is attached to the search path for easier
#'   access to its objects.
#' @export
#' @seealso [assign()], [search()], [temp_var()]
#' @keywords utilities
#' @concept temporary variables
#' @examples
#' ls(temp_env())
#'
#' # I have a vector v1 with this:
#' v1 <- c(a = "some v1 text", b = "another v1 text")
#' # I want to add items whose name is missing in v1 from v2
#' v2 <- c(a = "v2 text", c = "the missign item")
#' add_items(v1, v2, replace = FALSE)
#' # Not the same as
#' add_items(v1, v2, replace = TRUE)
#' # This yield different result (names not used and lost!)
#' add_items(v1, v2, use.names = FALSE)
#'
#' add_temp("tst", "item1", c(a = 1, b = 2))
#' # Retrieve this variable
#' get_temp("tst")
#' # Add to item1 in this list without replacement
#' add_temp("tst", "item1", c(a = 45, c = 3), replace = FALSE)
#' get_temp("tst")
#' # Same but with replacement of existing items
#' add_temp("tst", "item1", c(a = 45, c = 3), replace = TRUE)
#' get_temp("tst")
#' # Delete the whole variable
#' delete_temp("tst")
#'
#' assign_temp("test", 1:10)
#' # Retrieve this variable
#' get_temp("test")
#'
#' change_temp("tst", "item1", 1:10)
#' # Retrieve this variable
#' get_temp("tst")
#' # Create another item in the list
#' change_temp("tst", "item2", TRUE)
#' get_temp("tst")
#' # Change it
#' change_temp("tst", "item2", FALSE)
#' get_temp("tst")
#' # Delete it (= assign NULL to the item)
#' change_temp("tst", "item2", NULL)
#' get_temp("tst")
#' # Delete the whole variable
#' delete_temp("tst")
#'
#' assign_temp("test", 1:10)
#' # Check if this variable exists
#' exists_temp("test")
#' # Remove it
#' delete_temp("test")
#' # Does it still exists?
#' exists_temp("test")
temp_env <- function() {
  pos <-  match("SciViews:TempEnv", search())
  if (is.na(pos)) { # Must create it
    `SciViews:TempEnv` <- list()
    attach_env <- function(...) get("attach", mode = "function")(...)
    attach_env(`SciViews:TempEnv`, pos = length(search()) - 1L)
    rm(`SciViews:TempEnv`)
    pos <- match("SciViews:TempEnv", search())
  }
  pos.to.env(pos)
}

#' @export
#' @rdname temp_env
add_items <- function(x, y, use.names = TRUE, replace = TRUE) {
  if (isTRUE(replace)) res <- c(y, x) else res <- c(x, y)
  if (use.names) {
    res[!duplicated(names(res))]
  } else {
    sort(unique(res))
  }
}

#' @export
#' @rdname temp_env
add_temp <- function(x, item, value, use.names = TRUE, replace = TRUE) {
  x <- as.character(x)[1]
  item <- as.character(item)[1]
  if (exists_temp(x)) dat <- get_temp(x) else dat <- list()

  if (!inherits(dat, "list"))
    stop(x, " must be a list!")

  if (item %in% names(dat))
    value <- add_items(dat[[item]], value,
      use.names = use.names, replace = replace)
  dat[[item]] <- value
  assign_temp(x, dat)
}

#' @export
#' @rdname temp_env
assign_temp <- function(x, value, replace.existing = TRUE) {
  t_env <- temp_env()
  if (replace.existing || !exists(x, envir = t_env, mode = "any",
    inherits = FALSE))
    assign(x, value, envir = t_env)
}

#' @export
#' @rdname temp_env
change_temp <- function(x, item, value, replace.existing = TRUE) {
  x <- as.character(x)[1]
  item <- as.character(item)[1]
  if (exists_temp(x)) dat <- get_temp(x) else dat <- list()

  if (!inherits(dat, "list"))
    stop(x, " must be a list!")

  if (replace.existing || !item %in% names(dat)) {
    dat[[item]] <- value
    assign_temp(x, dat)
  }
}

#' @export
#' @rdname temp_env
exists_temp <- function(x, mode = "any")
  exists(x, envir = temp_env(), mode = mode, inherits = FALSE)

#' @export
#' @rdname temp_env
get_temp <- function(x, default = NULL, mode = "any", item = NULL) {
  if (is.null(item)) Mode <- mode else Mode <- "any"

  t_env <- temp_env()
  if  (exists(x, envir = t_env, mode = Mode, inherits = FALSE)) {
    dat <- get(x, envir = t_env, mode = Mode, inherits = FALSE)
    if (is.null(item)) {
      return(dat)
    } else {
      item <- as.character(item)[1]
      if (inherits(dat, "list") && item %in% names(dat)) {
        dat <- dat[[item]]
        if (mode != "any" && mode(dat) != mode) dat <- default
        return(dat)
      } else {
        return(default)
      }
    }
  } else {# Variable not found, return the default value
    default
  }
}

#' @export
#' @rdname temp_env
delete_temp <- function(x) {
  if (!is.character(x))
    stop("'x' must be character string(s)!")
  l <- length(x)
  res <- rep(TRUE, l)
  if (l > 1) names(res) <- x

  t_env <- temp_env()
  for (i in 1:l) {
    exists_in_temp <- exists(x[i], envir = t_env, inherits = FALSE)
    res0 <- try(if (exists_in_temp) rm(list = x[i],
      envir = t_env), silent = TRUE)
    if (!exists_in_temp || inherits(res0, "try-error")) res[i] <- FALSE
  }
  invisible(res)
}

#' @export
#' @rdname temp_env
rm_temp <- delete_temp


# Backward compatibility

#' @export
#' @rdname temp_env
TempEnv <- temp_env

#' @export
#' @rdname temp_env
addItems <- add_items

#' @export
#' @rdname temp_env
addTemp <- add_temp

#' @export
#' @rdname temp_env
assignTemp <- assign_temp

#' @export
#' @rdname temp_env
changeTemp <- change_temp

#' @export
#' @rdname temp_env
existsTemp <- exists_temp

#' @export
#' @rdname temp_env
getTemp <- get_temp

#' @export
#' @rdname temp_env
rmTemp <- delete_temp
