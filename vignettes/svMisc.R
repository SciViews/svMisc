## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(svMisc)

## -----------------------------------------------------------------------------
?kalman

## -----------------------------------------------------------------------------
library(svMisc)
about("kalman")

## -----------------------------------------------------------------------------
fake_process <- function(file) {
  message("Processing ", file, "...")
  flush.console()
  Sys.sleep(0.5)
  if (runif(1) > 0.7) {# Fail
    warning("fake_process was unable to process ", file)
    invisible(FALSE)
  } else invisible(TRUE)
}

## -----------------------------------------------------------------------------
# Run it in batch mode on ten items
batch(paste0("file", 1:10), fake_process)

## -----------------------------------------------------------------------------
.last.batch

## -----------------------------------------------------------------------------
for (i in 0:75) {
  progress(i, progress.bar = TRUE)
  # Some process here...
}

## -----------------------------------------------------------------------------
foo <- structure(function(x, type = c("histogram", "boxplot"), ...) {
  type <- match.arg(type, c("histogram", "boxplot"))
  switch(type,
    histogram = hist(x, ...),
    boxplot = boxplot(x, ...),
    stop("unknow type")
  )
}, class = c("function", "subsettable_type"))
foo

# This function can be used as usual:
foo(rnorm(50), type = "histogram")
# ... but also this way:
foo$histogram(rnorm(50))
foo$boxplot(rnorm(50))

## -----------------------------------------------------------------------------
captured <- capture_all(parse_text('1:2 + 1:3'), split = FALSE)
captured

## -----------------------------------------------------------------------------
writeLines(captured)

## -----------------------------------------------------------------------------
parse_text(c("1 + 1", "log(10)"))

## -----------------------------------------------------------------------------
parse_text("log(")

## -----------------------------------------------------------------------------
# A complex R object
# Note: we round doubles to 14 digits because precision is lost in the process
obj <- structure(list(
  a = as.double(c(1:5, 6)),
  LETTERS,
  c = c(c1 = 4.5, c2 = 7.8, c3 = Inf, c4 = -Inf, NA, c6 = NaN),
  c(TRUE, FALSE, NA),
  e = factor(c("a", "b", "a")),
  f = 'this is a "string" with quote',
  g = matrix(round(rnorm(4), 14), ncol = 2),
  `h&$@` = list(x = 1:3, y = round(rnorm(3), 14),
    fact = factor(c("b", "a", "b"))),
  i = Sys.Date(),
  j = list(1:5, y = "another item")),
  comment = "My comment",
  anAttrib = 1:10,
  anotherAttrib = list(TRUE, y = 1:4))

# Convert to RJSON
(rjson1 <- to_rjson(obj, attributes = TRUE))
# Get back an R object from Rjson
(obj2 <- eval_rjson(rjson1))
# Is it identical to obj?
identical(obj, obj2)

## -----------------------------------------------------------------------------
system_dir("temp")

## -----------------------------------------------------------------------------
system_dir("sysTemp")

## -----------------------------------------------------------------------------
system_dir("user")

## -----------------------------------------------------------------------------
system_dir("home")

## -----------------------------------------------------------------------------
system_dir("zip", exec = TRUE)

## -----------------------------------------------------------------------------
system_file("zip", exec = TRUE)

## -----------------------------------------------------------------------------
system_dir(package = "stats")

## -----------------------------------------------------------------------------
system_file("help", "AnIndex", package = "splines")

## -----------------------------------------------------------------------------
compare_r_version("5.6.0") # Probably older

## -----------------------------------------------------------------------------
compare_r_version("0.6.0") # Probably newer

## -----------------------------------------------------------------------------
is_win() # Windows?
is_mac() # MacOS?
is_rgui() # Is it RGui under Windows?
is_sdi() # Is RGui run in SDI mode (separate windows)?
is_rstudio() # Is it RStudio?
is_rstudio_desktop() # RStudio desktop?
is_rstudio_server() # RStudio server?
is_jgr() # Is R running under JGR?

## -----------------------------------------------------------------------------
def(0:2, mode = "logical", length.out = 5) # logical, size 5

## -----------------------------------------------------------------------------
args_tip("ls")

## -----------------------------------------------------------------------------
temp_var("my_var")

## -----------------------------------------------------------------------------
search()
# Assign a variable in a temporary environment
assign_temp("my_var", 1:5)
# The environment is named SciViews:TempEnv
search()
# Get the variable
get_temp("my_var")
# List variables in the temporary environment
ls(envir = TempEnv())
# Delete the variable
rm_temp("my_var")

