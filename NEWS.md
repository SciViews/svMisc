# svMisc 1.2.3

- Several examples set as \dontrun{} because time to run all examples is too long on CRAN.

# svMisc 1.2.2

- Link to a JSS paper replaced by the DOI link.

# svMisc 1.2.1

- No more assignation (`last.warning`) into base environment: not permitted any more in R >= 4.1.0.

# svMisc 1.2.0

- Two new functions `pcloud()` and `pcloud_crypto()` ease the management of files that are located in a p-Cloud drive (p-Cloud is a cloud storage system).

# svMisc 1.1.4

- Minor changes in the documentation to avoid notes in latest devel R CMD check.

# svMisc 1.1.3

- `.Traceback` is not set any more in the base environment by `capture_all()` because CRAN policy does not allow it. The variable `.Traceback_capture_all` is set in `temp_env()` instead. It can be retrieved with `get_temp(".Traceback_capture_all")`.

# svMisc 1.1.2

- A new argument is added to `completion()`: `name.or.addition=` which determines if the function returns the completion name, completion additions or both. Thanks to @rhuffy.

# svMisc 1.1.1

- `News.md` reworked in `R CMD build`-compatible format, and `TODO` reworked as `TODO.md`.

- 'pkgdown' site added.

# svMisc 1.1.0

- `about()` added and a special version of `?` accepts `.?` as a shortcut to `about()`, but is otherwise, compatible with `utils::?`.

- `subsettable` functions for arguments `type =` or `which =` added.

# svMisc 1.0.2

- `captureAll()` and `parseText()` were not exported anymore after switching help pages to Roxygen2. Fixed.

# svMisc 1.0-1

- This is a minor update to avoid 2 notes on CRAN `R CMD check`.

# svMisc 1.0-0

- New, snake_case names of all the functions. Old camelCase functions names are kept for compatibility, e.g., `captureAll()` -> `capture_all()`.

- All documentation converted to Roxygen2 format.

- Tests converted to `testthat` format.

- `package()` is reworked to record packages it cannot install, and the `Install()` function uses these automatically.

# svMisc 0.9-73

_ New `is_rstudio()`, and `is_rstudio_server()` functions. Also, `file_edit()` detects if RStudio is used and get an internal buffer to edit a file by default in that case.

- Vignette added.

# svMisc 0.9-72

- `toRjson()` adapted to be compatible with R >= 3.5.0 (thanks to Martin Maechler).

# svMisc 0.9-71

- Switch to Github for development; CI added.

- Code added to make 'svUnit' tests compatibles with RStudio and `devtools::test()`.

# svMisc 0.9-70

- Deprecated functions `Args()`, `CallTip()`, `clipsource()`, `Complete()`, `CompletePlus()`, `getEnvironment()`, `Parse()`, `r()`, `Sys.tempdir()`, `Sys.userdir()` are now defunct and eliminated from the package.

- Function `pkg()` is renamed `package()`.

- Reworked code that was using `:::`.

# svMisc 0.9-69

- On MacOS >= 10.7, the package could not load because of a warning (converted into an error) in `.onLoad()`. Solved. (thanks Travis Porco for bug report).

- `isHelp()` now uses `find.package()` instead of the deprecated `.find.package()`.

# svMisc 0.9-68

- The temporary environment that `TempEnv()` attaches to the search path is now called `'SciViews:TempEnv'` instead of simply `'TempEnv'` to avoid potential conflicts with other packages in CRAN.

# svMisc 0.9-67

- Added the `fileEdit()` function.

- Reworked `argsTip()` function to eliminate old code (for R < 2.10).

- `parseText()` reported an error instead of `NA` in the case last string is incomplete in R 2.15.

# svMisc 0.9-66

- Added function `isJGR()`.

- Added batch processing function `batch()`.

# svMisc 0.9-65

- Partial argument matching in `completion()`, `progress()` and `rmTemp()` code fixed.

- Call to `.Internal(...)` in `captureAll()` eliminated, but need to call `.signalSimpleWarning()` that is not intended for external use anyway.

- Call to `.Internal(dput(....))` in `toRjson()` eliminated.

# svMisc 0.9-64

- Added `listToJson()` for conversion of lists of character strings into correct JSON format.

- Added `pkgManXXX()` functions to run R code for the SciViews package manager.

# svMisc 0.9-63

- `progress()` used `\8`, that was treated as octal for R < 2.14. Changed to `\b` in order to correct this bug. Thanks Duncan Murdoch for pointing me on this.

# svMisc 0.9-62

- `captureAll()` now handles user interrupts and allows for `traceback()` afterwards and default value for `split=` now changed to `TRUE`. The `echo =` argument allows for echoing expressions being evaluated, like in the usual R console, but a mechanism allows to abbreviate very long expressions.

- `parseText()` is reworked internally and it uses the `srcfile`/`srcref` mechanism introduced in R recently. `firstline =`, `srcfilename =` and `encoding =` arguments are added.

- Unit tests added (should run with both 'svUnit' (advised) and 'RUnit').

- A bug in `objList()` is corrected: if the list of objects in the environment is cleared, e.g., by `rm(list = ls())`, the function returned as if nothing was changed with `compare = TRUE`. Now, an attribute `changed =` is set to `TRUE` or `FALSE` to differentiate between "no changes" and "changes towards an empty environment".

# svMisc 0.9-61

- Better handling of non syntactically correct names in `objList()`.

# svMisc 0.9-60

- A couple of functions are renamed: `Parse()` -> `parseText()`, `clipsource()` -> `sourceClipboard()`. Old names are declared deprecated, and will become defunct before version 1.0-0 of the package.

- `captureAll()` now returns `NA` in case of incomplete line of code parsed by `parseText()`. It also detects if `expr =` is a valid language expression or is `NA`.

- `isMac()` was not working correctly on Mac OS X Leopard and Snow Leopard (bug corrected).

- `Sys.userdir()` did not expanded tilde in recent R versions (corrected).

- Little change in `def()` arguments: `length.out =` instead of `length =` to use the same name as corresponding argument in `rep()`. Coercion to logical is now done using `as.logical()`... the result may differ from previous implementations.

- For `listTypes()`, the convention has changed. Method/type is now separated by an underscore instead as with two dots (like in `view_text.default`).

- `Sys.tempdir()` and `Sys.userdir()` are deprecated in favor of the new more general functions `systemFile()` and `systemDir()`.

- `r()` is deprecated in favor of `pkg()` (`r()` is not informative enough and more susceptible to be used elsewhere too).

- `Args()` is deprecated in favor of `argsTip()` and `CallTip()` is deprecated in favor of `callTip()` (further homogenization of svMisc function names). The new `argsTip()` and `callTip()` functions can reflow the tip to a given width, and `callTip()` can also return a short description of the function as well as the list of available methods if the tip os asked for a generic function.

- `Complete()` and `CompletePlus()` are deprecated in favor of a unique `completion()` function. Code of both original functions has been fused and reworked.

# svMisc 0.9-59

- RJSON objects now use a customized `list()` function to build lists, but also structures and new S4 objects.

- `captureAll()` has now a `split =` argument that allows to output to the R
  console, while capturing output.

- Bug correction in `captureAll()`: call[[1L]] is not subsettable.

# svMisc 0.9-58

- Additions of functions `toRjson()` and `evalRjson()` and specification of the RJSON (R-JavaScript Object Notation), an object exchange format not unlike JSON, but richer and more adapted to represent most R objects.

# svMisc 0.9-57

- Small changes to `objList()` (now look at objects in their correct environment).

- A bug in `descArgs()` with R >= 2.10 did not allowed to gest arguments description for functions using the `...` argument. Thanks to Diego Zardetto for pointing this bug.

# svMisc 0.9-56

- Temporary code to disable `index.search()` in devel R 2.11 in `isHelp()` and `descFun()`.

# svMisc 0.9-55

- Implement `descArgs()` using the new help system (`parse_Rd()`), this eliminates the need for the workaround of version 0.9-54

# svMisc 0.9-54

- Cosmetic changes in Rd files to make them compatible with R 2.11 (devel). A part of the example of `CompletePlus()` is eliminated because it raises an error.

# svMisc 0.9-53

- `Complete()` now sorts items alphabetically and does not return completions as factor type in the data frame any more.

- `CallTip()` does a better work to find current function, i.e., not only after the opening parentheses `(`.

- `Args()` now do no place a space anymore between the name of a function and its arguments

# svMisc 0.9-52

- `Complete()` now manages cases where code is like `iris[`, `iris[[`, or where last parsed token is empty (in this case, it returns the list of objects loaded in `.GlobalEnv`).

# svMisc 0.9-51

- `Complete()` now includes additions from `Complete2()` and `CompletePlus()`.

# svMisc 0.9-50

- `CompletePlus()` removes the weird object names ( `.__M__`, ...) which were causing trouble.

- `CompletePlus()` handles completions like `a[m`.

# svMisc 0.9-49

- `Complete2()` is a temporary new version of `Complete()` for experiments.

# svMisc 0.9-48

- `Parse()` does not detect incomplete R code any more, fixed [PhG].

# svMisc 0.9-47

- `objList()`, `print.objectList()` and `write.objList()` reworked [PhG].

# svMisc 0.9-46

- Added `objList()`, `print()` method for 'objList' objects and `write.objList()` [KB].

- Localization complete and French local file done.

# svMisc 0.9-45

- Made backward compatible with R 2.6.0 (was R >= 2.7.0 in previous version).

- `captureAll()` is reworked by Kamil Barton [KB]. Debugging code eliminated (data saved in `.GlobalEnv` for debugging).

- Error handler added to `captureAll()` - [KB].

# svMisc 0.9-44

- `CompletePlus()` reworked to use man pages instead of .Rd files.

- `descFun()` and `descArgs()` added.

# svMisc 0.9-43

- `CompletePlus()` created to obtain information on the completion possibilities.

# svMisc 0.9-42

- `objList()` did not place each item in a line when result is written in a file.

- `objXXX()` functions did not always returned results invisibly. Solved.

- `Args()` is more robust against bad `name =` parameter because it now calls `argsAnywhere()` within a `try()`.

# svMisc 0.9-41

- `objInfo()` returns also estimated size of objects that are not functions.

- `objSearch()` is reworked to return a single string using `sep =` as separator when `sep =` is not `NULL`.

# svMisc 0.9-40

- This is the first version distributed on R-forge. It is completely refactored from older versions (on CRAN since 2003) to make it run with SciViews-K and Komodo Edit (Tinn-R is also supported, but not SciViews-R Console any more).
