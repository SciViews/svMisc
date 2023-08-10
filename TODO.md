# svMisc To Do list

-   The various cases for section and aka printing, and str-ing + example.

-   Same as `pCloud()`, but for OneDrive, Google Drive & Dropbox.

-   For `install()`, allow to specify the list of packages in `...`, but how to specify installation arguments then? Also, allows to specify from where to install the packages (github, etc. should be also accepted).

-   A function derived from `import::from()`, but that populates `.packages_to_install`; idem for `::`, `:::`, `library()` and `require()`.

-   A liste of (remote) sources for packages to install from github, bitbucket, etc. + a list of dependencies and code to run to install these.

-   Finalize the actions management

-   Specification of dates in RJSON format

-   Write the code in `objList()` to list content inside objects, and perhaps also objects attributes.

-   Rework mechanism to call `.guiCmd()` from `objXXX()` functions.

-   Make indeterminate `progress()`ion.

-   Use this in call tips to get the call for S3 method for given objects `args(getS3method("predict", class(myobj), optional = TRUE))`. + `selectMethod()` for S4 object does the same thing!

-   `objMenu()`: add copy name to clipboard, send name to editor in menu.

-   Use `RShowDoc()` to access further R documentation.

-   Also include `RSiteSearch()`/`helpSearchWeb()`, sos, Rseek, RDocumentation, perhaps in a different function dedicated to search on the web!

-   Update po file.

-   Write a complete battery of tests for this package.
