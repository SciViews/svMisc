#' An easy package installation function that pairs with `package()`
#'
#' @description This is similar to [install.packages()], except it takes by
#' default the list of packages from `.packages_to_install` in
#' `SciViews:TempEnv`. That list is populated automatically by unfructuous calls
#' to `package()`, so that just a call to `install()` without arguments is
#' generaly sufficient.
#'
#' @param pkgs The list of packages to install (character vector). If missing,
#' the list is read from `packages_to_install`, which is cleared on success.
#' @param ... Further arguments passed to [install.packages()].
#' @param ask If `TRUE` and `pkgs` is missing, ask first to install the
#' packages.
#' @return Returns `TRUE` in case of success, `FALSE` otherwise. The function is
#' invoked for its side-effect of installing \R packages.
#' @export
#' @seealso [package()]
#' @keywords utilities
#' @concept package installation
Install <- function(pkgs = get_temp('.packages_to_install'), ..., ask = TRUE) {
  if (is.null(pkgs) || !length(pkgs)) {
    warning("Nothing to install.")
    return(invisible(TRUE)) # We consider there is no error!
  }
  pkgs_list <- '.packages_to_install'
  if (missing(pkgs) && isTRUE(ask)) {

    question <- function(title, message) {
      # In case we are under RStudio, this function is available
      dlg <- get0('.rs.api.showQuestion')
      if (is.null(dlg)) {# Use a prompt at the console instead
        cat("==", title, "==\n")
        res <- readline(paste0(message, " ([yes]/no): "))
        !res %in% c("N", "n", "No", "no", "NO")
      } else {
        dlg(title, message, ok = "Yes", cancel = "No")
      }
    }

    if (length(pkgs == 1)) {
      title <- "Install R package"
      message <- paste0("The package '", pkgs, "' will be installed.\nProceed?")
    } else {
      title <- "Install R packages"
      message <- paste0("The following packages will be installed:\n'",
        paste(pkgs, collapse = "', '"), "'\nProceed?")
    }
    if (!question(title, message)) {
      if (question(title, "Clear the list of packages to install? "))
        rm_temp(pkgs_list)
      return(invisible(FALSE))
    }
  }

  res <- try(install.packages(pkgs, ...), silent = TRUE)

  # Eliminate installed packages from the list in .packages_to_install
  # Note: we do so also if the installation failed, in order to stop keeping to
  # try to install those "uninstallable" packages
  to_install <- get_temp(pkgs_list)
  to_install <- to_install[!to_install %in% pkgs]
  assign_temp(pkgs_list, to_install, replace.existing = TRUE)

  invisible(!inherits(res, "try-error"))
}
