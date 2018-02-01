#' Functions to manage R side of the SciViews R package manager
#'
#' @description These functions should not be used directly by the end-user.
#' They implement the R-side code for the SciViews \R package manager.
#'
#' @param pkgname The name of one R package (character string).
#' @param print.it Should the result be printed?
#' @param page Which page to get?
#' @param pattern Selection pattern.
#' @param n The number of items to retrieve.
#' @param keep The columns to keep in the resulting data frame.
#' @param reload Do we force reload of the data and ignore cache version?
#' @param sep Field separator to use.
#' @param eol End-of-line sequence to use.
#' @param pkgs A list of packages to install.
#' @param install.deps Do we also install dependencies?
#' @param ask Do we prompt the user for package installation?
#' @param url The URL to use for the current CRAN mirror.
#' @return These functions return data that is intended to be used by the
#' SciViews \R package manager.
#' @author Kamil Barton <kamil.barton@uni-wuerzburg.de>
#' @export
#' @seealso [package()]
#' @keywords utilities
#' @concept SciViews R package manager
pkgman_describe <- function(pkgname, print.it = TRUE) {
  owarn <- getOption("warn")
  on.exit(options(warn = owarn))
  options(warn = -1)
  desc <- packageDescription(pkgname)
  options(warn = owarn)
  if (is.na(desc)) {
    # Package is apparently not installed... Try getting data from CRAN
    con <- url(file.path(getOption("repos")['CRAN'], "web", "packages",
      pkgname, 'DESCRIPTION', fsep = '/'))
    m <- try(open(con, "r"), silent = TRUE)
    if (!inherits(m, "try-error")) {
      on.exit(close(con), add = TRUE)
      dcf <- try(read.dcf(con))
      # Build a 'packageDescription' object
      desc <- as.list(dcf[1, ])
      class(desc) <- "packageDescription"
    } else {
      return(invisible(NULL))
    }
  }
  if (isTRUE(print.it)) {
    write.dcf(as.data.frame.list(desc[!sapply(desc, is.na)],
      optional = TRUE), width = Inf)
    invisible(desc)
  } else desc
}

#' @export
#' @rdname pkgman_describe
pkgman_get_mirrors <- function() {
  # Cache the list of CRAN mirrors in SciViews:TempEnv
  temp_var <- "pkgMan.CRANmirrors"
  if (exists_temp(temp_var)) {
    mirrors <- get_temp(temp_var)
  } else {
    mirrors <- getCRANmirrors()
    assign_temp(temp_var, mirrors)
  }
  write.table(mirrors[, c("Name", "URL", "CountryCode")],
    row.names = FALSE, col.names = FALSE, sep = ';', quote = FALSE, na = "")
}

#' @export
#' @rdname pkgman_describe
pkgman_get_available <- function(page = "next", pattern = "", n = 50,
keep = c("Package", "Version", "InstalledVersion", "Status"), reload = FALSE,
sep = ";", eol = "\t\n") {

  available_pkgs <- function(avpkg = available.packages(), installed = TRUE) {
    avpkg <- avpkg[order(toupper(avpkg[, "Package"])), , drop = FALSE]
    if (isTRUE(installed)) {
      inspkg <- installed.packages()
      ipkgnames <- unique(inspkg[, 'Package'])

      ipkgnames <- ipkgnames[ipkgnames %in% avpkg[, 'Package']]
      avpkg <- cbind(avpkg, InstalledVersion = NA, Status = NA)
      if (length(ipkgnames)) {
        pkgstatus <- sapply(ipkgnames, function(pkg) {
          compareVersion(avpkg[pkg, 'Version'], inspkg[pkg, 'Version'])
        })
        avpkg[ipkgnames, 'Status'] <- pkgstatus
        avpkg[ipkgnames, 'InstalledVersion'] <- inspkg[ipkgnames, 'Version']
      }
    }
    avpkg
  }

  if (!exists_temp('avpkg.list') || isTRUE(reload)) {
    avpkg.list <- available_pkgs(available.packages(filters = c("R_version",
      "OS_type", "duplicates")), installed = FALSE)
    assign_temp('avpkg.list', avpkg.list)
  } else {
    avpkg.list <- get_temp('avpkg.list')
  }
  if (page == "first") {
    new_search <- TRUE
    i0 <- 1
  } else {
    new_search <- get_temp('avpkg.pattern', "") != pattern
    i0 <- get_temp('avpkg.idx', default = 1)
  }

  if (is.character(pattern) && pattern != "") {
    if (new_search) {
      page <- "current"
      i0 <- 1
      idx <- grep(pattern, avpkg.list[,'Package'], ignore.case = TRUE)
      assign_temp('avpkg.pattern.idx', idx)
    } else {
      idx <- get_temp('avpkg.pattern.idx')
    }
    imax <- length(idx)
  } else {
    imax <- nrow(avpkg.list)
    idx <- seq(imax)
  }
  assign_temp('avpkg.pattern', pattern)

  if (page == "next") {
    i0 <- i0 + n
  } else if (page == "prev") {
    i0 <- i0 - n
  }
  outside <- i0 > imax || i0 < 1
  if (outside)
    return(NULL)
  assign_temp('avpkg.idx', i0)
  i1 <- min(i0 + n - 1, imax)
  i <- seq(i0, i1)
  cat(i0, i1, imax, "\t\n")
  write.table(available_pkgs(avpkg.list[idx[i], , drop = FALSE])[ ,
    keep, drop = FALSE], row.names = FALSE, col.names = FALSE, sep = sep,
    quote = FALSE, eol = eol, na = "")
}

#' @export
#' @rdname pkgman_describe
pkgman_get_installed <- function(sep = ";", eol = "\t\n") {
  inspkg <- installed.packages(fields = "Description")
  inspkg <- inspkg[order(toupper(inspkg[ , "Package"])),
    c("Package", "Version", "Description")]

  inspkg[, 3] <- gsub("\n", " ", inspkg[, 3])
  inspkg <- cbind(inspkg, Installed = inspkg[, 'Package'] %in% .packages())
  write.table(inspkg, row.names = FALSE, col.names = FALSE, sep = sep,
    quote = FALSE, eol = eol, na = "")
}

#' @export
#' @rdname pkgman_describe
pkgman_set_cran_mirror <- function(url) {
  repos <- getOption("repos")
  repos['CRAN'] <- url
  options(repos = repos)
}

#' @export
#' @rdname pkgman_describe
pkgman_install <- function(pkgs, install.deps = FALSE, ask = TRUE) {
  dep <- suppressMessages(getNamespace("utils")$getDependencies(pkgs,
    available = get_temp('avpkg.list')))
  msg <- status <- ""
  if (!isTRUE(ask) && (isTRUE(install.deps) || all(dep %in% pkgs))) {
    msg <- capture_all(install.packages(dep))
    status <- "done"
  } else {
    l <- length(dep)
    msg <- sprintf(ngettext(l,
      "This will install package %2$s.",
      "This will install packages: %s and %s.",
    ), paste(sQuote(dep[-l]), collapse = ", "), sQuote(dep[l]))
    status <- "question"
  }
  list(packages = dep, message = msg, status = status)
}

#' @export
#' @rdname pkgman_describe
pkgman_remove <- function(pkgname) {
  sapply(pkgname, function(pkgname) {
    pkg_search_name <- paste("package", pkgname, sep = ":")
    if (pkg_search_name %in% search()) detach(pkg_search_name,
      character.only = TRUE, unload = TRUE)
    if (pkgname %in% loadedNamespaces())
      unloadNamespace(pkgname)

    dlli <- getLoadedDLLs()[[pkgname]]
    if (!is.null(dlli))
      dyn.unload(dlli[['path']])

    pkg_path <- find.package(pkgname, quiet = TRUE)
    if (length(pkg_path) == 0L)
      return(FALSE)

    pkg_lib <- normalizePath(file.path(pkg_path, ".."))
    if (file.access(pkg_lib, 2) == 0) {
      remove.packages(pkgname, lib = pkg_lib)
      TRUE
    } else {
      #warning("No sufficient access rights to library", sQuote(pkglib))
      FALSE
    }
  }, simplify = FALSE)
}

#' @export
#' @rdname pkgman_describe
pkgman_load  <- function(pkgname) {
  sapply(pkgname, library, character.only = TRUE, logical.return = TRUE,
    simplify = FALSE)
}

#' @export
#' @rdname pkgman_describe
pkgman_detach <- function(pkgname) {
  sapply(pkgname, function(pkgname) {
    tryCatch({
      pkg_search_name <- paste("package", pkgname, sep = ":")
      if (pkg_search_name %in% search())
        detach(pkg_search_name, character.only = TRUE, unload = TRUE)
      if (pkgname %in% loadedNamespaces()) unloadNamespace(pkgname)
      TRUE
    }, error = function(e) conditionMessage(e))
  }, simplify = FALSE)
}


# Backward compatibility

#' @export
#' @rdname pkgman_describe
pkgManDescribe <- pkgman_describe

#' @export
#' @rdname pkgman_describe
pkgManGetMirrors <- pkgman_get_mirrors

#' @export
#' @rdname pkgman_describe
pkgManGetAvailable <- pkgman_get_available

#' @export
#' @rdname pkgman_describe
pkgManGetInstalled <- pkgman_get_installed

#' @export
#' @rdname pkgman_describe
pkgManSetCRANMirror <- pkgman_set_cran_mirror

#' @export
#' @rdname pkgman_describe
pkgManInstall <- pkgman_install

#' @export
#' @rdname pkgman_describe
pkgManRemove <- pkgman_remove

#' @export
#' @rdname pkgman_describe
pkgManLoad <- pkgman_load

#' @export
#' @rdname pkgman_describe
pkgManDetach <- pkgman_detach
