.onLoad <- function(lib, pkg) {
	.initialize()

  # Determine where to find the preferred file editor for fileEdit()
  if (is.null(getOption("fileEditor"))) {
    if (interactive()) {
      if (is_win()) {
        # First check if Notepad++ is installed in default location...
        pfdir <- Sys.getenv("ProgramFiles")
        if (pfdir == "") pfdir <- "c:\\program files"
        file_editor <- paste0(pfdir, "\\Notepad++\\notepad++.exe")
        # ... otherwise, fallback to notepad.exe
        if (!file.exists(file_editor))
          file_editor <- "notepad"  # Default for Rterm
      } else if (is_mac()) {
        # Note that, in R.app, one cannot edit and wait for it is done
        # So, I always define a different editor, but fall back to
        # internal R.app if internal.if.possible is TRUE
        # First check if 'edit' is there
        # (open files in BBEdit)
        if (length(suppressWarnings(system("which edit", intern = TRUE)))) {
          file_editor <- "bbedit"
        } else {# Fall back to the default text editor
          # Note: use "open -e -n -W \"%s\"" to force use of TextEdit
          file_editor <- "textedit"
        }
      } else {# This is probably linux or an unix
        # First check if gedit or kate is there... This is different
        # from file.edit() and editor that looks directly to EDITOR!
        if (length(suppressWarnings(system("which gedit", intern = TRUE)))) {
          file_editor <- "gedit"
        } else if (length(suppressWarnings(system("which kate",
          intern = TRUE)))) {
          file_editor <- "kate"
        } else {# Fall back to something that is likely to be installed
          # Look id EDITOR or VISUAL environment variable is defined
          file_editor <- Sys.getenv("EDITOR")
          if (nzchar(file_editor)) file_editor <- Sys.getenv("VISUAL")
          if (nzchar(file_editor)) file_editor <- "vi"
        }
      }
      options(fileEditor = file_editor)
    } else options(fileEditor = "") # Inactivate it!
  }
}

.initialize <- function(replace = TRUE) {
  # If the option svGUI.methods is not defined, give reasonable default values
  # Those are methods that can be applied to many objects without providing
  # additional argument and that will be added automatically to objects'
  # context menu in GUIs (print and show are not included, because we know
  # they must exist for all objects)
  # Rem: use addMethods() if you just want to add methods to this list
  if (is.null(getOption("svGUI.methods")))
    options(svGUI.methods = c("AIC", "anova", "confint", "BIC", "formula",
      "head", "hist", "logLik", "plot", "predict", "residuals", "summary",
      "tail", "vcov"))
}

# gettext() and hence gettextf() cannot retrieve messages ending with space
# in the "R" domain, because these functions stripe them out!
# This is a hack using ngettext() that uses unmodified version of the message
# Restriction: on the contrary to gettext(), .gettext() can translate only
# one message at a time, and default domain is changed to "R"
.gettext <- function(msg, domain = "R")
  ngettext(1, msg, "", domain = domain)

.gettextf <- function(fmt, ..., domain = "R")
  sprintf(ngettext(1, fmt, "", domain = domain), ...)
