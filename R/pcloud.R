#' Create the path to a file in the p-Cloud drive
#'
#' @description Similar to [file.path()] but creates a path to a file located
#' somewhere in a p-Cloud drive. [p-Cloud](https://www.pcloud.com/eu.html) is a
#' cloud storage system that comes with an application for Windows, MacOS or
#' Linux. It creates a virtual drive on the PC where files can be managed as if
#' they were local. However, the path to these files differ between OSes. This
#' function abstracts out the first part of the path for you. So, you just have
#' to provide the folders and files and it constructs a valid absolute path, no
#' matter which OS you are using. The [pcloud_crypto()] function does the same
#' for the special `Crypo Folder` that p-Cloud creates if you subscribe to the
#' encryption option.
#'
#' @param ... The folder, subfolder and file to form the path, starting to the
#' root of the p-Cloud drive, or the `Crypto Folder`.
#' @return A character string with the absolute path to the file or folder.
#' @export
#' @seealso [system_file()], [source_clipboard()], [file.path()]
#' @keywords utilities
#' @concept file path
#' @examples
#' \dontrun{
#' pcloud("subfolder", "file.txt")
#' # Only valid with the encryption option and the Crypto Folder is unlocked
#' pcloud_crypto("subfolder1", "subfolder2", "crypted_file.txt")
#' }
pcloud <- function(...) {
  if (is_win()) {
    root <- "P:" # TODO: manage the case it is Q: or R: instead
  } else if (is_mac()) {
    root <- "~/pCloud\ Drive"
  } else {# On Linux
    root <- "~/pCloudDrive"
  }
  file.path(root, ...)
}

#' @export
#' @rdname pcloud
pcloud_crypto <- function(...)
  pcloud("Crypto Folder", ...)
  # TODO: check for the existance of this folder and issue a warning otherwise
