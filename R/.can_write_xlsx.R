#' Check Whether an Excel File Can Be Written (Dry-Run)
#'
#' `can_write_xlsx()` performs a dry-run check to determine whether an Excel
#' file at `path` can be created or overwritten. It does not create any
#' directories, and it does not leave behind any test files if the check
#' succeeds.
#'
#' If the file is open or locked (for example, open in Excel on Windows), the
#' function stops with an informative error message. This is intended to be
#' called *before* performing expensive work, so that the pipeline can fail
#' fast when the output cannot be written.
#'
#' @param path Character scalar. Target `.xlsx` file path whose writeability
#'   should be tested.
#'
#' @return Invisibly returns `TRUE` if the file can be written; otherwise
#'   throws an error and stops.
#'
#' @details
#' Behaviour depends on whether `path` already exists:
#'
#' - If `path` **does not exist**:
#'   1. A small test workbook is written to a temporary file in the same
#'      directory.
#'   2. The temporary file is renamed to `path`.
#'   3. The newly created `path` is deleted.
#'
#' - If `path` **does exist**:
#'   1. The existing file is temporarily renamed to a backup path.
#'   2. The backup is immediately renamed back to the original `path`.
#'
#' Any failure indicates that the file cannot be safely written or overwritten
#' (for example, because it is open in Excel or the user lacks permissions).
#'
#' @examples
#' \dontrun{
#' can_write_xlsx("C:/some/folder/myfile.xlsx")
#' }
#'
#' @importFrom writexl write_xlsx
#' @export
.can_write_xlsx <- function(path) {
  
  if (!is.character(path) || length(path) != 1L) {
    stop("`path` must be a character scalar.")
  }
  
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    stop("Directory does not exist: ", dir_path)
  }
  
  # Case 1: file does not yet exist -> test we can create it
  if (!file.exists(path)) {
    
    tmp <- tempfile(pattern = ".__writecheck__", tmpdir = dir_path)
    
    on.exit({
      if (file.exists(tmp)) unlink(tmp)
    }, add = TRUE)
    
    # Write a tiny workbook to a temp file
    tryCatch(
      writexl::write_xlsx(
        list(.test = data.frame(ok = TRUE)),
        path = tmp
      ),
      error = function(e) {
        stop(
          "Unable to write a temporary Excel file in directory:\n",
          dir_path, "\nDetails: ", e$message
        )
      }
    )
    
    # Attempt to rename temp -> target WITHOUT emitting a warning
    rename_ok <- suppressWarnings(file.rename(tmp, path))
    
    if (!rename_ok) {
      stop(
        "Cannot create Excel file at path (likely permissions issue):\n",
        path
      )
    }
    
    # Clean up the created test file
    unlink(path)
    
  } else {
    
    # Case 2: file exists -> test we can rename it away and back
    backup <- tempfile(pattern = ".__backup__", tmpdir = dir_path)
    
    # Try renaming the existing file to backup (fails if locked/open)
    moved_out <- suppressWarnings(file.rename(path, backup))
    
    if (!moved_out) {
      stop(
        "Cannot write Excel file: likely open or locked.\n",
        "Target: ", path
      )
    }
    
    # Try to restore the original file name
    moved_back <- suppressWarnings(file.rename(backup, path))
    
    if (!moved_back) {
      stop(
        "Writeability check moved file to backup but could not restore it.\n",
        "Original: ", path, "\nBackup: ", backup
      )
    }
  }
  
  invisible(TRUE)
}
