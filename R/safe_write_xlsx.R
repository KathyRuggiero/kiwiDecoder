#' Safely Write Excel Files with Atomic Replacement
#'
#' `safe_write_xlsx()` writes a data frame (or list of data frames) to an Excel
#' file using [writexl::write_xlsx()] while avoiding partial writes and
#' providing basic protection against locked or in-use files.
#'
#' The function writes to a neutral temporary file in the same directory and
#' then atomically renames it to the final `path`. If the rename fails (for
#' example, because the file is open in Excel), the function stops without
#' modifying the original file.
#'
#' @param x A data frame or a named list of data frames to be written to Excel.
#' @param path Character scalar. Final `.xlsx` file path to create or overwrite.
#' @param use_temp_file Logical. If `TRUE` (default), write to a temporary file
#'   and then rename it into place. If `FALSE`, write directly to `path`
#'   (less robust).
#' @param verbose Logical. If `TRUE` (default), print diagnostic messages when
#'   write operations fail.
#' @param ... Additional arguments passed to [writexl::write_xlsx()].
#'
#' @return Invisibly returns `path` (the final written Excel file path).
#'
#' @details
#' No directories are created; the parent directory of `path` must already
#' exist. To fail fast when a file cannot be written (e.g., because it is open
#' in Excel), pair this with [can_write_xlsx()] before doing heavy work.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(id = 1:3, value = letters[1:3])
#' out_file <- file.path(tempdir(), "example.xlsx")
#' safe_write_xlsx(df, out_file)
#' }
#'
#' @seealso [can_write_xlsx()], [resolve_folder_sequence()],
#'   [writexl::write_xlsx()]
#'
#' @importFrom writexl write_xlsx
#' @export
safe_write_xlsx <- function(x,
                            path,
                            use_temp_file = TRUE,
                            verbose = TRUE,
                            ...) {
  
  if (!is.character(path) || length(path) != 1L) {
    stop("`path` must be a character scalar.")
  }
  
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    stop("Directory does not exist: ", dir_path)
  }
  
  .write <- function(target) {
    writexl::write_xlsx(x, path = target, ...)
    TRUE
  }
  
  if (isTRUE(use_temp_file)) {
    
    tmp <- tempfile(pattern = ".__writing__", tmpdir = dir_path)
    
    on.exit({
      if (file.exists(tmp)) unlink(tmp)
    }, add = TRUE)
    
    ok <- tryCatch(
      .write(tmp),
      error = function(e) {
        if (isTRUE(verbose)) {
          message("Error writing temporary Excel file: ", e$message)
        }
        FALSE
      }
    )
    
    if (!ok) {
      stop("Failed to write temporary Excel file; original file was not modified: ",
           path)
    }
    
    # Rename temp -> final, suppressing OS warnings
    rename_ok <- suppressWarnings(file.rename(tmp, path))
    if (!rename_ok) {
      stop("Failed to replace Excel file — the file may be open or locked: ",
           path)
    }
    
  } else {
    
    ok <- tryCatch(
      .write(path),
      error = function(e) {
        if (isTRUE(verbose)) {
          message("Error writing Excel file: ", e$message)
        }
        FALSE
      }
    )
    
    if (!ok) {
      stop("Failed to write Excel file: ", path)
    }
  }
  
  invisible(path)
}