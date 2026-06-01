#' Write Excel Index of Image Files in a Single Directory
#'
#' `write_directory_index()` lists image files in a single directory, applies
#' extension and stem filtering, and writes the result to a CSV file.
#'
#' The resulting CSV is designed to be a **stable per-directory index**
#' of images and is the canonical input format for downstream decoding with
#' [resolve_folder_sequence()]. In the typical workflow, these index files are
#' generated automatically by [scan_directories()], but
#' `write_directory_index()` can also be called directly when you want to
#' index a standalone folder or a small subset of the file tree.
#'
#' @param dir Character scalar. Directory to scan for image files.
#' @param extensions Character vector of allowed image extensions (case-insensitive).
#'   Defaults to \code{c("jpg", "jpeg", "png", "heic")}. HEIC support requires the
#'   \code{pillow-heif} Python package to be installed in the active conda environment.
#' @param prefixes Optional character vector of regular expression patterns
#'   matched against filename stems (without extensions). Use `NULL` or
#'   `character(0)` to disable stem filtering and index all files with the
#'   specified extensions.
#' @param csv_path Full file path for the CSV file to be written. By
#'   convention, [scan_directories()] uses a filename that matches the
#'   directory name (e.g. `"<subdir>/<subdir>.csv"`).
#'
#' @return
#' Invisibly returns the data frame that is written to CSV. This data frame
#' has one row per image file and contains:
#' \describe{
#'   \item{dir}{Directory containing the image file.}
#'   \item{file_name}{Image filename (basename including extension).}
#'   \item{ext}{Lowercased file extension (e.g., `"jpg"`, `"png"`).}
#'   \item{full_path}{Normalised absolute path to the image file.}
#' }
#'
#' These four columns form the **structural index** that downstream functions
#' use to locate and interpret images. Each subsequent pipeline step adds its
#' own columns to this base and writes the enriched result back to the same CSV.
#'
#' @details
#' This helper is the low-level building block behind [scan_directories()]:
#'
#' - `scan_directories()` performs a depth-first traversal over a root folder,
#'   and for each directory calls `write_directory_index()` to create or
#'   refresh its Excel index.
#' - The resulting per-directory workbooks can then be passed to
#'   [resolve_folder_sequence()], which decodes barcodes or QR codes in the
#'   listed image files and writes an enriched, long-format table back into
#'   those workbooks.
#'
#' You can also call `write_directory_index()` on its own when:
#' - you only need an inventory of images for a small subset of folders, or
#' - you want to create a manual test workbook for experimenting with
#'   [detect_codes_all_zxing()] and [resolve_sequence()] on a handful of
#'   images.
#'
#' @seealso
#'   [scan_directories()] for depth-first indexing of an entire tree, and
#'   [resolve_folder_sequence()] for decoding and resolving barcodes across
#'   the CSV indexes produced by this function.
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr bind_rows
#' @export
write_directory_index <- function(dir,
                                  extensions = c("jpg", "jpeg", "png", "heic"),
                                  prefixes   = NULL,
                                  csv_path) {
  if (!dir.exists(dir)) return(invisible(NULL))
  
  files <- list.files(dir, full.names = TRUE, recursive = FALSE)
  fi <- file.info(files)
  files <- files[!fi$isdir]
  
  if (length(files) == 0) return(invisible(NULL))
  
  # Extension filtering
  ext_pattern <- paste0("\\.(", paste(extensions, collapse = "|"), ")$")
  files <- files[grepl(ext_pattern, files, ignore.case = TRUE)]
  if (length(files) == 0) return(invisible(NULL))
  
  # Stem filtering
  if (!is.null(prefixes) && length(prefixes) > 0) {
    stems <- tools::file_path_sans_ext(basename(files))
    prefix_pattern <- paste(prefixes, collapse = "|")
    keep <- grepl(prefix_pattern, stems, ignore.case = TRUE)
    files <- files[keep]
  }
  if (length(files) == 0) return(invisible(NULL))
  
  # Build data frame
  df <- data.frame(
    dir       = dirname(files),
    file_name = basename(files),
    ext       = tolower(tools::file_ext(files)),
    full_path = normalizePath(files, winslash = "/", mustWork = FALSE),
    stringsAsFactors = FALSE
  )
  
  # Ensure directory exists for writing
  out_dir <- dirname(csv_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # Merge with existing CSV so decoded columns are never overwritten.
  # New files get one row appended; files already in the index are untouched.
  if (file.exists(csv_path)) {
    existing <- readr::read_csv(csv_path, show_col_types = FALSE)
    new_rows  <- df[!df$file_name %in% existing$file_name, , drop = FALSE]
    if (nrow(new_rows) == 0L) {
      return(invisible(existing))
    }
    message("  Adding ", nrow(new_rows), " new file(s) to existing index.")
    df <- dplyr::bind_rows(existing, new_rows)
  }

  readr::write_csv(df, file = csv_path)

  invisible(df)
}
