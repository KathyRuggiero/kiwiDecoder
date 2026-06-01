#' Enrich Index CSV Files with EXIF Photo Metadata
#'
#' `enrich_index_with_metadata()` reads every CSV index file produced by
#' [scan_directories()] under `path`, extracts EXIF metadata for each image
#' using [extract_image_metadata()], and writes the enriched result back to
#' the same CSV file. This is Step 4 in the kiwiDecoder pipeline.
#'
#' The following metadata columns are added to each CSV:
#'
#' \itemize{
#'   \item \code{file_owner}       – Windows file-system owner (photographer proxy)
#'   \item \code{camera_make}      – Camera manufacturer (e.g. `"Apple"`)
#'   \item \code{camera_model}     – Camera model (e.g. `"iPhone 13"`)
#'   \item \code{GPSLatitude}      – GPS latitude (if location services were on)
#'   \item \code{GPSLongitude}     – GPS longitude
#'   \item \code{DateTimeOriginal} – Capture date/time from EXIF
#' }
#'
#' The join between metadata and the index is on `full_path`. If `full_path`
#' is not already a column in the CSV it is constructed from `dir` + `file_name`.
#'
#' By default, CSV files that already contain all six metadata columns are
#' skipped. Set `overwrite = TRUE` to re-extract and replace them.
#'
#' To process a single CSV file use [enrich_one_index_with_metadata()].
#'
#' @param path Directory containing CSV index files. If no CSV files are found
#'   at the top level the search is extended recursively.
#' @param pattern Regular expression used to identify index CSV files.
#' @param overwrite Logical. If `TRUE`, re-extract and overwrite metadata
#'   columns that are already present. Default `FALSE`.
#'
#' @return Invisibly returns a named list of enriched tibbles, one per CSV
#'   processed.
#'
#' @seealso [enrich_one_index_with_metadata()], [extract_image_metadata()],
#'   [scan_directories()], [resolve_folder_sequence()]
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr left_join
#' @export
enrich_index_with_metadata <- function(path, pattern = "\\.csv$", overwrite = FALSE) {

  if (!dir.exists(path)) {
    stop("Path does not exist: ", path)
  }

  # -------------------------------------------------------------------------
  # Discover index CSV files (same logic as resolve_folder_sequence)
  # -------------------------------------------------------------------------
  csv_files <- list.files(path, pattern = pattern, full.names = TRUE)
  if (length(csv_files) == 0L) {
    csv_files <- list.files(path, pattern = pattern, recursive = TRUE, full.names = TRUE)
  }
  csv_files <- csv_files[!basename(csv_files) %in%
                           c("resolve_log.csv", "scan_log.csv", "catalogue.csv")]

  if (length(csv_files) == 0L) {
    stop("No CSV index files found in: ", path)
  }

  message("Found ", length(csv_files), " CSV index file(s).")

  meta_cols <- c("file_owner", "camera_make", "camera_model",
                 "GPSLatitude", "GPSLongitude", "DateTimeOriginal")

  results <- vector("list", length(csv_files))
  names(results) <- csv_files

  for (i in seq_along(csv_files)) {
    csv_file <- csv_files[i]
    message("\n[", i, "/", length(csv_files), "] ", csv_file)

    results[[i]] <- .enrich_one_csv(csv_file, meta_cols, overwrite)
  }

  message("\nAll files processed.")
  invisible(results)
}


#' Enrich a Single Index CSV with EXIF Photo Metadata
#'
#' Single-file counterpart to [enrich_index_with_metadata()]. Reads one CSV
#' index file, extracts EXIF metadata for all images it references, joins the
#' metadata columns, and writes the enriched CSV back to disk.
#'
#' @param index_csv_path Path to a single CSV index file produced by
#'   [scan_directories()] or [write_directory_index()].
#' @param overwrite Logical. If `TRUE`, re-extract and overwrite metadata
#'   columns that are already present. Default `FALSE`.
#'
#' @return Invisibly returns the enriched tibble.
#'
#' @seealso [enrich_index_with_metadata()], [extract_image_metadata()]
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr left_join
#' @export
enrich_one_index_with_metadata <- function(index_csv_path, overwrite = FALSE) {

  if (!file.exists(index_csv_path)) {
    stop("Index CSV does not exist: ", index_csv_path)
  }

  message("Processing: ", index_csv_path)

  meta_cols <- c("file_owner", "camera_make", "camera_model",
                 "GPSLatitude", "GPSLongitude", "DateTimeOriginal")

  result <- .enrich_one_csv(index_csv_path, meta_cols, overwrite)
  invisible(result)
}


# Internal workhorse — enriches one CSV and writes it back.
.enrich_one_csv <- function(csv_file, meta_cols, overwrite) {

  df <- readr::read_csv(csv_file, show_col_types = FALSE)

  if (!"file_name" %in% names(df)) {
    warning("Skipping (missing 'file_name' column): ", csv_file)
    return(df)
  }

  # Ensure full_path is available for the join
  if (!"full_path" %in% names(df)) {
    if (!all(c("dir", "file_name") %in% names(df))) {
      warning("Skipping (needs 'full_path' or 'dir' + 'file_name'): ", csv_file)
      return(df)
    }
    df$full_path <- file.path(df$dir, df$file_name)
  }

  # Row-level skip: find unique paths that still need metadata.
  # If file_owner exists and overwrite = FALSE, only process rows where it is NA.
  if (!overwrite && "file_owner" %in% names(df)) {
    paths_needed <- unique(df$full_path[is.na(df$file_owner)])
    if (length(paths_needed) == 0L) {
      message("  All images already have metadata — skipping.",
              " Use overwrite = TRUE to re-extract.")
      return(df)
    }
    n_done <- length(unique(df$full_path)) - length(paths_needed)
    message("  Extracting metadata for ", length(paths_needed),
            " image(s) (", n_done, " already enriched).")
  } else {
    paths_needed <- unique(df$full_path)
    message("  Extracting metadata for ", length(paths_needed), " image(s)...")
  }

  meta <- tryCatch(
    extract_image_metadata(paths_needed, verbose = FALSE),
    error = function(e) {
      warning("extract_image_metadata() failed: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(meta) || nrow(meta$data) == 0L) {
    message("  No metadata returned — CSV unchanged.")
    return(df)
  }

  keep_meta <- intersect(c("full_path", meta_cols), names(meta$data))
  meta_tbl  <- meta$data[, keep_meta, drop = FALSE]

  if (overwrite || !"file_owner" %in% names(df)) {
    # Fresh or forced re-extract: drop any existing meta cols then join all rows
    df <- df[, setdiff(names(df), meta_cols), drop = FALSE]
    df <- dplyr::left_join(df, meta_tbl, by = "full_path")
  } else {
    # Incremental: preserve row order, update only unenriched rows
    df$.row_idx. <- seq_len(nrow(df))
    unenriched   <- is.na(df$file_owner)
    df_done      <- df[!unenriched, , drop = FALSE]
    df_todo      <- df[ unenriched, setdiff(names(df), meta_cols), drop = FALSE]
    df_todo      <- dplyr::left_join(df_todo, meta_tbl, by = "full_path")
    df           <- dplyr::bind_rows(df_done, df_todo)
    df           <- df[order(df$.row_idx.), , drop = FALSE]
    df$.row_idx. <- NULL
  }

  # Print per-field coverage
  for (col in intersect(meta_cols, names(df))) {
    n_present <- sum(!is.na(df[[col]]))
    message("    ", col, ": ", n_present, "/", nrow(df), " non-missing")
  }

  message("  Writing updated CSV...")
  readr::write_csv(df, csv_file)

  df
}
