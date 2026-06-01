#' Build a Root-Level Catalogue of All Image Index Files
#'
#' `build_catalogue()` reads every per-folder CSV index file produced by
#' [scan_directories()] under `root`, concatenates them into one table, and
#' writes the result to `catalogue.csv` in the root directory.
#'
#' This is **Step 5** — the final step in the kiwiDecoder pipeline. Run it
#' after all four enrichment steps have been completed so the catalogue
#' contains the full set of columns: file location, barcode decode, biomaterial
#' identity, and photo metadata.
#'
#' The catalogue makes it easy to browse the entire image collection in one
#' place, link images to breeding records, or hand the data to an analyst
#' without requiring them to know the folder structure.
#'
#' Files excluded from the catalogue: `scan_log.csv`, `resolve_log.csv`, and
#' the catalogue file itself.
#'
#' @param root Character scalar. Root directory containing the per-folder index
#'   CSV files (produced by [scan_directories()]).
#' @param output_file Path for the output catalogue CSV. Defaults to
#'   `file.path(root, "catalogue.csv")`.
#' @param pattern Regular expression used to identify index CSV files.
#'   Default `"\\.csv$"`.
#'
#' @return Invisibly returns the concatenated tibble that was written to disk.
#'
#' @section Output columns:
#' The catalogue contains whatever columns are present in the per-folder CSVs.
#' After a full pipeline run (all four steps) those columns are:
#'
#' | Column | Source |
#' |---|---|
#' | `dir` | Folder containing the image |
#' | `file_name` | Image filename |
#' | `ext` | File extension |
#' | `full_path` | Absolute path to the image |
#' | `index` | Symbol index within the image |
#' | `code_format` | Barcode format decoded (e.g. `PDF417`, `QRCode`); `"unknown"` for undecodable images |
#' | `text` | Decoded barcode text; `NA` for undecodable images |
#' | `type` | Code classification (`scion`, `location`, …); `NA` for undecodable images |
#' | `decode_status` | How the barcode information was obtained: `"decoded"` (direct read), `"propagated"` (copied from an adjacent card image), or `"undecodable"` (no barcode could be read and no donor was available) |
#' | `donor_file` | Filename of the card image whose barcode was propagated to this row; `NA` unless `decode_status == "propagated"` |
#' | `scion_name` | Scion name from KiwiCloud |
#' | `scion_genotype` | Genotype string from KiwiCloud |
#' | `location_name` | Trial/site name from KiwiCloud |
#' | `location_address` | Vine address from KiwiCloud |
#' | `file_owner` | Windows file owner (photographer proxy) |
#' | `camera_make` | Camera manufacturer from EXIF |
#' | `camera_model` | Camera model from EXIF |
#' | `GPSLatitude` | GPS latitude (if recorded) |
#' | `GPSLongitude` | GPS longitude (if recorded) |
#' | `DateTimeOriginal` | Photo capture date/time from EXIF |
#'
#' @seealso [scan_directories()], [resolve_folder_sequence()],
#'   [enrich_index_with_biomaterial()], [enrich_index_with_metadata()]
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr bind_rows
#' @export
build_catalogue <- function(root,
                            output_file = file.path(root, "catalogue.csv"),
                            pattern     = "\\.csv$") {

  if (!dir.exists(root)) stop("Root directory does not exist: ", root)

  exclude_names <- c("scan_log.csv", "resolve_log.csv", basename(output_file))

  all_csvs <- list.files(root, pattern = pattern,
                         recursive = TRUE, full.names = TRUE)
  all_csvs <- all_csvs[!basename(all_csvs) %in% exclude_names]

  if (length(all_csvs) == 0L) {
    stop("No index CSV files found under: ", root)
  }

  message("Reading ", length(all_csvs), " index CSV file(s)...")

  tables <- vector("list", length(all_csvs))
  for (i in seq_along(all_csvs)) {
    tables[[i]] <- tryCatch(
      readr::read_csv(all_csvs[i], show_col_types = FALSE),
      error = function(e) {
        warning("Could not read: ", all_csvs[i], " — ", conditionMessage(e))
        NULL
      }
    )
  }

  tables    <- Filter(Negate(is.null), tables)
  catalogue <- dplyr::bind_rows(tables)

  # Coverage summary
  n_rows    <- nrow(catalogue)
  n_decoded <- if ("code_format" %in% names(catalogue))
    sum(!is.na(catalogue$code_format) & catalogue$code_format != "unknown",
        na.rm = TRUE) else NA_integer_
  n_scion   <- if ("scion_name" %in% names(catalogue))
    sum(!is.na(catalogue$scion_name), na.rm = TRUE) else NA_integer_
  n_gps     <- if ("GPSLatitude" %in% names(catalogue))
    sum(!is.na(catalogue$GPSLatitude), na.rm = TRUE) else NA_integer_

  message(sprintf(
    "  %d folders | %d rows | %d decoded | %d with scion_name | %d with GPS",
    length(all_csvs), n_rows,
    if (is.na(n_decoded)) 0L else n_decoded,
    if (is.na(n_scion))  0L else n_scion,
    if (is.na(n_gps))    0L else n_gps
  ))

  message("Writing catalogue to: ", output_file)
  readr::write_csv(catalogue, output_file)

  invisible(catalogue)
}
