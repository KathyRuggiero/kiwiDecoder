#' Enrich Index CSV Files with Biomaterial Identity from KiwiCloud
#'
#' `enrich_index_with_biomaterial()` reads every CSV index file produced by
#' [scan_directories()] under `path`, looks up each unique decoded barcode
#' text in the KiwiCloud database using [lookup_biomaterial_name()], and
#' writes the identity columns back to the same CSV file.
#'
#' This is the standalone equivalent of passing `con` to
#' [resolve_folder_sequence()]. Use it when:
#' \itemize{
#'   \item You previously decoded without a database connection and now want
#'         to add identity columns to already-decoded CSVs.
#'   \item You want to re-run the lookup after database records have been
#'         updated.
#' }
#'
#' The following columns are added (or refreshed if `overwrite = TRUE`):
#'
#' \itemize{
#'   \item \code{scion_name}        – human-readable scion name (e.g. `"Hayward"`)
#'   \item \code{genotype}          – genotype string from the database
#'   \item \code{location_name}     – site / trial name
#'   \item \code{location_address}  – precise vine address (e.g. `"TRC-17-13-19c"`)
#'   \item \code{site_name}         – orchard site
#'   \item \code{block}, \code{row}, \code{bay}, \code{position} – sub-site coordinates
#' }
#'
#' Codes that are `NA`, empty, or `"unknown"` are excluded from the lookup.
#' Codes that are not found in the database will have `NA` for all identity
#' columns.
#'
#' By default, CSV files that already contain a `scion_name` column are
#' skipped. Set `overwrite = TRUE` to re-query and replace all identity
#' columns.
#'
#' To process a single CSV file use [enrich_one_index_with_biomaterial()].
#'
#' @param path Directory containing CSV index files. If no CSV files are found
#'   at the top level the search is extended recursively.
#' @param con A DBI connection to the KiwiCloud (Yugabyte) database. The
#'   connection must have read access to `export_biomaterial_wide`.
#' @param pattern Regular expression used to identify index CSV files.
#' @param overwrite Logical. If `TRUE`, drop and re-query all identity columns
#'   even when they are already present. Default `FALSE`.
#'
#' @return Invisibly returns a named list of enriched tibbles, one per CSV
#'   processed.
#'
#' @seealso [enrich_one_index_with_biomaterial()], [lookup_biomaterial_name()],
#'   [resolve_folder_sequence()], [scan_directories()]
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr left_join
#' @export
enrich_index_with_biomaterial <- function(path, con, pattern = "\\.csv$",
                                          overwrite = FALSE) {

  if (!dir.exists(path)) {
    stop("Path does not exist: ", path)
  }

  # -------------------------------------------------------------------------
  # Discover index CSV files
  # -------------------------------------------------------------------------
  csv_files <- list.files(path, pattern = pattern, full.names = TRUE)
  if (length(csv_files) == 0L) {
    csv_files <- list.files(path, pattern = pattern, recursive = TRUE,
                            full.names = TRUE)
  }
  csv_files <- csv_files[!basename(csv_files) %in%
                           c("resolve_log.csv", "scan_log.csv", "catalogue.csv")]

  if (length(csv_files) == 0L) {
    stop("No CSV index files found in: ", path)
  }

  message("Found ", length(csv_files), " CSV index file(s).")

  bio_cols <- c("scion_name", "scion_genotype", "location_name", "location_address")

  results <- vector("list", length(csv_files))
  names(results) <- csv_files

  for (i in seq_along(csv_files)) {
    csv_file <- csv_files[i]
    message("\n[", i, "/", length(csv_files), "] ", csv_file)

    results[[i]] <- .enrich_one_csv_bio(csv_file, con, bio_cols, overwrite)
  }

  message("\nAll files processed.")
  invisible(results)
}


#' Enrich a Single Index CSV with Biomaterial Identity from KiwiCloud
#'
#' Single-file counterpart to [enrich_index_with_biomaterial()].
#'
#' @param index_csv_path Path to a single CSV index file produced by
#'   [scan_directories()] or [write_directory_index()].
#' @param con A DBI connection to the KiwiCloud (Yugabyte) database.
#' @param overwrite Logical. If `TRUE`, re-query and overwrite identity columns
#'   that are already present. Default `FALSE`.
#'
#' @return Invisibly returns the enriched tibble.
#'
#' @seealso [enrich_index_with_biomaterial()], [lookup_biomaterial_name()]
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr left_join
#' @export
enrich_one_index_with_biomaterial <- function(index_csv_path, con,
                                              overwrite = FALSE) {

  if (!file.exists(index_csv_path)) {
    stop("Index CSV does not exist: ", index_csv_path)
  }

  message("Processing: ", index_csv_path)

  bio_cols <- c("scion_name", "scion_genotype", "location_name", "location_address")

  result <- .enrich_one_csv_bio(index_csv_path, con, bio_cols, overwrite)
  invisible(result)
}


# Internal workhorse — looks up biomaterial identity for one CSV and writes back.
.enrich_one_csv_bio <- function(csv_file, con, bio_cols, overwrite) {

  df <- readr::read_csv(csv_file, show_col_types = FALSE)

  if (!"text" %in% names(df)) {
    warning("Skipping (no 'text' column — has this CSV been decoded yet?): ",
            csv_file)
    return(df)
  }

  # Row-level skip: if scion_name column exists and overwrite = FALSE, only
  # look up codes for rows where scion_name is currently NA (new images added
  # since the last enrich run).
  if (!overwrite && "scion_name" %in% names(df)) {
    new_codes <- unique(df$text[
      !is.na(df$text) & nzchar(df$text) & df$text != "unknown" &
        is.na(df$scion_name)
    ])
    if (length(new_codes) == 0L) {
      message("  All decoded rows already have identity — skipping.",
              " Use overwrite = TRUE to re-query.")
      return(df)
    }
    n_done <- length(unique(df$text[!is.na(df$scion_name)]))
    message("  Looking up ", length(new_codes), " new code(s) (",
            n_done, " already matched).")
    lookup_codes <- new_codes
  } else {
    lookup_codes <- unique(df$text[
      !is.na(df$text) & nzchar(df$text) & df$text != "unknown"
    ])
    if (length(lookup_codes) == 0L) {
      message("  No decodable codes found — CSV unchanged.")
      return(df)
    }
    message("  Looking up ", length(lookup_codes), " unique code(s) in KiwiCloud...")
  }

  lkp <- tryCatch(
    lookup_biomaterial_name(lookup_codes, con),
    error = function(e) {
      warning("lookup_biomaterial_name() failed: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(lkp) || nrow(lkp) == 0L) {
    message("  No matches returned — CSV unchanged.")
    return(df)
  }

  lkp_sel <- lkp[, intersect(c("code", bio_cols), names(lkp)), drop = FALSE]

  if (overwrite) {
    df <- df[, setdiff(names(df), bio_cols), drop = FALSE]
    df <- dplyr::left_join(df, lkp_sel, by = c("text" = "code"))
  } else if ("scion_name" %in% names(df)) {
    # Incremental: update only rows with NA scion_name, preserve row order
    df$.row_idx.   <- seq_len(nrow(df))
    unenriched     <- is.na(df$scion_name) & !is.na(df$text) & df$text != "unknown"
    df_done        <- df[!unenriched, , drop = FALSE]
    df_todo        <- df[ unenriched, setdiff(names(df), bio_cols), drop = FALSE]
    df_todo        <- dplyr::left_join(df_todo, lkp_sel, by = c("text" = "code"))
    df             <- dplyr::bind_rows(df_done, df_todo)
    df             <- df[order(df$.row_idx.), , drop = FALSE]
    df$.row_idx.   <- NULL
  } else {
    df <- dplyr::left_join(df, lkp_sel, by = c("text" = "code"))
  }

  # Coverage summary
  n_decoded <- sum(!is.na(df$text) & df$text != "unknown", na.rm = TRUE)
  n_matched <- sum(!is.na(df$scion_name), na.rm = TRUE)
  message("  Matched: ", n_matched, " / ", n_decoded,
          " decoded rows have a scion_name")

  message("  Writing updated CSV...")
  readr::write_csv(df, csv_file)

  df
}
