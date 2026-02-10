#' Resolve and Decode Barcodes for Images Listed in Excel Files (Joined Long Format)
#'
#' `resolve_folder_sequence()` searches a directory (optionally recursively)
#' for Excel workbooks containing image filenames, decodes **all** barcodes
#' and QR codes found in those images using [detect_codes_all_zxing()] in
#' long format (one row per detected symbol), applies [resolve_sequence()] to
#' propagate decoded values across sequentially numbered images, and then
#' performs a **one-to-many join** back onto the original Excel contents
#' using the filename as the key.
#'
#' The updated, joined table overwrites each workbook using
#' [safe_write_xlsx()]. Excel lock files (e.g., `~$file.xlsx`) are ignored.
#'
#' This design preserves all original per-file metadata columns (e.g. those
#' produced by [write_directory_index()]) while attaching 0–n barcode rows
#' per image in a clean long-format structure.
#'
#' @param path Directory containing `.xlsx` files or a root directory to scan.
#'   If the top-level directory contains no matching Excel files, subdirectories
#'   are searched recursively.
#' @param pattern Regular expression identifying Excel files. Default is
#'   `"\\.xlsx$"`.
#'
#' @return
#' Invisibly returns a named list of data frames, one per processed workbook.
#' Each data frame corresponds to the **final joined table written back to the
#' Excel file** and has:
#'
#' - All original columns from the workbook (e.g. `dir`, `subdir`, `file_name`,
#'   `ext`, `rel_path`, `full_path`, etc.), and
#' - Additional barcode-related columns (0–n rows per image), typically:
#'   \itemize{
#'     \item `index` — integer index of the symbol within the image.
#'     \item `code_format` — decoded barcode format (e.g., `"PDF417"`,
#'           `"QRCode"`, `"DataMatrix"`, `"Code39"`).
#'     \item `text` — decoded text content.
#'     \item `type` — semantic classification of the decoded text.
#'     \item `prefix`, `number` — filename-derived components as added by
#'           [resolve_sequence()].
#'     \item `resolved_text` — statefully propagated decoded value for the image.
#'   }
#'
#' If multiple barcodes are detected in the same image, that image will appear
#' in multiple rows (one per symbol). If no usable barcodes are detected,
#' a single row with `NA` barcode fields is joined to the original row for
#' that image.
#'
#' @details
#' For each workbook discovered under `path`, the function:
#'
#' \enumerate{
#'   \item Identifies Excel files matching `pattern`, excluding lock files of
#'         the form `~$*.xlsx`.
#'   \item Reads the workbook via [readxl::read_xlsx()]. The sheet must contain
#'         a `file_name` column (typically produced by
#'         [write_directory_index()]). If a `full_path` column is present, it is
#'         used as the image path; otherwise, image files are assumed to reside
#'         in the same directory as the workbook.
#'   \item Performs an early writeability test using [can_write_xlsx()]. If the
#'         workbook is open or locked, the function stops before decoding any
#'         images.
#'   \item For each row (image) in the workbook, calls [detect_codes_all_zxing()]
#'         on the resolved `full_path`. This returns one row per decoded symbol
#'         (or a single `NA` row when no usable symbol is found).
#'   \item Binds all decoded rows into a long-format tibble and adds context
#'         columns (`file_name`, `full_path`, `rel_path`, `dir`, `subdir`) so
#'         that symbol rows remain traceable to the source images.
#'   \item Applies [resolve_sequence()] to the long-format tibble, adding
#'         `prefix`, `number`, and `resolved_text` columns.
#'   \item Reduces the long-format tibble to **barcode-specific columns plus
#'         join key(s)** (e.g. `file_name`, optionally `full_path`) to avoid
#'         duplicating structural columns already present in the original
#'         workbook.
#'   \item Performs a one-to-many join of the long-format barcode results back
#'         onto the original workbook rows using `file_name` (and `full_path`
#'         when available). This yields a joined table that retains all original
#'         columns and adds 0–n barcode rows per image.
#'   \item Writes the joined table back to the same Excel file using
#'         [safe_write_xlsx()], replacing the previous sheet contents.
#' }
#'
#' Progress messages are printed to the console, and a simple text progress bar
#' is shown while decoding images in each workbook.
#'
#' @seealso [detect_codes_all_zxing()] for per-image decoding and
#'   [resolve_sequence()] for the stateful sequence logic applied to decoded
#'   values.
#'
#' @importFrom readxl read_xlsx
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom dplyr bind_rows left_join
#' @export
resolve_folder_sequence <- function(path, pattern = "\\.xlsx$") {
  
  if (!dir.exists(path)) stop("Path does not exist: ", path)
  
  # Discover Excel files
  top <- list.files(path, pattern = pattern, full.names = TRUE)
  if (length(top) > 0L) {
    excel_files <- top
  } else {
    excel_files <- list.files(
      path,
      pattern  = pattern,
      recursive = TRUE,
      full.names = TRUE
    )
  }
  
  # Remove Excel lock files (~$Something.xlsx)
  excel_files <- excel_files[
    !grepl("^~\\$.*\\.xlsx$", basename(excel_files))
  ]
  
  if (length(excel_files) == 0L) {
    stop("No valid Excel files found in ", path)
  }
  
  message("Found ", length(excel_files), " Excel file(s).")
  
  results <- vector("list", length(excel_files))
  names(results) <- excel_files
  
  for (i in seq_along(excel_files)) {
    
    ex_file <- excel_files[i]
    message("\n[", i, "/", length(excel_files), "] Processing: ", ex_file)
    
    df_orig <- readxl::read_xlsx(ex_file)
    
    if (!"file_name" %in% names(df_orig)) {
      stop("Excel file missing required column 'file_name': ", ex_file)
    }
    
    # EARLY STOP if output cannot be written
    message("  Checking write access for output file...")
    can_write_xlsx(ex_file)
    
    n <- nrow(df_orig)
    message("  Decoding ", n, " image(s)...")
    
    pb <- txtProgressBar(min = 0, max = n, style = 3)
    
    all_decoded_rows <- list()
    k <- 1L
    
    for (j in seq_len(n)) {
      
      fn <- df_orig$file_name[j]
      
      full_path <- if ("full_path" %in% names(df_orig)) {
        df_orig$full_path[j]
      } else {
        file.path(dirname(ex_file), fn)
      }
      
      decoded_tbl <- tryCatch(
        decode_hierarchical_zxing(full_path),
        error = function(e) {
          message("\n    ! Error decoding ", fn, ": ", e$message)
          tibble::tibble(
            file        = full_path,
            index       = NA_integer_,
            code_format = NA_character_,
            text        = NA_character_,
            type        = NA_character_,
            source      = NA_character_,
            region_x    = NA_integer_,
            region_y    = NA_integer_,
            region_w    = NA_integer_,
            region_h    = NA_integer_
          )
        }
      )
      
      # add context columns so long-format rows remain linked to the image
      decoded_tbl$file_name <- fn
      decoded_tbl$full_path <- full_path
      decoded_tbl$rel_path  <- if ("rel_path" %in% names(df_orig)) df_orig$rel_path[j] else fn
      decoded_tbl$dir       <- if ("dir" %in% names(df_orig)) df_orig$dir[j] else dirname(ex_file)
      decoded_tbl$subdir    <- if ("subdir" %in% names(df_orig)) df_orig$subdir[j] else basename(dirname(full_path))
      
      all_decoded_rows[[k]] <- decoded_tbl
      k <- k + 1L
      
      setTxtProgressBar(pb, j)
    }
    
    close(pb)
    
    long_df <- dplyr::bind_rows(all_decoded_rows)
    
    message("  Applying sequence resolution...")
    long_df <- resolve_sequence(long_df)
    
    # ---- reduce to barcode-specific columns + keys for joining ----
    barcode_cols <- c(
      "file_name",
      "full_path",
      "index",
      "code_format",
      "text",
      "type",
      "prefix",
      "number",
      "resolved_text"
    )
    barcode_cols <- intersect(barcode_cols, names(long_df))
    barcodes_for_join <- long_df[, barcode_cols, drop = FALSE]
    
    # Decide join key(s)
    if ("full_path" %in% names(df_orig) && "full_path" %in% names(barcodes_for_join)) {
      join_by <- c("file_name", "full_path")
    } else {
      join_by <- "file_name"
    }
    
    message("  Joining decoded barcodes back to original rows...")
    joined_df <- df_orig |>
      dplyr::left_join(barcodes_for_join, by = join_by)
    
    message("  Writing updated Excel file (joined table)...")
    safe_write_xlsx(joined_df, ex_file)
    
    results[[i]] <- joined_df
    
    message("[", i, "/", length(excel_files), "] Done: ", ex_file)
  }
  
  message("\nAll files processed.")
  invisible(results)
}
