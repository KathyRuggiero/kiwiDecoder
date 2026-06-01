#' Monitor image-level progress of resolve_folder_sequence()
#'
#' This helper reads a per-image log file (`resolve_log.csv`) written by
#' [resolve_folder_sequence()] and reports how many images have been processed
#' so far, which image was processed most recently, and (optionally) an
#' estimated percentage complete.
#'
#' The log file is expected to have one row per image, with columns:
#' \itemize{
#'   \item `time`       – timestamp
#'   \item `workbook`   – Excel workbook path
#'   \item `image_file` – filename of the image (e.g. `"IMG_0123.JPG"`)
#'   \item `full_path`  – full path to the image
#'   \item `prefix`     – parsed filename prefix
#'   \item `number`     – parsed numeric component
#'   \item `decoded`    – logical; `TRUE` if any symbol was decoded
#' }
#'
#' @param root Root directory passed to [resolve_folder_sequence()]. This is
#'   where `resolve_folder_sequence()` searches for `.xlsx` files and where
#'   `resolve_log.csv` is written.
#' @param log_file Name of the log file (default `"resolve_log.csv"`).
#' @param pattern Regular expression used to identify CSV index files.
#'   This should match the `pattern` argument used in [resolve_folder_sequence()].
#'   It is used only to estimate the total number of images for percentage
#'   reporting.
#' @param tail_n Number of last log entries to print.
#'
#' @return Invisibly returns a list with components:
#' \itemize{
#'   \item `summary` – the last log row (or `NULL` if none),
#'   \item `log_tail` – the last `tail_n` log rows (or `NULL`),
#'   \item `images` – a list with `completed`, `total`, and `pct` (percentage).
#' }
#'
#' @examples
#' \dontrun{
#'   # While resolve_folder_sequence() is running in the background:
#'   monitor_resolve_progress("N:/Projects/.../Flower_buds")
#'
#'   # Auto-refresh every 10 seconds:
#'   repeat {
#'     monitor_resolve_progress(index_root)
#'     Sys.sleep(10)
#'   }
#' }
#'
#' @importFrom utils read.csv tail
#' @export
monitor_resolve_progress <- function(
    root,
    log_file = "resolve_log.csv",
    pattern  = "\\.csv$",
    tail_n   = 10
) {
  log_path <- file.path(root, log_file)

  cat("=== kiwiDecoder image-level resolve progress ===\n")
  cat("Root folder: ", normalizePath(root, winslash = "/"), "\n", sep = "")
  cat("Log file:    ", log_path, "\n\n", sep = "")

  # 1. If no log file yet
  if (!file.exists(log_path)) {
    cat("No log file found yet.\n\n")
    return(invisible(list(
      summary  = NULL,
      log_tail = NULL,
      images   = list(completed = 0L, total = NA_integer_, pct = NA_real_)
    )))
  }

  # 2. Read and validate log file
  log_df <- tryCatch(
    read.csv(log_path, stringsAsFactors = FALSE),
    error = function(e) {
      warning("Failed to read resolve log: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(log_df) || nrow(log_df) == 0L) {
    cat("Log file exists but contains no entries yet.\n\n")
    return(invisible(list(
      summary  = NULL,
      log_tail = NULL,
      images   = list(completed = 0L, total = NA_integer_, pct = NA_real_)
    )))
  }

  # Ensure sorted by time
  if ("time" %in% names(log_df)) {
    log_df <- log_df[order(log_df$time), ]
  }

  n_done <- nrow(log_df)
  last   <- log_df[n_done, , drop = FALSE]

  # 3. Estimate total images by counting rows in all CSV index files under root
  csv_files <- list.files(
    root,
    pattern    = pattern,
    full.names = TRUE,
    recursive  = TRUE
  )
  csv_files <- csv_files[!basename(csv_files) %in% c("resolve_log.csv", "scan_log.csv")]

  total_images <- NA_integer_
  if (length(csv_files) > 0L) {
    # readLines is fast: no parsing, just line counting
    n_per <- vapply(csv_files, function(f) {
      tryCatch(
        length(readLines(f, warn = FALSE)) - 1L,  # subtract header row
        error = function(e) NA_integer_
      )
    }, integer(1))
    total_images <- sum(n_per, na.rm = TRUE)
  }

  pct <- if (!is.na(total_images) && total_images > 0L) {
    100 * n_done / total_images
  } else {
    NA_real_
  }

  # 4. Print summary
  cat("Images processed: ", n_done, "\n", sep = "")
  if (!is.na(total_images) && total_images > 0L) {
    cat("Total images:    ", total_images, "\n", sep = "")
    cat("Progress:        ", sprintf("%.1f", pct), "%\n", sep = "")
  } else {
    cat("Progress:        <unknown>\n")
  }

  cat("\nLast image:      ", last$image_file, "\n", sep = "")
  if ("prefix" %in% names(last) && "number" %in% names(last)) {
    cat("Prefix/Number:   ", paste0(last$prefix, "_", last$number), "\n", sep = "")
  }
  if ("decoded" %in% names(last)) {
    cat("Decoded OK:      ", last$decoded, "\n", sep = "")
  }
  if ("time" %in% names(last)) {
    cat("Last update:     ", last$time, "\n\n", sep = "")
  } else {
    cat("\n")
  }

  # 5. Tail of recent images
  tail_n <- min(tail_n, n_done)
  cat("---- Last ", tail_n, " images ----\n", sep = "")
  print(tail(log_df, tail_n))
  cat("\n")

  invisible(list(
    summary  = last,
    log_tail = tail(log_df, tail_n),
    images   = list(
      completed = n_done,
      total     = total_images,
      pct       = pct
    )
  ))
}
