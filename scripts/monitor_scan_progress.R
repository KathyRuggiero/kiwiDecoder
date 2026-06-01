#' Monitor progress of a background scan_directories() job
#'
#' This helper reads the scan log CSV and checkpoint RDS written by
#' `scan_directories()` and prints a compact progress summary.
#'
#' @param root Root directory being scanned (same as `root` passed to
#'   `scan_directories()`).
#' @param log_file Name of the log file (default "scan_log.csv").
#' @param checkpoint_file Name of the checkpoint file
#'   (default "scan_checkpoint.rds").
#' @param total_files Optional integer: total number of image files
#'   expected to be scanned. If supplied, a percentage complete is
#'   estimated. If NULL, no percentage is shown.
#' @param tail_n How many recent log rows to show.
#'
#' @return Invisibly returns a list with components `summary`,
#'   `log_tail`, and `checkpoint`.
#' @export
monitor_scan_progress <- function(
    root,
    log_file        = "scan_log.csv",
    checkpoint_file = "scan_checkpoint.rds",
    total_files     = NULL,
    tail_n          = 10
) {
  log_path        <- file.path(root, log_file)
  checkpoint_path <- file.path(root, checkpoint_file)

  cat("=== kiwiDecoder scan progress ===\n")
  cat("Root folder: ", normalizePath(root, winslash = "/"), "\n", sep = "")
  cat("Log file:    ", log_path, "\n", sep = "")
  cat("Checkpoint:  ", checkpoint_path, "\n\n", sep = "")

  # ---- 1. Read log file ----
  log_df <- NULL
  if (file.exists(log_path)) {
    # Use read.csv to avoid extra dependencies
    log_df <- tryCatch(
      utils::read.csv(log_path, stringsAsFactors = FALSE),
      error = function(e) {
        warning("Failed to read log file: ", conditionMessage(e))
        NULL
      }
    )
  } else {
    cat("No log file found yet.\n\n")
  }

  # ---- 2. Read checkpoint file ----
  checkpoint <- NULL
  if (file.exists(checkpoint_path)) {
    checkpoint <- tryCatch(
      readRDS(checkpoint_path),
      error = function(e) {
        warning("Failed to read checkpoint file: ", conditionMessage(e))
        NULL
      }
    )
  } else {
    cat("No checkpoint file found yet.\n\n")
  }

  # ---- 3. Summarise progress from log ----
  n_processed <- if (!is.null(log_df)) nrow(log_df) else 0

  # Try to get last processed path / time from log, being defensive
  last_path <- NA_character_
  last_time <- NA_character_

  if (!is.null(log_df) && n_processed > 0) {
    last_row <- log_df[n_processed, , drop = FALSE]

    # Common column names you might have in your log
    # Adjust these if your actual column names differ.
    if ("path" %in% names(last_row)) {
      last_path <- last_row$path
    } else if ("file" %in% names(last_row)) {
      last_path <- last_row$file
    }

    if ("timestamp" %in% names(last_row)) {
      last_time <- last_row$timestamp
    } else {
      # Fallback to file modification time
      last_time <- as.character(file.info(log_path)$mtime)
    }
  }

  cat("---- Summary ----\n")
  cat("Records logged: ", n_processed, "\n", sep = "")
  if (!is.na(last_path)) {
    cat("Last path:      ", last_path, "\n", sep = "")
  }
  if (!is.na(last_time)) {
    cat("Last update:    ", last_time, "\n", sep = "")
  }

  # Optional: percentage complete if total_files provided
  pct <- NA_real_
  if (!is.null(total_files) && is.numeric(total_files) && total_files > 0) {
    pct <- 100 * n_processed / total_files
    cat(
      "Estimated progress: ",
      sprintf("%.1f", pct), "% (", n_processed, " / ", total_files, ")\n",
      sep = ""
    )
  }

  cat("\n")

  # ---- 4. Show tail of log ----
  if (!is.null(log_df) && n_processed > 0) {
    cat("---- Last ", min(tail_n, n_processed),
        " log entries ----\n", sep = "")
    print(utils::tail(log_df, tail_n))
    cat("\n")
  }

  # ---- 5. Show a glimpse of the checkpoint ----
  if (!is.null(checkpoint)) {
    cat("---- Checkpoint snapshot ----\n")
    # Don't dump huge objects; show a compact structure
    utils::str(checkpoint, max.level = 2)
    cat("\n")
  }

  invisible(list(
    summary = list(
      n_processed = n_processed,
      last_path   = last_path,
      last_time   = last_time,
      total_files = total_files,
      pct         = pct
    ),
    log_tail   = if (!is.null(log_df)) utils::tail(log_df, tail_n) else NULL,
    checkpoint = checkpoint
  ))
}

monitor_scan_progress(
  root        = image_root,
  total_files = NULL,         # or set this if you know it
  tail_n      = 5             # show last 5 log entries
)

# === kiwiDecoder scan progress ===
#   Root folder: N:/Projects/Stage2 Clonal Trials/Images/A Auto-Naming
# Log file:    N:/Projects/.../scan_log.csv
# Checkpoint:  N:/Projects/.../scan_checkpoint.rds
#
# ---- Summary ----
#   Records logged:  1432
# Last path:       N:/Projects/.../IMG_01234.JPG
# Last update:     2026-02-11 12:34:56
# Estimated progress:  23.1% (1432 / 6200)
#
# ---- Last 5 log entries ----
#   path          status      timestamp ...
# ...
#
# ---- Checkpoint snapshot ----
#   List of 3
# $ last_path: chr "N:/Projects/.../IMG_01234.JPG"
# $ state    : ...
# ...

# # optional: quickly estimate total_files
# total_files <- length(list.files(
#   image_root,
#   pattern = "\\.(jpg|jpeg|png)$",
#   ignore.case = TRUE,
#   recursive = TRUE
# ))
#
# monitor_scan_progress(root = image_root,
#                       total_files = total_files)
#
#
# repeat {
#   monitor_scan_progress(root = image_root)
#   Sys.sleep(10)
# }

