#' Scan Directories for Image Files and Write Per-Directory Index
#'
#' `scan_directories()` performs a **depth-first scan** of a directory tree,
#' optionally resuming from a checkpoint, and writes per-directory Excel indexes
#' of image files using `write_directory_index()`. It is designed for **network-mounted drives**
#' where naive recursive scans can be very slow or unstable.
#'
#' Features:
#' * Depth-first traversal: completes one full subdirectory tree before moving to the next.
#' * Resumable scans via checkpointing (`checkpoint_file`).
#' * Dry-run mode: scan and report without writing Excel files.
#' * Logging of scan progress instead of printing to console.
#' * Detection and logging of paths that are too long for Excel.
#'
#' @param root Character scalar. Root directory for scanning.
#' @param extensions Character vector of allowed image file extensions (case-insensitive).
#'   Defaults to \code{c("jpg", "jpeg", "png", "heic")}. HEIC support requires the
#'   \code{pillow-heif} Python package installed in the active conda environment
#'   (run \code{reticulate::py_install("pillow-heif", pip = TRUE)} once to add it).
#' @param prefixes Character vector of regex patterns matched against filename stems (without extensions). Use `NULL` or `character(0)` to disable stem filtering.
#' @param checkpoint_file File path to RDS checkpoint. Default `"scan_checkpoint.rds"`.
#' @param resume Logical; if `TRUE`, resumes from the checkpoint file.
#' @param dry_run Logical; if `TRUE`, no Excel files are written, but scanning proceeds and logs are updated.
#' @param log_file Optional path to a CSV log file recording scan progress and long-path warnings. Default is `"scan_log.csv"` in `root`.
#' @param max_path_length Maximum allowed path + filename length. Defaults to 255.
#'
#' @return Invisibly returns a list containing:
#' * `dirs_scanned` – character vector of directories fully scanned.
#' * `long_path_warnings` – data frame of directories where CSV paths exceeded `max_path_length`.
#'
#' @param checkpoint_interval Integer. Save the RDS checkpoint every this many
#'   directories (and always at the end). Higher values reduce network I/O on
#'   slow drives. Default `10`.
#'
#' @export
scan_directories <- function(
    root,
    extensions          = c("jpg", "jpeg", "png", "heic"),
    prefixes            = c("^PMC_[0-9]{5}$", "^DSC_[0-9]{4}$",
                            "^IMG_[0-9]{4}$", "^P[0-9]{7}$"),
    checkpoint_file     = "scan_checkpoint.rds",
    resume              = TRUE,
    dry_run             = FALSE,
    log_file            = file.path(root, "scan_log.csv"),
    max_path_length     = 255,
    checkpoint_interval = 10L
) {
  if (!dir.exists(root)) stop("Root directory does not exist: ", root)
  
  # Initialize checkpoint / resume state
  checkpoint <- list(dirs_scanned = character(0))
  if (resume && file.exists(checkpoint_file)) {
    checkpoint <- readRDS(checkpoint_file)
  }
  dirs_scanned <- checkpoint$dirs_scanned
  dirs_to_scan <- setdiff(normalizePath(list.dirs(root, recursive = TRUE), winslash = "/"),
                          dirs_scanned)
  
  long_path_warnings <- data.frame(
    dir      = character(0),
    subdir   = character(0),
    csv_path = character(0),
    n_chars  = numeric(0),
    stringsAsFactors = FALSE
  )
  
  # Progress bar
  pb <- utils::txtProgressBar(min = 0, max = length(dirs_to_scan), style = 3)
  
  for (i in seq_along(dirs_to_scan)) {
    current_dir <- dirs_to_scan[i]
    subdir <- basename(current_dir)
    
    # Determine CSV path
    csv_path <- file.path(current_dir, paste0(subdir, ".csv"))
    n_chars  <- nchar(csv_path)

    if (n_chars > max_path_length) {
      warning_row <- data.frame(
        dir      = current_dir,
        subdir   = subdir,
        csv_path = csv_path,
        n_chars  = n_chars,
        stringsAsFactors = FALSE
      )
      long_path_warnings <- rbind(long_path_warnings, warning_row)
      utils::write.table(
        warning_row,
        file = log_file,
        sep = ",", row.names = FALSE,
        col.names = !file.exists(log_file), append = TRUE
      )
      next
    }

    # Write directory index unless dry_run
    if (!dry_run) {
      tryCatch({
        write_directory_index(
          dir        = current_dir,
          extensions = extensions,
          prefixes   = prefixes,
          csv_path   = csv_path
        )
      }, error = function(e) {
        warning("Failed to write CSV index for ", current_dir, ": ", e$message)
      })
    }

    # Update checkpoint every checkpoint_interval directories (and at end)
    dirs_scanned <- c(dirs_scanned, current_dir)
    if (i %% checkpoint_interval == 0L || i == length(dirs_to_scan)) {
      saveRDS(list(dirs_scanned = dirs_scanned), checkpoint_file)
    }
    
    # Update progress bar
    utils::setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  invisible(list(
    dirs_scanned       = dirs_scanned,
    long_path_warnings = long_path_warnings
  ))
}


