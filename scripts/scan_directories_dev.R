#' Scan Directories for Image Files and Write Per-Directory CSV Index
#'
#' Depth-first scan of a directory tree with optional checkpointing,
#' logging, heartbeat messages, and per-directory timing.
#'
#' @param root Character scalar. Root directory for scanning.
#' @param extensions Character vector of allowed image file extensions. Default c("jpg","jpeg","png").
#' @param prefixes Character vector of regex patterns for filename stems. NULL disables filtering.
#' @param checkpoint_file File path to RDS checkpoint. Default: ".scan_checkpoint.rds" in root.
#' @param resume Logical; if TRUE, resumes from checkpoint.
#' @param log_file Path to CSV log file. Default: "scan_log.csv" in root.
#' @param max_path_length Maximum allowed path + filename length. Default 255.
#' @param heartbeat_every Number of seconds between heartbeat messages. Default 10.
#'
#' @return Invisibly: list with dirs_scanned and long_path_warnings.
#' @importFrom fs dir_ls path_file path_ext file_info
#' @importFrom utils write.csv
#' @importFrom stringr str_detect
#' @export

safe_norm <- function(x) {
  out <- tryCatch(
    normalizePath(x, winslash = "/", mustWork = FALSE),
    error = function(e) NA_character_
  )
  if (is.na(out) || out == "") {
    # Fall back to user-supplied path
    return(gsub("\\\\", "/", x))
  }
  out
}

# --------------------------------------------------------------------
# Helper: %||% (safe default operator)
# --------------------------------------------------------------------
`%||%` <- function(x, y) if (!is.null(x)) x else y

# --------------------------------------------------------------------
# Streaming, resumable scan_directories()
# --------------------------------------------------------------------
scan_directories <- function(
    root,
    extensions        = c("jpg", "jpeg", "png"),
    prefixes          = c("^PMC_[0-9]{5}$", "^DSC_[0-9]{4}$",
                          "^IMG_[0-9]{4}$", "^P[0-9]{7}$"),
    checkpoint_file   = NULL,
    resume            = TRUE,
    log_file          = NULL,
    heartbeat_file    = NULL,
    max_path_length   = 255,
    heartbeat_every   = 10,
    save_every_n_dirs = 20
) {
  if (!dir.exists(root)) stop("Root directory does not exist: ", root)

  # --- Resolve paths / defaults -----------------------------------------------
  root            <- normalizePath(root, winslash = "/", mustWork = TRUE)
  checkpoint_file <- checkpoint_file %||% file.path(root, ".scan_checkpoint.rds")
  log_file        <- log_file        %||% file.path(root, "scan_log.csv")
  heartbeat_file  <- heartbeat_file  %||% file.path(root, "scan_heartbeat.csv")

  dir.create(dirname(log_file),       showWarnings = FALSE, recursive = TRUE)
  dir.create(dirname(heartbeat_file), showWarnings = FALSE, recursive = TRUE)

  # --- Initialise heartbeat file (header only, if not present) ----------------
  if (!file.exists(heartbeat_file)) {
    utils::write.csv(
      data.frame(
        timestamp = character(0),
        folder    = character(0),
        n_files   = integer(0),
        status    = character(0),
        stringsAsFactors = FALSE
      ),
      heartbeat_file,
      row.names = FALSE
    )
  }

  # --- Load checkpoint if resuming --------------------------------------------
  checkpoint <- if (resume && file.exists(checkpoint_file)) {
    readRDS(checkpoint_file)
  } else {
    list(
      dirs_scanned      = character(0),
      accumulated_index = data.frame()
    )
  }

  dirs_scanned <- if (length(checkpoint$dirs_scanned)) {
    normalizePath(checkpoint$dirs_scanned, winslash = "/", mustWork = FALSE)
  } else {
    character(0)
  }

  scanned_set <- setNames(rep(TRUE, length(dirs_scanned)), dirs_scanned)

  accumulated_index <- checkpoint$accumulated_index
  if (!is.data.frame(accumulated_index)) {
    accumulated_index <- data.frame()
  }

  long_path_warnings <- data.frame(
    dir    = character(0),
    n_chars = numeric(0),
    stringsAsFactors = FALSE
  )

  last_heartbeat        <- Sys.time()
  folders_since_last_save <- 0L

  # --- Logging helper ---------------------------------------------------------
  log_msg <- function(...) {
    msg  <- paste(..., collapse = " ")
    line <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg)
    cat(line, "\n", file = log_file, append = TRUE)
  }

  # --- Heartbeat row helper ---------------------------------------------------
  heartbeat <- function(folder, n_files, status) {
    entry <- data.frame(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      folder    = folder,
      n_files   = n_files,
      status    = status,
      stringsAsFactors = FALSE
    )

    utils::write.table(
      entry,
      heartbeat_file,
      sep       = ",",
      col.names = !file.exists(heartbeat_file),  # header if file missing
      row.names = FALSE,
      append    = TRUE
    )
  }

  # --- Inner: process_dir -----------------------------------------------------
  process_dir <- function(current_dir) {
    current_dir_norm <- safe_norm(current_dir)

    # Guard against NA / empty
    if (is.null(current_dir_norm) || is.na(current_dir_norm) || !nzchar(current_dir_norm)) {
      return(FALSE)
    }

    # Skip if already scanned (for resume-from-crash)
    if (length(scanned_set) > 0 &&
        current_dir_norm %in% names(scanned_set) &&
        isTRUE(scanned_set[current_dir_norm])) {
      return(FALSE)
    }

    # Path for per-directory CSV index: <subdir>_index.csv
    subdir   <- basename(current_dir_norm)
    csv_path <- file.path(current_dir_norm, paste0(subdir, "_index.csv"))
    n_chars  <- nchar(csv_path)

    # Long-path guard
    if (n_chars > max_path_length) {
      long_path_warnings <<- rbind(
        long_path_warnings,
        data.frame(dir = current_dir_norm, n_chars = n_chars, stringsAsFactors = FALSE)
      )
      log_msg("SKIPPED long path:", current_dir_norm)
      heartbeat(current_dir_norm, 0L, "skipped_long_path")
      return(FALSE)
    }

    # Use your write_directory_index() to generate per-folder CSV and index df
    index <- tryCatch(
      write_directory_index(
        dir        = current_dir_norm,
        extensions = extensions,
        prefixes   = prefixes,
        csv_path   = csv_path,
        log_file   = log_file
      ),
      error = function(e) {
        log_msg("ERROR indexing", current_dir_norm, ":", e$message)
        heartbeat(current_dir_norm, 0L, "error")
        return(data.frame())
      }
    )

    n_files <- if (is.data.frame(index)) nrow(index) else 0L
    status  <- if (n_files > 0L) "indexed" else "no_files"

    # Accumulate into in-memory index for later use (optional)
    if (n_files > 0L) {
      if (!is.data.frame(accumulated_index) || nrow(accumulated_index) == 0L) {
        accumulated_index <<- index
      } else {
        accumulated_index <<- rbind(accumulated_index, index)
      }
      log_msg("Indexed", n_files, "files in", current_dir_norm)
    } else {
      log_msg("No matching files in", current_dir_norm)
    }

    # Heartbeat row
    heartbeat(current_dir_norm, n_files, status)

    # Update checkpoint state
    dirs_scanned                     <<- c(dirs_scanned, current_dir_norm)
    scanned_set[current_dir_norm]    <<- TRUE   # NOTE: [] not [[ ]]
    folders_since_last_save          <<- folders_since_last_save + 1L

    # Console heartbeat (if interactive)
    now <- Sys.time()
    if (difftime(now, last_heartbeat, units = "secs") > heartbeat_every) {
      cat(format(now, "%H:%M:%S"), "- scanning:", current_dir_norm, "\n")
      flush.console()
      last_heartbeat <<- now
    }

    # Periodic checkpoint save
    if (folders_since_last_save >= save_every_n_dirs) {
      saveRDS(
        list(
          dirs_scanned      = dirs_scanned,
          accumulated_index = accumulated_index
        ),
        checkpoint_file
      )
      log_msg("Checkpoint saved after", folders_since_last_save, "folders")
      folders_since_last_save <<- 0L
    }

    TRUE
  }

  # ---------------------------------------------------------------------------
  # STREAMING DIRECTORY SCAN (Windows) or fallback (non-Windows)
  # ---------------------------------------------------------------------------
  if (.Platform$OS.type == "windows") {

    # 1. Process root explicitly
    process_dir(root)

    # 2. Stream subdirectories from Windows 'dir'
    cmd <- sprintf('cmd.exe /c dir "%s" /s /b /ad', root)
    con <- pipe(cmd, open = "rt")
    on.exit(close(con), add = TRUE)

    repeat {
      dirs_chunk <- readLines(con, n = 200L)
      if (length(dirs_chunk) == 0) break

      for (d in dirs_chunk) {
        d_norm <- safe_norm(d)
        process_dir(d_norm)
      }
    }

  } else {
    # Non-Windows fallback: original list.dirs-based scan
    all_dirs <- normalizePath(
      list.dirs(root, recursive = TRUE),
      winslash   = "/",
      mustWork   = FALSE
    )
    for (d in c(all_dirs, root)) {
      process_dir(d)
    }
  }

  # Final checkpoint save
  saveRDS(
    list(
      dirs_scanned      = dirs_scanned,
      accumulated_index = accumulated_index
    ),
    checkpoint_file
  )
  log_msg("Final checkpoint saved")

  invisible(list(
    dirs_scanned       = dirs_scanned,
    long_path_warnings = long_path_warnings,
    accumulated_index  = accumulated_index
  ))
}
