# --- scan_bg.R ---
#' Launch scan_directories() in a detached background R session
#'
#' @param root Character. Root folder to scan.
#' @param extensions Character vector of allowed file extensions.
#' @param prefixes Character vector of filename regex patterns.
#' @param checkpoint_file Path to RDS checkpoint file.
#' @param log_file Path to CSV log file.
#' @param max_path_length Maximum allowed path length.
#' @return A callr background job object.
#' @importFrom callr r_bg
#' @export
launch_scan_bg <- function(
    root,
    extensions      = c("jpg","jpeg","png"),
    prefixes        = NULL,
    checkpoint_file = NULL,
    log_file        = NULL,
    max_path_length = 255
) {
  if (is.null(checkpoint_file)) checkpoint_file <- file.path(root, "scan_checkpoint.rds")
  if (is.null(log_file)) log_file <- file.path(root, "scan_log.csv")

  job <- callr::r_bg(
    func = function(root, extensions, prefixes, checkpoint_file, log_file, max_path_length) {
      # Activate renv in background session
      source("renv/activate.R")

      # Source DEV versions of your functions
      source("scripts/scan_directories_dev.R")
      source("scripts/write_directory_index_dev.R")

      # Run the scan
      scan_directories(
        root            = root,
        extensions      = extensions,
        prefixes        = prefixes,
        checkpoint_file = checkpoint_file,
        resume          = TRUE,
        log_file        = log_file,
        max_path_length = max_path_length
      )
    },
    args = list(
      root            = root,
      extensions      = extensions,
      prefixes        = prefixes,
      checkpoint_file = checkpoint_file,
      log_file        = log_file,
      max_path_length = max_path_length
    ),
    supervise = TRUE
  )

  message("Scan started in background. Use job object to monitor progress.")
  return(job)
}

# --- Helper functions to monitor job ---
# Check if job is still running
is_scan_alive <- function(job) job$is_alive()

# Read stdout / stderr logs from job
scan_output <- function(job) job$read_output()
scan_error  <- function(job) job$read_error()

# Stop job safely
stop_scan <- function(job) {
  if (job$is_alive()) job$kill()
  message("Background scan job terminated.")
}
