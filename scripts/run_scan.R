# Activate renv in the main session
source("renv/activate.R")
library(callr)

image_root <- "//kbc-file.pfr.co.nz/files/Projects/Stage2 Clonal Trials/Images/A Auto-Naming"

bg_scan <- r_bg(
  func = function(root) {
    tryCatch({
      # --- Activate renv in background ---
      source("renv/activate.R")

      # --- Source your DEV functions ---
      source("scripts/scan_directories_dev.R")
      source("scripts/write_directory_index_dev.R")

      # --- Paths for logs and checkpoint ---
      log_file       <- file.path(root, "scan_log.csv")
      checkpoint_file <- file.path(root, "scan_checkpoint.rds")
      heartbeat_file  <- file.path(root, "scan_heartbeat.csv")

      # --- Start scan ---
      scan_directories(
        root = root,
        extensions        = c("jpg", "jpeg", "png"),
        prefixes          = c("^PMC_[0-9]{5}$", "^DSC_[0-9]{4}$",
                              "^IMG_[0-9]{4}$", "^P[0-9]{7}$"),
        checkpoint_file = checkpoint_file,
        resume          = TRUE,
        log_file        = log_file,
        heartbeat_file  = heartbeat_file,
        max_path_length = 255,
        heartbeat_every = 10,
        save_every_n_dirs = 20
      )
    }, error = function(e) {
      # Write any errors to bg_error.txt
      writeLines(paste(Sys.time(), "ERROR:", e$message),
                 con = file.path(root, "bg_error.txt"))
    })
  },
  args = list(root = image_root),
  supervise = TRUE  # <<< makes it detached from your main R session
)
