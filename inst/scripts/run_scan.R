#!/usr/bin/env Rscript

library(callr)
library(kiwiDecoder)

image_root <- "N:/path/to/images"

bg_job <- r_bg(
  func = function(root) {
    library(kiwiDecoder)
    scan_directories(
      root            = root,
      extensions      = c("jpg", "jpeg", "png"),
      prefixes        = c("^PMC_[0-9]{5}$", "^DSC_[0-9]{4}$",
                          "^IMG_[0-9]{4}$", "^P[0-9]{7}$"),
      checkpoint_file = "scan_checkpoint.rds",
      resume          = TRUE,
      dry_run         = FALSE,
      log_file        = file.path(root, "scan_log.csv"),
      max_path_length = 255
    )
  },
  args = list(root = image_root)
)

# print the PID or status
print(bg_job)


# job <- run_scan_bg()       # starts background scan
#
# job$is_alive()             # check later
# job$get_result()           # only AFTER it's finished
