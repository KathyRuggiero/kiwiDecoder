# ── scripts/launch_pipeline.R ────────────────────────────────────────────────
# Launches run_full_pipeline.R as an RStudio background job so that your
# R console stays free while the pipeline runs overnight.
#
# HOW TO USE
# ----------
# 1. Make sure the N:/ drive is mapped:
#      file.exists("N:/Projects/Stage2 Clonal Trials/Images/A Auto-Naming")
#      # must return TRUE before continuing
#
# 2. Source this file (not run_full_pipeline.R directly):
#      source("scripts/launch_pipeline.R")
#
# 3. Watch progress in the RStudio Background Jobs pane (bottom-left tab).
#    The pipeline also writes a timestamped log file to the package folder —
#    look for pipeline_log_YYYYMMDD_HHMMSS.txt there.
#
# 4. To resume after an interruption, just source this file again.
#    Step 1 (scan) resumes from its checkpoint; Steps 2-5 skip already-
#    processed files automatically.
# ─────────────────────────────────────────────────────────────────────────────

pkg_dir <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/my-r-packages/kiwiDecoder"

# Pre-flight: check N:/ is accessible before bothering to launch
full_root <- "N:/Projects/Stage2 Clonal Trials/Images/A Auto-Naming"
if (!dir.exists(full_root)) {
  stop(
    "\nCannot access the image root directory:\n  ", full_root,
    "\n\nThe N:/ network drive is probably not mapped.",
    "\nIn File Explorer: right-click 'This PC' → 'Map network drive' → N:",
    "\nThen re-run this script.\n"
  )
}

message("Launching pipeline as background job...")
rstudioapi::jobRunScript(
  path       = file.path(pkg_dir, "scripts", "run_full_pipeline.R"),
  name       = "kiwiDecoder full pipeline",
  workingDir = pkg_dir
)
message("Background job started. Check the 'Background Jobs' tab in RStudio.")
message("Log file will appear in: ", pkg_dir)
