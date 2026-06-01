# ── scripts/run_full_pipeline.R ──────────────────────────────────────────────
# Full 5-step pipeline across the complete A Auto-Naming image collection.
# Designed for overnight / long runs.
#
# BEFORE RUNNING — prevent your laptop sleeping:
#   Go to Settings → System → Power & Sleep
#   Set "When plugged in, PC goes to sleep after" → Never
#   Set it back to your normal value when the run is finished.
#   (Make sure the laptop stays plugged in.)
#
# Run from RStudio, or via Rscript:
#   Rscript --no-restore --no-save scripts/run_full_pipeline.R
#
# Pre-requisites
# --------------
# * YUGABYTE_PW set in .Renviron  (usethis::edit_r_environ())
# * kiwidecoder-py conda environment set up  (setup_kiwidecoder_env())
# * N:/ drive mapped and accessible
# ─────────────────────────────────────────────────────────────────────────────

pkg_dir <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/my-r-packages/kiwiDecoder"
source(file.path(pkg_dir, "renv/activate.R"))
devtools::load_all(pkg_dir, quiet = TRUE)

library(future)
library(furrr)
library(DBI)
library(RPostgres)

exifr::configure_exiftool(quiet = TRUE)

# ── Root directory ────────────────────────────────────────────────────────────
# Change this to the top-level folder you want to process.
# Everything underneath it will be scanned and decoded recursively.
full_root <- "N:/Projects/Stage2 Clonal Trials/Images/A Auto-Naming"

# ── Pre-flight checks ─────────────────────────────────────────────────────────
# Abort immediately with a helpful message rather than failing mid-run.
if (!dir.exists(full_root)) {
  stop(
    "\nCannot access the image root directory:\n  ", full_root,
    "\n\nThe N:/ network drive is probably not mapped.",
    "\nIn File Explorer: right-click 'This PC' → 'Map network drive' → N:",
    "\nThen re-run this script.\n"
  )
}
if (nchar(Sys.getenv("YUGABYTE_PW")) == 0L) {
  warning(
    "YUGABYTE_PW is not set in .Renviron — Steps 2 and 3 will skip DB lookups.",
    "\nRun usethis::edit_r_environ(), add YUGABYTE_PW=<password>, save, and restart R."
  )
}

# ── Log file ──────────────────────────────────────────────────────────────────
# Progress messages are written to both the console and a timestamped log file
# so you can check what happened in the morning without being at the machine.
# The log is stored locally (in the package folder) rather than on N:/ to avoid
# file-connection issues with network drives.
log_path <- file.path(
  pkg_dir,
  paste0("pipeline_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
)
message("Progress log: ", log_path)

.log <- function(...) {
  msg <- paste0(format(Sys.time()), " | ", ...)
  message(msg)                                          # console
  cat(msg, "\n", file = log_path, append = TRUE)       # log file (open/close each write — robust on all drives)
}

# ── Sleep prevention ──────────────────────────────────────────────────────────
# Attempt to disable AC sleep via powercfg. This may silently fail if your
# account does not have permission — in that case, set power settings manually
# before running (see the header comment above).
.log("Disabling PC sleep (powercfg)...")
sleep_ok <- (system("powercfg /change standby-timeout-ac 0", wait = TRUE) == 0L)
if (sleep_ok) {
  .log("  Sleep disabled. Will restore to 30 minutes on exit.")
  on.exit(system("powercfg /change standby-timeout-ac 30", wait = TRUE), add = TRUE)
} else {
  .log("  powercfg returned non-zero — set power settings manually if needed.")
}

# ── Step 1: Scan directories ──────────────────────────────────────────────────
.log("Step 1: scanning directories...")
scan_directories(
  root       = full_root,
  extensions = c("jpg", "jpeg", "png", "heic"),
  prefixes   = NULL,
  resume     = TRUE
)
.log("Step 1 complete.")

# ── Step 2: Decode barcodes in parallel ───────────────────────────────────────
# Network drive: use logical cores - 1 to exploit I/O overlap while keeping
# one core free for Windows and RStudio.
n_workers <- max(1L, parallel::detectCores() - 1L)
.log("Step 2: decoding barcodes (", n_workers, " parallel workers)...")
plan(multisession, workers = n_workers)

con <- tryCatch(
  dbConnect(
    RPostgres::Postgres(),
    host     = "a86e6e8c3e6f74590b0681a0128a39c3-76cf0d338a8ce628.elb.ap-southeast-2.amazonaws.com",
    port     = 5433,
    dbname   = "kup_obs_comp_prod",
    user     = "kup_ro",
    password = Sys.getenv("YUGABYTE_PW"),   # never hardcode — store in .Renviron
    sslmode  = "disable"
  ),
  error = function(e) { warning("No DB connection: ", e$message); NULL }
)

resolve_folder_sequence(path = full_root, con = con)
plan(sequential)
.log("Step 2 complete.")

# ── Step 3: Biomaterial name lookup ───────────────────────────────────────────
# Re-use the connection from Step 2 if it is still open; otherwise reconnect.
if (!is.null(con) && DBI::dbIsValid(con)) {
  .log("Step 3: enriching with biomaterial names...")
  enrich_index_with_biomaterial(path = full_root, con = con)
  tryCatch(dbDisconnect(con), error = function(e) NULL)
  .log("Step 3 complete.")
} else {
  con3 <- tryCatch(
    dbConnect(
      RPostgres::Postgres(),
      host     = "a86e6e8c3e6f74590b0681a0128a39c3-76cf0d338a8ce628.elb.ap-southeast-2.amazonaws.com",
      port     = 5433,
      dbname   = "kup_obs_comp_prod",
      user     = "kup_ro",
      password = Sys.getenv("YUGABYTE_PW"),
      sslmode  = "disable"
    ),
    error = function(e) { warning("No DB connection: ", e$message); NULL }
  )
  if (!is.null(con3)) {
    .log("Step 3: enriching with biomaterial names...")
    enrich_index_with_biomaterial(path = full_root, con = con3)
    dbDisconnect(con3)
    .log("Step 3 complete.")
  } else {
    .log("Step 3: skipped (no database connection).")
  }
}

# ── Step 4: EXIF / GPS / file owner metadata ──────────────────────────────────
.log("Step 4: extracting photo metadata (ExifTool)...")
enrich_index_with_metadata(path = full_root)
.log("Step 4 complete.")

# ── Step 5: Build master catalogue ────────────────────────────────────────────
.log("Step 5: building catalogue.csv...")
cat_out <- build_catalogue(root = full_root)
.log("Done. catalogue.csv written to: ", file.path(full_root, "catalogue.csv"))
.log("Total rows: ", nrow(cat_out))

# ── Summary ───────────────────────────────────────────────────────────────────
n_total       <- nrow(cat_out)
n_decoded     <- if ("decode_status" %in% names(cat_out))
  sum(cat_out$decode_status == "decoded",    na.rm = TRUE) else NA_integer_
n_propagated  <- if ("decode_status" %in% names(cat_out))
  sum(cat_out$decode_status == "propagated", na.rm = TRUE) else NA_integer_
n_undecodable <- if ("decode_status" %in% names(cat_out))
  sum(cat_out$decode_status == "undecodable", na.rm = TRUE) else NA_integer_
n_scion       <- sum(!is.na(cat_out$scion_name),  na.rm = TRUE)
n_gps         <- sum(!is.na(cat_out$GPSLatitude), na.rm = TRUE)

.log("── Summary ─────────────────────────────────────────────────────────────")
.log(sprintf("  Total image rows         : %d",           n_total))
.log(sprintf("  Barcode decoded directly : %d  (%.0f%%)", n_decoded,     100 * n_decoded     / max(n_total, 1)))
.log(sprintf("  Identity propagated      : %d  (%.0f%%)", n_propagated,  100 * n_propagated  / max(n_total, 1)))
.log(sprintf("  Undecodable (no donor)   : %d  (%.0f%%)", n_undecodable, 100 * n_undecodable / max(n_total, 1)))
.log(sprintf("  Scion name resolved      : %d  (%.0f%%)", n_scion,       100 * n_scion       / max(n_total, 1)))
.log(sprintf("  Images with GPS          : %d  (%.0f%%)", n_gps,         100 * n_gps         / max(n_total, 1)))
.log("────────────────────────────────────────────────────────────────────────")
