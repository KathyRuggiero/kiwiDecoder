# ── scripts/run_one_hour_demo.R ─────────────────────────────────────────────────
# Full 5-step pipeline demo on "2 Male photos" (~4,400 images; approx. 1 hour).
#
# Run this script interactively from within the kiwiDecoder project directory,
# or via Rscript with the renv library active:
#
#   cd "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/
#        Documents/kbc/projects/other/my-r-packages/kiwiDecoder"
#   Rscript --no-restore --no-save scripts/run_one_hour_demo.R
#
# Pre-requisites
# --------------
# * Set YUGABYTE_PW in .Renviron (Sys.getenv("YUGABYTE_PW") must return the
#   password — never paste it directly in this script).
# * pillow-heif must be installed in the conda environment:
#     reticulate::py_install("pillow-heif", pip = TRUE)   # run once
# ────────────────────────────────────────────────────────────────────────────────

# pkg_dir must be defined first so we can build an absolute path to renv/activate.R,
# which makes the source() call safe regardless of the current working directory.
pkg_dir <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/my-r-packages/kiwiDecoder"
source(file.path(pkg_dir, "renv/activate.R"))
devtools::load_all(pkg_dir, quiet = TRUE)

library(future)
library(furrr)    # required for parallel decode via future_map()
library(DBI)
library(RPostgres)

# Suppress ExifTool's one-time search messages (tells exifr where its bundled
# copy of ExifTool lives so subsequent loads skip the search).
exifr::configure_exiftool(quiet = TRUE)

# "2 Male photos" — ~4,400 images (JPEG + HEIC), barcodes present; fits in ~1 h.
# Switch demo_root to any other top-level subfolder of A Auto-Naming if desired.
demo_root <- "N:/Projects/Stage2 Clonal Trials/Images/A Auto-Naming/2 Male photos"

# ── Step 1: index every subfolder (fast — no image decoding) ──────────────────
cat(format(Sys.time()), "| Step 1: scanning directories...\n")
scan_directories(
  root       = demo_root,
  extensions = c("jpg", "jpeg", "png", "heic"),   # HEIC now supported
  prefixes   = NULL,                               # accept all filenames
  resume     = TRUE
)
cat(format(Sys.time()), "| Step 1 complete.\n\n")

# ── Step 2: decode barcodes in parallel ───────────────────────────────────────
# Worker count strategy:
#   - parallel::detectCores(logical = FALSE)  → physical cores only (conservative)
#   - parallel::detectCores() - 1L            → logical cores minus 1 (more aggressive;
#                                               useful when images are on a network drive
#                                               because I/O waits free up CPU for other workers)
# On this machine (Intel i7-1165G7): 4 physical / 8 logical cores.
# Using logical - 1 = 7 takes advantage of hyperthreading I/O overlap while
# keeping one logical core free for RStudio and Windows.
n_workers <- max(1L, parallel::detectCores(logical = FALSE))   # conservative: physical cores
# n_workers <- max(1L, parallel::detectCores() - 1L)           # aggressive: logical - 1
cat(format(Sys.time()), "| Step 2: decoding barcodes (", n_workers, "parallel workers)...\n")
plan(multisession, workers = n_workers)

con <- tryCatch(
  dbConnect(
    RPostgres::Postgres(),
    host     = "a86e6e8c3e6f74590b0681a0128a39c3-76cf0d338a8ce628.elb.ap-southeast-2.amazonaws.com",
    port     = 5433,
    dbname   = "kup_obs_comp_prod",
    user     = "kup_ro",
    password = Sys.getenv("YUGABYTE_PW"),   # store password in .Renviron — never hardcode
    sslmode  = "disable"
  ),
  error = function(e) { warning("No DB connection: ", e$message); NULL }
)

resolve_folder_sequence(path = demo_root, con = con)
plan(sequential)
cat(format(Sys.time()), "| Step 2 complete.\n\n")

# ── Step 3: biomaterial lookup (if Step 2 ran without a connection) ────────────
# If con was not NULL, the biomaterial lookup was already done inside
# resolve_folder_sequence() in Step 2. Just disconnect.
# If con was NULL (DB unreachable), attempt a fresh connection for a standalone lookup.
if (!is.null(con)) {
  tryCatch(dbDisconnect(con), error = function(e) NULL)   # guard against stale connection
} else {
  con2 <- tryCatch(
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
  if (!is.null(con2)) {
    cat(format(Sys.time()), "| Step 3: enriching with biomaterial names...\n")
    enrich_index_with_biomaterial(path = demo_root, con = con2)
    dbDisconnect(con2)
    cat(format(Sys.time()), "| Step 3 complete.\n\n")
  } else {
    cat(format(Sys.time()), "| Step 3: skipped (no database connection).\n\n")
  }
}

# ── Step 4: EXIF / GPS metadata ───────────────────────────────────────────────
cat(format(Sys.time()), "| Step 4: extracting photo metadata (ExifTool)...\n")
enrich_index_with_metadata(path = demo_root)
cat(format(Sys.time()), "| Step 4 complete.\n\n")

# ── Step 5: build master catalogue ────────────────────────────────────────────
cat(format(Sys.time()), "| Step 5: building catalogue.csv...\n")
cat_out <- build_catalogue(root = demo_root)
cat(format(Sys.time()), "| Done.\n")
cat("catalogue.csv written to:", file.path(demo_root, "catalogue.csv"), "\n")
cat("Total rows in catalogue:", nrow(cat_out), "\n")

# ── Quick summary for boss presentation ───────────────────────────────────────
cat("\n── Summary ─────────────────────────────────────────────────────────────\n")
n_total      <- nrow(cat_out)
# decode_status breakdown (decode_status column added by resolve_folder_sequence)
n_decoded    <- if ("decode_status" %in% names(cat_out))
  sum(cat_out$decode_status == "decoded",    na.rm = TRUE) else NA_integer_
n_propagated <- if ("decode_status" %in% names(cat_out))
  sum(cat_out$decode_status == "propagated", na.rm = TRUE) else NA_integer_
n_undecodable <- if ("decode_status" %in% names(cat_out))
  sum(cat_out$decode_status == "undecodable", na.rm = TRUE) else NA_integer_
n_scion      <- sum(!is.na(cat_out$scion_name),   na.rm = TRUE)
n_gps        <- sum(!is.na(cat_out$GPSLatitude),  na.rm = TRUE)
cat(sprintf("  Total image rows         : %d\n",  n_total))
cat(sprintf("  Barcode decoded directly : %d  (%.0f%%)\n",
            n_decoded,     100 * n_decoded     / max(n_total, 1)))
cat(sprintf("  Identity propagated      : %d  (%.0f%%)\n",
            n_propagated,  100 * n_propagated  / max(n_total, 1)))
cat(sprintf("  Undecodable (no donor)   : %d  (%.0f%%)\n",
            n_undecodable, 100 * n_undecodable / max(n_total, 1)))
cat(sprintf("  Scion name resolved      : %d  (%.0f%%)\n",
            n_scion,       100 * n_scion       / max(n_total, 1)))
cat(sprintf("  Images with GPS          : %d  (%.0f%%)\n",
            n_gps,         100 * n_gps         / max(n_total, 1)))
cat("────────────────────────────────────────────────────────────────────────\n")
