pkg_dir  <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/my-r-packages/kiwiDecoder"
test_dir <- "N:/Projects/Stage2 Clonal Trials/Images/A Auto-Naming/test_folder"
test_csv <- file.path(test_dir, "pipeline_test.csv")

setwd(pkg_dir)
devtools::load_all(quiet = TRUE)

# â”€â”€ 1. Count image files â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
imgs <- list.files(test_dir,
                   pattern    = "\\.(jpg|jpeg|png)$",
                   ignore.case = TRUE)
cat("Images in test_folder:", length(imgs), "\n\n")

# â”€â”€ Step 1: write_directory_index â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
cat("=== Step 1: write_directory_index ===\n")
if (file.exists(test_csv)) file.remove(test_csv)   # fresh run each time
t1 <- system.time(
  write_directory_index(
    dir        = test_dir,
    extensions = c("jpg", "jpeg", "png"),
    prefixes   = NULL,
    csv_path   = test_csv
  )
)
cat(sprintf("done in %.1f sec\n\n", t1["elapsed"]))

# â”€â”€ Step 2: resolve_index_csv (decode + propagate) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
cat("=== Step 2: resolve_index_csv ===\n")
t2 <- system.time(
  result <- resolve_index_csv(test_csv)
)
cat(sprintf("done in %.1f sec\n\n", t2["elapsed"]))

# â”€â”€ Step 3: enrich with biomaterial identity â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
cat("=== Step 3: enrich_one_index_with_biomaterial ===\n")
library(DBI)
library(RPostgres)

con <- tryCatch(
  dbConnect(
    RPostgres::Postgres(),
    host     = "a86e6e8c3e6f74590b0681a0128a39c3-76cf0d338a8ce628.elb.ap-southeast-2.amazonaws.com",
    port     = 5433,
    dbname   = "kup_obs_comp_prod",
    user     = "kup_ro",
    password = Sys.getenv("KUP_DB_PASSWORD"),   # store in .Renviron â€” never hardcode
    sslmode  = "disable"
  ),
  error = function(e) {
    cat("  Could not connect to KiwiCloud:", conditionMessage(e), "\n")
    NULL
  }
)

if (!is.null(con)) {
  t3 <- system.time(
    result <- enrich_one_index_with_biomaterial(test_csv, con = con)
  )
  dbDisconnect(con)
  cat(sprintf("done in %.1f sec\n\n", t3["elapsed"]))
} else {
  t3 <- c(elapsed = 0)
  cat("  Skipped (no database connection)\n\n")
}

# â”€â”€ Step 4: enrich with EXIF metadata â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
cat("=== Step 4: enrich_one_index_with_metadata ===\n")
t4 <- system.time(
  result <- enrich_one_index_with_metadata(test_csv)
)
cat(sprintf("done in %.1f sec\n\n", t4["elapsed"]))

# â”€â”€ Results summary â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
cat("=== Final output ===\n")
cat("Rows:", nrow(result), "  Columns:", ncol(result), "\n")
cat("Column names:\n  ", paste(names(result), collapse = "\n   "), "\n\n")

# Barcode results
if ("text" %in% names(result)) {
  decoded <- result[!is.na(result$text) & result$text != "unknown", ]
  cat("Decoded codes (unique):\n")
  print(unique(decoded[, intersect(c("text", "type", "code_format"), names(result))]))
  cat("\n")
}

# Biomaterial columns
bio_cols <- intersect(c("scion_name", "genotype", "location_name",
                        "location_address", "site_name"), names(result))
if (length(bio_cols) > 0) {
  cat("Biomaterial lookup columns present:", paste(bio_cols, collapse = ", "), "\n")
  cat("scion_name values:", paste(unique(result$scion_name), collapse = ", "), "\n\n")
}

# Metadata columns
meta_cols <- intersect(c("file_owner", "camera_make", "camera_model",
                         "DateTimeOriginal", "GPSLatitude"), names(result))
if (length(meta_cols) > 0) {
  cat("Metadata columns present:", paste(meta_cols, collapse = ", "), "\n")
  cat("file_owner values:", paste(unique(result$file_owner), collapse = ", "), "\n")
  cat("camera_make values:", paste(unique(result$camera_make), collapse = ", "), "\n")
  n_gps <- sum(!is.na(result$GPSLatitude))
  cat("Images with GPS:", n_gps, "/", nrow(result), "\n\n")
}

# Timing summary
total <- t1["elapsed"] + t2["elapsed"] + t3["elapsed"] + t4["elapsed"]
cat("=== Timing ===\n")
cat(sprintf("  Step 1  write_directory_index:          %5.1f sec\n", t1["elapsed"]))
cat(sprintf("  Step 2  resolve_index_csv (decode):     %5.1f sec\n", t2["elapsed"]))
cat(sprintf("  Step 3  enrich_one_index_with_biomaterial: %5.1f sec\n", t3["elapsed"]))
cat(sprintf("  Step 4  enrich_one_index_with_metadata: %5.1f sec\n", t4["elapsed"]))
cat(sprintf("  TOTAL                                   %5.1f sec  (%d images, %.1f sec/image)\n",
            total, length(imgs), total / length(imgs)))
