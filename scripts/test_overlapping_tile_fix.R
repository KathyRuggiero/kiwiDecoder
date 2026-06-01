pkg_dir <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/my-r-packages/kiwiDecoder"
img_dir <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/rename-images/images"

setwd(pkg_dir)
devtools::load_all(quiet = TRUE)

targets <- c(
  file.path(img_dir, "IMG_2570.JPG"),
  file.path(img_dir, "IMG_2774.JPG")
)

cat("\n===== BEFORE (detect_codes_all_zxing - single full-image pass) =====\n\n")
for (img in targets) {
  cat("Image:", basename(img), "\n")
  r <- tryCatch(detect_codes_all_zxing(img), error = function(e) {
    cat("  ERROR:", conditionMessage(e), "\n"); NULL
  })
  if (!is.null(r)) {
    cat("  code_format:", paste(r$code_format, collapse = " | "), "\n")
    cat("  text:       ", paste(r$text,        collapse = " | "), "\n")
  }
  cat("\n")
}

cat("\n===== AFTER (decode_hierarchical_zxing - overlapping tile fix) =====\n\n")
for (img in targets) {
  cat("Image:", basename(img), "\n")
  r <- tryCatch(decode_hierarchical_zxing(img), error = function(e) {
    cat("  ERROR:", conditionMessage(e), "\n"); NULL
  })
  if (!is.null(r)) {
    cat("  code_format:", paste(r$code_format, collapse = " | "), "\n")
    cat("  text:       ", paste(r$text,        collapse = " | "), "\n")
    cat("  source:     ", paste(r$source,      collapse = " | "), "\n")
  }
  cat("\n")
}
