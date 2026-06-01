pkg_dir <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/my-r-packages/kiwiDecoder"
img_dir <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/rename-images/images"

setwd(pkg_dir)
devtools::load_all(quiet = TRUE)

imgs <- list.files(img_dir, pattern = "\\.(jpg|jpeg|png|JPG|JPEG|PNG)$", full.names = TRUE)

cat("\n===== decode_hierarchical_zxing() on ALL images =====\n\n")
for (img in imgs) {
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
