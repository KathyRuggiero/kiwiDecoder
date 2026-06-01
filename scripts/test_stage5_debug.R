pkg_dir <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/my-r-packages/kiwiDecoder"
img_dir <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/rename-images/images"
img_path <- file.path(img_dir, "IMG_2774.JPG")

setwd(pkg_dir)
devtools::load_all(quiet = TRUE)

# Manually test what Stage 5 does in decode_hierarchical_zxing
cat("Testing magick-based Stage 5 preprocessing...\n\n")

img <- magick::image_read(img_path)
info <- magick::image_info(img)
w <- info$width
h <- info$height
cat("Original dimensions:", w, "x", h, "\n")

img_up <- img |>
  magick::image_convert(colorspace = "Gray") |>
  magick::image_normalize() |>
  magick::image_scale("200%")

tmp_up <- tempfile(fileext = ".png")
magick::image_write(img_up, tmp_up)
cat("Upscaled image written to:", tmp_up, "\n")
cat("Upscaled dimensions:", magick::image_info(img_up)$width, "x", magick::image_info(img_up)$height, "\n\n")

cat("Running detect_codes_all_zxing on upscaled/preprocessed image:\n")
r <- tryCatch(detect_codes_all_zxing(tmp_up), error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })
if (!is.null(r)) {
  cat("  code_format:", paste(r$code_format, collapse=" | "), "\n")
  cat("  text:       ", paste(r$text,        collapse=" | "), "\n")
}
file.remove(tmp_up)

cat("\n\nNow testing via decode_hierarchical_zxing (includes Stage 5):\n")
cat("(This should reach Stage 5 since Stages 1-4 fail)\n\n")
r2 <- tryCatch(decode_hierarchical_zxing(img_path), error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })
if (!is.null(r2)) {
  cat("  code_format:", paste(r2$code_format, collapse=" | "), "\n")
  cat("  text:       ", paste(r2$text,        collapse=" | "), "\n")
  cat("  source:     ", paste(r2$source,      collapse=" | "), "\n")
}
