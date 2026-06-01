pkg_dir <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/my-r-packages/kiwiDecoder"
img_dir <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/rename-images/images"
setwd(pkg_dir)
devtools::load_all(quiet = TRUE)

# Pick one image to inspect all available EXIF fields
img <- file.path(img_dir, "IMG_0127 WGT M2126 JMA 2024.jpg")
meta <- exifr::read_exif(img)

# Show all non-NA fields
cat("Available EXIF fields (non-NA):\n\n")
for (col in names(meta)) {
  val <- meta[[col]][1]
  if (!is.na(val) && !is.null(val) && nzchar(as.character(val))) {
    cat(sprintf("  %-35s : %s\n", col, as.character(val)))
  }
}

# Specifically check photographer-related fields
cat("\n\nPhotographer-related fields:\n")
photo_fields <- c("Artist", "CameraOwnerName", "AuthorNames", "Creator",
                  "Make", "Model", "Software", "Copyright",
                  "OwnerName", "SerialNumber", "LensInfo")
for (f in photo_fields) {
  val <- if (f %in% names(meta)) as.character(meta[[f]][1]) else "(not in EXIF)"
  cat(sprintf("  %-30s : %s\n", f, val))
}
