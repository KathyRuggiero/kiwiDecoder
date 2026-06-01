pkg_dir <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/my-r-packages/kiwiDecoder"
img_dir <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/rename-images/images"
setwd(pkg_dir)
devtools::load_all(quiet = TRUE)

# Pick the largest file (most likely a real camera photo)
imgs  <- list.files(img_dir, full.names = TRUE, pattern = "\\.(JPG|jpg)$")
sizes <- file.size(imgs)
img   <- imgs[which.max(sizes)]
cat("Checking:", basename(img), "  size:", format(sizes[which.max(sizes)], big.mark=","), "bytes\n\n")

meta <- exifr::read_exif(img)

photo_fields <- c("Artist", "CameraOwnerName", "AuthorNames", "Creator",
                  "Make", "Model", "Software", "Copyright", "OwnerName",
                  "SerialNumber", "LensInfo", "DateTimeOriginal", "CreateDate",
                  "GPSLatitude", "GPSLongitude", "ImageWidth", "ImageHeight",
                  "Megapixels", "Flash", "ExposureTime", "FNumber", "ISO")
cat("Fields:\n")
for (f in photo_fields) {
  val <- if (f %in% names(meta)) as.character(meta[[f]][1]) else "(not present)"
  cat(sprintf("  %-30s : %s\n", f, val))
}
