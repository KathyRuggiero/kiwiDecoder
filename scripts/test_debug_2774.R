pkg_dir <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/my-r-packages/kiwiDecoder"
img_dir <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/rename-images/images"

setwd(pkg_dir)
devtools::load_all(quiet = TRUE)

cat("\n===== debug_zxing_read on IMG_2774 (still failing) =====\n\n")
debug_zxing_read(file.path(img_dir, "IMG_2774.JPG"))

cat("\n\n===== debug_zxing_read on IMG_2573 (works - same label type?) =====\n\n")
debug_zxing_read(file.path(img_dir, "IMG_2573.JPG"))
