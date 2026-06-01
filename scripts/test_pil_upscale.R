pkg_dir <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/my-r-packages/kiwiDecoder"
img_dir <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/rename-images/images"
img_path <- normalizePath(file.path(img_dir, "IMG_2774.JPG"), winslash = "/")

setwd(pkg_dir)
devtools::load_all(quiet = TRUE)

Image    <- reticulate::import("PIL.Image",   convert = FALSE)
ImageOps <- reticulate::import("PIL.ImageOps", convert = FALSE)

tmp_up <- normalizePath(tempfile(fileext = ".png"), winslash = "/")

py_code <- sprintf(r"(
from PIL import Image, ImageOps

img  = Image.open(r"%s")
gray = img.convert("L")
ac   = ImageOps.autocontrast(gray)
w, h = img.size
up   = ac.resize((w * 2, h * 2), Image.LANCZOS)
up.save(r"%s")
print("Saved upscaled image:", r"%s")
)", img_path, tmp_up, tmp_up)

cat("Running PIL upscale + save...\n")
reticulate::py_run_string(py_code)
cat("Temp file exists:", file.exists(tmp_up), "\n\n")

cat("Running detect_codes_all_zxing on PIL-upscaled image:\n")
r <- tryCatch(detect_codes_all_zxing(tmp_up), error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })
if (!is.null(r)) {
  cat("  code_format:", paste(r$code_format, collapse=" | "), "\n")
  cat("  text:       ", paste(r$text,        collapse=" | "), "\n")
}
file.remove(tmp_up)
