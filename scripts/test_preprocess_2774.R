pkg_dir <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/my-r-packages/kiwiDecoder"
img_dir <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/rename-images/images"
img_path <- file.path(img_dir, "IMG_2774.JPG")

setwd(pkg_dir)
devtools::load_all(quiet = TRUE)

# Try preprocessing with PIL (grayscale + autocontrast + sharpen) then re-decode
py_code <- sprintf(r"(
from PIL import Image, ImageOps, ImageFilter
import zxingcpp, os, tempfile

path = r"%s"
img_orig  = Image.open(path)

results = []

# Variant 1: grayscale only
img_gray = img_orig.convert("L")
r1 = zxingcpp.read_barcodes(img_gray, try_rotate=True, try_downscale=True, return_errors=True)
for r in r1:
    results.append({"variant": "grayscale", "text": r.text, "format": str(r.format), "error": str(r.error), "valid": r.valid})

# Variant 2: grayscale + autocontrast
img_ac = ImageOps.autocontrast(img_gray)
r2 = zxingcpp.read_barcodes(img_ac, try_rotate=True, try_downscale=True, return_errors=True)
for r in r2:
    results.append({"variant": "autocontrast", "text": r.text, "format": str(r.format), "error": str(r.error), "valid": r.valid})

# Variant 3: grayscale + autocontrast + sharpen
img_sh = img_ac.filter(ImageFilter.SHARPEN)
r3 = zxingcpp.read_barcodes(img_sh, try_rotate=True, try_downscale=True, return_errors=True)
for r in r3:
    results.append({"variant": "autocontrast+sharpen", "text": r.text, "format": str(r.format), "error": str(r.error), "valid": r.valid})

# Variant 4: grayscale + autocontrast + 2x upscale
w, h = img_ac.size
img_up = img_ac.resize((w*2, h*2), Image.LANCZOS)
r4 = zxingcpp.read_barcodes(img_up, try_rotate=True, try_downscale=True, return_errors=True)
for r in r4:
    results.append({"variant": "autocontrast+2xupscale", "text": r.text, "format": str(r.format), "error": str(r.error), "valid": r.valid})

results
)", normalizePath(img_path, winslash = "/"))

cat("Testing preprocessing variants on IMG_2774.JPG ...\n\n")
reticulate::py_run_string(py_code)
results <- reticulate::py$results

if (length(results) == 0) {
  cat("No candidates found in any variant.\n")
} else {
  for (r in results) {
    cat(sprintf("Variant: %-28s  format: %-20s  valid: %-5s  text: %s  error: %s\n",
                r$variant, r$format, r$valid, r$text, r$error))
  }
}
