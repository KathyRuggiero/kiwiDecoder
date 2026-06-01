#' Debug ZXing Barcode Detection for a Single Image
#'
#' `debug_zxing_read()` is an interactive diagnostic helper that reveals **how
#' ZXing-CPP interpreted an image**, including partially detected and
#' undecodable symbols. This function is intended for troubleshooting cases
#' where barcode decoding fails or produces unexpected results.
#'
#' Unlike [detect_codes_all_zxing()], which filters results into a standardised
#' tibble, this diagnostic helper prints **raw per-symbol details** directly to
#' the console. It is useful when:
#'
#' \itemize{
#'   \item an image appears to contain a barcode but nothing is decoded,
#'   \item ZXing detects one or more symbol-like regions but cannot decode them
#'         (e.g., checksum failures),
#'   \item you need to distinguish between “no barcode found” and
#'         “barcode found but unreadable,”
#'   \item you want human-readable insight into what ZXing saw.
#' }
#'
#' ## What the output shows
#'
#' For each candidate region detected by ZXing, the function prints:
#'
#' \describe{
#'   \item{\code{format}}{The detected symbology (e.g., \code{QRCode},
#'         \code{Code39}, \code{RMQRCode}, \code{MicroQRCode}).}
#'   \item{\code{text}}{Decoded content; may be empty on failure.}
#'   \item{\code{error}}{ZXing error message, such as checksum failures.}
#'   \item{\code{valid}}{\code{TRUE} if decoding succeeded; \code{FALSE} if a
#'         symbol was detected but undecodable.}
#' }
#'
#' ## When ZXing finds *no* candidate symbols
#'
#' If ZXing cannot find any symbol regions at all, it returns an empty list even
#' with \code{return_errors = TRUE}. In this case, the function prints a
#' clear diagnostic message:
#'
#' \preformatted{
#' File folder: <directory>
#'
#' No candidate symbols or decode errors returned by ZXing for image file:
#' Debug ZXing Barcode Detection for a Single Image
#'
#' See vignette section 6.2.1 for examples and interpretation.}
#'
#' @param path Character path to an image file.
#' @return Invisibly returns `NULL`, printing diagnostics to the console.
#' @export
debug_zxing_read <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  folder <- dirname(path)
  fname  <- basename(path)

  Image <- tryCatch(
    reticulate::import("PIL.Image", convert = FALSE),
    error = function(e) {
      warning("Failed to import Pillow: ", conditionMessage(e))
      NULL
    }
  )
  zxing <- tryCatch(
    reticulate::import("zxingcpp", convert = FALSE),
    error = function(e) {
      warning("Failed to import zxingcpp: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(Image) || is.null(zxing)) {
    message("Python modules missing; install pillow + zxingcpp.")
    return(invisible(NULL))
  }

  # Open image and normalise MPO → RGB if needed
  img_py <- tryCatch(
    {
      img <- Image$open(path)
      if (inherits(img, "PIL.MpoImagePlugin.MpoImageFile")) {
        img <- img$convert("RGB")
      }
      img
    },
    error = function(e) {
      warning("Pillow failed to open image: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(img_py)) {
    cat("\nFile folder:", folder, "\n\n")
    cat("Image could not be opened: ", fname, "\n\n", sep = "")
    return(invisible(NULL))
  }

  # Call ZXing with return_errors = TRUE
  res_py <- tryCatch(
    zxing$read_barcodes(
      img_py,
      try_rotate    = TRUE,
      try_downscale = TRUE,
      return_errors = TRUE
    ),
    error = function(e) {
      warning("zxing.read_barcodes failed: ", conditionMessage(e))
      NULL
    }
  )

  # Case A: no candidate symbols at all
  if (is.null(res_py) || reticulate::py_len(res_py) == 0L) {
    cat("\nFile folder:", folder, "\n\n")
    cat(
      "No candidate symbols or decode errors returned by ZXing for image file: ",
      fname, ". This usually means ZXing could not find any barcode/QR region at all.\n\n",
      sep = ""
    )
    cat(
      "Try the following functions for (possibly) more information:\n",
      "  - detect_codes_all_zxing(path)\n",
      "  - decode_hierarchical_zxing(path)\n\n",
      "If these do not work, visually inspect the image — the symbol(s) may be\n",
      "too small, cropped, low‑contrast, blurred, rippled, or otherwise unreadable.\n",
      sep = ""
    )
    return(invisible(NULL))
  }

  # Case B: one or more symbol-like regions
  res <- reticulate::py_to_r(res_py)
  n   <- length(res)

  cat("ZXing returned", n, "result object(s).\n")

  for (i in seq_along(res)) {
    r <- res[[i]]
    fmt   <- tryCatch(as.character(r$format), error = function(e) NA)
    text  <- tryCatch(as.character(r$text),   error = function(e) NA)
    error <- tryCatch(as.character(r$error),  error = function(e) NA)
    valid <- tryCatch(as.character(r$valid),  error = function(e) NA)

    cat("\n--- Result", i, "---\n")
    cat(" format :", fmt,  "\n")
    cat(" text   :", text, "\n")
    cat(" error  :", error, "\n")
    cat(" valid  :", valid, "\n")
  }

  invisible(NULL)
}
