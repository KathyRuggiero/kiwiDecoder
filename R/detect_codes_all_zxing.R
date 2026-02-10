#' Detect all barcodes and QR codes in a single image using ZXing-CPP
#'
#' This function loads an image using Python's Pillow (`PIL.Image`) and
#' decodes all detectable barcodes or QR codes using the `zxingcpp`
#' Python bindings. Each detected symbol is returned as one row in the
#' resulting tibble, including its decoded text, parsed format, and
#' a semantic classification of the decoded value.
#'
#' Internally, the function:
#' \itemize{
#'   \item normalises the file path,
#'   \item imports `PIL.Image` and `zxingcpp` via \pkg{reticulate},
#'   \item opens the image using Pillow,
#'   \item calls `zxingcpp.read_barcodes()` on the Python image object,
#'   \item drops any results with `valid == FALSE` when that field is present,
#'   \item discards GS1 DataBar results (which are not used in this workflow),
#'   \item constructs a tibble of results, and
#'   \item assigns a semantic `type` using the internal `.classify_type()` helper.
#' }
#'
#' For every input image, the function returns **at least one row**:
#'
#' \itemize{
#'   \item If at least one usable symbol is found, each symbol is returned as
#'         a separate row with decoded fields populated.
#'   \item If no usable symbols are found for any reason (no candidates at all,
#'         only invalid symbols, only unsupported formats such as DataBar,
#'         Python import errors, or image open/decoding failures), the function
#'         returns a single-row tibble in which:
#'         \code{file} is the image path, and
#'         \code{index}, \code{code_format}, \code{text}, and \code{type}
#'         are all \code{NA}.
#' }
#'
#' This design guarantees per-file traceability in downstream workflows: the
#' output always contains at least one row per input image, even when no
#' usable barcodes are present.
#'
#' @param path Character scalar. Path to an image file.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{file}{Full normalised file path to the image.}
#'   \item{index}{Integer index (1, 2, ...) for symbols detected in the image,
#'     or \code{NA} when no usable symbol is found.}
#'   \item{code_format}{Decoded barcode format (e.g., `"PDF417"`, `"QRCode"`,
#'     `"DataMatrix"`, `"Code39"`), or \code{NA} when no usable symbol is found.}
#'   \item{text}{Decoded text content (character), or \code{NA} when no usable
#'     symbol is found.}
#'   \item{type}{Semantic classification of the decoded text, or \code{NA} when
#'     no usable symbol is found.}
#' }
#'
#' @examples
#' \dontrun{
#' # Example 1: image with genuine barcodes/QR codes
#' img_good <- "N:/Projects/Stage2 Clonal Trials/Images/Male Photos 2024_25/IMG_0001.JPG"
#' detect_codes_all_zxing(img_good)
#'
#' # Typical output:
#' # A tibble: 3 × 5
#' #   file         index code_format text    type
#' #   <chr>        <int> <chr>       <chr>   <chr>
#' # 1 N:/...       1     QRCode      RS0270  plant-genotype
#' # 2 N:/...       2     Code39      MC079   scion
#' # 3 N:/...       3     QRCode      SC-0123 scion
#'
#' # Example 2: image with no usable barcodes (only DataBar or no candidates)
#' img_bad <- "N:/Projects/.../Hannah Grafting 14 Dec 2023 (7).JPG"
#' detect_codes_all_zxing(img_bad)
#'
#' # Returns a single row with NA in the barcode columns:
#' # A tibble: 1 × 5
#' #   file                                 index code_format text type
#' #   <chr>                                <int> <chr>       <chr> <chr>
#' # 1 N:/.../Hannah Grafting 14 Dec 2023…    NA NA          NA    NA
#' }
#'
#' @importFrom reticulate import
#' @importFrom tibble tibble
#' @importFrom purrr map_chr map_lgl
#' @export
detect_codes_all_zxing <- function(path) {
  # Normalise file path
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  
  # A canonical "no usable result" row
  na_row <- tibble::tibble(
    file        = path,
    index       = NA_integer_,
    code_format = NA_character_,
    text        = NA_character_,
    type        = NA_character_
  )
  
  # Import Python modules inside the function so this works in parallel workers
  Image <- tryCatch(
    reticulate::import("PIL.Image", convert = FALSE),
    error = function(e) {
      warning(
        "Failed to import Python module 'PIL.Image'. ",
        "Check that the 'Pillow' package is installed in the active Python environment. (",
        conditionMessage(e), ")"
      )
      NULL
    }
  )
  
  zxing <- tryCatch(
    reticulate::import("zxingcpp", convert = TRUE),
    error = function(e) {
      warning(
        "Failed to import Python module 'zxingcpp'. ",
        "Check that the 'zxingcpp' Python package is installed in the active Python environment. (",
        conditionMessage(e), ")"
      )
      NULL
    }
  )
  
  # If imports failed, return NA row
  if (is.null(Image) || is.null(zxing)) {
    return(na_row)
  }
  
  # Open the image with Pillow
  img_py <- tryCatch(
    Image$open(path),
    error = function(e) {
      warning("Failed to open image with PIL: ", path, " (", conditionMessage(e), ")")
      NULL
    }
  )
  
  # If image cannot be opened, return NA row
  if (is.null(img_py)) {
    return(na_row)
  }
  
  # Run ZXing on the PIL image object
  res <- tryCatch(
    zxing$read_barcodes(
      img_py,
      try_rotate    = TRUE,
      try_downscale = TRUE
    ),
    error = function(e) {
      warning("zxing.read_barcodes failed on: ", path, " (", conditionMessage(e), ")")
      NULL
    }
  )
  
  # If ZXing found no symbol candidates at all, return NA row
  if (is.null(res) || length(res) == 0L) {
    return(na_row)
  }
  
  # ZXing saw at least one candidate (even if all end up invalid/unwanted)
  had_any_results <- length(res) > 0L
  
  # Filter out explicitly invalid ZXing results when the 'valid' flag exists.
  # If 'valid' is missing or NA, keep the result (be conservative).
  valid_vec <- purrr::map_lgl(
    res,
    function(x) {
      v <- tryCatch(x$valid, error = function(e) NA)
      if (is.null(v) || is.na(v)) {
        TRUE  # keep when validity is unknown
      } else {
        isTRUE(as.logical(v))
      }
    }
  )
  res <- res[valid_vec]
  
  # If all candidates were invalid but ZXing saw something, return NA row
  if (length(res) == 0L && had_any_results) {
    return(na_row)
  }
  
  # Extract text and format from the remaining results
  text_vec <- purrr::map_chr(res, ~ as.character(.x$text))
  fmt_raw  <- purrr::map_chr(res, ~ as.character(.x$format))
  
  tib <- tibble::tibble(
    file        = path,
    index       = seq_along(res),
    code_format = sub("^BarcodeFormat\\.", "", fmt_raw),
    text        = text_vec
  )
  
  # Drop DataBar decodes explicitly (you do not use DataBar in this workflow)
  bad_formats <- c("DataBar", "DATA_BAR", "GS1_DATABAR")
  tib <- tib[!(tib$code_format %in% bad_formats), , drop = FALSE]
  
  # If ZXing only found unwanted formats (e.g. DataBar), return NA row
  if (nrow(tib) == 0L) {
    return(na_row)
  }
  
  # Assign semantic type using internal classifier
  tib$type <- .classify_type(tib$text)
  
  tib
}
