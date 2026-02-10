#' Decode All Images in a Single Folder (Non-Recursive)
#'
#' `scan_one_folder()` applies the high-throughput hierarchical decoder
#' [decode_hierarchical_zxing()] to every image in a folder (non-recursive),
#' returning all decoded barcode/QR-code symbols in **long format**.
#'
#' This helper is intended for **interactive testing, spot‑checks, and
#' development workflows** where you want to quickly decode all images in a
#' single directory without scanning subfolders or writing Excel indexes.
#'
#' The function:
#' \itemize{
#'   \item normalises the folder path,
#'   \item finds all `.jpg`, `.jpeg`, and `.png` files,
#'   \item applies [decode_hierarchical_zxing()] to each file,
#'   \item guarantees **one or more rows per image** (via hierarchical decoding),
#'   \item binds all symbol rows into a single tibble.
#' }
#'
#' The output contains **one row per detected symbol** (barcode or QR), with
#' additional metadata columns from the hierarchical decoder (e.g. `source`,
#' `region_x`, `region_y`, `region_w`, `region_h`).
#'
#' Images that contain no decodable symbols return a **single NA-row** as per
#' [decode_hierarchical_zxing()], ensuring traceability of all input files.
#'
#' @param folder Character path to a folder containing images.
#'
#' @return A tibble in long format, containing at least:
#' \describe{
#'   \item{file}{Original image file path.}
#'   \item{index}{Index of symbol within the crop.}
#'   \item{code_format}{Barcode/QR format.}
#'   \item{text}{Decoded text content.}
#'   \item{type}{Semantic classification of decoded text.}
#'   \item{source}{Region label indicating where the symbol was found (from
#'         [decode_hierarchical_zxing()]).}
#'   \item{region_x, region_y, region_w, region_h}{Crop region coordinates.}
#' }
#'
#' @examples
#' \dontrun{
#'   scan_one_folder("N:/Projects/Stage2 Clonal Trials/Images/ExampleFolder")
#' }
#'
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#'
#' @export
scan_one_folder <- function(folder) {
  folder <- normalizePath(folder, winslash = "/", mustWork = TRUE)
  
  img_files <- list.files(
    folder,
    pattern    = "\\.(jpg|jpeg|png)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(img_files) == 0L) {
    message("No image files found in: ", folder)
    return(tibble::tibble(
      file        = character(0),
      index       = integer(0),
      code_format = character(0),
      text        = character(0),
      type        = character(0),
      source      = character(0),
      region_x    = integer(0),
      region_y    = integer(0),
      region_w    = integer(0),
      region_h    = integer(0)
    ))
  }
  
  # --- High-throughput hierarchical decoding ---
  decoded_list <- purrr::map(img_files, decode_hierarchical_zxing)
  
  dplyr::bind_rows(decoded_list)
}