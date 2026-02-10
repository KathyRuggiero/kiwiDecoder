#' Hierarchical ZXing Decode: Full, Halves, Thirds, then 3x3 Grid
#'
#' `decode_hierarchical_zxing()` applies a hierarchical cropping strategy to
#' locate barcodes and QR codes in difficult images while retaining **all**
#' decoded symbols in long format (one row per symbol).
#'
#' The search order is:
#' \enumerate{
#'   \item Full image.
#'   \item If no usable symbols are found, top half and bottom half.
#'   \item If still none, top/middle/bottom thirds (horizontal bands).
#'   \item If still none, a 3x3 grid over the whole image (and then stop,
#'         whether or not codes are found).
#' }
#'
#' At each stage, if at least one usable barcode/QR code is found, the function
#' returns all symbols from that stage and does not proceed further (except for
#' the final 3x3 stage, where it always returns after checking all tiles).
#'
#' A "usable" symbol is any row from [detect_codes_all_zxing()] where both
#' `code_format` and `text` are non-NA.
#'
#' @param path Character scalar. Path to an image file.
#'
#' @return
#' A tibble with columns:
#' \describe{
#'   \item{file}{Path to the original image file.}
#'   \item{index}{Integer index of the symbol within the crop (as reported by
#'     [detect_codes_all_zxing()]).}
#'   \item{code_format}{Barcode format (e.g. `"PDF417"`, `"QRCode"`,
#'     `"DataMatrix"`, `"Code39"`).}
#'   \item{text}{Decoded text content.}
#'   \item{type}{Semantic classification of the decoded text, as returned by
#'     [detect_codes_all_zxing()] (e.g. `"scion"`, `"plant-genotype"`,
#'     `"location"`, `"scion-genotype"`).}
#'   \item{source}{Character flag indicating which region yielded the symbol:
#'     `"full"`, `"half_top"`, `"half_bottom"`, `"third_top"`, `"third_mid"`,
#'     `"third_bottom"`, or `"grid_r_c"` for grid row/column (e.g.
#'     `"grid_2_3"`).}
#'   \item{region_x}{Left offset (in pixels) of the crop region within the
#'     original image. NA for the full image.}
#'   \item{region_y}{Top offset (in pixels) of the crop region.}
#'   \item{region_w}{Width (in pixels) of the crop region.}
#'   \item{region_h}{Height (in pixels) of the crop region.}
#' }
#'
#' If no usable symbols are found in any region, a single-row tibble is
#' returned with `file = path` and `index`, `code_format`, `text`, `type`,
#' `source`, `region_x`, `region_y`, `region_w`, `region_h` all `NA`.
#'
#' @details
#' This helper is designed to complement [detect_codes_all_zxing()] in cases
#' where:
#' \itemize{
#'   \item codes may be small relative to the full image (e.g., many pots on a
#'         greenhouse bench),
#'   \item codes are not always centred or may cluster in particular bands, and
#'   \item an image can contain multiple barcodes/QR codes.
#' }
#'
#' It does **not** change the underlying ZXing behaviour: for each region that
#' decodes successfully, all symbols reported by ZXing are preserved in long
#' format. The helper simply chooses which regions to present to ZXing in a
#' structured search order and annotates each symbol with region metadata.
#'
#' @seealso [detect_codes_all_zxing()] for the core decoding function,
#'   and [debug_zxing_read()] for low-level ZXing diagnostics.
#'
#' @importFrom magick image_read image_info image_crop image_write
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows mutate
#'
#' @examples
#' \dontrun{
#'   img <- "N:/Projects/Stage2 Clonal Trials/Images/Greenhouse/bench_001.JPG"
#'   decode_hierarchical_zxing(img)
#' }
#'
#' @export
decode_hierarchical_zxing <- function(path) {
  
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  
  # Helper: does this tibble contain any usable decoded symbols?
  .has_codes <- function(tbl) {
    !is.null(tbl) &&
      is.data.frame(tbl) &&
      nrow(tbl) > 0L &&
      any(!is.na(tbl$code_format) & !is.na(tbl$text))
  }
  
  # Helper: call detect_codes_all_zxing() on a given temp file and
  # enrich with region metadata, dropping NA-only rows.
  .decode_region <- function(tmp_path, src, x, y, w, h) {
    res <- detect_codes_all_zxing(tmp_path)
    
    if (!.has_codes(res)) {
      return(NULL)
    }
    
    # Use the original file path, not the temporary crop path
    res$file      <- path
    res$source    <- src
    res$region_x  <- x
    res$region_y  <- y
    res$region_w  <- w
    res$region_h  <- h
    
    res
  }
  
  # ---- 1. Full image ----
  full_res <- detect_codes_all_zxing(path)
  
  if (.has_codes(full_res)) {
    full_res$file      <- path
    full_res$source    <- "full"
    full_res$region_x  <- NA_integer_
    full_res$region_y  <- NA_integer_
    full_res$region_w  <- NA_integer_
    full_res$region_h  <- NA_integer_
    return(full_res)
  }
  
  # ---- 2+ require magick: load image once ----
  img  <- magick::image_read(path)
  info <- magick::image_info(img)
  
  w <- info$width
  h <- info$height
  
  # Convenience for writing crops
  .crop_and_decode <- function(x, y, cw, ch, src_label) {
    spec <- sprintf("%dx%d+%d+%d", cw, ch, x, y)
    crop <- magick::image_crop(img, spec)
    tmp  <- tempfile(fileext = ".png")
    magick::image_write(crop, tmp)
    .decode_region(tmp, src_label, x, y, cw, ch)
  }
  
  # ---- 2. Top & bottom halves ----
  half_h   <- floor(h / 2)
  top_y    <- 0L
  bottom_y <- h - half_h
  
  top_res <- .crop_and_decode(0L, top_y, w, half_h, "half_top")
  bot_res <- .crop_and_decode(0L, bottom_y, w, half_h, "half_bottom")
  
  if (!is.null(top_res) || !is.null(bot_res)) {
    out <- dplyr::bind_rows(
      list(top_res, bot_res)[!vapply(list(top_res, bot_res), is.null, logical(1))]
    )
    return(out)
  }
  
  # ---- 3. Top / middle / bottom thirds ----
  third_h <- floor(h / 3)
  
  y_top <- 0L
  y_mid <- third_h
  y_bot <- h - third_h
  
  top3_res <- .crop_and_decode(0L, y_top, w, third_h, "third_top")
  mid3_res <- .crop_and_decode(0L, y_mid, w, third_h, "third_mid")
  bot3_res <- .crop_and_decode(0L, y_bot, w, third_h, "third_bottom")
  
  if (!is.null(top3_res) || !is.null(mid3_res) || !is.null(bot3_res)) {
    out <- dplyr::bind_rows(
      list(top3_res, mid3_res, bot3_res)[
        !vapply(list(top3_res, mid3_res, bot3_res), is.null, logical(1))
      ]
    )
    return(out)
  }
  
  # ---- 4. 3x3 grid (always final stage) ----
  tile_w <- floor(w / 3)
  tile_h <- floor(h / 3)
  
  grid_results <- list()
  idx <- 1L
  
  for (row in 1:3) {
    for (col in 1:3) {
      
      x <- (col - 1L) * tile_w
      y <- (row - 1L) * tile_h
      
      # Ensure last row/col extend to the edge
      if (col == 3L) tile_w_eff <- w - x else tile_w_eff <- tile_w
      if (row == 3L) tile_h_eff <- h - y else tile_h_eff <- tile_h
      
      src_label <- sprintf("grid_%d_%d", row, col)
      
      tile_res <- .crop_and_decode(x, y, tile_w_eff, tile_h_eff, src_label)
      
      if (!is.null(tile_res)) {
        grid_results[[idx]] <- tile_res
        idx <- idx + 1L
      }
    }
  }
  
  if (length(grid_results) > 0L) {
    return(dplyr::bind_rows(grid_results))
  }
  
  # ---- Nothing found anywhere: return single NA row (like detect_codes_all_zxing) ----
  tibble::tibble(
    file        = path,
    index       = NA_integer_,
    code_format = NA_character_,
    text        = NA_character_,
    type        = NA_character_,
    source      = NA_character_,
    region_x    = NA_integer_,
    region_y    = NA_integer_,
    region_w    = NA_integer_,
    region_h    = NA_integer_
  )
}