#' Hierarchical ZXing Decode: Full, Halves, Thirds, then 3x3 Grid
#'
#' `decode_hierarchical_zxing()` applies a hierarchical cropping strategy to
#' locate barcodes and QR codes in difficult images while retaining **all**
#' decoded symbols in long format (one row per symbol).
#'
#' All five stages are always executed:
#' \enumerate{
#'   \item Full image.
#'   \item Top half and bottom half.
#'   \item Top / middle / bottom thirds (horizontal bands).
#'   \item Sliding-window 3×3 grid with 50 % overlap.
#'   \item 2× upscaled greyscale + autocontrast (PIL).
#' }
#'
#' Unique barcodes (identified by decoded text) are accumulated across every
#' stage: a barcode seen at an earlier stage is not duplicated when the same
#' text is re-detected at a later stage.  The final `index` column numbers the
#' unique barcodes 1, 2, 3, … in the order they were first discovered.
#'
#' Running all stages is necessary because some barcodes in an image are only
#' detectable at a particular resolution or crop.  For example, Stage 1 (full
#' image) may yield one barcode while Stage 5 (upscaled) recovers two further
#' barcodes that were too small for ZXing to decode at native resolution.
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
#'   \item{index}{Integer index (1, 2, 3, …) of each unique barcode across all
#'     stages, in the order first discovered.}
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
#' @importFrom tools file_ext
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

  # ---- HEIC/HEIF pre-conversion ----
  # Decoding a HEIC image requires decompressing the HEVC bitstream each time
  # the file is opened.  Without pre-conversion, Stage 1 (PIL direct), Stages
  # 2-4 (magick crop loop), and Stage 5 (PIL upscale) each open the file
  # independently — three separate HEVC decompressions per image.
  #
  # Converting to a lossless PNG once at the start (via magick, which
  # automatically applies the EXIF orientation tag) collapses those three
  # decompressions into a single one.  Subsequent stages read the pre-rotated
  # PNG, which PIL and magick both open in milliseconds.
  #
  # Confirmed performance on a representative HEIC (3024×4032 post-rotation):
  #   Stage 1 direct HEIC decode: 27.2 s
  #   magick HEIC→PNG conversion: 10.9 s  (one-time cost, replaces 3 × HEVC)
  #   Stage 1 decode on PNG:       1.3 s
  #   Net saving for Stage 1 alone: ~15 s; total pipeline gain is larger
  #   because Stages 2-5 also benefit from the pre-converted PNG.
  #
  # If the conversion fails for any reason the function falls back to using
  # the original path, preserving the pre-existing behaviour.
  heic_exts <- c("heic", "heif")
  ext_lower  <- tolower(tools::file_ext(path))
  work_path  <- path

  if (ext_lower %in% heic_exts) {
    tmp_conv <- tempfile(fileext = ".png")
    conv_ok  <- tryCatch({
      conv_img <- magick::image_read(path)   # auto-applies EXIF orientation
      magick::image_write(conv_img, path = tmp_conv, format = "png")
      TRUE
    }, error = function(e) FALSE)
    if (conv_ok) {
      work_path <- tmp_conv
      on.exit(unlink(tmp_conv), add = TRUE)
    }
  }

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
  
  # ---- Accumulator: collect unique barcodes across ALL stages ----
  # The original "stop at first success" design missed barcodes that only
  # became decodable at later stages (e.g. a barcode too small for Stage 1
  # but visible in a Stage 4 tile, while other barcodes are only readable
  # after the Stage 5 upscale).  Running every stage and keeping only texts
  # not yet seen guarantees that all barcodes in an image are reported,
  # regardless of which stage first detects them.
  seen_texts  <- character(0)
  all_results <- list()

  .add_unique <- function(res) {
    if (is.null(res) || !.has_codes(res)) return(invisible(NULL))
    new_rows <- res[!res$text %in% seen_texts & !is.na(res$text), , drop = FALSE]
    if (nrow(new_rows) > 0L) {
      all_results[[length(all_results) + 1L]] <<- new_rows
      seen_texts <<- c(seen_texts, new_rows$text)
    }
  }

  # ---- 1. Full image ----
  full_res <- detect_codes_all_zxing(work_path)

  if (.has_codes(full_res)) {
    # Restore the original path in the file column: detect_codes_all_zxing()
    # stamps whatever path it was given; we always want the original file path.
    full_res$file      <- path
    full_res$source    <- "full"
    full_res$region_x  <- NA_integer_
    full_res$region_y  <- NA_integer_
    full_res$region_w  <- NA_integer_
    full_res$region_h  <- NA_integer_
    .add_unique(full_res)
  }

  # ---- 2+ require magick: load image once ----
  # Use work_path (the pre-converted PNG for HEIC, or the original for other
  # formats) — the image is already correctly oriented by the pre-conversion.
  img  <- magick::image_read(work_path)
  info <- magick::image_info(img)

  w <- info$width
  h <- info$height

  # Convenience for writing crops; temp file is deleted on function exit.
  .crop_and_decode <- function(x, y, cw, ch, src_label) {
    spec <- sprintf("%dx%d+%d+%d", cw, ch, x, y)
    crop <- magick::image_crop(img, spec)
    tmp  <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)
    magick::image_write(crop, tmp)
    .decode_region(tmp, src_label, x, y, cw, ch)
  }

  # ---- 2. Top & bottom halves ----
  half_h   <- floor(h / 2)
  top_y    <- 0L
  bottom_y <- h - half_h

  .add_unique(.crop_and_decode(0L, top_y,    w, half_h, "half_top"))
  .add_unique(.crop_and_decode(0L, bottom_y, w, half_h, "half_bottom"))

  # ---- 3. Top / middle / bottom thirds ----
  third_h <- floor(h / 3)

  y_top <- 0L
  y_mid <- third_h
  y_bot <- h - third_h

  .add_unique(.crop_and_decode(0L, y_top, w, third_h, "third_top"))
  .add_unique(.crop_and_decode(0L, y_mid, w, third_h, "third_mid"))
  .add_unique(.crop_and_decode(0L, y_bot, w, third_h, "third_bottom"))

  # ---- 4. Sliding-window grid with 50 % overlap ----
  # A non-overlapping 3x3 grid can split a small label barcode across a tile
  # boundary so no single tile contains the complete symbol. Stepping by
  # tile/2 guarantees every point is covered by at least one complete tile.
  tile_w <- floor(w / 3)
  tile_h <- floor(h / 3)
  step_x <- max(1L, floor(tile_w / 2L))
  step_y <- max(1L, floor(tile_h / 2L))

  x_starts <- unique(c(seq(0L, w - tile_w, by = step_x), w - tile_w))
  y_starts <- unique(c(seq(0L, h - tile_h, by = step_y), h - tile_h))

  for (yi in seq_along(y_starts)) {
    for (xi in seq_along(x_starts)) {
      x  <- x_starts[xi]
      y  <- y_starts[yi]
      cw <- min(tile_w, w - x)
      ch <- min(tile_h, h - y)
      if (cw < 50L || ch < 50L) next

      src_label <- sprintf("grid_%d_%d", yi, xi)
      .add_unique(.crop_and_decode(x, y, cw, ch, src_label))
    }
  }

  # ---- 5. Grayscale + autocontrast + 2× upscale fallback (PIL) ----
  # Barcodes whose modules are too small at native resolution for ZXing to pass
  # checksum validation can often be recovered by upscaling: more pixels per
  # module → fewer ambiguous bit reads.  PIL is used (not magick) because PIL's
  # 8-bit greyscale PNG output is what ZXing reliably handles.
  up_res <- tryCatch({
    Image_up    <- reticulate::import("PIL.Image",    convert = FALSE)
    ImageOps_up <- reticulate::import("PIL.ImageOps", convert = FALSE)
    # Register HEIC/HEIF support if pillow-heif is installed
    tryCatch({
      ph <- reticulate::import("pillow_heif", convert = FALSE)
      ph$register_heif_opener()
    }, error = function(e) NULL)
    img_pil  <- Image_up$open(work_path)
    pil_size <- reticulate::py_to_r(img_pil$size)
    w_up     <- as.integer(pil_size[[1]] * 2L)
    h_up     <- as.integer(pil_size[[2]] * 2L)
    img_gray <- img_pil$convert("L")
    img_ac   <- ImageOps_up$autocontrast(img_gray)
    img_up2  <- img_ac$resize(reticulate::tuple(w_up, h_up), Image_up$LANCZOS)
    tmp_up   <- tempfile(fileext = ".png")
    img_up2$save(tmp_up)
    res <- .decode_region(tmp_up, "upscaled", NA_integer_, NA_integer_,
                          NA_integer_, NA_integer_)
    file.remove(tmp_up)
    res
  }, error = function(e) NULL)

  .add_unique(up_res)

  # ---- Combine results and re-number index sequentially (1, 2, 3, …) ----
  if (length(all_results) == 0L) {
    return(tibble::tibble(
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
    ))
  }

  out        <- dplyr::bind_rows(all_results)
  out$index  <- seq_len(nrow(out))
  out
}