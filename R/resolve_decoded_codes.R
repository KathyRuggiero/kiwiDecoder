#' Normalise Decoded Barcode / QR Code Output
#'
#' `resolve_decoded_codes()` converts the raw output from
#' `detect_codes_all_zxing()` into a clean, named structure suitable for
#' downstream joins and state resolution.
#'
#' This function does **not** apply sequencing rules, carry-forward logic,
#' or prefix resets. It only interprets what was decoded in a single image.
#'
#' @param decoded Raw output from `detect_codes_all_zxing()`.
#'
#' @return
#' A named list of decoded values (e.g. `genotype`, `location`), or `NULL`
#' if no usable codes were detected.
#'
#' @details
#' This function is the *single point of truth* for how barcode payloads
#' are interpreted. If barcode formats change, only this function should
#' need modification.
#'
#' @examples
#' \dontrun{
#' raw <- detect_codes_all_zxing("IMG_0001.jpg")
#' resolve_decoded_codes(raw)
#' }
#'
#' @export
resolve_decoded_codes <- function(decoded) {
  if (is.null(decoded) || length(decoded) == 0) {
    return(NULL)
  }
  
  # Example: assume decoded is a data frame with a `text` column
  if (is.data.frame(decoded) && "text" %in% names(decoded)) {
    values <- unique(decoded$text)
    
    return(list(
      decoded_values = values
    ))
  }
  
  NULL
}
