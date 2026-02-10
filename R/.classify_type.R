#' Classify decoded barcode/QR text into semantic categories
#'
#' Internal helper function used by the barcode/QR decoding pipeline.
#'
#' This function examines the decoded text string returned by ZXing and assigns
#' a semantic category based on predefined pattern rules. It is vectorised over
#' its input and returns a character vector of the same length.
#'
#' Classification rules (case-insensitive):
#'
#' * `"scion"` — text begins with `"SC"`.
#' * `"plant-genotype"` — text begins with `"RS"`.
#' * `"location"` — the first three characters follow the pattern:
#'   one letter followed by two digits (e.g., `"A07"`, `"b12"`).
#' * `"scion-genotype"` — any other non-empty text that does not match the
#'   patterns above.
#' * `NA` — if the input text is `NA` or an empty string.
#'
#' These categories are designed to support downstream interpretation of
#' decoded symbols, including distinguishing plant genotypes, scion material,
#' location markers, and other identifier classes used in the imaging workflow.
#'
#' @param x Character vector of decoded text values.
#'
#' @return A character vector of the same length as `x`, containing the inferred
#'   type for each element.
#'
#' @keywords internal
#' @noRd
.classify_type <- function(x) {
  # x is a character vector
  x_clean <- ifelse(is.na(x), "", x)
  
  # logical masks
  is_empty    <- x_clean == ""
  starts_SC   <- grepl("^SC", x_clean, ignore.case = TRUE)
  starts_RS   <- grepl("^RS", x_clean, ignore.case = TRUE)
  # letter + two digits for the first 3 chars
  is_location <- grepl("^[A-Za-z][0-9][0-9]", x_clean)
  
  out <- rep(NA_character_, length(x_clean))
  
  # Order of precedence:
  # 1) SC -> scion
  # 2) RS -> plant-genotype
  # 3) letter + 2 digits -> location
  # 4) other non-empty -> scion-genotype
  
  out[!is_empty & starts_SC]         <- "scion"
  out[!is_empty & !starts_SC & starts_RS] <- "plant-genotype"
  out[!is_empty & !starts_SC & !starts_RS & is_location] <- "location"
  out[!is_empty & !starts_SC & !starts_RS & !is_location] <- "scion-genotype"
  
  out
}
