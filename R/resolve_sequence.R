#' Resolve Decoded Barcodes Across Sequentially Numbered Image Files
#'
#' `resolve_sequence()` takes a **long‑format** data frame (typically produced
#' by [resolve_folder_sequence()]) in which each image contributes one or more
#' rows (one per decoded barcode or QR symbol) and applies **stateful sequence
#' logic** to infer a `resolved_text` value for every image in a numbered series.
#'
#' This function is useful when:
#' \itemize{
#'   \item some images decode successfully while others in the same numbered
#'         sequence fail (e.g., blurred or occluded tags),
#'   \item you want to carry forward a known decoded value to subsequent images
#'         that should share the same identifier,
#'   \item filenames follow a `PREFIX_NUMBER` scheme such as `IMG_0207.JPG`
#'         or `DSC_0001.JPG`.
#' }
#'
#' ## Filename parsing
#'
#' Each `file_name` is parsed into a `prefix` and a `number` using the pattern
#' \preformatted{
#'   ^([A-Za-z]+)_?([0-9]+)$
#' }
#'
#' This allows filenames with or without an underscore (e.g. `"IMG_0207.JPG"`,
#' `"IMG0207.JPG"`). Filenames that do not match produce `NA` for both
#' `prefix` and `number`.
#'
#' ## Resolution algorithm
#'
#' The algorithm proceeds in four stages:
#'
#' \enumerate{
#'   \item **Per‑image collapse**:
#'         All rows sharing the same `file_name` are grouped. The first
#'         non‑`NA` value of `text` becomes the file‑level `decoded_text`.
#'         If all values are `NA`, then `decoded_text` is `NA`.
#'
#'   \item **Ordering**:
#'         Images are ordered lexicographically by `(prefix, number)`.
#'
#'   \item **Stateful carry‑forward**:
#'         A running state is propagated across each prefix‑number sequence:
#'
#'         \itemize{
#'           \item If `decoded_text` is non‑`NA`, it becomes the new state.
#'           \item If `decoded_text` is `NA` *and* the prefix matches and the
#'                 number increments by +1, the state is inherited.
#'           \item Otherwise, the state resets to `"unknown"`.
#'         }
#'
#'         The result for each image is stored in `resolved_text`.
#'
#'   \item **Expansion back to long format**:
#'         The `resolved_text` value is joined back to **all rows** for the
#'         same `file_name`, preserving all existing columns (including
#'         additional fields produced by hierarchical decoding such as
#'         `source` or region coordinates).
#' }
#'
#'## Output
#'
#' The returned data frame contains all original columns plus:
#' \describe{
#'   \item{prefix}{Parsed alphabetic filename prefix, or `NA` if unavailable.}
#'   \item{number}{Parsed numeric sequence number, or `NA` if unavailable.}
#'   \item{resolved_text}{Propagated decoded value for the image.}
#' }
#'
#' No rows are dropped. Every input row receives a `resolved_text` value.
#'
#' @param df A long-format data frame containing at least:
#'   \itemize{
#'     \item `file_name` – the image filename,
#'     \item `text` – decoded text from barcode/QR symbols; may be `NA`.
#'   }
#'   All other columns are preserved.
#'
#' @return A tibble identical to `df` but with added `prefix`, `number`,
#'   and `resolved_text` columns.
#'
#' @examples
#' df <- tibble::tibble(
#'   file_name = c("IMG_0001.JPG", "IMG_0001.JPG", "IMG_0002.JPG", "IMG_0004.JPG"),
#'   text      = c("A1", NA, NA, "B2")
#' )
#'
#' resolve_sequence(df)$resolved_text
#' #> [1] "A1" "A1" "A1" "B2"
#'
#' @importFrom dplyr group_by summarise left_join mutate arrange
#' @export
resolve_sequence <- function(df) {
  
  if (!"file_name" %in% names(df)) {
    stop("resolve_sequence() requires a 'file_name' column.")
  }
  
  # ---- extract prefix + number from filename ----
  parse_sequence <- function(fname) {
    stem <- tools::file_path_sans_ext(basename(fname))
    m <- regmatches(stem, regexpr("^([A-Za-z]+)_([0-9]+)$", stem))
    if (length(m) == 0 || m == "") {
      return(list(prefix = NA_character_, number = NA_integer_))
    }
    parts <- strsplit(stem, "_")[[1]]
    list(prefix = parts[1], number = as.integer(parts[2]))
  }
  
  seq_info <- lapply(df$file_name, parse_sequence)
  df$prefix <- vapply(seq_info, `[[`, "prefix", FUN.VALUE = character(1))
  df$number <- vapply(seq_info, `[[`, "number", FUN.VALUE = integer(1))
  
  # ---- collapse long-format rows per image ----
  single_image <- df |>
    dplyr::group_by(file_name, prefix, number) |>
    dplyr::summarise(
      decoded_text = {
        x <- text
        if (all(is.na(x))) NA_character_ else x[!is.na(x)][1]
      },
      .groups = "drop"
    )
  
  # ---- order by filename sequence ----
  single_image <- dplyr::arrange(single_image, prefix, number)
  
  # ---- apply carry-forward logic ----
  single_image$resolved_text <- NA_character_
  current_state  <- NA_character_
  current_prefix <- NA_character_
  last_number    <- NA_integer_
  
  for (i in seq_len(nrow(single_image))) {
    dt <- single_image$decoded_text[i]
    px <- single_image$prefix[i]
    nm <- single_image$number[i]
    
    if (!is.na(dt)) {
      current_state  <- dt
      current_prefix <- px
      last_number    <- nm
      single_image$resolved_text[i] <- current_state
      
    } else if (!is.na(current_state) &&
               px == current_prefix &&
               !is.na(nm) && !is.na(last_number) &&
               nm - last_number == 1) {
      single_image$resolved_text[i] <- current_state
      last_number <- nm
      
    } else {
      current_state  <- "unknown"
      current_prefix <- px
      last_number    <- nm
      single_image$resolved_text[i] <- current_state
    }
  }
  
  # ---- join back to long format ----
  df <- dplyr::left_join(
    df,
    single_image[, c("file_name", "resolved_text")],
    by = "file_name"
  )
  
  df
}

